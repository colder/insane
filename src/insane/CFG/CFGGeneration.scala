package insane
package CFG

import utils._
import utils.Reporters._
import utils.Graphs.DotConverter

import scala.tools.nsc.Global

trait CFGGeneration extends CFGTreesDef { self: AnalysisComponent =>
  val global: Global
  val reporter: Reporter
  val settings: Settings

  import global._

  val CFG = CFGTrees

  class CFGGenerationPhase extends SubPhase {
    val name = "Generating CFGs"

    def run() {
      for(fun <- funDecls.values) {
        val cfg = convertASTToCFG(fun)

        val name = uniqueFunctionName(fun.symbol)
        if (settings.dumpCFG(safeFullName(fun.symbol))) {
          val dest = name+"-cfg.dot"

          reporter.info("Dumping CFG to "+dest+"...")
          new DotConverter(cfg, "CFG For "+name).writeFile(dest)
        }

        fun.setCFG(cfg)
      }
    }


    def convertASTToCFG(fun: AbsFunction): FunctionCFG = {
      import ExpressionExtractors._
      import StructuralExtractors._

      var labels    = Map[Symbol, (Vertex, List[Ident])]()
      var preLabels = Map[Symbol, (Vertex, Vertex, Apply)]()

      if (settings.displayFullProgress) {
        reporter.info("Converting CFG: "+uniqueFunctionName(fun.symbol)+"...")
      }
      object freshName {
        var count = 0

        def apply(prefix: String): String = {
          count = count + 1
          prefix + "#" + count
        }
      }
      def freshVariable(prefix: String = "v")  = new CFG.TempRef(freshName(prefix))


      val cfg = new FunctionCFG(freshVariable("retval") setTree fun.body)

      val unreachableVertex = cfg.newNamedVertex("unreachable")

      type Vertex = cfg.Vertex

      object Emit {
        private var pc: Vertex = cfg.entry
        def getPC: Vertex = pc 
        def setPC(v: Vertex) {
          pc = v
        }
          
        // emits a statement between two program points
        def statementBetween(from: Vertex, stat: CFG.Statement, to : Vertex) {
          cfg += (from, stat, to)
        } 

        // emits a statement from the current PC and sets the new PC after it
        def statement(stat: CFG.Statement) {
          val v = cfg.newVertex
          cfg += (pc, stat, v) 
          setPC(v)
        }     

        // emits a statement from the current PC to an existing program point
        def statementCont(stat: CFG.Statement, cont: Vertex) {
          cfg += (pc, stat, cont)
        }

        // emits an ''empty'' statement (equivalent to unconditional branch) from the current PC to an existing point
        def goto(cont: Vertex) {
          if (pc != unreachableVertex) {
            cfg += (pc, CFG.Skip, cont)
          }
        }
      }

      def identToRef(i: Ident): CFG.Ref = {
        if (i.symbol.isModule) {
          new CFG.ObjRef(i.symbol) setTree i
        } else {
          new CFG.SymRef(i.symbol) setTree i
        }
      }

      def convertSimpleExpr(tree: Tree): Option[CFG.SimpleValue] = tree match {
        case f @ Function(params, body) =>
          reporter.fatalError("Unnexpected Annon Function: "+f)
        case i : Ident =>
          Some(identToRef(i))
        case l : Literal =>
          Some(litToLit(l))
        case th @ This(name) =>

          def addThisRef(sym: Symbol): CFG.ThisRef = {
            val tr = CFG.ThisRef(th.symbol) setTree tree
            cfg.thisRefs += tr

            if (cfg.mainThisRef.symbol == NoSymbol || sym == fun.symbol.owner) {
              cfg.mainThisRef = tr
            }

            tr
          }

          val ref = if (cfg.thisRefs.isEmpty) {
            addThisRef(th.symbol)
          } else {
            cfg.thisRefs.find(_.symbol == th.symbol) match {
              case Some(tr) =>
                tr
              case None =>

                val tr = addThisRef(th.symbol)

                settings.ifDebug {
                  reporter.warn("Decrepency between multiple 'this' symbols: "+cfg.thisRefs.mkString(", "), th.pos)
                }

                tr
            }
          }

          Some(ref)
        case s : Super =>
          val sr = new CFG.SuperRef(s.symbol) setTree tree

          cfg.superRefs += sr

          Some(sr)

        case _ =>
          None
      }

      def convertTmpExpr(tree: Tree, prefix: String = "tmp"): CFG.SimpleValue = {
        convertSimpleExpr(tree) match {
          case Some(sv) =>
            sv
          case None =>
            val ref = freshVariable(prefix) setTree tree
            convertExpr(ref, tree)
            ref
        }
      }

      def convertExpr(to: CFG.Ref, tree: Tree) {
        tree match {
          case ArrayValue(tpt, elems) =>
            Emit.statement(new CFG.AssignNew(to, arrayType(tpt.tpe)) setTree tree)

            for ((elem, i) <- elems.zipWithIndex) {
              Emit.statement(new CFG.AssignApplyMeth(freshVariable("unused"), to, definitions.Array_update, List(new CFG.LongLit(i) setTree elem, convertTmpExpr(elem, "arrelem")), false) setTree elem)
            }

          case Block(stmts, expr) =>
            for (st <- stmts) {
              convertTmpExpr(st)
            }
            convertExpr(to, expr)

          case ExAssertEQExpression(lhs, rhs) =>
            Emit.statement(new CFG.AssertEQ(convertTmpExpr(lhs, "assertLHS"), convertTmpExpr(rhs, "assertRHS")) setTree tree)

          case ExAssertNEExpression(lhs, rhs) =>
            Emit.statement(new CFG.AssertNE(convertTmpExpr(lhs, "assertLHS"), convertTmpExpr(rhs, "assertRHS")) setTree tree)

          case v @ ValDef(_, _, _, rhs: Tree) =>
            val s = new CFG.SymRef(v.symbol) setTree v
            convertExpr(s, rhs)

          case i @ If(cond, then, elze) =>
            val whenTrue = cfg.newVertex
            val whenFalse = cfg.newVertex
            val endIf = cfg.newVertex
            decomposeBranches(cond, whenTrue, whenFalse)

            Emit.setPC(whenTrue)
            convertExpr(to, then)
            Emit.goto(endIf)

            Emit.setPC(whenFalse)
            convertExpr(to, elze)
            Emit.goto(endIf)

            Emit.setPC(endIf)
          case a @ DefDef(_, name, _, args, _, rhs) =>
          // ignore for now

          case a @ ExNew(sym, args) =>
            Emit.statement(new CFG.AssignNew(to, sym.owner.tpe) setTree a)
            Emit.statement(new CFG.AssignApplyMeth(freshVariable("unused") setTree a, to, sym, args.map(convertTmpExpr(_, "arg")), false) setTree a)

          case t @ Typed(ex, tpe) =>
            convertExpr(to, ex)

          case ad @ ApplyDynamic(o, args) =>
            val obj = convertTmpExpr(o, "obj")
            Emit.statement(new CFG.AssignApplyMeth(to, obj, ad.symbol, args.map(convertTmpExpr(_, "arg")), true) setTree ad)

          case t @ Throw(expr) =>
            convertTmpExpr(expr, "exception")
            settings.ifDebug {
              reporter.warn("Ignoring exception effects", t.pos)
            }
            Emit.goto(cfg.exit)
            Emit.setPC(unreachableVertex)

          case a @ Apply(s @ Select(o, meth), args) =>
            // We need to check for boolean short-circuiting methods, &&, ||

            if (s.symbol == definitions.Boolean_or || s.symbol == definitions.Boolean_and) {
              val whenTrue  = cfg.newVertex
              val whenFalse = cfg.newVertex
              val endIf     = cfg.newVertex

              decomposeBranches(a, whenTrue, whenFalse)

              Emit.setPC(whenTrue)
              Emit.statement(new CFG.AssignVal(to, new CFG.BooleanLit(true) setTree tree) setTree tree)
              Emit.goto(endIf)

              Emit.setPC(whenFalse)
              Emit.statement(new CFG.AssignVal(to, new CFG.BooleanLit(false) setTree tree) setTree tree)
              Emit.goto(endIf)

              Emit.setPC(endIf)
            } else {
              val obj = convertTmpExpr(o, "obj")
              Emit.statement(new CFG.AssignApplyMeth(to, obj, s.symbol, args.map(convertTmpExpr(_, "arg")), false) setTree a)
            }

          case ExWhile(cond, stmts) =>
            val beginWhile = Emit.getPC
            val whenTrue   = cfg.newNamedVertex("whenTrue")
            val endWhile   = cfg.newNamedVertex("endWhile")

            decomposeBranches(cond, whenTrue, endWhile)

            Emit.setPC(whenTrue)

            for (s <- stmts)
              convertTmpExpr(s)

            Emit.goto(beginWhile)

            Emit.setPC(endWhile)

          case ExDoWhile(cond, stmts) =>
            val beginWhile = Emit.getPC
            val endWhile   = cfg.newNamedVertex("endWhile")

            for (s <- stmts)
              convertTmpExpr(s)

            decomposeBranches(cond, beginWhile, endWhile)

            Emit.setPC(endWhile)

          case t @ Try(stmt, catches, finalizer) =>
            settings.ifDebug {
              reporter.warn("Ignoring try/catch effects", t.pos)
            }
            convertExpr(to, stmt)
            // execute it right after
            convertTmpExpr(finalizer, "finally")

          // Continuations
          case l @ LabelDef(name, idents, stmts) =>
            preLabels.get(l.symbol) match {
              case Some((contDef, contCall, ap)) =>
                // 1: we define the continuation
                Emit.goto(contDef)
                Emit.setPC(contDef)
                convertExpr(to, stmts)

                // 2: we go back at the place were the cont was called
                Emit.setPC(contCall)

                // 3: We assign args
                for ((a,i) <- ap.args zip idents) {
                  convertExpr(identToRef(i), a)
                }
                Emit.goto(contDef)

                preLabels -= l.symbol
              case None =>
                val v = cfg.newNamedVertex("lab("+name+")")
                labels += l.symbol -> ((v, idents))
                Emit.goto(v)
                Emit.setPC(v)
                convertExpr(to, stmts)
            }

          case a @ Apply(fun: Ident, args) =>
            labels.get(fun.symbol) match {
              case Some((v, idents)) =>
                // We assign args
                for ((a,i) <- args zip idents) {
                  convertExpr(identToRef(i), a)
                }
                // We goto
                Emit.goto(v)
                Emit.setPC(unreachableVertex)
              case None =>
                val contDef  = cfg.newNamedVertex("contDef")
                val contCall = Emit.getPC
                preLabels += fun.symbol -> (contDef, contCall, a)
                Emit.setPC(unreachableVertex)
            }

          case a @ Apply(ta @ TypeApply(s @ Select(o, meth), List(typ)), args) =>
            val obj = convertTmpExpr(o, "obj") match {
              case obj: CFG.Ref =>
                if (meth.toString == "$isInstanceOf") {
                  Emit.statement(new CFG.AssignTypeCheck(to, obj, typ.tpe) setTree a)
                } else if (meth.toString == "$asInstanceOf") {
                  Emit.statement(new CFG.AssignCast(to, obj, typ.tpe) setTree ta)
                } else {
                  reporter.error("Unknown TypeApply method: "+meth, a.pos)
                }
              case obj =>
                reporter.error("Invalid object reference type in: "+s, a.pos)
            }
          case Match(ta @ Typed(ex, tpt), cases) =>
            val expr = convertTmpExpr(ex, "matchEx")
            /* We transform:
            *  ex match {
            *    case a =>
            *        blockA
            *    ...
            *    case _ =>
            *        blockElse
            * }
            *
            * into:
            *
            *    if (ex == a) {
            *      BlockA
            *    } ...
            *    else {
            *      BlockElse
            *    }
            */
            val endMatch  = cfg.newNamedVertex("endMatch")

            for (cas <- cases) cas match {
              case CaseDef(l, _, body) if l.toString == "_" =>
                // else case
                convertExpr(to, body)
                Emit.goto(endMatch)

              case CaseDef(l : Literal, _, body) =>
                val beginCase = Emit.getPC
                Emit.statement(new CFG.Branch(new CFG.IfEqual(expr, litToLit(l))) setTree cas)
                convertExpr(to, body)
                Emit.goto(endMatch)

                Emit.setPC(beginCase)
                Emit.statement(new CFG.Branch(new CFG.IfNotEqual(expr, litToLit(l))) setTree cas)

              case _ =>
                reporter.error("Unhandled case in pattern matching: "+cas, cas.pos)
            }

            Emit.setPC(endMatch)


          case a @ Assign(s @ Select(o, field), rhs) =>
            val obj  = convertTmpExpr(o, "obj")
            val rhsV = convertTmpExpr(rhs, "rhs")

            obj match {
              case ref: CFG.Ref =>
                Emit.statement(new CFG.AssignFieldWrite(ref, s.symbol, rhsV) setTree tree)
              case _ =>
                reporter.error("Invalid value type for receiver in "+tree, a.pos)
            }

          case Assign(i @ Ident(name), rhs) =>
            convertExpr(identToRef(i), rhs)

          case s @ Select(o, field) =>
            convertTmpExpr(o, "obj") match {
              case obj: CFG.Ref =>
                Emit.statement(new CFG.AssignFieldRead(to, obj, s.symbol) setTree s)
              case obj =>
                reporter.error("Invalid object reference in select: "+s, s.pos)
            }
          case EmptyTree =>
          // ignore

          case Return(tre) =>
            convertExpr(cfg.retval, tre)
            Emit.goto(cfg.exit)
            Emit.setPC(unreachableVertex)

          case r =>
            convertSimpleExpr(r) match {
              case Some(sv: CFG.Unit) if Emit.getPC == unreachableVertex =>
                // ignore
              case Some(sv) =>
                Emit.statement(new CFG.AssignVal(to, sv) setTree tree)
              case _ =>
                reporter.warn("CFG: Unhandled Expression: "+tree+"("+tree.getClass+")", tree.pos)
            }
        }
      }

      def decomposeBranches(cond: Tree, whenTrue: Vertex, whenFalse: Vertex) {
        cond match {
          case ExAnd(lhs, rhs) =>
            // If true, go to rhs, if false, go to whenFalse
            val whenRhs = cfg.newVertex
            decomposeBranches(lhs, whenRhs, whenFalse)
            Emit.setPC(whenRhs)
            decomposeBranches(rhs, whenTrue, whenFalse)

          case ExOr(lhs, rhs) =>
            // If true, go to whenTrue, if false, try rhs
            val whenRhs = cfg.newVertex
            decomposeBranches(lhs, whenTrue, whenRhs)
            Emit.setPC(whenRhs)
            decomposeBranches(rhs, whenTrue, whenFalse)

          case ExNot(rhs) =>
            decomposeBranches(rhs, whenFalse, whenTrue)

          case ex =>
            val r = convertTmpExpr(ex, "branch")

            Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfTrue(r) setTree ex) setTree ex, whenTrue)
            Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfFalse(r) setTree ex) setTree ex, whenFalse)
        }
      }

      def litToLit(l: Literal): CFG.SimpleValue = l.value.tag match {
          case BooleanTag   => new CFG.BooleanLit(l.value.booleanValue) setTree l
          case ByteTag      => new CFG.ByteLit(l.value.byteValue) setTree l
          case ShortTag     => new CFG.ShortLit(l.value.shortValue) setTree l
          case CharTag      => new CFG.CharLit(l.value.charValue) setTree l
          case IntTag       => new CFG.IntLit(l.value.intValue) setTree l
          case LongTag      => new CFG.LongLit(l.value.longValue) setTree l
          case FloatTag     => new CFG.FloatLit(l.value.floatValue) setTree l
          case DoubleTag    => new CFG.DoubleLit(l.value.doubleValue) setTree l
          case StringTag    => new CFG.StringLit(l.value.stringValue) setTree l
          case NullTag      => new CFG.Null setTree l
          case UnitTag      => new CFG.Unit setTree l
          case ClassTag     => new CFG.ClassLit(l.tpe) setTree l
          case EnumTag      => new CFG.EnumLit(l.tpe) setTree l
      }

      // 1) Convert body
      convertExpr(cfg.retval, fun.body)

      // 2) Goto exit
      Emit.goto(cfg.exit)

      // 3) Remove skips
      removeSkips(cfg)

      // 4) Remove vertices that are without edges
      cfg.removeIsolatedVertices()

      // 5) Remove unreachable vertices
      val unreachable = cfg.removeUnreachable()

      settings.ifVerbose {
        for ((pos, edges) <- unreachable.groupBy(_.pos)) {
          reporter.warn("Unreachable code: "+edges.mkString("(", ", ", ")"), pos)
        }
      }

      // 5) Check that preLabels is empty
      if (!preLabels.isEmpty) {
        for ((s, (contDef, contCall, ap)) <- preLabels) {
          reporter.error("Label call to undefined label: "+ap, ap.pos)
        }
      }

      cfg
    }

    def removeSkips(cfg: FunctionCFG) {
      for (v <- cfg.V if v != cfg.entry && v != cfg.exit) {
        val out     = cfg.outEdges(v)
        (out.size, out find { case e => e.label == CFG.Skip }) match {
          case (1, Some(out)) =>
            cfg -= out

            for (in <- cfg.inEdges(v)) {
              cfg -= in
              cfg += (in.v1, in.label, out.v2)
            }
          case _ =>
        }
      }
    }
  }
}
