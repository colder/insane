package insane
package CFG

import AST.Extractors
import utils._
import utils.Graphs.DotConverter

import scala.tools.nsc.Global

trait CFGGeneration extends CFGTreesDef { self: AnalysisComponent =>
  val global: Global
  val reporter: Reporter
  val settings: Settings

  import global._
  import global.definitions._

  val CFG = CFGTrees

  class CFGGenerationPhase extends SubPhase {
    val name = "Generating CFGs"

    def run = {
      for(fun <- funDecls.values) {
        val cfg = convertASTToCFG(fun);

        val name = fun.symbol.fullName;
        if (settings.dumpCFG(name)) {
          val dest = name+".dot"

          reporter.info("Dumping CFG to "+dest+"...")
          new DotConverter(cfg, "CFG For "+name).toFile(dest)
        }

        fun.cfg = Some(cfg)
      }
    }


    def convertASTToCFG(fun: AbsFunction): ControlFlowGraph[CFG.Statement] = {
      import ExpressionExtractors._
      import StructuralExtractors._

      var labels    = Map[Symbol, (Vertex, List[Ident])]()
      var preLabels = Map[Symbol, (Vertex, Vertex, Apply)]()

      settings.ifPleonastic {
        reporter.info("Converting CFG: "+fun.symbol.fullName+"...")
      }

      val cfg = new ControlFlowGraph[CFG.Statement]()

      type Vertex = cfg.Vertex

      object freshName {
        var count = 0

        def apply(prefix: String): String = {
          count = count + 1
          prefix + "#" + count
        }
      }
      def freshVariable(prefix: String = "v")  = new CFG.TempRef(freshName(prefix))

      val retval = freshVariable("retval")

      object Emit {
        private var pc: Vertex = cfg.entry
        def getPC: Vertex = pc 
        def setPC(v: Vertex) = { pc = v }
          
        // emits a statement between two program points
        def statementBetween(from: Vertex, stat: CFG.Statement, to : Vertex): Unit = {
          cfg += (from, stat, to)
        } 

        // emits a statement from the current PC and sets the new PC after it
        def statement(stat: CFG.Statement): Unit = {
          val v = cfg.newVertex
          cfg += (pc, stat, v) 
          setPC(v)
        }     

        // emits a statement from the current PC to an existing program point
        def statementCont(stat: CFG.Statement, cont: Vertex) = {
          cfg += (pc, stat, cont)
        }

        // emits an ''empty'' statement (equivalent to unconditional branch) from the current PC to an existing point
        def goto(cont: Vertex) = {
          cfg += (pc, CFG.Skip, cont)
        }
        def join(from: Vertex, to: Vertex) = {
          cfg += (from, CFG.Skip, to)
        }
      }

      def convertSimpleExpr(tree: Tree): Option[CFG.SimpleValue] = {
        val r = tree match {
          case f @ Function(params, body) =>
            Predef.error("Unnexpected Annon Function: "+f)
          case i : Ident =>
            Some(new CFG.SymRef(i.symbol))
          case l : Literal =>
            Some(litToLit(l))
          case This(name) =>
            Some(new CFG.ThisRef(name))
          case Super(name, mix) =>
            Some(new CFG.SuperRef(name, mix))
          case _ =>
            None
        }

        r match {
          case Some(sv) => 
            Some(sv setTree tree)
          case _ =>
            None
        }
      }

      def convertTmpExpr(tree: Tree, prefix: String = "tmp"): CFG.SimpleValue = {
        convertSimpleExpr(tree) match {
          case Some(sv) =>
            sv
          case None =>
            val ref = freshVariable(prefix)
            convertExpr(ref, tree)
            ref
        }
      }

      def convertExpr(to: CFG.Ref, tree: Tree): Unit = tree match {
        case ArrayValue(tpt, elems) =>
          val newelems = elems.map(convertTmpExpr(_, "arrelem"))
          Emit.statement(new CFG.AssignArray(to, newelems, tpt.tpe))

        case Block(stmts, expr) =>
          for (st <- stmts) {
            convertTmpExpr(st)
          }
          convertExpr(to, expr)

        case v @ ValDef(_, _, _, rhs: Tree) =>
          val s = new CFG.SymRef(v.symbol);
          convertExpr(s, rhs)

        case i @ If(cond, then, elze) =>
          val whenTrue = cfg.newVertex
          val whenFalse = cfg.newVertex
          val endIf = cfg.newVertex
          decomposeBranches(cond, whenTrue, whenFalse)

          Emit.setPC(whenTrue)
          convertTmpExpr(then)
          Emit.goto(endIf)

          Emit.setPC(whenFalse)
          convertTmpExpr(elze)
          Emit.goto(endIf)

          Emit.setPC(endIf)
        case a @ DefDef(_, name, _, args, _, rhs) =>
          // ignore for now

        case s @ Select(o, field) =>
          convertTmpExpr(o, "obj") match {
            case obj: CFG.Ref =>
              Emit.statement(new CFG.AssignSelect(to, obj, s.symbol) setTree s)
            case obj =>
              reporter.error("Invalid object reference in select: "+s)
          }

        case a @ ExNew(sym, args) =>
          Emit.statement(new CFG.AssignNew(to, sym, args.map(convertTmpExpr(_, "arg"))) setTree a)

        case t @ Typed(ex, tpe) =>
          convertExpr(to, ex)

        case ad : ApplyDynamic =>
          if (settings.verbosity >= Verbosity.Verbose) {
            reporter.warn("Ingoring ApplyDynamic call at "+ad.pos)
          }

        case t @ Throw(expr) =>
          convertTmpExpr(expr, "exception")
          if (settings.verbosity >= Verbosity.Verbose) {
            reporter.warn("Ignoring exception effects at "+t.pos)
          }
          Emit.goto(cfg.exit)
          Emit.setPC(cfg.newNamedVertex("unreachable"))

        case a @ Apply(s @ Select(o, meth), args) =>
          val obj = convertTmpExpr(o, "obj")
          Emit.statement(new CFG.AssignApplyMeth(to, obj, s.symbol, args.map(convertTmpExpr(_, "arg"))) setTree a)

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
          if (settings.verbosity >= Verbosity.Verbose) {
            reporter.warn("Ignoring try/catch effects at "+t.pos)
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
                convertExpr(new CFG.SymRef(i.symbol), a)
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
                convertExpr(new CFG.SymRef(i.symbol), a)
              }
              // We goto
              Emit.goto(v)
              Emit.setPC(cfg.newNamedVertex("unreachable"))
            case None =>
              val contDef  = cfg.newNamedVertex("contDef")
              val contCall = Emit.getPC
              preLabels += fun.symbol -> (contDef, contCall, a)
              Emit.setPC(cfg.newNamedVertex("unreachable"))
          }

        case a @ Apply(ta @ TypeApply(s @ Select(o, meth), List(typ)), args) =>
          val obj = convertTmpExpr(o, "obj") match {
            case obj: CFG.Ref =>
              if (meth.toString == "$isInstanceOf") {
                Emit.statement(new CFG.AssignTypeCheck(to, obj, typ.tpe) setTree a)
              } else if (meth.toString == "$asInstanceOf") {
                Emit.statement(new CFG.AssignCast(to, obj, typ.tpe) setTree ta)
              } else {
                reporter.error("Unknown TypeApply method: "+meth)
              }
            case obj =>
              reporter.error("Invalid object reference type in: "+s)
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
          var endMatch  = cfg.newNamedVertex("endMatch")

          for (cas <- cases) cas match {
            case CaseDef(l, _, body) if l.toString == "_" =>
              // else case
              convertExpr(to, body)
              Emit.goto(endMatch)

            case CaseDef(l : Literal, _, body) =>
              var beginCase = Emit.getPC
              Emit.statement(new CFG.Branch(new CFG.IfEqual(expr, litToLit(l))) setTree cas)
              convertExpr(to, body)
              Emit.goto(endMatch)

              Emit.setPC(beginCase)
              Emit.statement(new CFG.Branch(new CFG.IfNotEqual(expr, litToLit(l))) setTree cas)

            case _ =>
              reporter.error("Unhandled case in pattern matching: "+cas)
          }

          Emit.setPC(endMatch)


        case Assign(s @ Select(o, field), rhs) =>
          val obj = convertTmpExpr(o, "obj")

          val rhsV = convertTmpExpr(rhs, "rhs")
          Emit.statement(new CFG.AssignVal(new CFG.SymRef(s.symbol) setTree s, rhsV))

        case Assign(i @ Ident(name), rhs) =>
          convertExpr(new CFG.SymRef(i.symbol) setTree i, rhs)

        case EmptyTree =>
          // ignore

        case Return(tre) =>
          convertExpr(retval, tre)
          Emit.goto(cfg.exit)
          Emit.setPC(cfg.newNamedVertex("unreachable"))

        case r =>
          convertSimpleExpr(r) match {
            case Some(sv) =>
              Emit.statement(new CFG.AssignVal(to, sv) setTree tree)
            case _ =>
              reporter.warn("CFG: Unhandled Expression: "+tree+"("+tree.getClass+") at "+tree.pos)
          }
      }

      def decomposeBranches(cond: Tree, whenTrue: Vertex, whenFalse: Vertex): Unit = cond match {
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

          Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfTrue(r)) setTree ex, whenTrue)
          Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfFalse(r)) setTree ex, whenFalse)
      }

      def litToLit(l: Literal): CFG.SimpleValue = l.value.tag match {
          case BooleanTag                               => new CFG.BooleanLit(l.value.booleanValue)
          case ByteTag | ShortTag | CharTag | IntTag    => new CFG.LongLit(l.value.intValue)
          case LongTag                                  => new CFG.LongLit(l.value.longValue)
          case FloatTag                                 => new CFG.DoubleLit(l.value.floatValue)
          case DoubleTag                                => new CFG.DoubleLit(l.value.doubleValue)
          case StringTag                                => new CFG.StringLit(l.value.stringValue)
          case _                                        => new CFG.StringLit("?")
      }

      // 1) Convert arguments
      for (a <- fun.args) {
        Emit.statement(new CFG.AssignArg(new CFG.SymRef(a.symbol) setTree a, a.symbol) setTree a)
      }

      // 2) Convert body
      convertExpr(retval, fun.body)

      // 3) Goto exit
      Emit.goto(cfg.exit)

      // 4) Remove skips
      removeSkips(cfg)

      // 5) Remove vertices that are without edges
      cfg.removeIsolatedVertices

      // 6) Check that preLabels is empty
      if (!preLabels.isEmpty) {
        for ((s, (contDef, contCall, ap)) <- preLabels) {
          reporter.error("Label call to undefined label: "+ap+" at "+ap.pos)
        }
      }

      cfg
    }

    def removeSkips(cfg: ControlFlowGraph[CFG.Statement]) = {
      for (v <- cfg.V) {
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
