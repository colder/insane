package insane
package CFG

import utils._
import utils.Reporters._
import utils.Graphs.DotConverter
import GlobalCounters.getCFGCounter
import GlobalCounters.withDebugCounter

import scala.tools.nsc.Global
import Graphs._

trait CFGGeneration extends CFGTreesDef { self: AnalysisComponent =>
  val global: Global
  val reporter: Reporter
  val settings: Settings

  import global._
  import icodes._

  val CFG = CFGTrees

  class CFGGenerationPhase extends SubPhase {
    val name = "Generating CFGs"

    def run() {
      if (settings.onDemandMode) {
        reporter.msg("Skipping batch CFG generation")
      } else {
        for(fun <- declaredFunctions.values) {
          val cfg = new CFGConverterFromAST(fun).getCFG

          fun.setCFG(cfg)
        }
      }
    }
  }

  abstract class CFGConverter(val fun: AbsFunction) {

    def freshName(prefix: String): String = {
      prefix + "#" + getCFGCounter
    }

    def freshVariable(tpe: Type, prefix: String = "v")  = new CFG.TempRef(freshName(prefix), NoUniqueID, tpe)

    def unusedVariable() = freshVariable(NoType, "unused");

    var cfg: FunctionCFG

    lazy val unreachableVertex = cfg.newNamedVertex("unreachable")

    type Vertex = CFGVertex

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

      def connect(from: Vertex, to: Vertex) {
        if (from != unreachableVertex && from != to) {
          cfg += (from, CFG.Skip, to)
        }
      }
    }

    def identToRef(i: Ident): CFG.Ref = {
      if (i.symbol.isModule) {
        new CFG.ObjRef(i.symbol, i.symbol.tpe) setTree i
      } else {
        new CFG.SymRef(i.symbol, NoUniqueID, i.symbol.tpe) setTree i
      }
    }

    def constToLit(c: Constant): CFG.SimpleValue = c.tag match {
        case BooleanTag   => new CFG.BooleanLit(c.booleanValue)
        case ByteTag      => new CFG.ByteLit(c.byteValue)
        case ShortTag     => new CFG.ShortLit(c.shortValue)
        case CharTag      => new CFG.CharLit(c.charValue)
        case IntTag       => new CFG.IntLit(c.intValue)
        case LongTag      => new CFG.LongLit(c.longValue)
        case FloatTag     => new CFG.FloatLit(c.floatValue)
        case DoubleTag    => new CFG.DoubleLit(c.doubleValue)
        case StringTag    => new CFG.StringLit(c.stringValue)
        case NullTag      => new CFG.Null
        case UnitTag      => new CFG.Unit
        case ClazzTag     => new CFG.ClassLit(c.typeValue)
        case EnumTag      => new CFG.EnumLit(c.typeValue)
    }

    def litToLit(l: Literal): CFG.SimpleValue = constToLit(l.value) setTree l

  }

  class CFGConverterFromAST(fun: AbsFunction) extends CFGConverter(fun) {
    import ExpressionExtractors._
    import StructuralExtractors._

    var labels    = Map[Symbol, (Vertex, List[Ident])]()
    var preLabels = Map[Symbol, (Vertex, Vertex, Apply)]()

    var cfg = new FunctionCFG(
      fun.symbol,
      fun.args.map ( sym => new CFG.SymRef(sym, NoUniqueID, sym.tpe)),
      freshVariable(fun.body.tpe.underlying, "retval") setTree fun.body,
      false
    )

    def convertSimpleExpr(tree: Tree): Option[CFG.SimpleValue] = tree match {
      case s @ Select(obj, field) if (s.symbol.isModule) =>
        Some(new CFG.ObjRef(s.symbol, s.symbol.tpe))
      case f @ Function(params, body) =>
        reporter.fatal("Unnexpected Annon Function: "+f)
      case i : Ident =>
        Some(identToRef(i))
      case l : Literal =>
        Some(litToLit(l))
      case th @ This(name) =>

        if ((th.symbol, th.symbol.tpe) != (cfg.mainThisRef.symbol, cfg.mainThisRef.tpe)) {
          // Alternative non-this ref
          val tpe = th.symbol.tpe

          if (tpe == NoType) {
            reporter.error("Could not find type for: "+th.symbol)
            debugSymbol(th.symbol);
          }
          Some(new CFG.ObjRef(th.symbol, tpe))
        } else {
          Some(cfg.mainThisRef)
        }
      case s : Super =>
        val sr = new CFG.SuperRef(s.symbol, NoUniqueID, s.symbol.superClass.tpe) setTree tree

        cfg = cfg.copy(superRefs = cfg.superRefs + sr)

        Some(sr)

      case _ =>
        None
    }

    def convertTmpExpr(tree: Tree, prefix: String = "tmp"): CFG.SimpleValue = {
      convertSimpleExpr(tree) match {
        case Some(sv) =>
          sv
        case None =>
          val ref = freshVariable(tree.tpe.underlying, prefix) setTree tree
          convertExpr(ref, tree)
          ref
      }
    }

    def convertExpr(to: CFG.Ref, tree: Tree) {
      tree match {
        case ArrayValue(tpt, elems) =>
          //FIXME
          Emit.statement(new CFG.AssignNew(to, arrayType(tpt.tpe)) setTree tree)

          for ((elem, i) <- elems.zipWithIndex) {
            val args = List(new CFG.LongLit(i) setTree elem, convertTmpExpr(elem, "arrelem"))

            Emit.statement(new CFG.AssignApplyMeth(unusedVariable() setTree elem,
                                                  to,
                                                  definitions.Array_update,
                                                  args) setTree elem)
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
          val s = new CFG.SymRef(v.symbol, NoUniqueID, v.symbol.tpe) setTree v
          convertExpr(s, rhs)

        case i @ If(cond, thn, elze) =>
          val whenTrue = cfg.newVertex
          val whenFalse = cfg.newVertex
          val endIf = cfg.newVertex
          decomposeBranches(cond, whenTrue, whenFalse)

          Emit.setPC(whenTrue)
          convertExpr(to, thn)
          Emit.goto(endIf)

          Emit.setPC(whenFalse)
          convertExpr(to, elze)
          Emit.goto(endIf)

          Emit.setPC(endIf)
        case a @ DefDef(_, name, _, args, _, rhs) =>
        // ignore for now

        case a @ ExNew(sym, tpe, args) =>
          val ref = freshVariable(a.tpe.underlying, "obj") setTree a
          Emit.statement(new CFG.AssignNew(ref, tpe) setTree a)
          Emit.statement(new CFG.AssignApplyMeth(unusedVariable() setTree a,
                                                 ref,
                                                 sym,
                                                 args.map(convertTmpExpr(_, "arg"))) setTree a)
          Emit.statement(new CFG.AssignVal(to, ref) setTree a)

        case t @ Typed(ex, tpe) =>
          convertExpr(to, ex)

        case ad @ ApplyDynamic(o, args) =>
          val obj = convertTmpExpr(o, "obj")
          Emit.statement(new CFG.AssignApplyMeth(to,
                                                 obj,
                                                 ad.symbol,
                                                 args.map(convertTmpExpr(_, "arg")),
                                                 style = CFG.DynamicCall) setTree ad)

        case t @ Throw(expr) =>
          convertTmpExpr(expr, "exception")
          if (settings.displayExceptionsWarn) {
            reporter.warn("Ignoring exception effects", t.pos)
          }
          Emit.setPC(unreachableVertex)

        case a @ Apply(s @ Select(o, meth), args) =>
          // We need to check for boolean short-circuiting methods, &&, ||

          if (s.symbol == definitions.Boolean_or || s.symbol == definitions.Boolean_and) {
            val whenTrue  = cfg.newVertex
            val whenFalse = cfg.newVertex
            val endIf     = cfg.newVertex

            decomposeBranches(a, whenTrue, whenFalse)

            // Check for unreachability
            if (cfg.graph.inEdges(whenTrue).exists(_.v1 != unreachableVertex)) {
              Emit.setPC(whenTrue)
              Emit.statement(new CFG.AssignVal(to, new CFG.BooleanLit(true) setTree tree) setTree tree)
              Emit.goto(endIf)
            }

            // Check for unreachability
            if (cfg.graph.inEdges(whenFalse).exists(_.v1 != unreachableVertex)) {
              Emit.setPC(whenFalse)
              Emit.statement(new CFG.AssignVal(to, new CFG.BooleanLit(false) setTree tree) setTree tree)
              Emit.goto(endIf)
            }

            Emit.setPC(endIf)
          } else {
            val obj = convertTmpExpr(o, "obj")
            Emit.statement(new CFG.AssignApplyMeth(to,
                                                   obj,
                                                   s.symbol,
                                                   args.map(convertTmpExpr(_, "arg"))) setTree a)
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
          if (settings.displayExceptionsWarn) {
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
              val v = cfg.newNamedVertex("lab__"+name)
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

        case a @ Apply(ta @ TypeApply(s @ Select(o, meth), tpes), args) =>
          import definitions._

          val obj = convertTmpExpr(o, "obj") match {
            case obj: CFG.Ref =>

              if (s.symbol == Object_isInstanceOf || s.symbol == Any_isInstanceOf) {
                Emit.statement(new CFG.AssignTypeCheck(to, obj, tpes.head.tpe) setTree a)
              } else if (s.symbol == Object_asInstanceOf || s.symbol == Any_asInstanceOf) {
                Emit.statement(new CFG.AssignCast(to, obj, tpes.head.tpe) setTree ta)
              } else {
                Emit.statement(new CFG.AssignApplyMeth(to,
                                                      obj,
                                                      s.symbol,
                                                      args.map(convertTmpExpr(_, "arg")),
                                                      typeArgs = tpes) setTree a)
              }
            case obj =>
              if (s.symbol == Object_asInstanceOf || s.symbol == Any_asInstanceOf) {
                Emit.statement(new CFG.AssignVal(to, obj) setTree ta)
              } else {
                reporter.error("Invalid object reference type in: "+s, a.pos)
              }
          }
        case Match(ta, cases) =>
          val expr = convertTmpExpr(ta, "matchEx")
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

            case CaseDef(casesOrLit, _, body) =>
              val beginCase = Emit.getPC

              val caseIsMatch = cfg.newVertex

              val alternatives = casesOrLit match {
                case l: Literal =>
                  List(l)
                case Alternative(trees) =>
                  trees.flatMap {
                    case t: Literal =>
                      Some(t)
                    case t =>
                      reporter.error("Unhandled non-literal in pattern matching alternatives: "+t+"("+t.getClass+")",t.pos)
                      None
                  }
                case _ =>
                  reporter.error("Unhandled case in pattern matching: "+casesOrLit+"("+casesOrLit.getClass+")", cas.pos)
                  List()
              }

              for (a <- alternatives) {
                Emit.statementBetween(beginCase, new CFG.Branch(new CFG.IfEqual(expr, litToLit(a))) setTree a, caseIsMatch)
              }

              Emit.goto(caseIsMatch)
              convertExpr(to, body)
              Emit.goto(endMatch)

              Emit.setPC(beginCase)
              for (a <- alternatives) {
                Emit.statement(new CFG.Branch(new CFG.IfNotEqual(expr, litToLit(a))) setTree a)
              }

            case _ =>
              reporter.error("Unhandled case in pattern matching: "+cas+"("+cas.getClass+")", cas.pos)
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
          if (s.symbol == definitions.BoxedUnit_UNIT) {
            // Special boxed unit case for which we really want unit anyway since we will mostly ignore those 

            Emit.statement(new CFG.AssignVal(to, new CFG.Unit setTree s) setTree s)
          } else {
            convertTmpExpr(o, "obj") match {
              case obj: CFG.Ref =>
                Emit.statement(new CFG.AssignFieldRead(to, obj, s.symbol) setTree s)
              case obj =>
                reporter.error("Invalid object reference in select: "+s, s.pos)
            }
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

        case ex @ ExEq(lhs, rhs) =>
          val rlhs = convertTmpExpr(lhs, "lhs")

          if (Emit.getPC != unreachableVertex) {
            val rrhs = convertTmpExpr(rhs, "rhs")

            if (Emit.getPC != unreachableVertex) {
              Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfEqual(rlhs, rrhs) setTree ex) setTree ex, whenTrue)
              Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfNotEqual(rlhs, rrhs) setTree ex) setTree ex, whenFalse)
            }
          }

        case ex @ ExNe(lhs, rhs) =>
          val rlhs = convertTmpExpr(lhs, "lhs")

          if (Emit.getPC != unreachableVertex) {
            val rrhs = convertTmpExpr(rhs, "rhs")

            if (Emit.getPC != unreachableVertex) {
              Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfEqual(rlhs, rrhs) setTree ex) setTree ex, whenFalse)
              Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfNotEqual(rlhs, rrhs) setTree ex) setTree ex, whenTrue)
            }
          }

        case a @ Apply(fun: Ident, args) =>
          convertTmpExpr(a, "cont")

        case t @ Throw(expr) =>
          convertTmpExpr(t, "throw")

        case ex =>
          val r = convertTmpExpr(ex, "branch")

          if (Emit.getPC != unreachableVertex) {
            Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfTrue(r) setTree ex) setTree ex, whenTrue)
            Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfFalse(r) setTree ex) setTree ex, whenFalse)
          }
      }
    }

    def getCFG: FunctionCFG = {

      if (settings.displayFullProgress) {
        reporter.msg("Converting CFG: "+uniqueFunctionName(fun.symbol)+"...")
      }

      // 0) Generate a SKIP from empty to avoid entry being in a SCC
      val codeEntry = cfg.newVertex
      Emit.goto(codeEntry)
      Emit.setPC(codeEntry)

      // 1) Convert body
      convertExpr(cfg.retval, fun.body)

      // 2) Goto exit
      Emit.goto(cfg.exit)

      // 3) Remove skips
      cfg = cfg.removeSkips

      // 4) Remove vertices that are without edges
      cfg = cfg.removeIsolatedVertices

      // 5) Remove unreachable vertices
      val (ncfg, unreachable) = cfg.removeUnreachable
      cfg = ncfg

      settings.ifVerbose {
        for ((pos, edges) <- unreachable.groupBy(_.pos)) {
          reporter.warn("Unreachable code in "+uniqueFunctionName(fun.symbol)+": "+edges.mkString("(", ", ", ")"), pos)
        }
      }

      // 5) Check that preLabels is empty
      if (!preLabels.isEmpty) {
        for ((s, (contDef, contCall, ap)) <- preLabels) {
          debugSymbol(ap.symbol)
          reporter.error("Label call to undefined label: "+ap, ap.pos)
        }
      }

      cfg = BasicBlocksBuilder.composeBlocks(cfg, { case _ : CFG.AssignApplyMeth => true })

      val name = uniqueFunctionName(fun.symbol)
      if (settings.dumpCFG(safeFullName(fun.symbol))) {
        val dest = safeFileName(name)+"-cfg.dot"

        reporter.msg("Dumping CFG to "+dest+"...")
        new CFGDotConverter(cfg, "CFG For "+name).writeFile(dest)
      }

      cfg
    }
  }

  class CFGConverterFromICode(ifun: ICodeFunction) extends CFGConverter(ifun) {

    var cfg = new FunctionCFG(
      fun.symbol,
      fun.args.map ( sym => new CFG.SymRef(sym, NoUniqueID, sym.tpe)),
      freshVariable(NoType, "retval"), // TODO
      false
    )

    def getCFG: FunctionCFG = {
      convertIMethod(ifun.iMethod, ifun.iClass)
    }

    type ICodeStack      = collection.mutable.Stack[CFG.SimpleValue];
    def  EmptyICodeStack = new ICodeStack();

    def localToRef(local: Local): CFG.Ref = {
      new CFG.SymRef(local.sym, NoUniqueID, kindToType(local.kind))
    }

    def kindToLit(kind: TypeKind): CFG.SimpleValue = kind match {
      case UNIT            =>
        new CFG.Unit
      case BOOL            =>
        new CFG.AnyBooleanLit
      case BYTE            =>
        new CFG.AnyByteLit
      case SHORT           =>
        new CFG.AnyShortLit
      case CHAR            =>
        new CFG.AnyCharLit
      case INT             =>
        new CFG.AnyIntLit
      case LONG            =>
        new CFG.AnyLongLit
      case FLOAT           =>
        new CFG.AnyFloatLit
      case DOUBLE          =>
        new CFG.AnyDoubleLit
      case REFERENCE(cls)  =>
        reporter.error("Unnexpected kind in kindToLit: "+kind)
        new CFG.Null
      case ARRAY(elem)     =>
        reporter.error("Unnexpected kind in kindToLit: "+kind)
        new CFG.Null
    }


    def boxMethod(kind: TypeKind): Symbol = {
      val boxedType = definitions.boxedClass(kind.toType.typeSymbol)
      val nme       = newTermName("boxTo" + boxedType.decodedName)

      definitions.BoxesRunTimeModule.info.decl(nme)
    }

    def unboxMethod(kind: TypeKind): Symbol = {
      val nme = newTermName("unboxTo" + kind.toType.typeSymbol.decodedName)

      definitions.BoxesRunTimeModule.info.decl(nme)
    }

    def kindToType(kind: TypeKind): Type = kind.toType

    case class BBInfo(entry: CFGVertex, entryStack: List[CFG.SimpleValue]);

    var info         = Map[BasicBlock, BBInfo]();
    var knownSources = Map[BasicBlock, Set[CFGVertex]]().withDefaultValue(Set())

    def convertBasicBlock(bb: BasicBlock, stackFrom: List[CFG.SimpleValue], from: CFGVertex) {
      import opcodes._


      /**
       * First task is to connect this basic block to the from vertex, we
       * assign stack values to fresh variables as well
       */
      if (info contains bb) {
        // We already converted this BasicBlock, see if we need to connect it
        // to additional source:
        if (knownSources(bb) contains from) {
          // Everything is done
        } else {
          Emit.setPC(from)
          for ((from, here: CFG.Ref) <- stackFrom zip info(bb).entryStack) {
            Emit.statement(new CFG.AssignVal(here, from))
          }
          Emit.goto(info(bb).entry)
          knownSources += bb -> (knownSources(bb) + from)
        }
      } else {
        var stack       = EmptyICodeStack
        val bEntry      = cfg.newNamedVertex("blockentry")

        Emit.setPC(from)
        for (from <- stackFrom.reverse) {
          val ref = freshVariable(from.tpe)
          stack push ref
          Emit.statement(new CFG.AssignVal(ref, from))
        }

        Emit.goto(bEntry)

        info += bb -> BBInfo(bEntry, stack.toList)
        knownSources += bb -> (knownSources(bb) + from)

        println("Converting Block: "+bb.label+" fromStack: "+stackFrom)
        for (istr <- bb.iterator) {
          println(" - "+istr+" (-"+istr.consumed+" +"+istr.produced+")")
        }


        Emit.setPC(bEntry)

        def getFieldObj(field: Symbol, isStatic: Boolean): Option[CFG.Ref] = {
          val res = if (isStatic) {
            Some(new CFG.ObjRef(field.owner, field.owner.tpe))
          } else {
            val ref = stack.pop

            ref match {
              case obj: CFG.Ref =>
                Some(obj)
              case notObj =>
                reporter.error("Invalid object reference on stack: "+notObj)
                None
            }
          }
          res
        }

        def callMethod(method: Symbol, style: InvokeStyle, data: List[CFG.SimpleValue], returns: Boolean) {
          val ret    = freshVariable(method.info.resultType, "ret")

          val (rec, args) = style match {
            case Static(false) =>
              // static, no instance, receiver is the owner module
              (new CFG.ObjRef(method.owner, method.owner.tpe), data)
            case SuperCall(nme) =>
              data.head match {
                case r: CFG.Ref =>
                  (new CFG.SuperRef(r.tpe.typeSymbol, NoUniqueID, r.tpe.typeSymbol.superClass.tpe), data.tail) // Might be wrong
                case _ =>
                  reporter.error("Cannot call to super of a non-ref receiver: "+method)
                  return;
              }
            case _ =>
              (data.head, data.tail)
          }

          rec match {
            case rec: CFG.Ref =>

              val callStyle: CFG.CallStyle = style match {
                case Static(_) =>
                  CFG.StaticCall
                case _ if style.isDynamic =>
                  CFG.DynamicCall
                case _ =>
                  CFG.VirtualCall
              }

              Emit.statement(new CFG.AssignApplyMeth(ret,
                                                     rec,
                                                     method,
                                                     args,
                                                     style = callStyle))
            case _ =>
              reporter.error("Cannot call method on a non-ref receiver: "+method)
          }

          if (returns) {
            // Might not produce any if constructor or returning Unit
            stack push ret
          }
        }

        for (istr <- bb.iterator) istr match {
          case THIS(clasz) =>
            if ((clasz, clasz.tpe) != (cfg.mainThisRef.symbol, cfg.mainThisRef.tpe)) {
              // Alternative non-this ref
              val tpe = clasz.tpe

              if (tpe == NoType) {
                reporter.error("Could not find type for: "+clasz)
                debugSymbol(clasz);
              }
              stack push new CFG.ObjRef(clasz, tpe)
            } else {
              stack push cfg.mainThisRef
            }

          case CONSTANT(const) =>
            stack push constToLit(const)
          
          case DUP(kind) =>
            val top = stack.top
            stack push top

          case LOAD_ARRAY_ITEM(kind) =>
            val index = stack.pop
            val array = stack.pop

            callMethod(definitions.arrayApplyMethod, Dynamic, List(array, index), true)

          case LOAD_LOCAL(local) =>
            stack push localToRef(local)

          case LOAD_FIELD(field, isStatic) =>
            val to = freshVariable(field.tpe, "read");

            getFieldObj(field, isStatic) match {
              case Some(obj) =>
                debugSymbol(field)
                Emit.statement(new CFG.AssignFieldRead(to, obj, field))
              case _ =>
                // ignore
            }

            stack push to
          case LOAD_MODULE(module) =>
            val value = stack.pop
            val index = stack.pop
            val array = stack.pop
            stack push new CFG.ObjRef(module, module.tpe)

          case STORE_THIS(kind) =>
            reporter.warn("Unandled ICode OPCODE: "+istr)
          case STORE_ARRAY_ITEM(kind) =>
            reporter.warn("Unandled ICode OPCODE: "+istr)
          case STORE_LOCAL(local) =>
            val value = stack.pop
            Emit.statement(new CFG.AssignVal(localToRef(local), value))

          case STORE_FIELD(field, isStatic) =>
            val value = stack.pop

            getFieldObj(field, isStatic) match {
              case Some(obj) =>
                debugSymbol(field)
                Emit.statement(new CFG.AssignFieldWrite(obj, field, value))
              case _ =>
                // ignore
            }

          case CALL_PRIMITIVE(primitive) =>
            primitive match {
              case Negation(kind)        =>
                stack.pop
                stack.push(kindToLit(kind))

              case Test(op, kind, true)  =>
                stack.pop
                stack.push(kindToLit(BOOL))

              case Test(op, kind, false) =>
                stack.pop
                stack.pop
                stack.push(kindToLit(BOOL))

              case Comparison(op, kind)  =>
                stack.pop
                stack.pop
                stack.push(kindToLit(INT))

              case Arithmetic(NOT, kind) =>
                stack.pop
                stack.push(kindToLit(kind))
                  
              case Arithmetic(op, kind)  =>
                stack.pop
                stack.pop
                stack.push(kindToLit(kind))

              case Logical(op, kind)     =>
                stack.pop
                stack.pop
                stack.push(kindToLit(kind))

              case Shift(op, kind)       =>
                stack.pop
                stack.pop
                stack.push(kindToLit(kind))

              case Conversion(from, to)  =>
                stack.pop
                stack.push(kindToLit(to))

              case ArrayLength(kind)     =>
                stack.pop
                stack.push(kindToLit(INT))

              case StringConcat(kind)    =>
                stack.pop
                stack.pop
                stack.push(new CFG.AnyStringLit) // StringBuffer here?

              case StartConcat           =>
                stack.push(new CFG.AnyStringLit) // StringBuffer here?

              case EndConcat             =>
                stack.pop
                stack.push(new CFG.AnyStringLit) // StringBuffer here?
            }

          case cm @ CALL_METHOD(method, style) =>
            var data = EmptyICodeStack
            for (i <- 1 to cm.consumed) {
              data.push(stack.pop)
            }

            callMethod(cm.method, cm.style, data.toList, cm.produced > 0)

          case n @ NEW(kind) =>
            val rec  = freshVariable(kindToType(kind), "rec")
            val argsSize = n.consumed - 1 // rec is not on the stack yet

            var data = EmptyICodeStack
            for (i <- 1 to argsSize) {
              data.push(stack.pop)
            }

            data.push(rec)

            Emit.statement(new CFG.AssignNew(rec, rec.tpe))

            callMethod(n.init.method, n.init.style, data.toList, false)

            stack.push(rec)

          case CREATE_ARRAY(elem, dims) =>
            for (i <- 1 to istr.consumed) {
              stack.pop
            }

            val tpe = arrayType(kindToType(elem));
            val to = freshVariable(tpe)
            Emit.statement(new CFG.AssignNew(to, tpe))
            stack.push(to)

          case IS_INSTANCE(tpe) =>
            stack.pop
            stack.push(kindToLit(BOOL))

          case BOX(boxType) =>
            val r = stack.pop
            callMethod(boxMethod(boxType), Static(false), List(r), true) 

          case UNBOX(boxType) =>
            val r = stack.pop
            callMethod(unboxMethod(boxType), Static(false), List(r), true) 

          case CHECK_CAST(tpe) =>
            // Assume casts are always valid here

          case SWITCH(tags, labels) =>
            // Possible jump to all labels, depending on tags.find(_.contains(index))
            val index = stack.pop

          case JUMP(whereto) =>
            // jump
            convertBasicBlock(whereto, stack.toList, Emit.getPC)

          case CJUMP(success, failure, cond, kind) =>
            val op1 = stack.pop
            val op2 = stack.pop
            val jmp     = Emit.getPC
            val iftrue  = cfg.newNamedVertex("iftrue")
            val iffalse = cfg.newNamedVertex("iffalse")
            Emit.connect(jmp, iftrue)
            Emit.connect(jmp, iffalse)

            // jump
            convertBasicBlock(success, stack.toList, iftrue)
            convertBasicBlock(failure, stack.toList, iffalse)

          case CZJUMP(success, failure, cond, kind) =>
            val op1 = stack.pop
            val jmp     = Emit.getPC
            val iftrue  = cfg.newNamedVertex("iftrue")
            val iffalse = cfg.newNamedVertex("iffalse")
            Emit.connect(jmp, iftrue)
            Emit.connect(jmp, iffalse)

            // jump
            convertBasicBlock(success, stack.toList, iftrue)
            convertBasicBlock(failure, stack.toList, iffalse)

          case RETURN(kind) =>
            if (kind == UNIT) {
              Emit.statement(new CFG.AssignVal(cfg.retval, kindToLit(UNIT)))
            } else {
              val retval = stack.pop
              Emit.statement(new CFG.AssignVal(cfg.retval, retval))
            }

            // jump
            Emit.connect(Emit.getPC, cfg.exit)

          case THROW(clasz) =>
            val op1 = stack.pop
            // jump to unreachable

            Emit.setPC(unreachableVertex)

          case DROP(kind) =>
            stack.pop

          // Ignored OPCODES:
          case MONITOR_ENTER() =>
            // Ignore this one
            stack.pop
          case MONITOR_EXIT() =>
            // Ignore this one
            stack.pop
          case SCOPE_ENTER(lv) =>
            // ignore
          case SCOPE_EXIT(lv) =>
            // ignore
          case LOAD_EXCEPTION(clasz) =>
            // ignore
        }
      }
    }

    def convertIMethod(iMethod: IMethod, iClass: IClass): FunctionCFG = {
      reporter.debug("Converting ICode to CFG for "+iMethod.symbol.fullName+" ("+iMethod.symbol.id+")")


      // 0) Generate a SKIP from empty to avoid entry being in a SCC
      val codeEntry = cfg.newVertex
      Emit.connect(cfg.entry, codeEntry)

      convertBasicBlock(iMethod.startBlock, EmptyICodeStack.toList, codeEntry)

      // 2) Remove skips
      cfg = cfg.removeSkips

      // 3) Remove vertices that are without edges
      cfg = cfg.removeIsolatedVertices

      // 4) Remove unreachable vertices
      val (ncfg, unreachable) = cfg.removeUnreachable
      cfg = ncfg

      settings.ifVerbose {
        for ((pos, edges) <- unreachable.groupBy(_.pos)) {
          reporter.warn(List("Unreachable code in "+uniqueFunctionName(ifun.symbol)+": ") ::: edges.toList.map(e => " "+e.toString), pos)
        }
      }

      cfg = BasicBlocksBuilder.composeBlocks(cfg, { case _ : CFG.AssignApplyMeth => true })

      val name = uniqueFunctionName(ifun.symbol)
      if (settings.dumpCFG(safeFullName(ifun.symbol))) {
        val dest = safeFileName(name)+"-cfg.dot"

        reporter.msg("Dumping CFG to "+dest+"...")
        new CFGDotConverter(cfg, "CFG For "+name).writeFile(dest)
      }

      cfg
    }
  }

  object BasicBlocksBuilder {
    type Graph      = LabeledImmutableDirectedGraphImp[CFG.Statement, CFGVertex, CFGEdge[CFG.Statement]]
    type Predicate  = PartialFunction[CFG.Statement, Boolean]

    val excludeNothing: Predicate = { case _ => false }

    def composeBlocks(cfg: FunctionCFG, pred: Predicate = excludeNothing): FunctionCFG = {
      cfg.copy(graph = composeBlocks(cfg.graph, pred))
    }

    def composeBlocks(graph: Graph, pred: Predicate): Graph = {
      val exclude = pred orElse excludeNothing

      var newGraph = graph.mutable

      // We compact basic blocks together
      for (v <- newGraph.V) (newGraph.inEdges(v), newGraph.outEdges(v)) match {
        case (ins, outs) if (ins.size == 1) && (outs.size == 1) =>
          (ins.head, outs.head) match {
            case (in  @ CFGEdge(inFrom,   inLabel, inTo), out @ CFGEdge(outFrom, outLabel, outTo)) =>
              if (!(exclude(inLabel)) && !(exclude(outLabel))) {
                val inStmts = inLabel match {
                  case bb: CFG.BasicBlock =>
                    bb.stmts
                  case s =>
                    Seq(s)
                }
                val outStmts = outLabel match {
                  case bb: CFG.BasicBlock =>
                    bb.stmts
                  case s =>
                    Seq(s)
                }

                newGraph -= in
                newGraph -= out
                newGraph -= v

                val stmts = inStmts ++ outStmts

                val tree =  stmts.find(s => s.tree != None && s.tree != Some(EmptyTree)).map(_.getTree).getOrElse(EmptyTree)

                newGraph += CFGEdge(inFrom, (new CFG.BasicBlock(stmts): CFG.Statement) setTree tree , outTo)
              }
          }
          
        case _ =>
      }

      newGraph.immutable
    }
  }
}
