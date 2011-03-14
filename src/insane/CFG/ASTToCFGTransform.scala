package insane
package CFG

import AST.Extractors
import utils._

import scala.tools.nsc.Global

trait ASTToCFGTransform extends CFGTreesDef { self: Extractors => 
  val global: Global
  val reporter: Reporter
  val settings: Settings

  import global._
  import global.definitions._

  def extractCFGs(unit: CompilationUnit): Unit = {
    new ForeachTreeTraverser(ASTToCFGTransformer.step).traverse(unit.body)
  }

  val CFG = CFGTrees

  object ASTToCFGTransformer {
    import ExpressionExtractors._
    import StructuralExtractors._

    def step(tree: Tree): Unit = tree match {
      case d : DefDef =>
        val cfg = convertASTToCFG(d)


        val name = d.symbol.fullName;
        if (settings.dumpcfg.contains(name) || settings.dumpcfg.contains("_")) {
          val dest = name+".dot"

          reporter.info("Dumping CFG to "+dest+"...")
          cfg.writeDotToFile(dest, "CFG For "+name)
        }

      case _ =>
    }

    def convertASTToCFG(fun: DefDef): ControlFlowGraph[CFG.Statement] = {
      if (settings.verbosity >= Verbosity.Verbose) reporter.info("Converting CFG: "+fun.symbol.fullName+"...")

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
          case i : Ident =>
            Some(new CFG.SymRef(i.symbol))
          case l : Literal =>
            Some(litToLit(l))
          case This(name) =>
            Some(new CFG.This(name))
          case Super(name, mix) =>
            Some(new CFG.Super(name, mix))
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
          val obj = convertTmpExpr(o, "obj")
          Emit.statement(new CFG.AssignSelect(to, obj, s.symbol) setTree s)

        case a @ Apply(s @ Select(o, meth), args) =>
          val obj = convertTmpExpr(o, "obj")
          Emit.statement(new CFG.AssignApplyMeth(to, obj, s.symbol, args.map(convertTmpExpr(_, "arg"))) setTree a)

        case a @ Apply(id @ Ident(name), args) =>
          Emit.statement(new CFG.AssignApplyFun(to, id.symbol, args.map(convertTmpExpr(_, "arg"))) setTree a)

        case Assign(s @ Select(o, field), rhs) =>
          val obj = convertTmpExpr(o, "obj")

          val rhsV = convertTmpExpr(rhs, "rhs")
          Emit.statement(new CFG.AssignVal(new CFG.SymRef(s.symbol) setTree s, rhsV))

        case Assign(i @ Ident(name), rhs) =>
          convertExpr(new CFG.SymRef(i.symbol) setTree i, rhs)


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

        case r =>
          convertSimpleExpr(r) match {
            case Some(sv) =>
              Emit.statement(new CFG.AssignVal(to, sv) setTree tree)
            case _ =>
              reporter.warn("CFG: Unhandled Expression: "+tree+" at "+tree.pos)
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

      convertTmpExpr(fun.rhs)

      Emit.goto(cfg.exit)

      removeSkips(cfg)

      cfg
    }

    def removeSkips(cfg: ControlFlowGraph[CFG.Statement]) = {
      for (v <- cfg.V) {
        val out     = cfg.outEdges(v)
        (out.size, out find { case e => e.lab == CFG.Skip }) match {
          case (1, Some(out)) =>
            cfg -= (out.v1, out.lab, out.v2)

            for (in <- cfg.inEdges(v)) {
              cfg -= (in.v1, in.lab, in.v2)
              cfg += (in.v1, in.lab, out.v2)
            }
          case _ =>
        }
      }
    }

  }
}
