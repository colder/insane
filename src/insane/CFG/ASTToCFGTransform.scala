package insane
package CFG

import scala.tools.nsc.Global

trait ASTToCFGTransform extends CFGTreesDef {
  val global: Global

  import global._
  import global.definitions._

  def extractCFGs(unit: CompilationUnit): Unit = {
    new ForeachTreeTraverser(ASTToCFGTransformer.step).traverse(unit.body)
  }

  val CFG = CFGTrees

  object ASTToCFGTransformer {

    def step(tree: Tree): Unit = tree match {
        case d : DefDef =>
          convertASTToCFG(d)
        case _ =>
    }

    def convertASTToCFG(fun: DefDef) {
        println("Converting "+fun.name+"...")
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

        def convertTmpExpr(tree: Tree, prefix: String = "tmp"): CFG.Ref = {
          val ref = freshVariable(prefix)
          convertExpr(ref, tree)
          ref
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
            Emit.join(whenTrue, endIf)
            Emit.join(whenFalse, endIf)
            decomposeBranches(cond, whenTrue, whenFalse)

            Emit.setPC(endIf)

          case a @ DefDef(_, name, _, args, _, rhs) =>
            // ignore for now

          case a @ Apply(receiver, args) =>

          case Assign(lhs: Tree, rhs: Tree) =>
            val r = treeToRef(lhs)
            convertExpr(r, rhs)

          case l : Literal =>
            Emit.statement(new CFG.AssignVal(to, litToLit(l)) setTree tree)

          case _ =>
            println("Unhandled Expression: "+tree)
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
            // plain Expression
            val r = convertTmpExpr(ex, "branch")

            Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfTrue(r)) setTree ex, whenTrue)
            Emit.statementBetween(Emit.getPC, new CFG.Branch(new CFG.IfFalse(r)) setTree ex, whenFalse)
        }

        def litToLit(l: Literal): CFG.SimpleValue = {
          new CFG.StringLit("?") setTree l
        }

        def treeToRef(t: Tree): CFG.Ref = {
          println("Got tree: "+t);

          freshVariable("plop")
        }

        convertTmpExpr(fun.rhs)
    }
  }
}
