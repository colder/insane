package insane
package analysis

import scala.tools.nsc._
import CFG.ControlFlowGraph

trait Functions {
  self : AnalysisComponent =>
  val global: Global

  import global._

  sealed abstract class AbsFunction {
    val symbol: Symbol
    val body: Tree
    var cfg: Option[FunctionCFG] = None
    val args: Seq[ValDef]

    val CFGArgs = args.map(a => CFGTrees.SymRef(a.symbol))

    /* contracts */
    var contrRequires = Seq[Requires]()
    var contrEnsures  = Seq[Ensures]()

    override def toString = symbol.name.toString()
  }

  final class FunctionCFG(val retval: CFGTrees.Ref) extends ControlFlowGraph[CFGTrees.Statement] {
    var thisReferences =  Seq[CFGTrees.ThisRef]()
  }

  class NamedFunction(val symbol: Symbol, val name: Name, val args: Seq[ValDef], val body: Tree) extends AbsFunction {

  }

  class AnnonFunction(val symbol: Symbol, val args: Seq[ValDef], val body: Tree) extends AbsFunction {

  }
}
