package insane
package analysis

import scala.tools.nsc._
import CFG.{ControlFlowGraph,CFGVertex}

trait Functions {
  self : AnalysisComponent =>
  val global: Global

  import global._

  sealed abstract class AbsFunction {
    val symbol: Symbol
    val body: Tree

    override def toString = symbol.name.toString()

    /* CFG storage for each function */
    private[this] var ocfg: Option[FunctionCFG] = None

    lazy val cfg = ocfg.getOrElse(sys.error("No CFG defined at this point for "+symbol+"?!"))

    def setCFG(cfg: FunctionCFG) = ocfg = Some(cfg)

    val args: Seq[ValDef]

    val CFGArgs = args.map(a => new CFGTrees.SymRef(a.symbol))

    /* contracts */
    var contrRequires = Seq[Requires]()
    var contrEnsures  = Seq[Ensures]()
    var contrAsserts  = Seq[Assert]()

    /* Call graph info */
    var callTargets   = Map[CFG.AssignApplyMeth, (Set[Symbol], Boolean)]()

    /* Point-to Analysis */
    var pointToResult:PTEnv = BottomPTEnv

    var pointToInfos = Map[CFGVertex[CFG.Statement], PTEnv]().withDefaultValue(BottomPTEnv)


    def uniqueName = {
      uniqueFunctionName(symbol)
    }
  }

  final class FunctionCFG(val retval: CFGTrees.Ref) extends ControlFlowGraph[CFGTrees.Statement] {
    var mainThisRef = CFGTrees.ThisRef(NoSymbol)
    var thisRefs    = Set[CFGTrees.ThisRef]()
  }

  class NamedFunction(val symbol: Symbol, val name: Name, val args: Seq[ValDef], val body: Tree) extends AbsFunction

  class AnnonFunction(val symbol: Symbol, val args: Seq[ValDef], val body: Tree) extends AbsFunction

  def uniqueFunctionName(sym: Symbol) = {
    sym.fullName+"("+sym.tpe.toString+")"
  }
  def uniqueClassName(sym: Symbol) = {
    sym.fullName
  }
}

