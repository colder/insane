package insane
package utils

import scala.tools.nsc._
import CFG.{ControlFlowGraph,CFGVertex}

trait Functions {
  self : AnalysisComponent =>

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

    lazy val pointToArgs: Seq[PointToGraphs.Node] = {
      import PointToGraphs._

      /**
       * Preprocess arguments by creating nodes with corresponding ObjectSets
       */

      Seq(PNode(0, ObjectSet.subtypesOf(symbol.owner))) ++
      args.zipWithIndex.map { case (a, i) =>
        if (isGroundClass(a.symbol.tpe.typeSymbol)) {
          typeToLitNode(a.symbol.tpe)
        } else {
          PNode(i+1, ObjectSet.subtypesOf(a.symbol.tpe))
        }
      }
    }


    def uniqueName = {
      uniqueFunctionName(symbol)
    }
  }

  final class FunctionCFG(val retval: CFGTrees.Ref) extends ControlFlowGraph[CFGTrees.Statement] {
    var mainThisRef = CFGTrees.ThisRef(NoSymbol)
    var thisRefs    = Set[CFGTrees.ThisRef]()

    var superRefs   = Set[CFGTrees.SuperRef]()
  }

  class NamedFunction(val symbol: Symbol, val name: Name, val args: Seq[ValDef], val body: Tree) extends AbsFunction

  class AnnonFunction(val symbol: Symbol, val args: Seq[ValDef], val body: Tree) extends AbsFunction

  def uniqueFunctionName(sym: Symbol) = {
    safeFullName(sym)+"("+sym.tpe.toString+")"
  }
  def uniqueClassName(sym: Symbol) = {
    safeFullName(sym)+":"+(if(sym.isModuleClass) "m" else "c")
  }

  def safeFullName(sym: Symbol) = {
    try { sym.fullName } catch { case _ => "("+sym.name+")<name-error>" }
  }
}

