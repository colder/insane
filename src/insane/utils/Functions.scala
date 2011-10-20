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
    private[this] var _cfg:   Option[FunctionCFG] = None
    private[this] var _ptcfg: Option[FunctionCFG] = None

    lazy val cfg   = _cfg.getOrElse(sys.error("No CFG defined at this point for "+symbol+"?!"))
    lazy val ptcfg = _ptcfg.getOrElse(sys.error("No CFG defined at this point for "+symbol+"?!"))

    def setCFG(cfg: FunctionCFG) = _cfg = Some(cfg)
    def setPTCFG(ptcfg: FunctionCFG) = _ptcfg = Some(ptcfg)

    val args: Seq[ValDef]

    val CFGArgs = args.map(a => new CFGTrees.SymRef(a.symbol, 0))

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

  final class FunctionCFG(val symbol: Symbol, val retval: CFGTrees.Ref) extends ControlFlowGraph[CFGTrees.Statement] {
    val mainThisRef = CFGTrees.ThisRef(symbol.owner, 0)

    var thisRefs    = Set[CFGTrees.ThisRef]() + mainThisRef
    var objectRefs  = Set[CFGTrees.ObjRef]()

    var superRefs   = Set[CFGTrees.SuperRef]()

    def deepCopy() = {
      val newCFG = new FunctionCFG(symbol, retval)
      newCFG -= newCFG.entry
      newCFG -= newCFG.exit

      val vertexMap = V.map(v => v -> new Vertex(v.name, v.id)).toMap

      newCFG.entry = vertexMap(entry)
      newCFG.exit  = vertexMap(exit)

      newCFG.thisRefs    = thisRefs
      newCFG.objectRefs  = objectRefs
      newCFG.superRefs   = superRefs

      for (e <- E) {
        newCFG += (vertexMap(e.v1), e.label, vertexMap(e.v2))
      }

      newCFG
    }
  }

  class NamedFunction(val symbol: Symbol, val name: Name, val args: Seq[ValDef], val body: Tree) extends AbsFunction

  class AnnonFunction(val symbol: Symbol, val args: Seq[ValDef], val body: Tree) extends AbsFunction

  def uniqueFunctionName(sym: Symbol) = {
    safeFullName(sym)+"("+sym.tpe.toString+")"
  }
  def uniqueFunctionClassName(clas: Symbol, sym: Symbol) = {
    safeFullName(clas)+"."+sym.name+"("+sym.tpe.toString+")"
  }

  def uniqueClassName(sym: Symbol) = {
    ClassSymbolSerializer(sym).serialize()
  }

  def safeFullName(sym: Symbol) = {
    try { sym.fullName } catch { case _ => "("+sym.name+")<name-error>" }
  }
}

