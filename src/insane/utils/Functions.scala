package insane
package utils

import scala.tools.nsc._
import CFG.{ControlFlowGraph,CFGVertex,CFGEdge}

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

      Seq(LVNode(cfg.mainThisRef, ObjectSet.subtypesOf(symbol.owner.tpe))) ++
      args.zipWithIndex.map { case (a, i) =>
        if (isGroundClass(a.symbol.tpe.typeSymbol)) {
          typeToLitNode(a.symbol.tpe)
        } else {
          LVNode(CFGArgs(i), ObjectSet.subtypesOf(a.symbol.tpe))
        }
      }
    }


    def uniqueName = {
      uniqueFunctionName(symbol)
    }
  }

  final class FunctionCFG(
    val symbol: Symbol,
    val retval: CFGTrees.Ref,
    _entry: CFGVertex[CFGTrees.Statement],
    _exit: CFGVertex[CFGTrees.Statement],
    _id: Int = insane.CFG.CFGGlobalCounters.nextCFGID()
  ) extends ControlFlowGraph[CFGTrees.Statement](_entry, _exit, _id) {

    def this(symbol: Symbol, retval: CFGTrees.Ref, id: Int = insane.CFG.CFGGlobalCounters.nextCFGID()) = {
      this(symbol, retval, new CFGVertex("entry", id), new CFGVertex("exit", id), id)
    }

    val mainThisRef = CFGTrees.ThisRef(symbol.owner, 0)

    var thisRefs    = Set[CFGTrees.ThisRef]() + mainThisRef
    var objectRefs  = Set[CFGTrees.ObjRef]()

    var superRefs   = Set[CFGTrees.SuperRef]()

    def deepCopy() = {
      val vertexMap = V.map(v => v -> new Vertex(v.name, v.id)).toMap

      val newCFG = new FunctionCFG(symbol, retval, vertexMap(entry), vertexMap(exit))

      newCFG.thisRefs    = thisRefs
      newCFG.objectRefs  = objectRefs
      newCFG.superRefs   = superRefs

      for (e <- E) {
        newCFG += (vertexMap(e.v1), e.label, vertexMap(e.v2))
      }

      newCFG
    }
  }

  class FunctionCFGRenamer(cfg: FunctionCFG, initMappings: Map[CFGTrees.Ref, CFGTrees.Ref]) {
    import CFGTrees._

    var mappings: Map[Ref, Ref] = initMappings

    def rename: FunctionCFG = {
      val newCFG = new FunctionCFG(cfg.symbol, cfg.retval, cfg.entry, cfg.exit)

      for (e @ CFGEdge(v1, lab, v2) <- cfg.E) {
        newCFG += CFGEdge(v1, renStmt(lab), v2)
      }

      newCFG
    }

    def renStmt(e: Statement) = {
      val newStmt = e match {
        case stmt: AssignCast =>
          new AssignCast(renRef(stmt.r), renRef(stmt.rhs), renType(stmt.tpe)) 
        case stmt: AssignTypeCheck =>
          new AssignTypeCheck(renRef(stmt.r), renRef(stmt.lhs), renType(stmt.tpe)) 
        case stmt: AssignVal =>
          new AssignVal(renRef(stmt.r), renSV(stmt.v)) 
        case stmt: AssignFieldRead =>
          new AssignFieldRead(renRef(stmt.r), renRef(stmt.obj), renSymbol(stmt.field)) 
        case stmt: AssignFieldWrite =>
          new AssignFieldWrite(renRef(stmt.obj), renSymbol(stmt.field), renSV(stmt.rhs)) 
        case stmt: AssignNew =>
          new AssignNew(renRef(stmt.r), renType(stmt.tpe)) 
        case stmt: AssignApplyMeth =>
          new AssignApplyMeth(renRef(stmt.r), renSV(stmt.obj), renSymbol(stmt.meth), stmt.args.map(renSV), stmt.isDynamic) 
        case stmt: CFGTrees.AssertEQ =>
          new CFGTrees.AssertEQ(renSV(stmt.lhs), renSV(stmt.rhs)) 
        case stmt: CFGTrees.AssertNE =>
          new CFGTrees.AssertNE(renSV(stmt.lhs), renSV(stmt.rhs)) 
        case stmt: Branch =>
          new Branch(renBC(stmt.cond)) 
        case stmt: Effect =>
          // TODO
          new Effect(stmt.env, stmt.name) 
        case _ =>
          sys.error("Unnexpected edge type at this point")
      }

      newStmt setTreeFrom e
    }

    def renRef(r: CFGTrees.Ref) = {
      mappings.get(r) match {
        case Some(rr) =>
          rr
        case None =>
          val newRef = r match {
            case SymRef(symbol, version) =>
              SymRef(symbol, nextVersion)
            case TempRef(name, version, tpe) =>
              TempRef(name, nextVersion, tpe)
            case _ =>
              r
          }

          newRef setTreeFrom r

          mappings += r -> newRef

          newRef
      }
    }
    def renSV(sv: CFGTrees.SimpleValue) = sv match {
      case r: Ref =>
        renRef(r)
      case _ =>
        sv
    }
    def renType(t: Type) = t
    def renSymbol(s: Symbol) = s

    def renBC(bc: BranchCondition) = bc match {
      case bc: IfTrue => 
        new IfTrue(renSV(bc.sv)) setTreeFrom bc
      case bc: IfFalse =>
        new IfFalse(renSV(bc.sv)) setTreeFrom bc
      case bc: IfEqual =>
        new IfEqual(renSV(bc.rhs), renSV(bc.lhs)) setTreeFrom bc
      case bc: IfNotEqual =>
        new IfNotEqual(renSV(bc.rhs), renSV(bc.lhs)) setTreeFrom bc
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

