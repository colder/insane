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

  class FunctionCFGCopier() {
    import CFGTrees._
    import PointToGraphs._

    type Vertex = CFGVertex[Statement]

    object PTEnvCFGCopier extends PTEnvCopier {
      override val graphCopier = new GraphCopier {
        override def copyNode(n: Node) = n match {
          case LVNode(ref, types) =>
            LVNode(copyRef(ref), types)
          case _ =>
            super.copyNode(n)
        }
      }
      override def copyLocRef(ref: CFG.Ref): CFG.Ref = copyRef(ref)
    }

    var vertexMap = Map[Vertex, Vertex]()

    def copyStmt(e: Statement) = {
      val newStmt = e match {
        case stmt: AssignCast =>
          new AssignCast(copyRef(stmt.r), copyRef(stmt.rhs), copyType(stmt.tpe)) 
        case stmt: AssignTypeCheck =>
          new AssignTypeCheck(copyRef(stmt.r), copyRef(stmt.lhs), copyType(stmt.tpe)) 
        case stmt: AssignVal =>
          new AssignVal(copyRef(stmt.r), copySV(stmt.v)) 
        case stmt: AssignFieldRead =>
          new AssignFieldRead(copyRef(stmt.r), copyRef(stmt.obj), copySymbol(stmt.field)) 
        case stmt: AssignFieldWrite =>
          new AssignFieldWrite(copyRef(stmt.obj), copySymbol(stmt.field), copySV(stmt.rhs)) 
        case stmt: AssignNew =>
          new AssignNew(copyRef(stmt.r), copyType(stmt.tpe)) 
        case stmt: AssignApplyMeth =>
          new AssignApplyMeth(copyRef(stmt.r), copySV(stmt.obj), copySymbol(stmt.meth), stmt.args.map(copySV), stmt.isDynamic) 
        case stmt: CFGTrees.AssertEQ =>
          new CFGTrees.AssertEQ(copySV(stmt.lhs), copySV(stmt.rhs)) 
        case stmt: CFGTrees.AssertNE =>
          new CFGTrees.AssertNE(copySV(stmt.lhs), copySV(stmt.rhs)) 
        case stmt: Branch =>
          new Branch(copyBC(stmt.cond)) 
        case stmt: Effect =>
          new Effect(PTEnvCFGCopier.copy(stmt.env), stmt.name) 
        case _ =>
          sys.error("Unnexpected edge type at this point")
      }

      newStmt setTreeFrom e
    }

    def copyRef(r: CFGTrees.Ref) = r match {
      case r: ThisRef  => copyThisRef(r)
      case r: SuperRef => copySuperRef(r)
      case r: TempRef  => copyTmpRef(r)
      case r: ObjRef   => copyObjRef(r)
      case r: SymRef   => copySymref(r)
    }

    def copyThisRef(r: ThisRef) = r
    def copySuperRef(r: SuperRef) = r
    def copyObjRef(r: ObjRef) = r

    def copySymref(r: SymRef): Ref = r
    def copyTmpRef(r: TempRef): Ref = r

    def copySV(sv: SimpleValue) = sv match {
      case r: Ref =>
        copyRef(r)
      case _ =>
        sv
    }

    def copyType(t: Type) = t
    def copySymbol(s: Symbol) = s

    def copyBC(bc: BranchCondition) = bc match {
      case bc: IfTrue => 
        new IfTrue(copySV(bc.sv)) setTreeFrom bc
      case bc: IfFalse =>
        new IfFalse(copySV(bc.sv)) setTreeFrom bc
      case bc: IfEqual =>
        new IfEqual(copySV(bc.rhs), copySV(bc.lhs)) setTreeFrom bc
      case bc: IfNotEqual =>
        new IfNotEqual(copySV(bc.rhs), copySV(bc.lhs)) setTreeFrom bc
    }

    def getVertex(v: Vertex): Vertex = {
      vertexMap.get(v) match {
        case Some(nv) =>
          nv

        case None =>
          val nv = copyVertex(v)
          vertexMap += v -> nv
          nv
      }
    }

    def copyVertex(v: Vertex): Vertex = new Vertex(v.name, v.id)

    def copy(cfg: FunctionCFG): FunctionCFG = {
      val newCFG = new FunctionCFG(copySymbol(cfg.symbol), copyRef(cfg.retval), getVertex(cfg.entry), getVertex(cfg.exit))

      newCFG.thisRefs    = cfg.thisRefs map copyThisRef
      newCFG.objectRefs  = cfg.objectRefs map copyObjRef
      newCFG.superRefs   = cfg.superRefs map copySuperRef

      for (e <- cfg.E) {
        newCFG += (getVertex(e.v1), copyStmt(e.label), getVertex(e.v2))
      }

      newCFG
    }
  }

  class FunctionCFGRefRenamer(initRefMappings: Map[CFGTrees.Ref, CFGTrees.Ref]) extends FunctionCFGCopier {
    import CFGTrees._

    var refMappings: Map[Ref, Ref] = initRefMappings

    override def copySymref(r: CFGTrees.SymRef) = refMappings.get(r) match {
      case Some(sr) => sr
      case None =>
        val nr = SymRef(r.symbol, nextVersion)
        refMappings += r -> nr
        nr
    }

    override def copyTmpRef(r: CFGTrees.TempRef) = refMappings.get(r) match {
      case Some(sr) => sr
      case None =>
        val nr = TempRef(r.name, nextVersion, r.tpe)
        refMappings += r -> nr
        nr
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

