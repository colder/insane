package insane
package utils

import scala.tools.nsc._
import CFG.{ControlFlowGraph,CFGVertex,CFGEdge,CFGGlobalCounters}
import Graphs._

trait Functions {
  self : AnalysisComponent =>

  import global._

  sealed abstract class AbsFunction {
    val symbol: Symbol
    val body: Tree

    override def toString = symbol.name.toString()

    /* CFG storage for each function */
    var optCFG:   Option[FunctionCFG] = None

    lazy val cfg   = optCFG.getOrElse(CFGConverter.convert(this))

    def setCFG(cfg: FunctionCFG) = optCFG = Some(cfg)

    var ptCFGs     = Map[Seq[ObjectSet], (FunctionCFG, Boolean)]()
    var flatPTCFGs = Map[Seq[ObjectSet], FunctionCFG]()

    val args: Seq[ValDef]

    /* contracts */
    var contrRequires = Seq[Requires]()
    var contrEnsures  = Seq[Ensures]()
    var contrAsserts  = Seq[Assert]()

    /* Call graph info */
    var callTargets   = Map[CFG.AssignApplyMeth, (Set[Symbol], Boolean)]()

    def uniqueName = {
      uniqueFunctionName(symbol)
    }
  }

  final case class FunctionCFG(
    val symbol: Symbol,
    val retval: CFGTrees.Ref,
    val args: Seq[CFGTrees.Ref],
    val mainThisRef: CFGTrees.ThisRef,
    val isFlat: Boolean,
    val thisRefs:  Set[CFGTrees.ThisRef],
    val objectRefs: Set[CFGTrees.ObjRef],
    val superRefs: Set[CFGTrees.SuperRef],
    override val entry: CFGVertex,
    override val exit: CFGVertex,
    override val graph: LabeledImmutableDirectedGraphImp[CFGTrees.Statement, CFGVertex, CFGEdge[CFGTrees.Statement]]
  ) extends ControlFlowGraph[CFGTrees.Statement](entry, exit, graph) {

    def this(symbol: Symbol,
             args: Seq[CFGTrees.Ref],
             retval: CFGTrees.Ref,
             thisRef: CFGTrees.ThisRef,
             isFlat: Boolean,
             entry: CFGVertex = CFGGlobalCounters.newNamedVertex("entry"),
             exit: CFGVertex = CFGGlobalCounters.newNamedVertex("exit"),
             id: Int = CFGGlobalCounters.nextCFGID) = {

      this(symbol,
           retval,
           args,
           thisRef,
           isFlat,
           Set(thisRef),
           Set(),
           Set(),
           entry,
           exit,
           new LabeledImmutableDirectedGraphImp[CFGTrees.Statement, CFGVertex, CFGEdge[CFGTrees.Statement]](Set(entry, exit), Set()))
    }

    def this(symbol: Symbol,
             args: Seq[CFGTrees.Ref],
             retval: CFGTrees.Ref,
             isFlat : Boolean) = {

      this(symbol,
           args,
           retval,
           new CFGTrees.ThisRef(symbol.owner, NoUniqueID),
           isFlat)
    }

    def getFlatEffect: PTEnv = {
      assert(isFlat, "Trying to access flat effect of a non-flat CFG");

      graph.E.head match {
        case CFGEdge(_, ef: CFGTrees.Effect, _) =>
          ef.env

        case CFGEdge(_, CFGTrees.Skip, _) =>
          BottomPTEnv

        case e =>
          sys.error("Unexpected edge: "+e.label)
      }


    }

    def +(v1: CFGVertex, lab: CFGTrees.Statement, v2: CFGVertex): FunctionCFG = {
      this + CFGEdge[CFGTrees.Statement](v1, lab, v2)
    }

    def +(e: CFGEdge[CFGTrees.Statement]): FunctionCFG = {
      copy(graph = graph + e)
    }

    def -(v1: CFGVertex, lab: CFGTrees.Statement, v2: CFGVertex): FunctionCFG = {
      this - CFGEdge[CFGTrees.Statement](v1, lab, v2)
    }

    def -(e: CFGEdge[CFGTrees.Statement]): FunctionCFG = {
      copy(graph = graph - e)
    }

    def removeSkips: FunctionCFG = {
      var newCFG = this;

      for (v <- graph.V if v != entry && v != exit) {
        newCFG.graph.outEdges(v).toList match {
          case List(out @ CFGEdge(_, CFG.Skip, v2)) =>
            newCFG -= out

            for (in <- newCFG.graph.inEdges(v)) {
              newCFG -= in
              newCFG += (in.v1, in.label, v2)
            }
          case _ =>
        }
      }

      newCFG
    }

    def removeIsolatedVertices: FunctionCFG = {
      var newCFG = this;

      for (v <- graph.V if graph.inEdges(v).isEmpty && graph.outEdges(v).isEmpty && v != newCFG.entry && v != newCFG.exit) {
        newCFG = newCFG.copy(graph = newCFG.graph - v)
      }

      newCFG
    }

    def removeUnreachable: (FunctionCFG, Set[CFGTrees.Statement]) = {
      var lookAt = graph.V.filter(v => v != entry && graph.inEdges(v).isEmpty)
      val result = lookAt.flatMap(v => graph.outEdges(v).map(_.label))
      var newCFG = this

      while (!lookAt.isEmpty) {
        val v = lookAt.head
        lookAt = lookAt.tail

        if (v != entry && newCFG.graph.inEdges(v).isEmpty) {
          lookAt ++= newCFG.graph.outEdges(v).map(_.v2)
          newCFG = newCFG.copy(graph = newCFG.graph - v)
        }
      }

      (newCFG, result)
    }
  }

  class FunctionCFGCopier() {
    import CFGTrees._
    import PointToGraphs._

    type Vertex = CFGVertex
    type Edge   = CFGEdge[Statement]

    object PTEnvCFGCopier extends PTEnvCopier {
      override val graphCopier = new PTGraphCopier {
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

    def copyStmt(e: Statement): Statement = {
      val newStmt = e match {
        case bb: BasicBlock =>
          new BasicBlock(bb.stmts.map(copyStmt(_))) 
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
          new AssignApplyMeth(copyRef(stmt.r), copySV(stmt.obj), copySymbol(stmt.meth), stmt.args.map(copySV), stmt.isDynamic, stmt.excludedSymbols, stmt.fixedSymbols) 
        case stmt: CFGTrees.AssertEQ =>
          new CFGTrees.AssertEQ(copySV(stmt.lhs), copySV(stmt.rhs)) 
        case stmt: CFGTrees.AssertNE =>
          new CFGTrees.AssertNE(copySV(stmt.lhs), copySV(stmt.rhs)) 
        case stmt: Branch =>
          new Branch(copyBC(stmt.cond)) 
        case stmt: Effect =>
          new Effect(PTEnvCFGCopier.copy(stmt.env), stmt.name) 
        case Skip =>
          Skip
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

    def copyVertex(v: Vertex): Vertex = v

    def copyEdge(e: Edge) = new Edge(getVertex(e.v1), copyStmt(e.label), getVertex(e.v2))

    def copyGraph(gr: LabeledImmutableDirectedGraphImp[Statement, Vertex, Edge]) = {
      new ImmutableDirectedGraphImp[Vertex, Edge](
        gr.vertices map getVertex,
        gr.edges map copyEdge,
        gr.groups,
        gr.vToG map { case (v,g) => getVertex(v) -> g },
        (gr.ins  map { case (v, edges) => getVertex(v) -> (edges map copyEdge)}).withDefaultValue(Set()),
        (gr.outs map { case (v, edges) => getVertex(v) -> (edges map copyEdge)}).withDefaultValue(Set())
      )
    }

    def copy(cfg: FunctionCFG): FunctionCFG = {
      new FunctionCFG(
        copySymbol(cfg.symbol),
        copyRef(cfg.retval),
        cfg.args map copyRef,
        copyThisRef(cfg.mainThisRef),
        cfg.isFlat,
        cfg.thisRefs map copyThisRef,
        cfg.objectRefs map copyObjRef,
        cfg.superRefs map copySuperRef,
        getVertex(cfg.entry),
        getVertex(cfg.exit),
        copyGraph(cfg.graph)
      )
    }
  }

  class FunctionCFGRefRenamer(initRefMappings: Map[CFGTrees.Ref, CFGTrees.Ref], callSite: UniqueID) extends FunctionCFGCopier {
    import CFGTrees._

    var refMappings: Map[Ref, Ref] = initRefMappings

    override def copySymref(r: CFGTrees.SymRef) = refMappings.get(r) match {
      case Some(sr) => sr
      case None =>
        val nr = SymRef(r.symbol, r.version safeAdd callSite)
        refMappings += r -> nr
        nr
    }

    override def copyTmpRef(r: CFGTrees.TempRef) = refMappings.get(r) match {
      case Some(sr) => sr
      case None =>
        val nr = TempRef(r.name, r.version safeAdd callSite, r.tpe)
        refMappings += r -> nr
        nr
    }

    override def copyRef(r: CFGTrees.Ref) = refMappings.get(r) match {
      case Some(nr) => nr
      case None =>
        super.copyRef(r)
    }

    override def copyVertex(v: Vertex) = new Vertex(v.name, CFGGlobalCounters.nextVertexID)
  }

  class NamedFunction(val symbol: Symbol, val name: Name, val args: Seq[ValDef], val body: Tree) extends AbsFunction

  class AnnonFunction(val symbol: Symbol, val args: Seq[ValDef], val body: Tree) extends AbsFunction

  def uniqueFunctionName(sym: Symbol) = {
    safeFullName(sym)+"("+sym.tpe.toString+")"
  }

  def safeFileName(str: String) = {
    val limit = 200
    val name  = str.replaceAll(" ", "_").replaceAll("[^a-zA-Z0-9_.-]", "-")

    if (name.length > limit) {
      val stripped = name.substring(0, limit-11)+"-"+str.hashCode
    } else {
      name
    }
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

