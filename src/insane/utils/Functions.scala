package insane
package utils

import scala.tools.nsc._
import CFG.{ControlFlowGraph,CFGVertex,CFGEdge,CFGGlobalCounters}
import Graphs._

trait Functions {
  self : AnalysisComponent =>

  import global._
  import icodes._

  sealed abstract class AbsFunction {
    val symbol: Symbol
    val body: Tree

    override def toString = symbol.name.toString()

    /* CFG storage for each function */
    var optCFG:   Option[FunctionCFG] = None

    lazy val cfg   = optCFG.getOrElse(new CFGConverterFromAST(this).getCFG)

    def setCFG(cfg: FunctionCFG) = optCFG = Some(cfg)

    var ptCFGs     = Map[TypeSignature, (FunctionCFG, Boolean)]()
    var flatPTCFGs = Map[TypeSignature, FunctionCFG]()

    var flatPTCFGsTime = Map[TypeSignature, Long]().withDefaultValue(0l)

    val args: Seq[Symbol]

    /* contracts */
    var contrRequires = Seq[Requires]()
    var contrEnsures  = Seq[Ensures]()
    var contrAsserts  = Seq[Assert]()

    /* Call graph info */
    var callTargets   = Map[CFG.AssignApplyMeth, (Set[Symbol], Boolean)]()

    def uniqueName = {
      uniqueFunctionName(symbol)
    }

    var implOfMethod : Option[Symbol] = None
    var implOfClass  : Option[Symbol] = None
  }

  class NamedFunction(val symbol: Symbol, val name: Name, val args: Seq[Symbol], val body: Tree) extends AbsFunction

  class AnnonFunction(val symbol: Symbol, val args: Seq[Symbol], val body: Tree) extends AbsFunction

  class ICodeFunction(val iMethod: IMethod, val iClass: IClass) extends AbsFunction {
    val args              = iMethod.params.map(_.sym)
    val symbol            = iMethod.symbol
    lazy val body         = reporter.fatal("ICode functions have no body")

    override lazy val cfg = optCFG.getOrElse(new CFGConverterFromICode(this).getCFG)
  }

  object ICodeFunction {

    def loadICodeFromClass(sym: Symbol): Option[IClass] = {
      icode(sym) match {
        case c @ Some(iClass) =>
          c
        case None =>
          try {
            val (c1, c2) = icodeReader.readClass(sym)

            assert(c1.symbol == sym || c2.symbol == sym,
              "c1.symbol = %s, c2.symbol = %s, sym = %s".format(c1.symbol, c2.symbol, sym))
            loaded += (c1.symbol -> c1)
            loaded += (c2.symbol -> c2)

            loaded.get(sym)
          } catch {
            case e: Throwable => // possible exceptions are MissingRequirementError, IOException and TypeError -> no better common supertype
              reporter.warn(List("Failed to load IClass for "+sym.fullName, e.getMessage))
              e.printStackTrace
              None
          }
      }
    }

    def fromSymbol(sym: Symbol): Option[ICodeFunction] = {
      loadICodeFromClass(sym.owner) match { // Force load if necessary
        case Some(iClass) =>
          iClass.lookupMethod(sym) match {
            case Some(iMethod) =>
              Some(new ICodeFunction(iMethod, iClass))
            case None =>
              debugSymbol(sym.owner)
              debugSymbol(sym.companionClass)
              reporter.warn("No ICode available for method "+sym.fullName)
              reporter.warn(List("Found methods: ") ::: iClass.methods.map(m => "  - "+m.toString))
              None
          }
        case None =>
          None
      }
    }
  }


  final case class FunctionCFG(
    val symbol: Symbol,
    val retval: CFGTrees.Ref,
    val args: Seq[CFGTrees.Ref],
    val mainThisRef: CFGTrees.ThisRef,
    val isFlat: Boolean,
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
           new CFGTrees.ThisRef(symbol.owner, NoUniqueID, symbol.owner.tpe),
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

    def isBottom: Boolean = isFlat && getFlatEffect.isBottom

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

    val graphCopier = new PTEnvCopier {
      override def copyRef(ref: CFG.Ref): CFG.Ref = FunctionCFGCopier.this.copyRef(ref)
      override def copyTypes(info: TypeInfo): TypeInfo = FunctionCFGCopier.this.copyTypes(info)
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
          new AssignApplyMeth(copyRef(stmt.r), copySV(stmt.obj), copySymbol(stmt.meth), stmt.args.map(copySV), stmt.typeArgs.map(copyTypeArg), stmt.isDynamic, stmt.excludedSymbols) 
        case stmt: CFGTrees.AssertEQ =>
          new CFGTrees.AssertEQ(copySV(stmt.lhs), copySV(stmt.rhs)) 
        case stmt: CFGTrees.AssertNE =>
          new CFGTrees.AssertNE(copySV(stmt.lhs), copySV(stmt.rhs)) 
        case stmt: Branch =>
          new Branch(copyBC(stmt.cond)) 
        case stmt: Effect =>
          new Effect(graphCopier.copy(stmt.env), stmt.name) 
        case Skip =>
          Skip
        case _ =>
          sys.error("Unnexpected edge type at this point")
      }

      newStmt setTreeFrom e
    }

    def copyTypeArg(t: global.Tree) = t
    def copyTypes(t: TypeInfo) = t

    def copyRef(r: CFGTrees.Ref) = r match {
      case r: ThisRef  => copyThisRef(r)
      case r: SuperRef => copySuperRef(r)
      case r: TempRef  => copyTmpRef(r)
      case r: ObjRef   => copyObjRef(r)
      case r: SymRef   => copySymref(r)
    }

    def copyThisRef(r: ThisRef)   = 
      ThisRef(copySymbol(r.symbol), r.version, copyType(r.tpe))

    def copySuperRef(r: SuperRef) = 
      SuperRef(copySymbol(r.symbol), r.version, copyType(r.tpe))

    def copyObjRef(r: ObjRef)     =
      ObjRef(copySymbol(r.symbol), copyType(r.tpe))

    def copySymref(r: SymRef): Ref  =
      SymRef(copySymbol(r.symbol), r.version, copyType(r.tpe))

    def copyTmpRef(r: TempRef): Ref = {
      TempRef(r.name, r.version, copyType(r.tpe))
    }

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
        cfg.objectRefs map copyObjRef,
        cfg.superRefs map copySuperRef,
        getVertex(cfg.entry),
        getVertex(cfg.exit),
        copyGraph(cfg.graph)
      )
    }
  }

  class FunctionCFGInliner(initRefMappings: Map[CFGTrees.Ref, CFGTrees.Ref], typeMap: TypeMap, callSite: UniqueID) extends FunctionCFGCopier {
    import CFGTrees._

    var refMappings: Map[Ref, Ref] = initRefMappings

    override def copySymref(r: CFGTrees.SymRef) = refMappings.get(r) match {
      case Some(sr) => sr
      case None =>
        val nr = SymRef(r.symbol, r.version safeAdd callSite, copyType(r.tpe))
        refMappings += r -> nr
        nr
    }


    override def copyTypeArg(t: global.Tree) = t match {
      case tt: TypeTree =>
        TypeTree(typeMap(tt.tpe)).setPos(tt.pos)
      case _ =>
        reporter.debug("@@@@@@@@@ I don't know how to copy this: "+t+":"+t.getClass)
        t
    }

    override def copyTypes(oset: TypeInfo) = {
      typeMap(oset)
    }

    override def copyType(tpe: Type) = {
      typeMap(tpe)
    }

    override def copyTmpRef(r: CFGTrees.TempRef) = refMappings.get(r) match {
      case Some(sr) => sr
      case None =>
        val nr = TempRef(r.name, r.version safeAdd callSite, copyType(r.tpe))
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

