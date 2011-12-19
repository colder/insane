package insane
package utils

object Graphs {
  abstract class VertexAbs extends Serializable {
    val name: String

    override def toString = name

    val dotName = "v"+DotHelpers.uniqueName(this)
  }

  abstract class EdgeAbs[V <: VertexAbs] extends Serializable  {
    val v1: V
    val v2: V

    override def toString = v1 + "->" + v2
  }

  case class EdgeSimple[V <: VertexAbs](v1: V, v2: V) extends EdgeAbs[V]

  abstract class LabeledEdgeAbs[T, V <: VertexAbs] extends EdgeAbs[V] {
    val label: T

    val dotName = "e"+DotHelpers.uniqueName(this)
  }

  private var groupN: Int = 0
  def getFreshGroupN = {
    groupN += 1
    groupN
  }

  sealed abstract class GroupAbs extends Serializable {
    val parentGroup: Option[GroupAbs]
    val name: String

    val id = getFreshGroupN

  }

  case object RootGroup extends GroupAbs {
    val parentGroup = None
    val name = "root"
  }

  final case class Group(val name: String, val parent: GroupAbs) extends GroupAbs {
    val parentGroup = Some(parent)
  }

  /* Directed Graph */
  trait DirectedGraph[V <: VertexAbs, E <: EdgeAbs[V]] extends Serializable {
    type Vertex = V
    type Edge   = E
    /** The vertices */
    def V: Set[Vertex]
    /** The edges */
    def E: Set[Edge]
    /** The groups of vertices */
    def G: Set[GroupAbs]
    /** Returns the set of incoming edges for a given vertex */
    def inEdges(v: Vertex): Set[Edge]
    /** Returns the set of outgoing edges for a given vertex */
    def outEdges(v: Vertex): Set[Edge]
    /** The groups of vertices */
    def vToG: Map[Vertex, GroupAbs]
    /** Returns the set of edges between two vertices */
    def edgesBetween(from: Vertex, to: Vertex) = {
      E.filter(e => (e.v1 == from && e.v2 == to))
    }
  }

  /** Mutable Directed Graph */
  trait MutableDirectedGraph[V <: VertexAbs, E <: EdgeAbs[V]] extends DirectedGraph[V, E] {
    /** Adds a new vertex  */
    def += (v: Vertex)
    /** Adds a new edge */
    def += (e: Edge)
    /** Removes a vertex from the graph */
    def -= (from: Vertex)
    /** Removes an edge from the graph */
    def -= (from: Edge)
    /** Returns the set of incoming edges for a given vertex */
    def inEdges(v: Vertex): Set[Edge]
    /** Returns the set of outgoing edges for a given vertex */
    def outEdges(v: Vertex): Set[Edge]
  }

  /** Immutable Directed Graph */
  trait ImmutableDirectedGraph[V <: VertexAbs, E <: EdgeAbs[V], +This <: ImmutableDirectedGraph[V,E,This]] extends DirectedGraph[V, E] {

    protected type That = This

    /** Adds a new vertex  */
    def + (v: Vertex): This
    /** Adds new vertices  */
    def ++ (vs: Iterable[Vertex]): This
    /** Adds a new edge */
    def + (e: Edge): This
    /** Removes a vertex from the graph */
    def - (from: Vertex): This
    /** Removes an edge from the graph */
    def - (from: Edge): This
    /** Returns the set of incoming edges for a given vertex */
    def inEdges(v: Vertex)  = E.filter(_.v2 == v)
    /** Returns the set of outgoing edges for a given vertex */
    def outEdges(v: Vertex) = E.filter(_.v1 == v)
  }

  case class ImmutableDirectedGraphImp[Vertex <: VertexAbs, Edge <: EdgeAbs[Vertex]](
    vertices: Set[Vertex],
    edges: Set[Edge],
    groups: Set[GroupAbs],
    vToG: Map[Vertex, GroupAbs],
    ins: Map[Vertex, Set[Edge]],
    outs: Map[Vertex, Set[Edge]]
  ) extends ImmutableDirectedGraph[Vertex, Edge, ImmutableDirectedGraphImp[Vertex, Edge]] {


    override def equals(o: Any): Boolean = (o : @unchecked) match {
      case other: ImmutableDirectedGraphImp[Vertex, Edge] =>
        this.vertices == other.vertices &&
        this.edges    == other.edges &&
        this.groups   == other.groups &&
        (this.ins.keySet ++ other.ins.keySet).forall  (k   => this.ins(k)  == other.ins(k)) &&
        (this.outs.keySet ++ other.outs.keySet).forall(k => this.outs(k) == other.outs(k))

      case _ => false
    }

    def this (vertices: Set[Vertex], edges: Set[Edge]) =
      this(vertices,
           edges,
           Set(RootGroup),
           vertices.map(_ -> RootGroup).toMap,
           edges.groupBy(_.v2).toMap.withDefaultValue(Set()),
           edges.groupBy(_.v1).toMap.withDefaultValue(Set()))

    def this() = this(Set(), Set())

    val V = vertices
    val E = edges
    val G = groups

    def + (v: Vertex) = copy(
      vertices = vertices+v,
      vToG     = vToG + (v -> RootGroup)
    )

    override def inEdges(v: Vertex)  = ins(v)
    override def outEdges(v: Vertex) = outs(v)

    def ++ (v: Iterable[Vertex]) = copy(
      vertices = vertices++v,
      vToG     = vToG ++ (v.map(_ -> RootGroup))
    )

    def + (e: Edge)   = copy(
      vertices = vertices + e.v1 + e.v2,
      vToG     = vToG + (e.v1 -> RootGroup) + (e.v2 -> RootGroup),
      edges    = edges + e,
      ins      = ins + (e.v2 -> (ins(e.v2) + e)),
      outs     = outs + (e.v1 -> (outs(e.v1) + e))
    )

    def - (v: Vertex) = copy(
      vertices = vertices-v,
      vToG     = vToG - v,
      edges    = edges -- outs(v) -- ins(v),
      ins      = ((ins - v)  map { case (vm, edges) => vm -> (edges -- outs(v)) }).withDefaultValue(Set()) ,
      outs     = ((outs - v) map { case (vm, edges) => vm -> (edges -- ins(v))  }).withDefaultValue(Set())
    )

    def - (e: Edge)   = copy(
      vertices = vertices,
      edges    = edges-e,
      ins      = ins + (e.v2 -> (ins(e.v2) - e)),
      outs     = outs + (e.v1 -> (outs(e.v1) - e))
    )

    def union(that: That): That = copy(
      vertices = this.V ++ that.V,
      edges    = this.E ++ that.E,
      groups   = this.groups ++ that.groups,
      vToG     = this.vToG ++ that.vToG,
      ins      = ((this.ins.keySet  ++ that.ins.keySet) map { k => (k -> (this.ins(k) ++ that.ins(k))) }).toMap.withDefaultValue(Set()),
      outs     = ((this.outs.keySet ++ that.outs.keySet) map { k => (k -> (this.outs(k) ++ that.outs(k))) }).toMap.withDefaultValue(Set())
    )

    override def toString = "IDGraph[V: "+vertices+" | E:"+edges+" | vToG: "+vToG+" | Groups: "+groups+"]"
  }

  class MutableDirectedGraphImp[Vertex <: VertexAbs, Edge <: EdgeAbs[Vertex]] extends MutableDirectedGraph[Vertex, Edge] {

    private var vertices = Set[Vertex]()
    private var edges    = Set[Edge]()
    private var groups   = Set[GroupAbs](RootGroup)
    private var currentGroup: GroupAbs = RootGroup
    private var ins      = Map[Vertex, Set[Edge]]().withDefaultValue(Set())
    private var outs     = Map[Vertex, Set[Edge]]().withDefaultValue(Set())

    var vToG   = Map[Vertex, GroupAbs]()

    def inEdges(v: Vertex)  = ins(v)
    def outEdges(v: Vertex) = outs(v)

    def V = vertices
    def E = edges
    def G = groups

    def +=(v: Vertex) = {
      addVertex(v)
    }

    def +=(e: Edge) = {
      edges += e
      addVertex(e.v1)
      addVertex(e.v2)
      outs += e.v1 -> (outs(e.v1) + e)
      ins  += e.v2 -> (ins(e.v2) + e)
    }

    protected def addVertex(v: Vertex) {
      if (!(vertices contains v)) {
        vertices += v
        inGroup(v, currentGroup)
      }
    }

    protected def delVertex(v: Vertex) = {
      vertices -= v
      vToG -= v
    }

    def -=(v: Vertex) = {
      delVertex(v)
      for (e <- outEdges(v) ++ inEdges(v)) {
        this -= e
      }
    }

    def -=(e: Edge) = {
      edges -= e

      outs += e.v1 -> (outs(e.v1) - e)
      ins  += e.v2 -> (ins(e.v2) - e)
    }

    /* Dot related stuff */
    def inGroup(v: Vertex, g: GroupAbs) {
      vToG += v -> g
    }

    def addGroup(gr: Group) {
      groups +=  gr
    }
    def newSubGroup(name: String): GroupAbs = {
      val gr = new Group(name, currentGroup)

      addGroup(gr)

      currentGroup = gr
      gr
    }

    def closeGroup() {
      assert(currentGroup.parentGroup != None)
      currentGroup = currentGroup.parentGroup.get
    }
  }

  type LabeledMutableDirectedGraphImp[LabelType, Vertex <: VertexAbs, Edge <: LabeledEdgeAbs[LabelType, Vertex]] = MutableDirectedGraphImp[Vertex, Edge]

  type LabeledImmutableDirectedGraphImp[LabelType, Vertex <: VertexAbs, Edge <: LabeledEdgeAbs[LabelType, Vertex]] = ImmutableDirectedGraphImp[Vertex, Edge]


  class DotConverter[Vertex <: VertexAbs, Edge <: EdgeAbs[Vertex]](val graph: DirectedGraph[Vertex, Edge], val title: String, val prefix: String = "") {
    object Orentitations extends Enumeration {
      val Vertical,Horizontal = Value 
    }

    val orientation = Orentitations.Vertical

    /** The following method prints out a string readable using GraphViz. */
    override def toString: String = {
      val res = new StringBuffer()

      res append "digraph D {\n"
      res append " label=\""+DotHelpers.escape(title)+"\"\n"

      if (orientation == Orentitations.Horizontal) {
        res append " rankdir=\"LR\"\n"
      }

      drawGraph(res)

      res append "}\n"

      res.toString
    }

    def drawGraph(res: StringBuffer) {
      graph.G.foreach(g =>
        groupToString(res, g)
      )

      graph.E.foreach(edge =>
        edgeToString(res, edge)
      )
    }

    def groupToString(res: StringBuffer, g: GroupAbs) {
      if (g.parentGroup != None) {
        res append """
    subgraph cluster"""+g.id+""" {
        node [style=filled, color=white, shape=record];
        style=filled;
        labeljust=l;
        label="""+"\""+DotHelpers.escape(g.name)+"\""+""";
        color="""+DotHelpers.nextColor+";\n";
      }

      for (g2 <- graph.G if g2.parentGroup == Some(g)) {
        groupToString(res, g2)
      }

      for ((v2,g2) <- graph.vToG if g2 == g) {
        vertexToString(res, v2)
      }

      if (g.parentGroup != None) {
        res append """
    } """;
      }
    }

    def eToS(e: LabeledEdgeAbs[_,_])   = prefix+e.dotName
    def vToS(v: Vertex)                = prefix+v.dotName

    def edgeToString(res: StringBuffer, e: Edge): Unit = e match {
      case le: LabeledEdgeAbs[_, _] =>
        res append DotHelpers.box(eToS(le), le.label.toString)
        res append DotHelpers.arrow(vToS(e.v1), eToS(le))
        res append DotHelpers.arrow(eToS(le), vToS(e.v2))
      case le: EdgeAbs[_] =>
        res append DotHelpers.arrow(vToS(e.v1), vToS(e.v2))
    }

    def vertexToString(res: StringBuffer, v: Vertex) {
        res append (vToS(v) +" [label=\""+DotHelpers.escape(v.name)+"\"];\n")
    }

    /** Writes the graph to a file readable with GraphViz. */
    def writeFile(fname: String) {
      import java.io.{BufferedWriter, FileWriter}
      val out = new BufferedWriter(new FileWriter(fname))
      out.write(toString)
      out.close()
    }
  }
}
