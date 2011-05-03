package insane
package utils

object Graphs {
  abstract class VertexAbs[E <: EdgeAbs[_]] {
      val name: String
      var in  = Set[E]()
      var out = Set[E]()

      override def toString = name

      val dotName = DotHelpers.nextName
  }

  abstract class EdgeAbs[V <: VertexAbs[_]] {
    val v1: V
    val v2: V

    override def toString = v1 + "->" + v2
  }

  case class EdgeSimple[V <: VertexAbs[_]](v1: V, v2: V) extends EdgeAbs[V]

  abstract class LabeledEdgeAbs[T, V <: VertexAbs[_ <: LabeledEdgeAbs[T, _]]] extends EdgeAbs[V] {
    val label: T

    val dotName = DotHelpers.nextName
  }

  private var groupN: Int = 0
  def getFreshGroupN = {
    groupN += 1
    groupN
  }

  sealed abstract class GroupAbs {
    val parentGroup: Option[GroupAbs]
    val name: String

    val id = getFreshGroupN

  }

  object RootGroup extends GroupAbs {
    val parentGroup = None
    val name = "root"
  }

  final class Group(val name: String, val parent: GroupAbs) extends GroupAbs {
    val parentGroup = Some(parent)
  }

  /* Directed Graph */
  trait DirectedGraph[V <: VertexAbs[E], E <: EdgeAbs[V]] {
    type Vertex = V
    type Edge   = E
    /** The vertices */
    def V: Set[Vertex]
    /** The edges */
    def E: Set[Edge]
    /** The groups of vertices */
    def G: Set[GroupAbs]
    /** The groups of vertices */
    def vToG: Map[Vertex, GroupAbs]
    /** Returns the set of incoming edges for a given vertex */
    def inEdges(v: Vertex)  = v.in
    /** Returns the set of outgoing edges for a given vertex */
    def outEdges(v: Vertex) = v.out
    /** Returns the set of edges between two vertices */
    def edgesBetween(from: Vertex, to: Vertex) = {
      E.filter(e => (e.v1 == from && e.v2 == to))
    }
  }

  /** Mutable Directed Graph */
  trait MutableDirectedGraph[V <: VertexAbs[E], E <: EdgeAbs[V]] extends DirectedGraph[V, E] {
    /** Adds a new vertex  */
    def += (v: Vertex)
    /** Adds a new edge */
    def += (e: Edge)
    /** Removes a vertex from the graph */
    def -= (from: Vertex)
    /** Removes an edge from the graph */
    def -= (from: Edge)
  }

  /** Immutable Directed Graph */
  trait ImmutableDirectedGraph[V <: VertexAbs[E], E <: EdgeAbs[V], +This <: ImmutableDirectedGraph[V,E,This]] extends DirectedGraph[V, E] {

    protected type That = This

    /** Adds a new vertex  */
    def + (v: Vertex): This
    /** Adds a new edge */
    def + (e: Edge): This
    /** Removes a vertex from the graph */
    def - (from: Vertex): This
    /** Removes an edge from the graph */
    def - (from: Edge): This
  }

  class ImmutableDirectedGraphImp[Vertex <: VertexAbs[Edge], Edge <: EdgeAbs[Vertex]](val vertices: Set[Vertex], val edges: Set[Edge] ) extends ImmutableDirectedGraph[Vertex, Edge, ImmutableDirectedGraphImp[Vertex, Edge]] {

    val groups: Set[GroupAbs] = Set(RootGroup)
    val vToG: Map[Vertex, GroupAbs] = Map().withDefaultValue(RootGroup)

    def this() = this(Set(), Set())

    protected def create(v: Set[Vertex], e: Set[Edge]) = new ImmutableDirectedGraphImp(v,e)

    val V = vertices
    val E = edges
    val G = groups

    def + (v: Vertex) = create(vertices+v, edges)
    def + (e: Edge)   = create(vertices + e.v1 + e.v2, edges + e)
    def - (v: Vertex) = create(vertices-v, edges.filter(e => e.v1 != v && e.v2 != v))
    def - (e: Edge)   = create(vertices, edges-e)

    def union(that: That): That = create(this.V++that.V, this.E++that.E)
  }

  class MutableDirectedGraphImp[Vertex <: VertexAbs[Edge], Edge <: EdgeAbs[Vertex]] extends MutableDirectedGraph[Vertex, Edge] {

    private var vertices = Set[Vertex]()
    private var edges    = Set[Edge]()
    private var groups   = Set[GroupAbs](RootGroup)
    private var currentGroup: GroupAbs = RootGroup
    var vToG   = Map[Vertex, GroupAbs]()

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
      e.v1.out += e
      e.v2.in  += e
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
      for (e <- v.out ++ v.in) {
        this -= e
      }
    }

    def -=(e: Edge) = {
      edges -= e

      e.v1.out -= e
      e.v2.in  -= e
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

  type LabeledMutableDirectedGraphImp[LabelType, Vertex <: VertexAbs[Edge], Edge <: LabeledEdgeAbs[LabelType, Vertex]] = MutableDirectedGraphImp[Vertex, Edge]

  type LabeledImmutableDirectedGraphImp[LabelType, Vertex <: VertexAbs[Edge], Edge <: LabeledEdgeAbs[LabelType, Vertex]] = ImmutableDirectedGraphImp[Vertex, Edge]


  class DotConverter[Vertex <: VertexAbs[Edge], Edge <: EdgeAbs[Vertex]](val graph: DirectedGraph[Vertex, Edge], val title: String) {
    /** The following method prints out a string readable using GraphViz. */
    override def toString: String = {
      val res = new StringBuffer()

      res append "digraph D {\n"
      res append " label=\""+DotHelpers.escape(title)+"\"\n"
      res append " rankdir=\"LR\"\n"

      graph.G.foreach(g =>
        groupToString(res, g)
      )

      graph.E.foreach(edge =>
        edgeToString(res, edge)
      )

      res append "}\n"

      res.toString
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

    def edgeToString(res: StringBuffer, e: Edge) = e match {
      case le: LabeledEdgeAbs[_, _] =>
        res append DotHelpers.box(le.dotName, le.label.toString)
        res append DotHelpers.arrow(e.v1.dotName, le.dotName)
        res append DotHelpers.arrow(le.dotName, e.v2.dotName)
      case le: EdgeAbs[_] =>
        res append DotHelpers.arrow(e.v1.dotName, e.v2.dotName)
    }

    def vertexToString(res: StringBuffer, v: Vertex) {
        res append (v.dotName +" [label=\""+DotHelpers.escape(v.name)+"\"];\n")
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
