package insane
package utils

abstract class VertexAbs[E <: EdgeAbs[_]] {
    val name: String
    var in  = Set[E]()
    var out = Set[E]()

    override def toString = name

    val dotName = DotHelpers.nextName

    def toDotString(res: StringBuffer) = {
        res append (dotName +" [label=\""+DotHelpers.escape(name)+"\"]")
    }
}

abstract class EdgeAbs[V <: VertexAbs[_]] {
  val name: String
  val v1: V
  val v2: V

  override def toString = name + ":" + v1 + "->" + v2

  def toDotString(res: StringBuffer) = {
    res append DotHelpers.arrow(v1.dotName, v2.dotName)
  }
}

abstract class LabeledEdgeAbs[T, V <: VertexAbs[_ <: LabeledEdgeAbs[T, _]]] extends EdgeAbs[V] {
  val label: T

  val dotName = DotHelpers.nextName

  override def toDotString(res: StringBuffer) = {
    res append DotHelpers.box(dotName, label.toString)
    res append DotHelpers.arrow(v1.dotName, dotName)
    res append DotHelpers.arrow(dotName, v2.dotName)
  }
}

/** Mutable Directed Graph */
abstract trait DirectedGraph[V <: VertexAbs[E], E <: EdgeAbs[V]] {
  type Vertex = V
  type Edge   = E
  /** The vertices */
  def V: Set[Vertex]
  /** The edges */
  def E: Set[Edge]
  /** Adds a new edge  */
  def += (v: Vertex)
  /** Adds a new vertex  */
  def += (e: Edge)
  /** Returns the set of incoming edges for a given vertex */
  def inEdges(v: Vertex): Set[Edge]
  /** Returne the set of outgoing edges for a given vertex */
  def outEdges(v: Vertex): Set[Edge]
  /** Returne the set of edges between two vertices */
  def edgesBetween(from: Vertex, to: Vertex): Set[Edge]
  /** Removes a vertex from the graph */
  def -= (from: Vertex)
  /** Removes an edge from the graph */
  def -= (from: Edge)
}

class DirectedGraphImp[Vertex <: VertexAbs[Edge], Edge <: EdgeAbs[Vertex]] extends DirectedGraph[Vertex, Edge] {

  private var vertices = Set[Vertex]()
  private var edges    = Set[Edge]()

  def V = vertices
  def E = edges

  def inEdges(v: Vertex)  = v.in
  def outEdges(v: Vertex) = v.out

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

  protected def addVertex(v: Vertex) = {
    vertices += v
    inGroup(v, currentGroup)
  }

  protected def delVertex(v: Vertex) = {
    vertices -= v
    vToG -= v
  }

  def edgesBetween(from: Vertex, to: Vertex) = {
    edges.filter(e => (e.v1 == from && e.v2 == to))
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

  private var currentGroup: GroupAbs = RootGroup

  def newSubGroup(name: String) = {
    val gr = new Group(name, currentGroup)
    groups = gr :: groups

    currentGroup = gr
  }

  def closeGroup = {
    assert(currentGroup.parentGroup != None)
    currentGroup = currentGroup.parentGroup.get
  }

  private var groups = List[GroupAbs](RootGroup)
  private var vToG   = Map[Vertex, GroupAbs]()

  private var groupN: Int = 0
  def getFreshGroupN = {
    groupN += 1
    groupN
  }

  sealed abstract class GroupAbs {
    val parentGroup: Option[GroupAbs]
    val name: String

    val id = getFreshGroupN

    def toDotString(res: StringBuffer) {
      if (parentGroup != None) {
        res append """
    subgraph cluster"""+id+""" {
        node [style=filled, color=white];
        style=filled;
        labeljust=l;
        label="""+DotHelpers.escape(name)+""";
        color="""+DotHelpers.nextColor+";\n";
      }

      for (g <- groups if g.parentGroup == Some(this)) {
        g.toDotString(res)
      }

      for ((v,g) <- vToG if g == this) {
        v.toDotString(res)
      }

      if (parentGroup != None) {
        res append """
    } """;
      }

    }
  }

  object RootGroup extends GroupAbs {
    val parentGroup = None
    val name = "root"
  }

  final class Group(val name: String, val parent: GroupAbs) extends GroupAbs {
    val parentGroup = Some(parent)
  }

  /** The following method prints out a string readable using GraphViz. */
  def toDotString(title: String): String = {
    var res: StringBuffer = new StringBuffer()

    res append "digraph D {\n"
    res append " label=\""+DotHelpers.escape(title)+"\"\n"

    groups.foreach(g =>
      g.toDotString(res)
    )

    edges.foreach(edge =>
      edge.toDotString(res)
    )

    res append "}\n"

    res.toString
  }

  /** Writes the graph to a file readable with GraphViz. */
  def writeDotToFile(fname: String, title: String): Unit = {
    import java.io.{BufferedWriter, FileWriter}
    val out = new BufferedWriter(new FileWriter(fname))
    out.write(toDotString(title))
    out.close
  }
}

class LabeledDirectedGraphImp[LabelType, Vertex <: VertexAbs[Edge], Edge <: LabeledEdgeAbs[LabelType, Vertex]] extends DirectedGraphImp[Vertex, Edge]
