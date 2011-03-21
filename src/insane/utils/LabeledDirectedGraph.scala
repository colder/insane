package insane
package utils

/** Mutable Directed Graph with Labels */
abstract trait LabeledDirectedGraph[LabelType] {
  /** The type of the vertices */
  type Vertex
  /** The type of the edges */
  type Edge
  /** The vertices */
  def V: Set[Vertex]
  /** The edges */
  def E: Set[Edge]
  /** Adds a new vertex to the graph */
  def newNamedVertex(prefix: String): Vertex
  /** Adds a new vertex to the graph */
  def newVertex: Vertex
  /** Adds a new labeled edge between two vertices */
  def += (from: Vertex, lab: LabelType, to: Vertex)
  /** Returns the set of incoming edges for a given vertex */
  def inEdges(v: Vertex): Set[Edge]
  /** Returne the set of outgoing edges for a given vertex */
  def outEdges(v: Vertex): Set[Edge]
  /** Returne the set of edges between two vertices */
  def edgesBetween(from: Vertex, to: Vertex): Set[Edge]
  /** Removes an edge from the graph */
  def -= (from: Vertex, lab: LabelType, to: Vertex)
}


case class VertexImp[L](var name: String) {
  var in: Set[EdgeImp[L]] = Set[EdgeImp[L]]()
  var out: Set[EdgeImp[L]] = Set[EdgeImp[L]]()
  override def toString = name
}

case class EdgeImp[L](v1: VertexImp[L], lab: L, v2: VertexImp[L]) {
  val name = EdgeCounter.getNew
  override def toString = name + ":" + v1 + "-" + lab + "-" + v2
}

object EdgeCounter {
  var count = 0
  def getNew: String = {
    count = count + 1

    "e" + count
  }
}

abstract class LabeledDirectedGraphImp[LabelType] extends LabeledDirectedGraph[LabelType] {
  type Vertex = VertexImp[LabelType]
  type Edge   = EdgeImp[LabelType]

  private var vertices = Set[Vertex]()
  private var edges    = Set[Edge]()

  def V = vertices
  def E = edges

  var counter = 0
  def newVertex = {
    counter += 1
    newNamedVertex("v"+counter)
  }

  def newNamedVertex(name: String) = {
    new Vertex(name)
  }

  def inEdges(v: Vertex)  = v.in
  def outEdges(v: Vertex) = v.out

  def +=(v: Vertex) = {
    addVertex(v)
  }

  def +=(from: Vertex, lab : LabelType, to: Vertex) = {
    val edge = EdgeImp[LabelType](from, lab, to)
    edges += edge
    addVertex(from)
    addVertex(to)
    from.out += edge
    to.in += edge
  }

  def addVertex(v: Vertex) = {
    vertices += v
    vToG += v -> currentGroup
  }

  def delVertex(v: Vertex) = {
    vertices -= v
    vToG -= v
  }

  def edgesBetween(from: Vertex, to: Vertex) = {
    edges.filter(e => (e.v1 == from && e.v2 == to))
  }

  def -=(from: Vertex, lab: LabelType, to: Vertex) = -=(from,lab, to)

  def -=(from: Vertex, lab: LabelType, to: Vertex, cleanEmpty: Boolean) = {
    val edge = EdgeImp[LabelType](from, lab, to)

    edges -= edge

    if (cleanEmpty && !edges.exists(e => (e.v1 == from || e.v2 == from))) {
        delVertex(from)
    }
    if (cleanEmpty && !edges.exists(e => (e.v1 == to || e.v2 == to))) {
        delVertex(to)
    }

    from.out -= edge
    to.in -= edge
  }

  /* Dot related stuff */
  def escape(s: String) =
    s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\"", "\\\\\"").replaceAll("\\\n", "\\\\n").replaceAll("[^<>a-zA-Z0-9;$.,!# \t=^:_\\\\\"'*+/&()\\[\\]{}-]", "?")

  val bgColors = List("bisque", "khaki", "mistyrose", "lightcyan", "mediumorchid", "aquamarine", "antiquewhite")

  def color(id: Int) = {
    val colornumber: String = if((id/bgColors.size)%3 == 0) "" else ((id/bgColors.size)%3)+"";
    bgColors(id%bgColors.size)+colornumber
  }

  private var nameId  = 0
  private var nameIds = Map[String, Int]()

  def nameToId(name: String) = {
    if (!(nameIds contains name)) {
      nameId += 1
      nameIds += name -> nameId
    }

    nameIds(name)
  }

  private var currentGroup: GroupAbs = RootGroup
  private var groups = List[GroupAbs]()
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
        label="""+escape(name)+""";
        color="""+color(id)+";\n";
      }

      for (g <- groups if g.parentGroup == Some(this)) {
        res append g.toDotString(res)
      }

      for ((v,g) <- vToG if g == this) {
        res append nameToId(v.name) +" [label=\""+escape(v.name)+"\"]";
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

  final class Group(val name: String, val parent: Group) extends GroupAbs {
    val parentGroup = Some(parent)
  }

  /** The following method prints out a string readable using GraphViz. */
  def toDotString(title: String): String = {
    var res: StringBuffer = new StringBuffer()
    def emit(s: String) = res.append(s)
    def arrow(x: String, y: String) = {
      emit("  "); emit(x); emit(" -> "); emit(y); emit(";\n")
    }

    def makeBoxed(id : String, name : String) = {
      emit(id); emit("[shape=box,color=lightblue,style=filled,label=\"");
      emit(escape(name)); emit("\"];\n")
    }

    emit("digraph D {\n")
    emit(" label=\""+title+"\"\n")

    groups.foreach(g =>
      g.toDotString(res)
    )

    edges.foreach(edge => {
      arrow(edge.v1.name, edge.name)
      arrow(edge.name, edge.v2.name)
      makeBoxed(edge.name, edge.lab.toString)
    })

    emit("}\n") 
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
