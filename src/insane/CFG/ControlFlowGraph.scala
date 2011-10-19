package insane
package CFG

import utils.Graphs._

final case class CFGVertex[T](name: String, id: Int) extends MutVertexAbs[CFGEdge[T]] {

    override def toString = name+"#"+id
    override val dotName  = toString
}

case class CFGEdge[T](v1: CFGVertex[T], label: T, v2: CFGVertex[T]) extends LabeledEdgeAbs[T, CFGVertex[T]]


object CFGGlobalCounters {
  private var _nextVertexID = 0
  private var _nextCFGID = 0

  def newNamedVertex[T](name: String): CFGVertex[T] = {
    _nextVertexID += 1
    new CFGVertex[T](name, _nextVertexID)
  }

  def nextCFGID(): Int = {
    _nextCFGID += 1
    _nextCFGID
  }

}

class ControlFlowGraph[T] extends LabeledMutableDirectedGraphImp[T, CFGVertex[T], CFGEdge[T]] {
  private var vToMutV = Map[CFGVertex[T], CFGVertex[T]]()

  val id = CFGGlobalCounters.nextCFGID()

  def deepCopy() = {
    val newCFG = new ControlFlowGraph[T]()

    val vertexMap: Map[CFGVertex[T], CFGVertex[T]] = V.map(v => v -> new CFGVertex[T](v.name, v.id)).toMap

    for (e <- E) {
      newCFG += (vertexMap(e.v1), e.label, vertexMap(e.v2))
    }

    newCFG
  }

  def newNamedVertex(name: String): CFGVertex[T] = CFGGlobalCounters.newNamedVertex(name)

  def newVertex = newNamedVertex("v")

  def +=(v1: CFGVertex[T], lab: T, v2: CFGVertex[T]) {
    this += (CFGEdge[T](v1, lab, v2))
  }

  val entry: Vertex = new CFGVertex[T]("entry", id)
  val exit: Vertex  = new CFGVertex[T]("exit",  id)

  def removeIsolatedVertices() {
    for (v <- V if v.in.isEmpty && v.out.isEmpty && v != entry && v != exit) {
      this -= v
    }
  }

  // Removes and returns unreachable vertices
  def removeUnreachable(): Set[T] = {
    var lookAt = V.filter(v => v != entry && v.in.isEmpty)
    val result = lookAt.flatMap(_.out.map(_.label))

    while (!lookAt.isEmpty) {
      val v = lookAt.head
      lookAt = lookAt.tail

      if (v != entry && v.in.isEmpty) {
        lookAt ++= v.out.map(_.v2)
        this -= v
      }
    }

    result
  }

  this += entry
  this += exit
}
