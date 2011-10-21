package insane
package CFG

import utils.Graphs._

final case class CFGVertex[T](name: String, id: Int) extends MutVertexAbs[CFGEdge[T]] {
    override def toString = name+"#"+id
    override val dotName  = name+"__"+id
}

final case class CFGEdge[T](v1: CFGVertex[T], label: T, v2: CFGVertex[T]) extends LabeledEdgeAbs[T, CFGVertex[T]]


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

class ControlFlowGraph[T](val entry: CFGVertex[T], val exit: CFGVertex[T], val id: Int = CFGGlobalCounters.nextCFGID()) extends LabeledMutableDirectedGraphImp[T, CFGVertex[T], CFGEdge[T]] {
  

  def newNamedVertex(name: String): CFGVertex[T] = CFGGlobalCounters.newNamedVertex(name)

  def newVertex = newNamedVertex("v")

  def +=(v1: CFGVertex[T], lab: T, v2: CFGVertex[T]) {
    this += (CFGEdge[T](v1, lab, v2))
  }

  def this(id: Int = CFGGlobalCounters.nextCFGID()) = {
    this(new CFGVertex[T]("entry", id), new CFGVertex[T]("exit", id), id)
  }

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
