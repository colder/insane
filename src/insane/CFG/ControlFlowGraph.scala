package insane
package CFG

import utils.Graphs._

final case class CFGVertex[T](name: String);

case class CFGMutVertex[T](v: CFGVertex[T]) extends MutVertexAbs[CFGEdge[T]] {
  val name = v.name
}

case class CFGEdge[T](v1: CFGMutVertex[T], label: T, v2: CFGMutVertex[T]) extends LabeledEdgeAbs[T, CFGMutVertex[T]]

class ControlFlowGraph[T] extends LabeledMutableDirectedGraphImp[T, CFGMutVertex[T], CFGEdge[T]] {
  private var nextVertexName = 0
  private var vToMutV = Map[CFGVertex[T], CFGMutVertex[T]]()

  def deepCopy() = {
    val newCFG = new ControlFlowGraph[T]()
    newCFG.nextVertexName = this.nextVertexName

    val vertexMap: Map[CFGMutVertex[T], CFGMutVertex[T]] = V.map(mv => mv -> new CFGMutVertex[T](mv.v)).toMap

    for (e <- E) {
      newCFG += (vertexMap(e.v1), e.label, vertexMap(e.v2))
    }

    newCFG
  }

  def newNamedVertex(prefix: String) = {
    nextVertexName += 1
    new CFGMutVertex(new CFGVertex[T](prefix+nextVertexName))
  }
  def newVertex = newNamedVertex("v")


  def +=(v1: CFGVertex[T], lab: T, v2: CFGVertex[T]) {
    val mv1 = vToMutV.get(v1) match {
      case Some(mv1) =>
        mv1
      case None =>
        val n = new CFGMutVertex(v1)
        vToMutV += v1 -> n
        n
    }

    val mv2 = vToMutV.get(v2) match {
      case Some(mv2) =>
        mv2
      case None =>
        val n = new CFGMutVertex(v2)
        vToMutV += v2 -> n
        n

    }
    this += (CFGEdge[T](mv1, lab, mv2))
  }

  def +=(v1: CFGMutVertex[T], lab: T, v2: CFGMutVertex[T]) {
    this += (CFGEdge[T](v1, lab, v2))
  }

  val entry: Vertex = new CFGMutVertex(new CFGVertex[T]("entry"))
  val exit: Vertex  = new CFGMutVertex(new CFGVertex[T]("exit"))

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
