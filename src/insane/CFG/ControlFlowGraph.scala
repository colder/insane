package insane
package CFG

import utils.Graphs._

case class CFGVertex[T](name: String) extends VertexAbs[CFGEdge[T]]

case class CFGEdge[T](v1: CFGVertex[T], label: T, v2: CFGVertex[T]) extends LabeledEdgeAbs[T, CFGVertex[T]]

class ControlFlowGraph[T,R](val retval: R) extends LabeledMutableDirectedGraphImp[T, CFGVertex[T], CFGEdge[T]] {
  private var nextVertexName = 0

  def newNamedVertex(prefix: String) = {
    nextVertexName += 1
    new Vertex(prefix+nextVertexName)
  }
  def newVertex = newNamedVertex("v")

  def +=(v1: CFGVertex[T], lab: T, v2: CFGVertex[T]) {
    this += (CFGEdge[T](v1, lab, v2))
  }


  val entry: Vertex = new Vertex("entry")
  val exit: Vertex  = new Vertex("exit")

  def removeIsolatedVertices() {
    for (v <- V if v.in.isEmpty && v.out.isEmpty && v != entry && v != exit) {
      this -= v
    }
  }

  this += entry
  this += exit
}
