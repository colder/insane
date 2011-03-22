package insane
package CFG

import utils._

case class CFGVertex[T](val name: String) extends VertexAbs[CFGEdge[T]]

case class CFGEdge[T](val v1: CFGVertex[T], val label: T, val v2: CFGVertex[T]) extends LabeledEdgeAbs[T, CFGVertex[T]]

class ControlFlowGraph[T] extends LabeledDirectedGraphImp[T, CFGVertex[T], CFGEdge[T]] {
  private var nextVertexName = 0

  def newNamedVertex(prefix: String) = {
    nextVertexName += 1
    new Vertex(prefix+nextVertexName)
  }
  def newVertex = newNamedVertex("v")

  def +=(v1: CFGVertex[T], lab: T, v2: CFGVertex[T]): Unit = {
    this += (CFGEdge[T](v1, lab, v2))
  }


  val entry: Vertex = new Vertex("entry")
  val exit: Vertex  = new Vertex("exit")

  this += entry
  this += exit
}
