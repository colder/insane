package insane
package CFG

import utils.Graphs._

final case class CFGVertex(name: String, id: Int) extends VertexAbs {
    override def toString = name+"#"+id
    override val dotName  = name+"__"+id
}

final case class CFGEdge[T](v1: CFGVertex, label: T, v2: CFGVertex) extends LabeledEdgeAbs[T, CFGVertex]


object CFGGlobalCounters {
  private var _nextVertexID = 0
  private var _nextCFGID = 0

  def newNamedVertex(name: String): CFGVertex = {
    new CFGVertex(name, nextVertexID)
  }

  def nextVertexID: Int = {
    _nextVertexID += 1
    _nextVertexID
  }

  def nextCFGID: Int = {
    _nextCFGID += 1
    _nextCFGID
  }

}

class ControlFlowGraph[T](
  val entry: CFGVertex,
  val exit: CFGVertex,
  val graph: LabeledImmutableDirectedGraphImp[T, CFGVertex, CFGEdge[T]]
) {

  def newNamedVertex(name: String): CFGVertex = CFGGlobalCounters.newNamedVertex(name)
  def newVertex = newNamedVertex("v")
}
