package insane
package CFG

import utils.Graphs._
import utils.DotHelpers
import utils.GlobalCounters.getCFGCounter

final case class CFGVertex(name: String, id: Int) extends VertexAbs {
    override def toString = name+"#"+id
    override val dotName  = DotHelpers.escapeStrict(name)+"__"+id
}

object CFGVertex {
  def newNamedVertex(name: String): CFGVertex = {
    new CFGVertex(name, getCFGCounter)
  }
}

final case class CFGEdge[T](v1: CFGVertex, label: T, v2: CFGVertex) extends LabeledEdgeAbs[T, CFGVertex]

class ControlFlowGraph[T](
  val entry: CFGVertex,
  val exit: CFGVertex,
  val graph: LabeledImmutableDirectedGraphImp[T, CFGVertex, CFGEdge[T]]
) {

  def newNamedVertex(name: String): CFGVertex = CFGVertex.newNamedVertex(name)
  def newVertex = newNamedVertex("v")
}
