package insane
package alias

import utils.Graphs._

trait PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._

  object PointToGraphs {
    sealed abstract class Field
    case class SymField(symbol: Symbol) extends Field
    case object ArrayFields extends Field

    sealed abstract class Node(val name: String) extends VertexAbs[Edge]

    case class PNode(pId: Int)    extends Node("Np("+pId+")")
    case class INode(pPoint: Int) extends Node("Ni(@"+pPoint+")")
    case class LNode(pId: Int)    extends Node("Nl("+pId+")")

    case object GBNode extends Node("Ngb")

    sealed abstract class Edge(val v1: Node, val label: Field, val v2: Node) extends LabeledEdgeAbs[Field, Node]

    case class IEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)
    case class OEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)

    type PointToGraph = LabeledImmutableDirectedGraphImp[Field, Node, Edge]
  }
}
