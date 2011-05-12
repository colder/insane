package insane
package alias

import utils.Graphs._

trait PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._

  object PointToGraphs {
    sealed abstract class FieldAbs
    case class SymField(symbol: Symbol) extends FieldAbs
    case object ArrayFields extends FieldAbs

    sealed abstract class NodeAbs(val name: String) extends VertexAbs[EdgeAbs]

    case class PNode(pId: Int)    extends NodeAbs("Np("+pId+")")
    case class INode(pPoint: Int) extends NodeAbs("Ni(@"+pPoint+")")
    case class LNode(pId: Int)    extends NodeAbs("Nl("+pId+")")

    case object GBNode extends NodeAbs("Ngb")

    sealed abstract class EdgeAbs(val v1: NodeAbs, val label: FieldAbs, val v2: NodeAbs) extends LabeledEdgeAbs[FieldAbs, NodeAbs]

    case class IEdge(_v1: NodeAbs, _label: FieldAbs, _v2: NodeAbs) extends EdgeAbs(_v1, _label, _v2)
    case class OEdge(_v1: NodeAbs, _label: FieldAbs, _v2: NodeAbs) extends EdgeAbs(_v1, _label, _v2)

    type PointToGraph = LabeledImmutableDirectedGraphImp[FieldAbs, NodeAbs, EdgeAbs]
  }
}
