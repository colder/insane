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

    object Edge {
      def unapply(e: Edge) = Some((e.v1, e.label, e.v2))

    }

    case class IEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)
    case class OEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)

    type PointToGraph = LabeledImmutableDirectedGraphImp[Field, Node, Edge]

    class PTDotConverter(_graph: PointToGraph, _title: String) extends DotConverter(_graph, _title) {
      import utils.DotHelpers

      override def edgeToString(res: StringBuffer, e: Edge) = e match {
        case IEdge(v1, l, v2) =>
          res append DotHelpers.labeledDashedArrow(e.v1.dotName, e.label.toString, e.v2.dotName)
        case OEdge(v1, l, v2) =>
          res append DotHelpers.labeledArrow(e.v1.dotName, e.label.toString, e.v2.dotName)
      }

      override def vertexToString(res: StringBuffer, v: Node) = v match {
        case LNode(pPoint) =>
          res append DotHelpers.node(v.dotName, v.name)
        case PNode(pPoint) =>
          res append DotHelpers.node(v.dotName, v.name)
        case INode(pPoint) =>
          res append DotHelpers.node(v.dotName, v.name)
        case GBNode =>
          res append DotHelpers.node(v.dotName, v.name)
      }
    }

  }
}
