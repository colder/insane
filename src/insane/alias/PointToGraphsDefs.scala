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

    case class VNode(ref: CFG.Ref)  extends Node(ref.toString)
    case class PNode(pId: Int)      extends Node("P("+pId+")")
    case class INode(pPoint: Int)   extends Node("I(@"+pPoint+")")
    case class LNode(pId: Int)      extends Node("L("+pId+")")

    case object GBNode extends Node("Ngb")


    sealed abstract class Edge(val v1: Node, val label: Field, val v2: Node) extends LabeledEdgeAbs[Field, Node]

    object Edge {
      def unapply(e: Edge) = Some((e.v1, e.label, e.v2))

    }

    case class IEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)
    case class OEdge(_v1: Node, _label: Field, _v2: Node) extends Edge(_v1, _label, _v2)
    case class VEdge(_v1: Node, _v2: Node) extends Edge(_v1, SymField(NoSymbol), _v2)

    type PointToGraph = LabeledImmutableDirectedGraphImp[Field, Node, Edge]

    class PTDotConverter(_graph: PointToGraph, _title: String, returnNodes: Set[Node]) extends DotConverter(_graph, _title) {
      import utils.DotHelpers

      def labelToString(f: Field): String = f match {
        case SymField(sym) =>
          sym.name.toString
        case ArrayFields =>
          "[*]"
      }

      override def edgeToString(res: StringBuffer, e: Edge) = e match {
        case VEdge(v1, v2) => // Variable edge, used to draw graphs only (var -> nodes)
          res append DotHelpers.arrow(e.v1.dotName, e.v2.dotName, List("arrowhead=vee", "color=blue4"))
        case IEdge(v1, l, v2) =>
          res append DotHelpers.labeledArrow(e.v1.dotName, labelToString(e.label), e.v2.dotName)
        case OEdge(v1, l, v2) =>
          res append DotHelpers.labeledDashedArrow(e.v1.dotName, labelToString(e.label), e.v2.dotName)
      }

      override def vertexToString(res: StringBuffer, v: Node) = {
        val opts = if(returnNodes contains v) List("shape=doublecircle") else List("shape=circle")
        
        v match {
          case VNode(ref) => // Variable node, used to draw graphs only (var -> nodes)
            res append DotHelpers.invisNode(v.dotName, v.name, List("fontcolor=blue4"))
          case LNode(pPoint) =>
            res append DotHelpers.dashedNode(v.dotName, v.name, opts)
          case PNode(pPoint) =>
            res append DotHelpers.dashedNode(v.dotName, v.name, opts)
          case INode(pPoint) =>
            res append DotHelpers.node(v.dotName, v.name, opts)
          case GBNode =>
            res append DotHelpers.node(v.dotName, v.name, opts)
        }
      }
    }

  }
}
