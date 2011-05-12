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

    case class PointToGraph(
         nodes:              Set[NodeAbs] = Set[NodeAbs](),
         override val edges: Set[EdgeAbs] = Set[EdgeAbs](),
         insideEdges:        Set[IEdge] = Set[IEdge](),
         outsideEdges:       Set[OEdge] = Set[OEdge](),
         locState:           Map[CFG.Ref, Set[NodeAbs]] = Map[CFG.Ref, Set[NodeAbs]]().withDefaultValue(Set[NodeAbs]()),
         escapeNodes:        Set[NodeAbs] = Set[NodeAbs](),
         returnNodes:        Set[NodeAbs] = Set[NodeAbs]()
      ) extends LabeledImmutableDirectedGraphImp[FieldAbs, NodeAbs, EdgeAbs](nodes, edges) {



      override type Graph = PointToGraph

      def union(that: PointToGraph) = {
        PointToGraph(
          nodes++that.nodes,
          edges++that.edges,
          insideEdges++that.insideEdges,
          outsideEdges++that.outsideEdges,
          ((locState.keySet++that.locState.keySet).map(k => k -> (locState(k)++that.locState(k)))).toMap,
          escapeNodes++that.escapeNodes,
          returnNodes++that.returnNodes
        )
      }

      def makeCopy(vertices: Set[NodeAbs] = this.nodes, edges: Set[EdgeAbs] = this.edges): Graph = {
        copy(nodes = vertices, edges = edges)
      }
    }
  }
}
