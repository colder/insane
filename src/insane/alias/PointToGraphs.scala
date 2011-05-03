package insane
package alias

import utils.Graphs._

object PointToGraphs {

  sealed abstract class PTNodeAbs[T] extends VertexAbs[PTEdgeAbs[T]] {
  }

  case class PTParamNode[T](pId: Int) extends PTNodeAbs[T] {
    val name = "Np("+pId+")"
  }
  case class PTInsNode[T](pPoint: Int) extends PTNodeAbs[T] {
    val name = "Ni("+pPoint+")"
  }
  case class PTLoadNode[T](pPoint: Int) extends PTNodeAbs[T] {
    val name = "Nl("+pPoint+")"
  }

  case class PTGblNode[T]() extends PTNodeAbs[T] {
     val name ="Nglb"
  }

  sealed abstract class PTEdgeAbs[T](val v1: PTNodeAbs[T], val label: T, val v2: PTNodeAbs[T]) extends LabeledEdgeAbs[T, PTNodeAbs[T]] {

  }

  case class PTInsEdge[T](_v1: PTNodeAbs[T], _label: T, _v2: PTNodeAbs[T]) extends PTEdgeAbs[T](_v1, _label, _v2)
  case class PTOutEdge[T](_v1: PTNodeAbs[T], _label: T, _v2: PTNodeAbs[T]) extends PTEdgeAbs[T](_v1, _label, _v2)

  case class PointToGraph[T,R](
       nodes:              Set[PTNodeAbs[T]] = Set[PTNodeAbs[T]](),
       override val edges: Set[PTEdgeAbs[T]] = Set[PTEdgeAbs[T]](),
       insideEdges:        Set[PTInsEdge[T]] = Set[PTInsEdge[T]](),
       outsideEdges:       Set[PTOutEdge[T]] = Set[PTOutEdge[T]](),
       locState:           Map[R, Set[PTNodeAbs[T]]] = Map[R, Set[PTNodeAbs[T]]]().withDefaultValue(Set[PTNodeAbs[T]]()),
       escapeNodes:        Set[PTNodeAbs[T]] = Set[PTNodeAbs[T]](),
       returnNodes:        Set[PTNodeAbs[T]] = Set[PTNodeAbs[T]]()
    ) extends LabeledImmutableDirectedGraphImp[T, PTNodeAbs[T], PTEdgeAbs[T]](nodes, edges) {


    override type Graph = PointToGraph[T,R]

    def union(that: PointToGraph[T,R]) = {
      PointToGraph[T,R](
        nodes++that.nodes,
        edges++that.edges,
        insideEdges++that.insideEdges,
        outsideEdges++that.outsideEdges,
        ((locState.keySet++that.locState.keySet).map(k => k -> (locState(k)++that.locState(k)))).toMap,
        escapeNodes++that.escapeNodes,
        returnNodes++that.returnNodes
      )
    }

    def makeCopy(vertices: Set[PTNodeAbs[T]] = this.nodes, edges: Set[PTEdgeAbs[T]] = this.edges): Graph = {
      copy(nodes = vertices, edges = edges)
    }
  }
}
