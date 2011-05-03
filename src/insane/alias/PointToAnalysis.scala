package insane
package alias

import utils._
import analysis._
import CFG.ControlFlowGraph

trait PointToAnalysis extends PointToGraphsDefs {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._

  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    case class Env(ptGraph: PointToGraph,
                   locState: Map[CFG.Ref, Set[Node]],
                   iEdges: Set[IEdge],
                   oEdges: Set[OEdge],
                   eNodes: Set[Node],
                   rNodes: Set[Node]) extends DataFlowEnvAbs[Env, CFG.Statement] {

      def union(that: Env) = {
        Env(ptGraph union that.ptGraph,
            (locState.keySet ++ that.locState.keySet).map(k => k -> (locState(k)++that.locState(k))).toMap,
            iEdges union that.iEdges,
            oEdges union that.oEdges,
            eNodes union that.eNodes,
            rNodes union that.rNodes)
      }

      def setL(ref: CFG.Ref, nodes: Set[Node]): Env = {
        copy(locState = locState + (ref -> nodes))
      }

      def getL(ref: CFG.Ref): Set[Node] = locState(ref)

      def addInsideNode(node: INode) =
        copy(ptGraph = ptGraph + node)

      def addIEdges(lv1: Set[Node], field: Field, lv2: Set[Node]) = {
        var newGraph = ptGraph
        for (v1 <- lv1; v2 <- lv2) {
          newGraph += IEdge(v1, field, v2)
        }
        copy(ptGraph = newGraph)
      }

      def addENodes(nodes: Set[Node]) = {
        copy(eNodes = eNodes ++ nodes)
      }

      def setRNodes(nodes: Set[Node]) = {
        copy(rNodes = nodes)
      }

      def addGlobalNode: Env = {
        copy(ptGraph = ptGraph + GBNode)
      }

      def duplicate = this
    }

    class PointToTF(cfg: FunctionCFG) extends TransferFunctionAbs[Env, CFG.Statement] {

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        var env = oldEnv

        def getNodes(sv: CFG.SimpleValue): Set[Node] = sv match {
          case r2: CFG.Ref => env.getL(r2)
          case n : CFG.Null => Set()
          case _ => Set()
        }

        st match {
          case av: CFG.AssignVal =>
            if (av.r == cfg.retval) {
              env = env.setRNodes(getNodes(av.v))
            } else {
              env = env.setL(av.r, getNodes(av.v))
            }

          case afr: CFG.AssignFieldRead =>
            afr.obj match {
              case CFG.SymRef(symbol) if symbol.isModule =>
                // If we have r = obj.field where obj is a global object, we have that r is pointing to GLB
                env = env.addGlobalNode.setL(afr.r, Set(GBNode))
              case _ =>
                // TODO: process load
            }
          case afw: CFG.AssignFieldWrite =>
            afw.obj match {
              case CFG.SymRef(symbol) if symbol.isModule =>
                // If we do obj.field = rhs, where obj is a global object, rhs is potentially escaping from the scope
                env = env.addENodes(getNodes(afw.rhs))
              case _ =>
                // Otherwise, we have obj.field = rhs
                env = env.addIEdges(getNodes(afw.obj), SymField(afw.field), getNodes(afw.rhs))
            }

          case an: CFG.AssignNew =>
            val iNode = INode(an.uniqueID)
            env = env.addInsideNode(iNode).setL(an.r, Set(iNode))
            // TODO: elements

          case aa: CFG.AssignArray =>
            val iNode = INode(aa.uniqueID)
            env = env.addInsideNode(iNode).setL(aa.r, Set(iNode))
            // TODO: elements

          case ac: CFG.AssignCast =>
            env = env.setL(ac.r, env.getL(ac.rhs))


          case _ =>
        }
        env
      }
    }
    def run {
     buildPTGraph(funDecls.values.head)
    }

    def buildPTGraph(fun: AbsFunction): PointToGraph = {
      new PointToGraph()
    }
  }
}

