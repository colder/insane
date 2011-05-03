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

    case class Env(ptGraph: PointToGraph) extends DataFlowEnvAbs[Env, CFG.Statement] {

      def union(that: Env) = {
        Env(ptGraph union that.ptGraph)
      }

      def setL(ref: CFG.Ref, nodes: Set[NodeAbs]): Env = {
        Env(ptGraph.copy(locState = ptGraph.locState + (ref -> nodes)))
      }

      def getL(ref: CFG.Ref): Set[NodeAbs] = ptGraph.locState(ref)

      def newInsideNode(label: Int): (Env, INode) = {
        val n = new INode(label)
        (Env(ptGraph + n), n)
      }

      def addInsideEdges(lv1: Set[NodeAbs], field: FieldAbs, lv2: Set[NodeAbs]) = {
        var newGraph = ptGraph
        for (v1 <- lv1; v2 <- lv2) {
          newGraph += IEdge(v1, field, v2)
        }
        Env(newGraph)
      }

      def addEscapes(e: Set[NodeAbs]) = {
        Env(ptGraph.copy(escapeNodes = ptGraph.escapeNodes ++ e))
      }

      def setReturns(r: Set[NodeAbs]) = {
        Env(ptGraph.copy(returnNodes = r))
      }

      def addGlobalNode: (Env, NodeAbs) = {
        (Env(ptGraph + GBNode), GBNode)
      }

      def copy = this
    }

    class PointToTF(cfg: FunctionCFG) extends TransferFunctionAbs[Env, CFG.Statement] {

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        var env = oldEnv

        def getNodes(sv: CFG.SimpleValue): Set[NodeAbs] = sv match {
          case r2: CFG.Ref => env.getL(r2)
          case n : CFG.Null => Set()
          case _ => Set()
        }

        st match {
          case av: CFG.AssignVal =>
            if (av.r == cfg.retval) {
              env = env.setReturns(getNodes(av.v))
            } else {
              env = env.setL(av.r, getNodes(av.v))
            }

          case afr: CFG.AssignFieldRead =>
            afr.obj match {
              case CFG.SymRef(symbol) if symbol.isModule =>
                // If we have r = obj.field where obj is a global object, we have that r is pointing to GLB
                val (newEnv, n) = env.addGlobalNode
                env = newEnv.setL(afr.r, Set(n))
              case _ =>
                // TODO: process load
            }
          case afw: CFG.AssignFieldWrite =>
            afw.obj match {
              case CFG.SymRef(symbol) if symbol.isModule =>
                // If we do obj.field = rhs, where obj is a global object, rhs is potentially escaping from the scope
                env = env.addEscapes(getNodes(afw.rhs))
              case _ =>
                // Otherwise, we have obj.field = rhs
                env = env.addInsideEdges(getNodes(afw.obj), SymField(afw.field), getNodes(afw.rhs))
            }

          case an: CFG.AssignNew =>
            val (newEnv, n) = env.newInsideNode(an.uniqueID)
            env = newEnv.setL(an.r, Set(n))
            // TODO: elements

          case aa: CFG.AssignArray =>
            val (newEnv, n) = env.newInsideNode(aa.uniqueID)
            env = newEnv.setL(aa.r, Set(n))
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

