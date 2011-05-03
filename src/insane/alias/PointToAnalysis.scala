package insane
package alias

import utils._
import analysis._
import PointToGraphs._
import CFG.ControlFlowGraph

trait PointToAnalysis {
  self: AnalysisComponent =>

  import global._

  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    sealed abstract class PTField
    case class PTSymField(symbol: Symbol) extends PTField
    case object PTArrayFields extends PTField

    type INode = PTInsNode[PTField]
    type LNode = PTLoadNode[PTField]
    type PNode = PTParamNode[PTField]
    val  GBLNode  = new PTGblNode[PTField]()
    type Node  = PTNodeAbs[PTField]

    case class PTEnv(ptGraph: PointToGraph[PTField, CFG.Ref]) extends DataFlowEnvAbs[PTEnv, CFG.Statement] {

      def union(that: PTEnv) = {
        PTEnv(ptGraph union that.ptGraph)
      }

      def getL(ref: CFG.Ref): Set[Node] = {
        ptGraph.locState(ref)
      }

      def setL(ref: CFG.Ref, nodes: Set[Node]): PTEnv = {
        PTEnv(ptGraph.copy(locState = ptGraph.locState + (ref -> nodes)))
      }

      def newInsideNode(label: Int): (PTEnv, INode) = {
        val n = new INode(label)
        (PTEnv(ptGraph + n), n)
      }

      def addInsideEdges(lv1: Set[Node], field: PTField, lv2: Set[Node]) = {
        var newGraph = ptGraph
        for (v1 <- lv1; v2 <- lv2) {
          newGraph += PTInsEdge[PTField](v1, field, v2)
        }
        PTEnv(newGraph)
      }

      def addEscapes(e: Set[Node]) = {
        PTEnv(ptGraph.copy(escapeNodes = ptGraph.escapeNodes ++ e))
      }

      def setReturns(r: Set[Node]) = {
        PTEnv(ptGraph.copy(returnNodes = r))
      }

      def addGlobalNode: (PTEnv, Node) = {
        (PTEnv(ptGraph + GBLNode), GBLNode)
      }

      def copy = this
    }

    class PointToTF extends TransferFunctionAbs[PTEnv, CFG.Statement, CFG.Ref] {
      type Env = PTEnv

      override def apply(st: CFG.Statement, oldEnv: Env, cfg: ControlFlowGraph[CFG.Statement, CFG.Ref]): Env = {
        var env = oldEnv

        def getNodes(sv: CFG.SimpleValue): Set[Node] = sv match {
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
                env = env.addInsideEdges(getNodes(afw.obj), PTSymField(afw.field), getNodes(afw.rhs))
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

    def buildPTGraph(fun: AbsFunction): PointToGraph[Symbol, Symbol] = {
      new PointToGraph[Symbol, Symbol]()
    }
  }
}

