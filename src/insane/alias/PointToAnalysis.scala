package insane
package alias

import utils._
import analysis._
import PointToGraphs._

trait PointToAnalysis {
  self: AnalysisComponent =>

  import global._

  class PointToAnalysisPhase extends SubPhase {
    val name = "Point-to Analysis"

    sealed abstract class PTField
    case class PTRefSymbol(ref: CFG.Ref) extends PTField
    case object PTArrayFields extends PTField

    type INode = PTInsNode[PTField]
    type LNode = PTLoadNode[PTField]
    type PNode = PTParamNode[PTField]
    type Node  = PTNodeAbs[PTField]

    case class PTEnv(ptGraph: PointToGraph[PTField]) extends DataFlowEnvAbs[PTEnv, CFG.Statement] {

      def union(that: PTEnv) = {
        PTEnv(ptGraph union that.ptGraph)
      }

      def getL(ref: CFG.Ref): Set[Node] = {
        ptGraph.locState(PTRefSymbol(ref))
      }

      def setL(ref: CFG.Ref, nodes: Set[Node]): PTEnv = {
        PTEnv(ptGraph.copy(locState = ptGraph.locState + (PTRefSymbol(ref) -> nodes)))
      }

      def newInsideNode(label: Int): (PTEnv, INode) = {
        val n = new INode(label)
        (PTEnv(ptGraph + n), n)
      }

      def copy = this
    }

    class PointToTF extends TransferFunctionAbs[PTEnv, CFG.Statement] {
      type Env = PTEnv

      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        var env = oldEnv
        st match {
          case av: CFG.AssignVal => av.v match {
            case r2: CFG.Ref =>
              env = env.setL(av.r, env.getL(r2))
            case n: CFG.Null =>
              env = env.setL(av.r, Set())
            case _ =>
              // ignore
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

    def buildPTGraph(fun: AbsFunction): PointToGraph[Symbol] = {
      new PointToGraph[Symbol]()
    }
  }
}

