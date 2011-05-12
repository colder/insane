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
    case class PTFieldSymbol(sym: Symbol) extends PTField
    case object PTArrayFields extends PTField

    class PTEnv(val ptGraph: PointToGraph[PTField]) extends DataFlowEnvAbs[PTEnv, CFG.Statement] {
      var isBottom = false

      def copy = new PTEnv(ptGraph)

      def union(that: PTEnv) = {
        new PTEnv(ptGraph union that.ptGraph)
      }

      override def equals(that: Any) = that match {
        case a: PTEnv =>
          (a.ptGraph == ptGraph) && (a.isBottom == isBottom)
        case _ => false
      }
    }

    object PTBaseEnv extends PTEnv(new PointToGraph[PTField]) {
      isBottom = true
    }

    class PointToTF extends TransferFunctionAbs[PTEnv, CFG.Statement] {
      type Env = PTEnv
      def apply(st: CFG.Statement, oldEnv: Env): Env = {
        val env = oldEnv.copy

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

