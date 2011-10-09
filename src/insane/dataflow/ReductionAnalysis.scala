package insane
package dataflow

import utils._
import utils.Graphs._
import utils.Reporters._

import CFG._

class ReductionAnalysis[E <: EnvAbs[E, S], S] (lattice : LatticeAbs[E, S], baseEnv : E, settings: Settings) extends Analysis[E, S](lattice, baseEnv, settings) {

  /*
   * Called 1) each step of the in-SCC fixpoint 2) at the end of the SCC fixpoint
   * Note: Not called twice for the last step.
   */
  override def cfgTransFun(cfg: ControlFlowGraph[S], scc: SCC[Vertex], vertices: Set[Vertex], transferFun: TransferFunctionAbs[E,S]): ControlFlowGraph[S] = {
    cfg
  }
}
