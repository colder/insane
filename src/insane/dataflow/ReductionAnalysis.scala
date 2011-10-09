package insane
package dataflow

import utils._
import utils.Graphs._
import utils.Reporters._

import CFG._

class ReductionAnalysis[E <: EnvAbs[E, S], S] (lattice : LatticeAbs[E, S], baseEnv : E, settings: Settings) extends Analysis[E, S](lattice, baseEnv, settings) {

  override def cfgTransFun(cfg: ControlFlowGraph[S], scc: SCC[Vertex], vertices: Set[Vertex], transferFun: TransferFunctionAbs[E,S]): ControlFlowGraph[S] = {
    cfg
  }
}
