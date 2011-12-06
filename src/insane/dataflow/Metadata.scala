package insane
package dataflow

import CFG._
import utils._

class Metadata[E <: EnvAbs[E], S, C <: ControlFlowGraph[S]] (
  val analysis: Analysis[E,S,C],
  val cfg: C,
  val currentCFGSCC: SCC[CFGVertex]
) {

}
