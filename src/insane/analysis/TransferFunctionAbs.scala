package insane
package analysis

import CFG.ControlFlowGraph

abstract class TransferFunctionAbs[E, S, R] {
  def apply(node : S, x : E, cfg: ControlFlowGraph[S, R]) : E = x
  def apply(node : S, x : E) : E = x
}
