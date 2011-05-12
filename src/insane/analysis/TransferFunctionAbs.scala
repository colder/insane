package insane
package analysis

import CFG.ControlFlowGraph

abstract class TransferFunctionAbs[E, S] {
  def apply(node : S, x : E) : E
}
