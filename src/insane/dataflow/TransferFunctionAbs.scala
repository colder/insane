package insane
package dataflow

import CFG._
import utils._

abstract class TransferFunctionAbs[E <: EnvAbs[E], S] {
  type Meta = Metadata[E, S, _ <: ControlFlowGraph[S]]
  def apply(edge: CFGEdge[S], x : E, metadata: Meta) : E
}
