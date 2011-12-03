package insane
package dataflow

import CFG._
import utils._

abstract class TransferFunctionAbs[E, S] {
  def apply(edge: CFGEdge[S], x : E) : E
}
