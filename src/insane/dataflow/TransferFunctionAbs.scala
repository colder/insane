package insane
package dataflow

abstract class TransferFunctionAbs[E, S] {
  def apply(node : S, x : E) : E
}
