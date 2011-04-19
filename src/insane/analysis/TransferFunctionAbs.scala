package insane
package analysis

abstract class TransferFunctionAbs[E, S] {
  def apply(node : S, x : E) : E
}
