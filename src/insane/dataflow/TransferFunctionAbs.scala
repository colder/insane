package insane
package dataflow

import CFG._
import utils._

abstract class TransferFunctionAbs[E <: EnvAbs[E], S] {
  def apply(edge: CFGEdge[S], x : E, scc: SCC[CFGVertex]) : E
}

abstract class SimpleTransferFunctionAbs[E <: EnvAbs[E], S] extends TransferFunctionAbs[E, S]  {
  def apply(l: S, x: E): E

  def apply(edge: CFGEdge[S], x : E, scc: SCC[CFGVertex]) : E = {
    apply(edge.label, x)
  }
}

abstract class UnitTransferFunctionAbs[E <: EnvAbs[E], S] extends TransferFunctionAbs[E, S] {
  def apply(l: S, x: E): Unit

  def apply(edge: CFGEdge[S], x : E, scc: SCC[CFGVertex]) : E = {
    apply(edge.label, x)
    x
  }
}
