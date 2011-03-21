package insane
package CFG

import utils._

class ControlFlowGraph[T] extends LabeledDirectedGraphImp[T] {

  val entry: Vertex = newVertex
  val exit: Vertex  = newVertex

  entry.name = "entry"
  exit.name  = "exit"

  override def toString = "[>" + entry + super.toString + exit + "<]"
}
