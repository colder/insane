package insane
package alias

import utils.Graphs._
import utils._

trait ModifyClauses {
  self: AnalysisComponent =>

  import global._

  sealed case class ModifyClause(effects: Set[ModifyClauseEffect]) {
    val isPure = effects.isEmpty

    override def toString = {
      if (isPure) {
        "@Pure"
      } else {
        "@Modifies"+effects.mkString("(", ", ", ")")
      }
    }
  }

  object PureModifyClause extends ModifyClause(Set()) {
  }

  case class ModifyClauseEffect(chain: Traversable[Field], root: PointToGraphs.Node) {
    override def toString = {
      root.toString+chain.map(_.name.toString.trim).mkString(".", ".", "")
    }
  }
}
