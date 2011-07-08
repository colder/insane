package insane
package alias

import utils.Graphs._
import utils._

trait ModifyClauses {
  self: AnalysisComponent =>

  import global._

  sealed case class Field(var fullName: String, name: String)
  object NoField extends Field(NoSymbol.fullName, NoSymbol.name.toString)

  object Field {
    def apply(sym: Symbol) = new Field(sym.fullName, sym.name.toString)
  }

  sealed case class ModifyClause(effects: Set[ModifyClauseEffect]) {
    val isPure = effects.isEmpty

    override def toString = {
      if (isPure) {
        "@pure"
      } else {
        effects.map(e => "@modifies "+e).mkString(", ")
      }
    }
  }

  object PureModifyClause extends ModifyClause(Set()) {
  }

  case class ModifyClauseEffect(chain: Traversable[Field], root: PointToGraphs.Node) {
    override def toString = {
      root.toString+chain.map(_.name.trim).mkString(".", ".", "")
    }
  }
}
