package insane
package utils

trait ObjectSets { self: AnalysisComponent =>

  import global._

  case class ObjectSet(symbols: Set[Symbol], isExhaustive: Boolean) {
    override def toString = {
      if (isExhaustive) {
        symbols.map(_.fullName).mkString("{", ", ", "}")
      } else {
        symbols.map(_.fullName).mkString("{", ", ", "} and subtypes")
      }
    }

    def ++ (that: ObjectSet) = ObjectSet(symbols ++ that.symbols, isExhaustive && that.isExhaustive)
  }

  object AllObjects extends ObjectSet(Set(definitions.ObjectClass.tpe.typeSymbol), false) {
    override def toString = {
      "{.. All objects ..}"
    }
  }

  object ObjectSet {
    def empty = apply(Set(), true)
    def singleton(symbol: Symbol) = apply(Set(symbol), true)
  }
}
