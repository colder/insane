package insane
package utils

trait ObjectSets { self: AnalysisComponent =>

  import global._

  case class ObjectSet(types: Set[Type], isExhaustive: Boolean) {
    override def toString = {
      if (isExhaustive) {
        types.mkString("{", ", ", "}")
      } else {
        types.mkString("{", ", ", "} and subtypes")
      }
    }

    def ++ (that: ObjectSet) = ObjectSet(types ++ that.types, isExhaustive && that.isExhaustive)
  }

  object AllObjects extends ObjectSet(Set(definitions.ObjectClass.tpe), false) {
    override def toString = {
      "{.. All objects ..}"
    }
  }

  object ObjectSet {
    def empty = apply(Set(), true)
    def singleton(tpe: Type) = apply(Set(tpe), true)
  }
}
