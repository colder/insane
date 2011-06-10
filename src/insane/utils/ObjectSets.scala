package insane
package utils

trait ObjectSets { self: AnalysisComponent =>

  import global._

  case class ObjectSet(subtypesOf: Set[Type], exactTypes: Set[Type]) {

    val isExhaustive = subtypesOf.isEmpty

    override def toString = {
      exactTypes.mkString("{", ", ", "}")+(if (subtypesOf.isEmpty) "" else subtypesOf.mkString(" and subtypes of {", ", ", "}"))
    }

    def ++ (that: ObjectSet) = ObjectSet(subtypesOf ++ that.subtypesOf, exactTypes ++ that.exactTypes)

    def resolveTypes: Set[Type] = exactTypes ++ subtypesOf.flatMap(st => getDescendents(st.typeSymbol).map(_.tpe))
  }

  object ObjectSet {
    def empty = new ObjectSet(Set(), Set())
    def singleton(tpe: Type) = new ObjectSet(Set(), Set(tpe))

    def apply(types: Set[Type], isExhaustive: Boolean): ObjectSet = {
      if (isExhaustive) {
        new ObjectSet(Set(), types)
      } else {
        new ObjectSet(types, types)
      }
    }

    def subtypesOf(s: Symbol): ObjectSet =  {
      new ObjectSet(Set(s.tpe), Set(s.tpe))
    }
    def subtypesOf(t: Type): ObjectSet =  {
      new ObjectSet(Set(t), Set(t))
    }
  }
}
