package insane
package utils

trait ObjectSets { self: AnalysisComponent =>

  import global._

  case class ObjectSet(subtypesOf: Set[Type], exactTypes: Set[Type]) {

    val isExhaustive = subtypesOf.isEmpty

    override def toString = {
      exactTypes.map(t => (if (subtypesOf(t)) "_ <: " else "")+t.toString).mkString("{", ", ", "}")
    }

    def ++ (that: ObjectSet) = ObjectSet(subtypesOf ++ that.subtypesOf, exactTypes ++ that.exactTypes)

    def resolveTypes: Set[Type] = exactTypes ++ subtypesOf.flatMap(st => getDescendents(st.typeSymbol).map(_.tpe))

    def isSubTypeOf(t: Type) = exactTypes.forall(t <:< _)

    def intersectWith(t: Type): Set[Type] = {
      if (this == ObjectSet.top) {
        // Shortcut when oset is Everthing
        Set(t)
      } else {
        def intersect(tp: Type): Set[Type] = {
          if (tp <:< t) {
            Set(tp)
          } else {
            getDirectDescendents(tp.typeSymbol).flatMap(s => intersect(s.tpe))
          }
        }
        exactTypes.flatMap( tp => if (t <:< tp) Set(t) else intersect(tp) )
      }
    }
  }

  object ObjectSet {
    def empty  = new ObjectSet(Set(), Set())
    def bottom = empty
    def top    = subtypesOf(definitions.ObjectClass)

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
