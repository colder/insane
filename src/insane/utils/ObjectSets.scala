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

    def isSubTypeOf(t: Type) = exactTypes.forall(_ <:< t)

    def intersectWith(that: ObjectSet): Set[Type] = {
      // TODO: Check validity of considering only exactTypes
      if (this == ObjectSet.top) {
        // Shortcut when oset is Everthing
        that.exactTypes
      } else if (that == ObjectSet.top) {
        this.exactTypes
      } else {
        var res = Set[Type]()
        for (t <- that.exactTypes) {
          res ++= this.intersectWith(t) 
        }
        for (t <- this.exactTypes) {
          res ++= that.intersectWith(t) 
        }
        res
      }
    }

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
      if (s.isFinal) {
        singleton(s.tpe)
      } else {
        new ObjectSet(Set(s.tpe), Set(s.tpe))
      }
    }
    def subtypesOf(t: Type): ObjectSet =  {
      if (t.typeSymbol.isFinal) {
        singleton(t)
      } else {
        new ObjectSet(Set(t), Set(t))
      }
    }
  }
}
