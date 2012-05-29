package insane
package types

trait TypeInfos { self: AnalysisComponent =>

  import global._

  case class TypeInfo(tpe: Type, orSubtypes: Boolean) {

    def resolveTypes: Set[Type] = Set(tpe) ++ (if (orSubtypes) getDescendents(tpe.typeSymbol).map(_.tpe) else Set())

    val isExhaustive = !orSubtypes
    val isEmpty      = tpe == NoType

    def union(that: TypeInfo) = {
      if (this.isEmpty) {
        that
      } else if (that.isEmpty) {
        this
      } else {
        TypeInfo(lub(List(this.tpe, that.tpe)), this.orSubtypes || that.orSubtypes || (this.tpe != that.tpe))
      }
    }

    def intersectWith(that: TypeInfo): Option[TypeInfo] = {
      if (this == that) {
        Some(this)
      } else if (this == TypeInfo.top) {
        // Shortcut when oset is Everthing
        Some(that)
      } else if (that == TypeInfo.top) {
        Some(this)
      } else if (!this.orSubtypes) {
        if (that.orSubtypes && this.tpe <:< that.tpe) {
          Some(this)
        } else {
          None
        }
      } else if (!that.orSubtypes) {
        if (this.orSubtypes && that.tpe <:< this.tpe) {
          Some(that)
        } else {
          None
        }
      } else {
        def intersect(tp: Type): Set[Type] = {
          if (tp <:< that.tpe) {
            Set(tp)
          } else {
            getDirectDescendents(tp.typeSymbol).flatMap(s => intersect(s.tpe))
          }
        }

        val set = if (that.tpe <:< this.tpe) Set(that.tpe) else intersect(this.tpe)

        if (set.isEmpty) {
          None
        } else {
          Some(TypeInfo.fromTypes(set, false))
        }
      }
    }

    def isMorePreciseThan(that: TypeInfo): Boolean = {
      if (this == that) {
        true
      } else {
        (that.orSubtypes || !this.orSubtypes) && this.tpe <:< that.tpe
      }
    }

    def isStrictlyMorePreciseThan(that: TypeInfo): Boolean = {
      if (this == that) {
        false
      } else {
        isMorePreciseThan(that)
      }
    }

    def incompatibleWith(that: TypeInfo): Boolean = {
      this.intersectWith(that).isEmpty
    }

    def intersectWith(tpe: Type): Option[TypeInfo] = {
      this intersectWith TypeInfo.subtypeOf(tpe)
    }

    override def toString = (if (orSubtypes) "_ <: " else "")+tpeToString(tpe)
  }

  object TypeInfo {
    def fromTypes(tpes: Traversable[Type], canBeExact: Boolean = true) = {
      TypeInfo(lub(tpes.toList), !(canBeExact && tpes.size == 1))
    }

    def subtypeOf(tpe: Type): TypeInfo =
      if (allGroundClasses(tpe.typeSymbol) || tpe.typeSymbol.isFinal) {
        TypeInfo(tpe, false)
      } else {
        TypeInfo(tpe, true)
      }

    def exact(tpe: Type): TypeInfo =
      TypeInfo(tpe, false)

    def apply(tpe: Type): TypeInfo =
      exact(tpe)

    val empty = 
      TypeInfo(NoType, false)

    val top =
      TypeInfo(definitions.ObjectClass.tpe, true)
  }

}
