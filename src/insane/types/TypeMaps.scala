package insane
package types

import utils.Reporters._

trait TypeMaps { self: AnalysisComponent =>

  import global._

  def computeClassTypeMap(meth: Symbol, receiverTypes: ObjectSet): ClassTypeMap = {
    ClassTypeMap(meth.owner.tpe.typeArgs.map{ t =>
      t.typeSymbol -> receiverTypes.exactTypes.map{tt => 
        t.asSeenFrom(tt, meth.owner)}.toSet
    }.toMap)
  }

  def computeMethodTypeMap(meth: Symbol, callTypeParams: Seq[Tree]): MethodTypeMap = {
    MethodTypeMap(meth.tpe match {
      case PolyType(params, _) =>
        (params zip callTypeParams).map{ case (a,v) => a -> v.tpe }.toMap
      case t =>
        Map()
    })
  }

  def computeTypeMap(meth: Symbol, callTypeParams: Seq[Tree], receiverTypes: ObjectSet): DualTypeMap = {
    DualTypeMap(computeClassTypeMap(meth, receiverTypes),
                computeMethodTypeMap(meth, callTypeParams))
  }

  class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
    protected override def matches(sym1: Symbol, sym2: Symbol) =
      if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
      else sym1 eq sym2
  }

  trait TypeMap {
    def apply(t: Type): Type
    def apply(oset: ObjectSet): ObjectSet

    val isEmpty: Boolean
  }

  /**
   * ClassTypeMap represents the map between class type arguments and their
   * call-site instantiation.
   * i.e. :
   *  class A[T] {
   *    def foo(..) = { .. }
   *  }
   *  class B[T2] extends A[T] {
   *
   *  }
   *
   *  (if (..) new A[Int] else B[Double]).foo(..)
   *
   *   will yield the map T -> Set(Int, Double)
   */
  case class ClassTypeMap(tm: Map[Symbol, Set[Type]]) extends TypeMap {
    val classTypeMapSingle  = tm.map{ case (s, tpes) => (s, lub(tpes.toList)) }.toList.unzip

    def apply(t: Type): Type = {
      t.instantiateTypeParams(classTypeMapSingle._1, classTypeMapSingle._2)
    }

    def apply(oset: ObjectSet): ObjectSet = {
      var newOset = oset

      for ((from, tos) <- tm) {
        var subst = Set[Type]()
        var exact = Set[Type]()
        for (to <- tos) {
          subst ++= newOset.subtypesOf.map(_.instantiateTypeParams(List(from), List(to)))
          exact ++= newOset.exactTypes.map(_.instantiateTypeParams(List(from), List(to)))
        }
        newOset = ObjectSet(subst, exact)
      }

      newOset
    }

    val isEmpty = tm.isEmpty
  }

  /**
   * MethodTypeMap represents the map between method type arguments and their
   * call-site instantiation.
   * i.e. :
   *    def foo[B] (..) = { .. }
   *
   *    foo[Into](..)
   * will yield the map B -> Int
   */
  case class MethodTypeMap(tm: Map[Symbol, Type]) extends TypeMap {
    val methodTypeMapSingle = tm.toList.unzip
    val mapSkolems = new SubstSkolemsTypeMap(methodTypeMapSingle._1, methodTypeMapSingle._2)

    def apply(t: Type): Type = {
      mapSkolems(t)
    }

    def apply(oset: ObjectSet): ObjectSet = {
      if (tm.isEmpty) {
        oset
      } else {
        ObjectSet(oset.subtypesOf.map(mapSkolems), oset.exactTypes.map(mapSkolems))
      }
    }

    val isEmpty = tm.isEmpty
  }

  case class DualTypeMap(classTM: ClassTypeMap, methodTM: MethodTypeMap) extends TypeMap {
    def apply(t: Type): Type = {
      methodTM(classTM(t))
    }

    def apply(oset: ObjectSet): ObjectSet = {
      classTM(methodTM(oset))
    }

    val isEmpty = classTM.isEmpty && methodTM.isEmpty

    override def toString = "{C: "+classTM+" | M: "+methodTM+"}"
  }
                      
}
