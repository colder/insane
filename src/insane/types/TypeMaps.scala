package insane
package types

import utils.Reporters._

trait TypeMaps { self: AnalysisComponent =>

  import global._

  def computeClassTypeMap(meth: Symbol, receiverType: TypeInfo): ClassTypeMap = {
    ClassTypeMap(meth.owner.tpe.typeArgs.map{ t =>
      t.typeSymbol -> t.asSeenFrom(receiverType.tpe, meth.owner)
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

  def computeTypeMap(meth: Symbol, callTypeParams: Seq[Tree], receiverType: TypeInfo): DualTypeMap = {
    DualTypeMap(computeClassTypeMap(meth, receiverType),
                computeMethodTypeMap(meth, callTypeParams))
  }

  class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
    protected override def matches(sym1: Symbol, sym2: Symbol) =
      if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
      else sym1 eq sym2
  }

  trait TypeMap {
    def apply(t: Type): Type
    def apply(info: TypeInfo): TypeInfo

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
   *   will yield the map T -> Int lub Double
   */
  case class ClassTypeMap(tm: Map[Symbol, Type]) extends TypeMap {
    val classTypeMapSingle = tm.toList.unzip

    def apply(t: Type): Type = {
      t.instantiateTypeParams(classTypeMapSingle._1, classTypeMapSingle._2)
    }

    def apply(info: TypeInfo): TypeInfo = {
      info.copy(tpe = apply(info.tpe))
    }

    val isEmpty = tm.isEmpty

    override def toString = {
      tm.mkString(", ")
    }
  }

  object ClassTypeMap {
    val empty = ClassTypeMap(Map())
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

    def apply(info: TypeInfo): TypeInfo = {
      if (tm.isEmpty) {
        info
      } else {
        info.copy(tpe = apply(info.tpe))
      }
    }

    val isEmpty = tm.isEmpty

    override def toString = {
      tm.mkString(", ")
    }
  }

  object MethodTypeMap {
    val empty = MethodTypeMap(Map())
  }

  case class DualTypeMap(classTM: ClassTypeMap, methodTM: MethodTypeMap) extends TypeMap {
    def apply(t: Type): Type = {
      methodTM(classTM(t))
    }

    def apply(info: TypeInfo): TypeInfo = {
      classTM(methodTM(info))
    }

    val isEmpty = classTM.isEmpty && methodTM.isEmpty

    override def toString = {
      if (isEmpty) {
        ""
      } else {
        "[ "+classTM+" | "+methodTM+" ]"
      }
    }
  }

  object DualTypeMap {
    val empty = DualTypeMap(ClassTypeMap.empty, MethodTypeMap.empty)
  }
                      
}
