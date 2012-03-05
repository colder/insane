package insane
package types

import utils.Reporters._

trait TypeHelpers { self: AnalysisComponent =>

  import global._

  def isGroundClass(s: Symbol) = atPhase(currentRun.typerPhase){s.tpe.parents exists (s => s.typeSymbol == definitions.AnyValClass)}

  def isGroundOSET(oset: ObjectSet) = (oset.exactTypes.size == 1) && isGroundClass(oset.exactTypes.head.typeSymbol) && oset.exactTypes.head != definitions.BooleanClass.tpe

  def getMatchingMethods(methodName: Name, methodType: Type, types: Set[Type], pos: Position, silent: Boolean): Set[Symbol] = {

    var failures = Set[Type]();

    def getMatchingMethodIn(tpe: Type): Option[Symbol] = {
      println("=> Matching In "+tpe+":") 
      var tpeChain = tpe.baseTypeSeq.toList

      var res: Option[Symbol] = None

      for (tpe <- tpeChain if res.isEmpty) {
        val found = tpe.decls.lookupAll(methodName).find{sym => 
          println("In "+tpe+" ("+tpe.typeSymbol+", "+tpe.typeSymbol.isClass+"):") 

          val foundType  = tpe.memberType(sym)
          val targetType = methodType.asSeenFrom(tpe, tpe.typeSymbol)

          println(" is "+foundType+" <:< "+targetType+"?") 
          println(if (foundType <:< targetType) "yes" else "no")
          println(" is "+foundType+" match "+targetType+"?") 
          println(if (foundType matches targetType) "yes" else "no")

          foundType <:< targetType
        }

        if (!found.isEmpty) {
          res = Some(found.get)
        }
      }

      if (res.isEmpty && tpe.typeSymbol != definitions.NothingClass) {
        failures += tpe
      }

      res match {
        // We ignore abstract methods
        case Some(m) if m.isDeferred =>
          None
        case r =>
          r
      }
    }

    val r = types map { tpe => getMatchingMethodIn(tpe) } collect { case Some(ms) => ms }


    def conciseSet(a: Traversable[_]) = if (a.size > 5) {
      (a.take(5) ++ List(" "+(a.size-5)+" more...")).mkString("{", ",", "}");
    } else {
      a.mkString("{", ",", "}");
    }

    if (!failures.isEmpty && !silent) {
      reporter.warn("Failed to find method "+methodName+": "+methodType+" in classes "+conciseSet(failures)+" amongst "+conciseSet(types), pos)
    }

    r
  }

  def arrayType(tpe: Type) =
    TypeRef(NoPrefix, definitions.ArrayClass, List(tpe))

  def methodReturnType(methodSymbol: Symbol): ObjectSet = {
    val resType = methodSymbol.tpe.resultType

    val r = resType match {
      case TypeRef(_, definitions.ArrayClass, List(tpe)) =>
        // resType is a parametrized array, we keep that type precise, ignore
        // descendents in this case
        ObjectSet.singleton(resType)
      case _ =>
        // General case
        ObjectSet.subtypesOf(resType)
    }
    r
  }

  def mappedSingleType(tpe: Type, typeMap: Map[Symbol, Set[Type]]): Type = {
    tpe.instantiateTypeParams(typeMap.keys.toList, typeMap.values.map(tpes => lub(tpes.toList)).toList)
  }

  def computeTypeMap(meth: Symbol, callTypeParams: Seq[Tree], receiverTypes: ObjectSet): TypeMap = {
    val methTypeMap: Map[Symbol, Type] = meth.tpe match {
      case PolyType(params, _) =>
        (params zip callTypeParams).map{ case (a,v) => a -> v.tpe }.toMap
      case t =>
        Map()
    }

    val classTypeMap: Map[Symbol, Set[Type]] = meth.owner.tpe.typeArgs.map{ t =>
      t.typeSymbol -> receiverTypes.exactTypes.map(tt => t.asSeenFrom(tt, meth.owner)).toSet
    }.toMap

    TypeMap(classTypeMap, methTypeMap)
  }

  class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
    protected override def matches(sym1: Symbol, sym2: Symbol) =
      if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
      else sym1 eq sym2
  }

  case class TypeMap(classTypeMap: Map[Symbol, Set[Type]], methodTypeMap: Map[Symbol, Type]) {
    val methodTypeMapSingle = methodTypeMap.toList.unzip
    val classTypeMapSingle  = classTypeMap.map{ case (s, tpes) => (s, lub(tpes.toList)) }.toList.unzip

    val mapSkolems = new SubstSkolemsTypeMap(methodTypeMapSingle._1, methodTypeMapSingle._2)

    def apply(t: Type): Type = {
      mapSkolems(t.instantiateTypeParams(classTypeMapSingle._1, classTypeMapSingle._2))
    }

    def apply(oset: ObjectSet): ObjectSet = {
      var newOset = if (methodTypeMap.isEmpty) {
        oset
      } else {
        ObjectSet(oset.subtypesOf.map(mapSkolems), oset.exactTypes.map(mapSkolems))
      }

      for ((from, tos) <- classTypeMap) {
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

    val isEmpty = classTypeMap.isEmpty && methodTypeMap.isEmpty

    override def toString = "{C: "+classTypeMap.mkString(", ")+" | M: "+methodTypeMap.mkString(", ")+"}"
  }
                      
}
