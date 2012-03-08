package insane
package types

import utils.Reporters._

trait TypeHelpers { self: AnalysisComponent =>

  import global._

  def isGroundClass(s: Symbol) = atPhase(currentRun.typerPhase){s.tpe.parents exists (s => s.typeSymbol == definitions.AnyValClass)}

  def isGroundOSET(oset: ObjectSet) = (oset.exactTypes.size == 1) && isGroundClass(oset.exactTypes.head.typeSymbol) && oset.exactTypes.head != definitions.BooleanClass.tpe

  def instantiateChildTypeParameters(parentTpe: Type, childTpe: Type): Option[(Type, Map[Symbol, Type])] = {
    val childSym  = childTpe.typeSymbol
    val parentSym = parentTpe.typeSymbol

    if (childTpe == parentTpe) {
      parentTpe match {
        case TypeRef(_, _, params) =>
          return Some((childTpe, (childSym.typeParams zip params).toMap))
        case _ =>
          return Some((childTpe, (childSym.typeParams.map(p => (p, p.tpe)).toMap)))
      }
    }

    val childTypeVars     = childSym.typeParams.map(s => TypeVar(s.tpeHK, new TypeConstraint, Nil, Nil))
    
    val childAppliedType  = appliedType(childSym.tpe, childTypeVars)

    val parentAppliedType = parentTpe match {
      case TypeRef(pre, sym, params) =>
        //println("Found in "+sym.fullName+" params:"+ params)

        val paramMap = (params zip sym.typeParams) flatMap { case (tp, p) =>
          val sym = tp.typeSymbol

          if (sym.isTypeParameter || sym.isTypeSkolem) {
            val tv = TypeVar(sym, true /* untouchable */)

            //println(" param "+p+" refers to "+sym+": "+tp+" with TV: "+tv+" with constraints: "+tv.constr)

            Some((sym, tv))
          } else {
            None
          }
        } unzip

        val skolemMap = new SubstSkolemsTypeMap(paramMap._1, paramMap._2)
        //println("Map is: "+(paramMap.zipped).toMap)
        val parentResult = skolemMap(parentTpe).subst(paramMap._1, paramMap._2)
        //println("Parent type becomes: "+parentResult)
        parentResult
      case _ =>
        parentTpe
    }

    //println("childSym            = "+childSym)
    //println("parentSym           = "+parentSym)
    //println("parentAppliedType   = "+parentAppliedType)
    //println("childTypeVars       = "+childTypeVars)
    //println("childAppliedType    = "+childAppliedType)

    //val skolems = new scala.collection.mutable.ListBuffer[TypeSymbol]
    val types   = new scala.collection.mutable.ListBuffer[Type]

    object tvToParam extends VariantTypeMap {
      def apply(tp: Type) = mapOver(tp) match {
        case tv: TypeVar if tv.untouchable =>
          tv.origin
        case t => t
      }
    }

    object tvToSkolem extends VariantTypeMap {
      def apply(tp: Type) = mapOver(tp) match {
        case tv: TypeVar =>
          val tpSym  = tv.origin.typeSymbol
          val bounds = TypeBounds(glb(tv.constr.loBounds), lub(tv.constr.hiBounds))

          val tpe = if (tpSym.isContravariant) {
            bounds.lo
          } else if (tpSym.isCovariant) {
            bounds.hi
          } else if (bounds.hi =:= bounds.lo) {
            bounds.hi
          } else {
            reporter.error("Type variable "+tv+" refers to a symbol that is invariant, and we didn't find a valid fixed bound: "+bounds+", using hi")
            bounds.hi
          }

          val resolvedTpe = tvToParam(tpe)
          types += resolvedTpe
          resolvedTpe

          // val skolem = tpSym.owner.newExistentialSkolem(tpSym, tpSym) setInfo bounds
          // skolems += skolem
          // skolem.tpe
        case t => t
      }
    }

    if (childAppliedType <:< parentAppliedType) {
      val tp   = tvToSkolem(childAppliedType)

      //val instantiatedType = newExistentialType(skolems.toList, tp)
      //val inferredMap = (childSym.typeParams zip skolems.map(_.tpe).toList).toMap
    
      val instantiatedType = tp
      val inferredMap      = (childSym.typeParams zip types).toMap

      Some(instantiatedType, inferredMap)
    } else {
      None
    }
  }

  def getMatchingMethods(methodName: Name, methodSymbol: Symbol, methodType: Type, oset: ObjectSet, pos: Position, silent: Boolean): Set[(Symbol, Map[Symbol, Type])] = {

    var failures = Set[Type]();

    def getMatchingMethodIn(parentTpe: Type, childTpe: Type): Option[(Symbol, Map[Symbol, Type])] = {
      //println(" ==> Matching "+childTpe+" <: "+parentTpe+" for method "+methodType)

      /**
       * We only need to look in the upward type chain for methods in case we
       * analyse the top-parent one.
       * 
       *  class A { def f; }
       *  class B extends A { }
       *  class C extends B { override def f; }
       *  class D extends C { }
       *  class E extends D { override def f; }
       * 
       * Receiver is { _ <: B } ~=> B,C,D,E
       * Matching function will be called with (B, B), (B, C), (B, D), and (B, E)
       * 
       * It must find A.f, C.f, E.f
       */
      var upwardTypeChain = if (parentTpe == childTpe) {
        childTpe.baseTypeSeq.toList
      } else {
        List(childTpe)
      }

      for (tpe <- upwardTypeChain) {
        val parentMethodIntoChildTpe = tpe.typeSymbol.thisType.memberType(methodSymbol)
        val childMethodSym           = tpe.decl(methodName)

        if (childMethodSym.isDeferred) {
          //println("&&& ~~~ Found abstract method, skipping")
          return None
        } else if (parentMethodIntoChildTpe matches childMethodSym.tpe) {
          val childClass = childMethodSym.owner
          /**
           * We found a method symbol in childClass that matches
           * the prototype, now let's see if we can find an instantiation
           * childTpeInst c: parentTpe such that
           * childTpeInst.memberTpe(childMethodSym) c: parentTpe.memberType(methodSymbol)
           */

          instantiateChildTypeParameters(parentTpe, childClass.tpe) match {
            case Some((refinedChildTpe, inferedMap)) =>
              settings.ifDebug {
      //          reporter.debug("&&& ~~~ Found instantiation s.t. "+childClass.tpe+" <: "+parentTpe)
      //          reporter.debug("&&& => "+refinedChildTpe+" with map: "+ inferedMap.mapValues{ t => t+" {"+t.bounds+"}"})
              }

              return Some((childMethodSym, inferedMap))
            case None =>
              settings.ifDebug {
      //          reporter.debug("&&& ~~~ "+childClass.tpe+" </: "+parentTpe)
              }
              return None
          }
        }
      }

      if (parentTpe == childTpe) {
        failures += parentTpe
      }

      None
    }

    val typeTuples =
      (oset.exactTypes).map(t => (t, t)) ++
      (oset.subtypesOf).flatMap(st => getDescendents(st.typeSymbol).map(s => (st, s.tpe)))

    val r = typeTuples flatMap { case (t, ct) => getMatchingMethodIn(t, ct) }

    def conciseSet(a: Traversable[_]) = if (a.size > 5) {
      (a.take(5) ++ List(" "+(a.size-5)+" more...")).mkString("{", ",", "}");
    } else {
      a.mkString("{", ",", "}");
    }

    if (!failures.isEmpty && !silent) {
      reporter.warn("Failed to find method "+methodName+": "+methodType+" in classes "+conciseSet(failures)+" amongst "+conciseSet(oset.exactTypes), pos)
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

  def computeTypeMap(meth: Symbol, callTypeParams: Seq[Tree], receiverTypes: ObjectSet): TypeMap = {
    val methTypeMap: Map[Symbol, Type] = meth.tpe match {
      case PolyType(params, _) =>
        (params zip callTypeParams).map{ case (a,v) => a -> v.tpe }.toMap
      case t =>
        Map()
    }

    val classTypeMap: Map[Symbol, Set[Type]] = meth.owner.tpe.typeArgs.map{ t =>
      t.typeSymbol -> receiverTypes.exactTypes.map{tt => 
        t.asSeenFrom(tt, meth.owner)}.toSet
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
