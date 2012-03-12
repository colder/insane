package insane
package types

import utils.Reporters._

trait TypeHelpers extends TypeMaps with TypeSignatures { self: AnalysisComponent =>

  import global._

  def isGroundClass(s: Symbol) = atPhase(currentRun.typerPhase){s.tpe.parents exists (s => s.typeSymbol == definitions.AnyValClass)}

  def isGroundOSET(oset: ObjectSet) = (oset.exactTypes.size == 1) && isGroundClass(oset.exactTypes.head.typeSymbol) && oset.exactTypes.head != definitions.BooleanClass.tpe

  def instantiateChildTypeParameters(parentTpe: Type, childTpe: Type): Option[(Type, ClassTypeMap)] = {
    val childSym  = childTpe.typeSymbol
    val parentSym = parentTpe.typeSymbol

    if (childTpe == parentTpe) {
      parentTpe match {
        case TypeRef(_, _, params) =>
          return Some((childTpe, ClassTypeMap((childSym.typeParams zip params.map(Set(_))).toMap)))
        case _ =>
          return Some((childTpe, ClassTypeMap(childSym.typeParams.map(p => (p, Set(p.tpe))).toMap)))
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
      val inferredMap      = (childSym.typeParams zip types.map(Set(_))).toMap

      Some((instantiatedType, ClassTypeMap(inferredMap)))
    } else {
      None
    }
  }

  def canBeSubtypeOf(child: Type, parent: Type): Option[Type] = {
    instantiateChildTypeParameters(child, parent).map(_._1)
  }

  def getMatchingMethods(methodName: Name, methodSymbol: Symbol, methodType: Type, oset: ObjectSet, pos: Position, silent: Boolean): Set[(Symbol, ClassTypeMap)] = {

    var failures = Set[Type]();

    def getMatchingMethodIn(parentTpe: Type, childTpe: Type): Option[(Symbol, ClassTypeMap)] = {
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
}
