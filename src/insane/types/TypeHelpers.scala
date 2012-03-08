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

    val parentAppliedType   = parentTpe
    //val parentAppliedType = parentTpe match {
    //  case TypeRef(pre, sym, params) =>
    //    println("Found in "+sym.fullName+" params:"+ params)

    //    val paramMap = (params zip sym.typeParams) flatMap { case (tp, p) =>
    //      val sym = tp.typeSymbol

    //      val bound = if (p.isContravariant) {
    //        tp.bounds.lo
    //      } else {
    //        tp.bounds.hi
    //      }

    //      println(" param "+p+" refers to "+sym+": "+tp+" with bounds: "+bound)

    //      if (sym.isTypeParameter) {
    //        println(sym+" is type patameter!")
    //        Some((sym, bound))
    //      } else if (sym.isTypeSkolem) {
    //        println(sym+" is type skolem!")
    //        Some((sym, bound))
    //        None
    //      } else {
    //        None
    //      }
    //    } unzip

    //    val skolemMap = new SubstSkolemsTypeMap(paramMap._1, paramMap._2)
    //    println("Map is: "+(paramMap.zipped).toMap)
    //    val parentResult = skolemMap(parentTpe).subst(paramMap._1, paramMap._2)
    //    println("Upper bound becomes: "+parentResult)
    //    parentResult
    //  case _ =>
    //    parentTpe
    //}

    //println("childSym            = "+childSym)
    //println("parentSym           = "+parentSym)
    //println("parentAppliedType   = "+parentAppliedType)
    //println("childTypeVars       = "+childTypeVars)
    //println("childAppliedType    = "+childAppliedType)

    val skolems = new scala.collection.mutable.ListBuffer[TypeSymbol]
    //val types   = new scala.collection.mutable.ListBuffer[Type]

    object tvToSkolem extends VariantTypeMap {
      //def instBounds(tvar: TypeVar): (Type, Type) = {
      //  val tparam = tvar.origin.typeSymbol
      //  val instType = toOrigin(tvar.constr.inst)
      //  val (loBounds, hiBounds) =
      //    if (instType != NoType && isFullyDefined(instType)) (List(instType), List(instType))
      //    else (tvar.constr.loBounds, tvar.constr.hiBounds)
      //  val lo = lub(tparam.info.bounds.lo :: loBounds map toOrigin)
      //  val hi = glb(tparam.info.bounds.hi :: hiBounds map toOrigin)
      //  (lo, hi)
      //}

      def apply(tp: Type) = mapOver(tp) match {
        case tv: TypeVar =>
          val tpSym  = tv.origin.typeSymbol
          // val tpe = if (tpSym.isContravariant) {
          //   glb(tv.constr.loBounds)
          // } else {
          //   lub(tv.constr.hiBounds)
          // }
          // types += tpe
          // tpe
          val bounds = TypeBounds(glb(tv.constr.loBounds), lub(tv.constr.hiBounds))
          val skolem = tpSym.owner.newExistentialSkolem(tpSym, tpSym) setInfo bounds
          skolems += skolem
          skolem.tpe
        case tp1 => tp1
      }
    }

    if (childAppliedType <:< parentAppliedType) {
      val tp   = tvToSkolem(childAppliedType)
      Some((newExistentialType(skolems.toList, tp), (childSym.typeParams zip skolems.map(_.tpe).toList).toMap))
      //Some((tp, (childSym.typeParams zip types.toList).toMap))
    } else {
      None
    }
  }

  def getMatchingMethods(methodName: Name, methodSymbol: Symbol, methodType: Type, oset: ObjectSet, pos: Position, silent: Boolean): Set[(Symbol, Map[Symbol, Type])] = {

    var failures = Set[Type]();

    def getMatchingMethodIn(parentTpe: Type, childTpe: Type): Option[(Symbol, Map[Symbol, Type])] = {
      println(" ==> Matching "+childTpe+" <: "+parentTpe+" for method "+methodType)

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
          println("&&& ~~~ Found abstract method, skipping")
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
              println("&&& ~~~ Instantiated so that: "+childClass.tpe+" <: "+parentTpe)
              println("&&& => "+refinedChildTpe+" with map: "+inferedMap)

              return Some((childMethodSym, inferedMap))
            case None =>
              println("&&& ~~~ Failed to instantiate types so that: "+childClass.tpe+" <: "+parentTpe)
              println("&&& => Discarding method "+childMethodSym.fullName)
              return None
          }
        }
      }

      if (parentTpe == childTpe) {
        failures += parentTpe
      }

/*
      instantiateChildTypeParameters(parentTpe, childTpe) match {
        case Some((actualChildTpe, map)) =>
          println(" ~~> "+actualChildTpe+" with map "+map)

          var tpeChain = if (parentTpe == childTpe) {
            actualChildTpe.baseTypeSeq.toList
          } else {
            List(actualChildTpe)
          }

          for (tpe <- tpeChain if res.isEmpty) {
            val found = tpe.decls.lookupAll(methodName).find{sym => 
              println("In "+tpe+" ("+sym.tpe+"):") 

              val foundType  = tpe.memberType(sym)
              val targetType = methodType.asSeenFrom(tpe, tpe.typeSymbol)

              println(" is "+foundType+" <:< "+targetType+"?") 
              println(if (foundType <:< targetType) "yes" else "no")

              foundType <:< targetType
            }

            if (!found.isEmpty) {
              res = Some((found.get, map))
            }
          }

          if (res.isEmpty && actualChildTpe.typeSymbol != definitions.NothingClass) {
            failures += childTpe
          }

        case None =>
          println(" ~~> <!> impossible ")
          // Incompatible
      }

      */
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
