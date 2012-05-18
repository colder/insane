package insane
package types

import utils.Reporters._

trait TypeHelpers extends TypeMaps with TypeSignatures { self: AnalysisComponent =>

  import global._

  /* Some classes are specialized into specific value nodes, cannot be ground
   * types as it would prevent that distinction
   */
  lazy val specializedGroundClasses: Set[Symbol] = Set(
    definitions.BooleanClass,  
    definitions.IntClass,
    definitions.LongClass
  )

  lazy val allGroundClasses: Set[Symbol] = definitions.ScalaValueClasses.map(cs => cs : Symbol).toSet

  def isGroundClass(s: Symbol) = allGroundClasses(s) && !specializedGroundClasses(s)

  def isGroundTypeInfo(info: TypeInfo) = {
    isGroundClass(info.tpe.typeSymbol)
  }

  def tpeToString(t: Type): String = {
    // Try to still gather information
    val str = t.toString

    if (str != "...") { 
      str
    } else {
      "?"+t.typeSymbol.toString  
    }
  }

  def instantiateChildTypeParameters(parentTpe: Type, childTpe: Type): Option[(Type, ClassTypeMap)] = {
    val childSym  = childTpe.typeSymbol
    val parentSym = parentTpe.typeSymbol

    if (childTpe == parentTpe) {
      parentTpe match {
        case TypeRef(_, _, params) =>
          return Some((childTpe, ClassTypeMap((childSym.typeParams zip params).toMap)))
        case _ =>
          return Some((childTpe, ClassTypeMap(childSym.typeParams.map(p => (p, p.tpe)).toMap)))
      }
    }

    val childTypeVars     = childSym.typeParams.map(s => TypeVar(s.tpeHK, new TypeConstraint, Nil, Nil))
    
    val childAppliedType  = appliedType(childSym.tpe, childTypeVars)

    val parentAppliedType = parentTpe match {
      case TypeRef(pre, sym, params) =>
        //println("Found in "+sym.fullName+" params:"+ params)

        val paramMap = ((params zip sym.typeParams) flatMap { case (tp, p) =>
          val sym = tp.typeSymbol

          if (sym.isTypeParameter || sym.isTypeSkolem) {
            val tv = TypeVar.untouchable(sym)

            //println(" param "+p+" refers to "+sym+": "+tp+" with TV: "+tv+" with constraints: "+tv.constr)

            Some((sym, tv))
          } else {
            None
          }
        }).unzip

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
            //reporter.warn("Type variable "+tv+" refers to a symbol that is invariant, and we didn't find a valid fixed bound: "+bounds+", using hi")
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

      Some((instantiatedType, ClassTypeMap(inferredMap)))
    } else {
      None
    }
  }

  def canBeSubtypeOf(child: Type, parent: Type): Option[Type] = {
    if (child <:< parent) {
      Some(child)
    } else {
      instantiateChildTypeParameters(child, parent).map(_._1)
    }
  }

  var methodLookupCache = Map[(Symbol, Type, TypeInfo), Set[(Symbol, ClassTypeMap)]]()

  def getMatchingMethods(methodSymbol: Symbol, methodType: Type, info: TypeInfo): Set[(Symbol, ClassTypeMap)] = {
    val k = (methodSymbol, methodType, info);
    methodLookupCache.getOrElse(k, {
      val r = lookupMatchingMethods(methodSymbol, methodType, info)
      methodLookupCache += k -> r
      r
    })
  }

  def lookupMatchingMethods(methodSymbol: Symbol, methodType: Type, info: TypeInfo): Set[(Symbol, ClassTypeMap)] = {

    reporter.debug("@@@> Looking for method "+methodSymbol+" ("+methodSymbol.tpe+") in "+info);

    val methodName = methodSymbol.name

    var failures = Set[Type]();

    def findMethod(method: Symbol, inClass: Type): Option[Symbol] = {
      val methodSymbol = inClass.decl(method.name)

      if (methodSymbol == NoSymbol) {
        None
      } else if (methodSymbol.isDeferred) {
        Some(methodSymbol)
      } else {
        val res = methodSymbol.filter { sym => sym.tpe =:= method.tpe }

        if (res.isOverloaded) {
          reporter.warn(List("Found method with =:= but too many valid alternatives compatible:",
                             "  found:       ") :::
                        res.alternatives.map(sym => "   -> "+sym.tpe) :::
                        List("  looking for: "+method.tpe))
          None
        } else if (res == NoSymbol) {
          val resSub = methodSymbol.filter { sym => (sym.tpe <:< method.tpe) && !sym.isBridge}

          if (resSub.isOverloaded) {
            reporter.warn(List("Found method with <:< but too many valid alternatives compatible:",
                               "  found:       ") :::
                          resSub.alternatives.map(sym => "   -> "+sym.tpe) :::
                          List("  looking for: "+method.tpe))

            resSub.alternatives.foreach(debugSymbol)
            None
          } else if (resSub == NoSymbol) {
            //reporter.warn(List("Found method with <:< but none compatible:",
            //                   "  found:       ") :::
            //              methodSymbol.alternatives.map(sym => "   -> "+sym.tpe) :::
            //              List("  looking for: "+method.tpe))
            //resSub.alternatives.foreach(debugSymbol)
            None
          } else {
            Some(resSub)
          }
        } else {
          Some(res)
        }
      }
    }

    def lookUp(from: Type): Option[(Symbol, ClassTypeMap)] = {
      for (tpe <- from.baseTypeSeq.toList) {
        val methodSymOpt = findMethod(methodSymbol, tpe)

        methodSymOpt match {
          case Some(methodSym) if methodSym.isDeferred =>
            reporter.debug("Found deferred method: "+uniqueFunctionName(methodSym))
            return None
          case Some(methodSym) =>
            //settings.ifDebug {
            //  reporter.debug("=    Found method on receiver:" )
            //  reporter.debug("=                          == "+methodSym.fullName+"["+methodSym.tpe+"]")
            //  reporter.debug("=                          == "+tpe.memberType(methodSymbol))
            //}
            return Some((methodSym,  computeClassTypeMapFromInstType(tpe)))
          case None =>
            // continue
        }
      }
      reporter.warn(List(
        "Failed to find:",
        uniqueFunctionName(methodSymbol)+" in ",
        from.baseTypeSeq.toList.mkString(", ")
      ))

      None
    }

    def lookDown(from: Type, at: Type): Option[(Symbol, ClassTypeMap)] = {
      val methodSymOpt = findMethod(methodSymbol, at)

      methodSymOpt match {
        case None =>
          None
        case Some(sym) if sym.isDeferred =>
          None
        case Some(sym) =>
          val childClass = sym.owner
          /**
           * We found a method symbol in childClass that matches
           * the prototype, now let's see if we can find an instantiation
           * childTpeInst c: parentTpe such that
           * childTpeInst.memberTpe(childMethodSym) c: parentTpe.memberType(methodSymbol)
           */

          instantiateChildTypeParameters(from, childClass.tpe) match {
            case Some((refinedChildTpe, inferedMap)) =>
//              settings.ifDebug {
//                reporter.debug("=@@    Found "+sym.tpe)
//              }
//
              Some((sym, inferedMap))
            case _ =>
              reporter.warn("Failed to instanciate type parameters")
              None
          }
      }
    }

    //def getMatchingMethodIn(recType: Type, tentativeType: Type): Option[(Symbol, ClassTypeMap)] = {
    //  /**
    //   * We only need to look in the upward type chain for methods in case we
    //   * analyse the top-parent one.
    //   * 
    //   *  class A { def f; }
    //   *  class B extends A { }
    //   *  class C extends B { override def f; }
    //   *  class D extends C { }
    //   *  class E extends D { override def f; }
    //   * 
    //   * Receiver is { _ <: B } ~=> B,C,D,E
    //   * Matching function will be called with (B, B), (B, C), (B, D), and (B, E)
    //   * 
    //   * It must find A.f, C.f, E.f
    //   */
    //  var upwardTypeChain = if (recType == tentativeType) {
    //    recType.baseTypeSeq.toList
    //  } else {
    //    List(tentativeType)
    //  }

    //  reporter.debug("===> Looking for method "+methodSymbol.fullName+" in "+tentativeType+" as seen from "+recType);

    //  for (tpe <- upwardTypeChain) {
    //    if (tpe == upwardTypeChain.head) {
    //      /*
    //       * Looking down. Normal instantiation of types
    //       */
    //      } else {
    //        println("=    Signature mismatch: "+parentMethodIntoChildTpe+" -/- "+childMethodType)
    //      }
    //    } else {
    //      /*
    //       * Looking up, the parent type is already instanciated by baseTypeSeq
    //       */
    //      val parentMethodSym= tpe.decl(methodName)

    //      if (parentMethodSym.isDeferred) {
    //        println("=    Found abstract method, skipping")
    //        return None
    //      } else if (parentMethodSym != NoSymbol) {
    //        reporter.debug("=    Method symbol in "+tpe+": "+methodSymbol)
    //        reporter.debug("=    Type map here: "+computeClassTypeMapFromInstType(tpe))
    //        return Some((parentMethodSym,  computeClassTypeMapFromInstType(tpe)))
    //      }
    //    }
    //  }

    //  if (recType == tentativeType) {
    //    failures += recType
    //  }

    //  None
    //}

    val downTypes = info.resolveTypes - info.tpe

    downTypes.flatMap{ t => lookDown(info.tpe, t) } ++ lookUp(info.tpe)
  }

  def arrayType(tpe: Type) =
    TypeRef(NoPrefix, definitions.ArrayClass, List(tpe))

  def methodReturnType(methodSymbol: Symbol): TypeInfo = {
    val resType = methodSymbol.tpe.resultType

    val r = resType match {
      case TypeRef(_, definitions.ArrayClass, List(tpe)) =>
        // resType is a parametrized array, we keep that type precise, ignore
        // descendents in this case
        TypeInfo.exact(resType)
      case _ =>
        // General case
        TypeInfo.subtypeOf(resType)
    }
    r
  }
}
