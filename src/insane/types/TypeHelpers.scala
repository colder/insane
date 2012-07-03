package insane
package types

import utils.Reporters._

trait TypeHelpers extends TypeMaps with TypeSignatures { self: AnalysisComponent =>

  import global._

  case class UnresolvedTargetInfo(sym: Symbol, sig: TypeSignature);
  case class ResolvedTargetInfo(cfg: FunctionCFG, sig: TypeSignature);

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

  var methodLookupCache = Map[(Symbol, CFG.CallStyle, TypeSignature), Set[UnresolvedTargetInfo]]()

  def getMatchingMethods(methodSymbol: Symbol, style: CFG.CallStyle, callSig: TypeSignature): Set[UnresolvedTargetInfo] = {
    val k = (methodSymbol,  style, callSig);
    methodLookupCache.getOrElse(k, {
      val r = lookupMatchingMethods(methodSymbol, style, callSig)
      methodLookupCache += k -> r
      r
    })
  }

  def lookupMatchingMethods(methodSymbol: Symbol, style: CFG.CallStyle, callSig: TypeSignature): Set[UnresolvedTargetInfo] = {

    settings.ifDebug {
      reporter.debug("@@@> Looking for method "+methodSymbol+" ("+methodSymbol.tpe+") in "+callSig.rec.info);
    }

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

    def lookUp(from: Type): Option[UnresolvedTargetInfo] = {
      for (tpe <- from.baseTypeSeq.toList) {
        val methodSymOpt = findMethod(methodSymbol, tpe)

        methodSymOpt match {
          case Some(methodSym) if methodSym.isDeferred =>
            return None
          case Some(methodSym) =>
            //settings.ifDebug {
            //  reporter.debug("=    Found method on receiver:" )
            //  reporter.debug("=                          == "+methodSym.fullName+"["+methodSym.tpe+"]")
            //  reporter.debug("=                          == "+tpe.memberType(methodSymbol))
            //}
            return Some(UnresolvedTargetInfo(methodSym, callSig.clampAccordingTo(methodSym)))
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

    def lookDown(from: Type, at: Type): Option[UnresolvedTargetInfo] = {
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
              Some(UnresolvedTargetInfo(sym, callSig.copy(tm = callSig.tm.copy(classTM = inferedMap)).clampAccordingTo(sym)))
            case _ =>
              reporter.warn("Failed to instanciate type parameters")
              None
          }
      }
    }

    if (style == CFG.StaticCall) {
      if (methodSymbol.isDeferred) {
        Set()
      } else {
        Set(UnresolvedTargetInfo(methodSymbol, callSig.clampAccordingTo(methodSymbol)))
      }
    } else {
      val info      = callSig.rec.info

      if (info == TypeInfo.empty && methodSymbol == definitions.Object_equals) {
        // Special case of null.equals(..)
        // We translate it to null.eq(..)
        Set(UnresolvedTargetInfo(definitions.Object_eq, callSig))
          
      } else {
        val downTypes = info.resolveTypes - info.tpe

        downTypes.flatMap{ t => lookDown(info.tpe, t) } ++ lookUp(info.tpe)
      }
    }
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
