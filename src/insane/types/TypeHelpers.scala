package insane
package types

import utils.Reporters._

trait TypeHelpers { self: AnalysisComponent =>

  import global._

  def isGroundClass(s: Symbol) = atPhase(currentRun.typerPhase){s.tpe.parents exists (s => s.typeSymbol == definitions.AnyValClass)}

  def getMatchingMethods(methodSymbol: Symbol, types: Set[Type], pos: Position, silent: Boolean): Set[Symbol] = {
    assert(methodSymbol.isMethod, "Matching methods of non-method type: "+methodSymbol)

    var failures = Set[Type]();

    def getMatchingMethodIn(tpe: Type): Option[Symbol] = {
      var classes = Seq(tpe) ++ tpe.baseClasses.map(_.tpe)

      var res: Option[Symbol] = None

      for (cltpe <- classes if res.isEmpty) {
        val found = cltpe.decls.lookupAll(methodSymbol.name).find(sym => cltpe.memberType(sym) <:< methodSymbol.tpe)

        if (!found.isEmpty) {
          res = Some(found.get)
        }
      }

      if (res.isEmpty && tpe.typeSymbol != definitions.NothingClass) {
        failures += tpe
      }

      res
    }

    val r = types map { tpe => getMatchingMethodIn(tpe) } collect { case Some(ms) => ms }

    if (!failures.isEmpty && !silent) {
      reporter.warn("Failed to find method "+uniqueFunctionName(methodSymbol)+" in classes "+failures.mkString("{", ",", "}")+" amongst "+types.mkString("{", ",", "}"), pos)
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
