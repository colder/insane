package insane
package types

import utils.Reporters._

trait TypeHelpers { self: AnalysisComponent =>

  import global._

  def isGroundClass(s: Symbol) = atPhase(currentRun.typerPhase){s.tpe.parents exists (s => s.typeSymbol == definitions.AnyValClass)}

  def getMatchingMethods(methodSymbol: Symbol, types: Set[Type], pos: Position, silent: Boolean): Set[Symbol] = {
    assert(methodSymbol.isMethod, "Matching methods of non-method type: "+methodSymbol)

    val classes = types.map(_.typeSymbol)

    var failures = Set[Symbol]();

    def getMatchingMethodIn(classSymbol: Symbol): Option[Symbol] = {
      val classes = Seq(classSymbol) ++ classSymbol.ancestors

      var res: Option[Symbol] = None

      for (cl <- classes if res.isEmpty) {
        val found = cl.tpe.decls.lookupAll(methodSymbol.name).find(sym => cl.tpe.memberType(sym) <:< methodSymbol.tpe)

        if (!found.isEmpty) {
          res = Some(found.get)
        }
      }

      if (res.isEmpty && classSymbol != definitions.NothingClass) {
        failures += classSymbol
      }

      res
    }

    val r = classes map { cs => getMatchingMethodIn(cs) } collect { case Some(cs) => cs }

    if (!failures.isEmpty && !silent) {
      reporter.warn("Failed to find method "+uniqueFunctionName(methodSymbol)+" in classes "+failures.map(c => uniqueClassName(c)).mkString(","), pos)
    }
    r
  }

  def arrayType(tpe: Type) =
    TypeRef(NoPrefix, definitions.ArrayClass, List(tpe))

  def methodReturnType(methodSymbol: Symbol): ObjectSet = {
    val resType = methodSymbol.tpe.resultType

    resType match {
      case TypeRef(_, definitions.ArrayClass, List(tpe)) =>
        // resType is a parametrized array, we keep that type precise, ignore
        // descendents in this case
        ObjectSet.singleton(resType)
      case _ =>
        // General case
        ObjectSet.subtypesOf(resType)
    }
  }

  class DeflatedType(tpe: Type) {
    def inflate: Type = NoType
  }
}
