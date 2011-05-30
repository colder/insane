package insane
package utils

trait TypeHelpers { self: AnalysisComponent =>

  import global._

  def isGroundClass(s: Symbol) = atPhase(currentRun.typerPhase){s.tpe.typeSymbol == definitions.StringClass || (s.tpe.parents exists (s => s.typeSymbol == definitions.AnyValClass))}

  def getMatchingMethods(methodSymbol: Symbol, classes: Set[Symbol], position: Position): Set[Symbol] = {
    assert(methodSymbol.isMethod, "Matching methods of non-method type: "+methodSymbol)

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

      if (res.isEmpty) {
        failures += classSymbol
      }

      res
    }

    val r = classes map { cs => getMatchingMethodIn(cs) } collect { case Some(cs) => cs }

    if (!failures.isEmpty) {
      reporter.warn("Failed to find method "+methodSymbol.fullName+" (type: "+methodSymbol.tpe+") in classes "+failures.map(_.name).mkString(",")+" at "+position)
    }
    r
  }
}
