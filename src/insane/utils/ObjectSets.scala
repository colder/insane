package insane
package utils

trait ObjectSets { self: AnalysisComponent =>

  import global._

  case class ObjectSet(symbols: Set[Symbol], isExhaustive: Boolean) {
    override def toString = {
      if (isExhaustive) {
        symbols.map(_.name.toString()).mkString("{", ", ", "}")
      } else {
        symbols.map(_.name.toString()).mkString("{", ", ", "} and subtypes")
      }
    }

    def ++ (that: ObjectSet) = ObjectSet(symbols ++ that.symbols, isExhaustive && that.isExhaustive)
  }

  object AllObjects extends ObjectSet(Set(definitions.ObjectClass.tpe.typeSymbol), false) {
    override def toString = {
      "{.. All objects ..}"
    }
  }

  object ObjectSet {
    def empty = apply(Set(), true)
    def singleton(symbol: Symbol) = apply(Set(symbol), true)
  }

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
