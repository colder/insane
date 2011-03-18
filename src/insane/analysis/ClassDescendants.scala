package insane
package analysis

trait ClassDescendants { self: AnalysisComponent =>
  import global._

  var descendantsCache = Map[Symbol, Set[Symbol]]()

  def getDescendants(sym: Symbol): Set[Symbol] = {
    if (sym.isSealed) {
      sym.sealedDescendants.toSet
    } else {
      if (!descendantsCache.contains(sym)) {
        descendantsCache += sym -> sym.tpe.members.map(getDescendants(_)).foldRight(Set[Symbol]())(_ ++ _)
      }

      descendantsCache(sym)
    }
  }

}
