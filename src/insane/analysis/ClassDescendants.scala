package insane
package analysis

trait ClassDescendants { self: AnalysisComponent =>
  import global._

  var descendantsCache = Map[Symbol, ObjectSet]()

  def getDescendants(sym: Symbol): ObjectSet = {
    if (sym.isSealed) {
      ObjectSet(sym.sealedDescendants.toSet, true)
    } else {
      if (!descendantsCache.contains(sym)) {
        descendantsCache += sym -> sym.tpe.members.map(getDescendants(_)).foldRight(ObjectSet.empty)(_ ++ _)
      }

      descendantsCache(sym)
    }
  }

  case class ObjectSet(val symbols: Set[Symbol], val isExhaustive: Boolean) {
    override def toString = {
      if (isExhaustive) {
        symbols.mkString("[", ", ", "]")
      } else {
        symbols.mkString("]", ", ", "[")
      }
    }

    def ++ (that: ObjectSet) = ObjectSet(symbols ++ that.symbols, isExhaustive && that.isExhaustive)
  }

  object ObjectSet {
    def empty = apply(Set(), true)
  }

}
