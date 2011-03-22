package insane
package analysis

import utils._

trait ClassDescendants { self: AnalysisComponent =>

  import global._

  case class CDVertex(val symbol: Symbol) extends VertexAbs[CDEdge] {
    val name = symbol.name.toString
    var children = Set[CDVertex]()
  }

  case class CDEdge(val v1: CDVertex, val v2: CDVertex) extends EdgeAbs[CDVertex]

  object CDGraph extends DirectedGraphImp[CDVertex, CDEdge] {
    var sToV = Map[Symbol, CDVertex]()

    def addEdge(parent: Symbol, child: Symbol) = {
      if (!sToV.contains(parent)) {
        sToV += parent -> CDVertex(parent)
      }

      if (!sToV.contains(child)) {
        sToV += child -> CDVertex(child)
      }

      val vParent = sToV(parent)
      val vChild  = sToV(child)

      this += CDEdge(vParent, vChild)
      vParent.children += vChild
    }

    def generate(root: Symbol) = {

      def recurse(sym: Symbol): Unit = {
        if (sym.rawInfo.isComplete) {
          if (sym.isClass || sym.isModule || sym.isTrait) {
            val ances = sym.ancestors

            if (!ances.isEmpty) {
              val parent = ances.head;
              addEdge(parent, sym)
            }

            sym.tpe.members.foreach(recurse _)
          } else if (sym.isPackage) {
            sym.tpe.members.foreach(recurse _)
          }
        }
      }

      recurse(root)
    }
  }

  def generateCDGraph() = {
    CDGraph.generate(definitions.RootClass)

    if (settings.dumpClassDescendents) {
      val path = "classgraph.dot";
      reporter.info("Dumping Class Graph to "+path)
      CDGraph.writeDotToFile(path, "Class Graph");
    }
  }

  var descendantsCache = Map[Symbol, ObjectSet]()

  def getDescendants(tpe: Type): ObjectSet = {
    val sym = tpe.typeSymbol

    if (!sym.isClass) {
      ObjectSet.empty
    } else {

      if (!descendantsCache.contains(sym)) {
        val oset = if (sym.isSealed) {
          val exaust = sym.sealedDescendants.forall(sym => sym.isSealed)
          ObjectSet(sym.sealedDescendants.toSet + sym, exaust)
        } else {
          assert(CDGraph.sToV contains sym, "Graph does not contain symbol: "+sym)

          ObjectSet(CDGraph.sToV(sym).children.flatMap(n => getDescendants(n.symbol.tpe).symbols) + sym, false)
        }

        descendantsCache += sym -> oset
      }

      descendantsCache(sym)
    }
  }

  case class ObjectSet(val symbols: Set[Symbol], val isExhaustive: Boolean) {
    override def toString = {
      if (isExhaustive) {
        symbols.map(_.name.toString).mkString("{", ", ", "}")
      } else {
        symbols.map(_.name.toString).mkString("{", ", ", "} and subtypes")
      }
    }

    def ++ (that: ObjectSet) = ObjectSet(symbols ++ that.symbols, isExhaustive && that.isExhaustive)
  }

  object ObjectSet {
    def empty = apply(Set(), true)
    def singleton(symbol: Symbol) = apply(Set(symbol), true)
  }

}
