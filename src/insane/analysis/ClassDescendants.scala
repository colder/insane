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

    def addSingleNode(node: Symbol) = {
      if (!sToV.contains(node)) {
        sToV += node -> CDVertex(node)
        this += sToV(node)
      }
    }

    def generate(root: Symbol) = {

      def recurse(sym: Symbol): Unit = {
        if (sym.rawInfo.isComplete) {
          val tpesym = if (sym.isType) sym else sym.tpe.typeSymbol

          if (tpesym.name == nme.NOSYMBOL) {
            return
          }

          if (sym.isClass || sym.isModule || sym.isTrait || sym.isPackage) {

            val ances = tpesym.ancestors


            if (!ances.isEmpty) {
              val parent = ances.head;
              addEdge(parent, tpesym)
            } else {
              addSingleNode(tpesym)
            }

            tpesym.tpe.members.foreach{ recurse _ }
          } else if (!sym.isMethod && !sym.isValue) {
            reporter.warn("Ingored "+sym)
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

  def getDescendants(s: Symbol): ObjectSet = {
    val tpesym = if (s.isType) s else s.tpe.typeSymbol

    if (!tpesym.isClass) {
      ObjectSet.empty
    } else {

      if (!descendantsCache.contains(tpesym)) {
        val oset = if (tpesym.isSealed) {
          val exaust = tpesym.sealedDescendants.forall(_.isSealed)
          ObjectSet(tpesym.sealedDescendants.toSet + tpesym, exaust)
        } else if (CDGraph.sToV contains tpesym) {
          ObjectSet(CDGraph.sToV(tpesym).children.flatMap(n => getDescendants(n.symbol).symbols) + tpesym, false)
        } else {
          reporter.warn("Ignoring subclasses of unvisited type: "+tpesym)

          ObjectSet(Set(), false)
        }

        descendantsCache += tpesym -> oset
      }

      descendantsCache(tpesym)
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
