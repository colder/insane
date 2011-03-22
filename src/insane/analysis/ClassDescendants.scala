package insane
package analysis

import utils._

trait ClassDescendants { self: AnalysisComponent =>
  class CDVertex(val name: String) extends VertexAbs[CDEdge] {

  }

  class CDEdge(val v1: CDVertex, val v2: CDVertex) extends EdgeAbs[CDVertex] {

  }

  import global._

  object CDGraph extends DirectedGraphImp[CDVertex, CDEdge] {
    var nodes = Map[Symbol, Node]()

    case class Node(symbol: Symbol) {
      var children = Set[Node]()
    }

    def addEdge(parent: Symbol, child: Symbol) = {
      if (!nodes.contains(parent)) {
        nodes += parent -> Node(parent)
      }

      if (!nodes.contains(child)) {
        nodes += child -> Node(child)
      }

      nodes(parent).children += nodes(child)
    }

    def generate(root: Symbol) = {

      def recurse(sym: Symbol): Unit = {
        if (sym.isClass) {
          if (sym.rawInfo.isComplete) {
            val ances = sym.ancestors

            if (!ances.isEmpty) {
              val parent = ances.head;
              addEdge(parent, sym)
            }

            if (sym.isPackageClass || sym.isModuleClass) {
              sym.tpe.members.foreach(recurse _)
            }
          }
        } else if (sym.isPackage) {
          sym.tpe.members.foreach(recurse _)
        }
      }

      recurse(root)
    }

    def toDotFile(title: String = "Class Graph", path: String) {
      /*
      val g = new LabeledDirectedGraphImp[String]

      var nToV = Map[Node, g.Vertex]()

      nodes.foreach { case (sym, node) => nToV += node -> g.newNamedVertex(sym.fullName) }

      nodes.values.foreach(node => node.children.foreach(nc => g += (nToV(node), "", nToV(nc))));

      g.writeDotToFile(path, title)
      */
    }
  }

  def generateCDGraph() = {
    CDGraph.generate(definitions.RootClass)

    if (settings.dumpClassDescendents) {
      val path = "classgraph.dot";
      reporter.info("Dumping Class Graph to "+path)
      CDGraph.toDotFile("Class Graph", path);
    }
  }

  var descendantsCache = Map[Symbol, ObjectSet]()

  def getDescendants(sym: Symbol): ObjectSet = {
    if (!sym.isClass) {
      ObjectSet.empty
    } else {

      if (!descendantsCache.contains(sym)) {
        val oset = if (sym.isSealed) {
          val exaust = sym.sealedDescendants.forall(sym => sym.isSealed)
          ObjectSet(sym.sealedDescendants.toSet + sym, exaust)
        } else {
          assert(CDGraph.nodes contains sym, "Graph does not contain symbol: "+sym)

          ObjectSet(CDGraph.nodes(sym).children.flatMap(n => getDescendants(n.symbol).symbols) + sym, false)
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
