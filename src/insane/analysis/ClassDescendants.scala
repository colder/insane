package insane
package analysis

trait ClassDescendants { self: AnalysisComponent =>
  import global._

  object CDGraph {
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

    def toDotString(title: String = "Class Graph"): String = {
      var res: StringBuffer = new StringBuffer()
      def emit(s: String) = res.append(s)

      emit("digraph D {\n")
      emit(" label=\""+title+"\"\n")

      nodes.keys.foreach(sym => emit(sym.id+"[label=\""+sym.fullName+"\"]\n"));

      emit("\n")

      nodes.values.foreach(node => node.children.foreach(nc =>
        emit(""+node.symbol.id+" -> "+nc.symbol.id+"\n")
      ));

      emit("}\n") 

      res.toString
    }

    def writeDotToFile(fname: String, title: String): Unit = {
      import java.io.{BufferedWriter, FileWriter}
      val out = new BufferedWriter(new FileWriter(fname))
      out.write(toDotString(title))
      out.close
    }
  }

  def generateCDGraph() = {
    CDGraph.generate(definitions.RootClass)

    CDGraph.writeDotToFile("classgraph.dot", "Class Graph");
  }

  var descendantsCache = Map[Symbol, ObjectSet]()

  def getDescendants(sym: Symbol): ObjectSet = {
    if (!sym.isClass) {
      ObjectSet.empty
    } else if (sym.isSealed) {
      ObjectSet(sym.sealedDescendants.toSet + sym, true)
    } else {
      if (!descendantsCache.contains(sym)) {
        descendantsCache += sym -> sym.tpe.members.map(getDescendants(_)).foldRight(new ObjectSet(Set(sym), false))(_ ++ _)
      }

      descendantsCache(sym)
    }
  }

  case class ObjectSet(val symbols: Set[Symbol], val isExhaustive: Boolean) {
    override def toString = {
      if (isExhaustive) {
        symbols.map(_.name.toString).mkString("[", ", ", "]")
      } else {
        symbols.map(_.name.toString).mkString("]", ", ", "[")
      }
    }

    def ++ (that: ObjectSet) = ObjectSet(symbols ++ that.symbols, isExhaustive && that.isExhaustive)
  }

  object ObjectSet {
    def empty = apply(Set(), true)
    def singleton(symbol: Symbol) = apply(Set(symbol), true)
  }

}
