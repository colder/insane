package insane
package analysis

import utils.Graphs._
import utils._
import utils.Reporters.CompilerReporterPassThrough
import collection.mutable.Queue

trait ClassDescendents { self: AnalysisComponent =>

  import global._

  class ClassDescendentsPhase extends SubPhase {
    val name = "Generating class hierarchy"
    def run {
      import collection.mutable.Set

      // We traverse the symbols, for previously compiled symbols
      val oldReporter = global.reporter

      global.reporter = CompilerReporterPassThrough( (msg, pos) => settings.ifDebug( reporter.error(msg +" at "+pos) ))

      var seen  = Set[Symbol]()
      var lastSeen = seen;
      var i = 0;
      do {
        i += 1;

        lastSeen = seen
        seen = Set()

        var queue = Queue[Symbol](definitions.RootClass)
        while (!queue.isEmpty) {
          val sym = queue.dequeue
          if (sym.isClass || sym.isModule || sym.isTrait || sym.isPackage) {
            if (sym.rawInfo.isComplete || !sym.fullName.contains("$")) {
              val tpesym = if (sym.isType) sym else sym.tpe.typeSymbol

              if (!(seen contains tpesym) && tpesym != NoSymbol) {
                seen += tpesym

                val parent = tpesym.superClass

                if (!sym.isPackage) {
                  if (parent != NoSymbol) {
                    classDescendentGraph.addEdge(parent, tpesym)
                  } else {
                    // Some symbols really do not have any superClass
                    classDescendentGraph.addSingleNode(tpesym)
                  }
                }

                queue ++= tpesym.tpe.members
              }
            }
          } else if (!sym.isMethod && !sym.isValue) {
            reporter.warn("Ingored "+sym)
          }
        }
      } while(lastSeen != seen)

      reporter.info("Loaded "+seen.size+" symbols in "+i+" descents")

      global.reporter = oldReporter

      if (settings.dumpClassDescendents) {
        val path = "classgraph.dot";
        reporter.info("Dumping Class Graph to "+path)
        new DotConverter(classDescendentGraph, "Class Graph").writeFile(path)
      }
    }
  }

  case class CDVertex(symbol: Symbol) extends MutVertexAbs[CDEdge] {
    val name = symbol.name.toString()
    var children = Set[CDVertex]()
  }

  case class CDEdge(v1: CDVertex, v2: CDVertex) extends EdgeAbs[CDVertex]

  class ClassDescendentGraph extends MutableDirectedGraphImp[CDVertex, CDEdge] {
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

  }
  def debugSymbol(sym: Symbol) {
    println("Symbol: "+sym+" (ID: "+sym.id+")") 
    if (sym == NoSymbol) return;
    val isComplete = sym.rawInfo.isComplete
    println("  owner:         "+sym.owner+" (ID: "+sym.owner.id+")")
    println("  cont. in own.: "+(sym.owner.tpe.members contains sym))
    println("  isComplete:    "+isComplete)
    println("  isClass:       "+sym.isClass)
    val comp = if(sym.isModuleClass) sym.companionModule else sym.companionClass
    println("  companion:     "+comp+" (ID: "+comp.id+")")
    println("  isModule:      "+sym.isModule)
    println("  isModuleClass: "+sym.isModuleClass)
    println("  isTrait:       "+sym.isTrait)
    println("  isfinal:       "+sym.isFinal)
    println("  isPackage:     "+sym.isPackage)
    println("  isMethod:      "+sym.isMethod)
    println("  isValue:       "+sym.isValue)

    if (isComplete) {
      val tpesym = if (sym.isType) sym else sym.tpe.typeSymbol
      println("  isType:        "+sym.isType)
      println("  sym==type:     "+(sym == tpesym))
      println("  Type:          "+tpesym)
      println("  TypeAncestors: "+tpesym.ancestors.mkString(", "))
      println("  Superclass:    "+tpesym.superClass)
    }
  }

  def symbolInfo(sym: Symbol) = {
    List(
      if(sym.isClass) "Cl" else "",
      if(sym.rawInfo.isComplete) "Co" else "",
      if(sym.isModule) "Mo" else "",
      if(sym.isModuleClass) "Mc" else "",
      if(sym.isPackage) "P" else ""
    ).mkString(",")
  }

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

  var descendentsCache = Map[Symbol, ObjectSet]()

  def getDescendents(s: Symbol): ObjectSet = {
    val tpesym = if (s.isType) s else s.tpe.typeSymbol

    if (!tpesym.isClass) {
      ObjectSet.empty
    } else {

      if (!descendentsCache.contains(tpesym)) {
        val oset = if (tpesym == definitions.ObjectClass.tpe.typeSymbol) {
          AllObjects
        } else if (tpesym.isFinal) {
          ObjectSet.singleton(tpesym)
        } else if (tpesym.isSealed) {
          val exhaust = tpesym.sealedDescendants.forall(_.isSealed)
          ObjectSet(tpesym.sealedDescendants.toSet + tpesym, exhaust)
        } else if (classDescendentGraph.sToV contains tpesym) {
          val set = classDescendentGraph.sToV(tpesym).children.flatMap(n => getDescendents(n.symbol).symbols) + tpesym
          ObjectSet(set, set.forall(s => s.isSealed || s.isFinal))
        } else {
          reporter.warn("Unable to obtain descendents of unvisited type: "+tpesym)
          debugSymbol(tpesym)
          ObjectSet(Set(), false)
        }

        descendentsCache += tpesym -> oset
      }

      descendentsCache(tpesym)
    }
  }

}
