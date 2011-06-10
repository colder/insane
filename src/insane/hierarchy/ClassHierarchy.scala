package insane
package hierarchy

import utils.Graphs._
import utils._
import storage.Database
import utils.Reporters.{CompilerReporterPassThrough,posToOptPos}
import collection.mutable.Queue

trait ClassHierarchy { self: AnalysisComponent =>

  import global._

  class ClassHierarchyPhase extends SubPhase {
    val name = "Generating class hierarchy"

    def loadFromClassfiles() {
      import collection.mutable.Set

      // We traverse the symbols, for previously compiled symbols
      val oldReporter = global.reporter

      global.reporter = CompilerReporterPassThrough( (msg, pos) => settings.ifVerbose( reporter.warn(msg, pos.asInstanceOf[tools.nsc.util.Position]) ))

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
            if (sym.rawInfo.isComplete || !safeFullName(sym).contains("$")) {
              val tpesym = if (sym.isType) sym else sym.tpe.typeSymbol

              if (!(seen contains tpesym) && tpesym != NoSymbol) {
                seen += tpesym

                val parent = tpesym.superClass

                if (!sym.isPackage) {
                  if (parent != NoSymbol) {
                    classHierarchyGraph.addEdge(parent, tpesym)
                  } else {
                    // Some symbols really do not have any superClass
                    classHierarchyGraph.addSingleNode(tpesym)
                  }
                }

                queue ++= tpesym.tpe.members
              }
            }
          } else if (!sym.isMethod && !sym.isValue) {
            reporter.warn("Ignored "+sym, sym.pos)
          }
        }
      } while(lastSeen != seen)

      reporter.info("Loaded "+seen.size+" symbols in "+i+" descents")

      global.reporter = oldReporter

    }

    def loadFromTrees() {
      def traverseStep(tree: Tree) = tree match {
        case cd @ ClassDef(modes, name, tparams, impl) =>
          val classSymbol = cd.symbol
          val parent = if(classSymbol.superClass == NoSymbol) { definitions.ObjectClass } else { classSymbol.superClass }

          assert(classSymbol.isType, "Class symbol "+uniqueClassName(classSymbol)+" is not a type!")

          classHierarchyGraph.addEdge(parent, classSymbol)
        case _ =>
      }
      for (unit <- currentRun.units) {
        new ForeachTreeTraverser(traverseStep).traverse(unit.body)
      }
    }

    def run() {
      if (settings.buildLib) {
        loadFromClassfiles()

        if (settings.dumpClassDescendents) {
          val path = "classgraph.dot";
          reporter.info("Dumping Class Graph to "+path)
          new DotConverter(classHierarchyGraph, "Class Graph").writeFile(path)
        }

        fillDatabase()
      } else {
        loadFromTrees()
      }
    }

    def fillDatabase() {
      def fixChain(s: Symbol) {
        if (s != definitions.ObjectClass) {
          val parent = if (s.superClass == NoSymbol) definitions.ObjectClass else s.superClass

          classHierarchyGraph.addEdge(parent, s)

          fixChain(parent)
        }
      }

      var roots = classHierarchyGraph.V &~ classHierarchyGraph.V.flatMap(v => v.out.map(_.v2))

      for (r <- roots) {
        fixChain(r.symbol)
      }

      reporter.info("Inserting "+classHierarchyGraph.V.size+"entries in the database...")

      var toInsert = Set[(String, Long, Long)]()

      def insert(v: CHVertex, left: Long): Long = {
        val sym = v.symbol

        var currentLeft = left + 1

        for (CDEdge(_, v2) <- v.out) {
          currentLeft = insert(v2, currentLeft)
        }

        toInsert += ((uniqueClassName(sym), left, currentLeft))

        currentLeft + 1
      }

      insert(classHierarchyGraph.sToV(definitions.ObjectClass), 1)

      Database.Hierarchy.insertAll(toInsert)
    }
  }

  case class CHVertex(symbol: Symbol) extends MutVertexAbs[CDEdge] {
    val name = symbol.name.toString()
    var children = Set[CHVertex]()
  }

  case class CDEdge(v1: CHVertex, v2: CHVertex) extends EdgeAbs[CHVertex]

  class ClassHierarchyGraph extends MutableDirectedGraphImp[CHVertex, CDEdge] {
    var sToV = Map[Symbol, CHVertex]()

    def addEdge(parent: Symbol, child: Symbol) = {
      if (!sToV.contains(parent)) {
        sToV += parent -> CHVertex(parent)
      }

      if (!sToV.contains(child)) {
        sToV += child -> CHVertex(child)
      }

      val vParent = sToV(parent)
      val vChild  = sToV(child)

      this += CDEdge(vParent, vChild)
      vParent.children += vChild
    }

    def addSingleNode(node: Symbol) = {
      if (!sToV.contains(node)) {
        sToV += node -> CHVertex(node)
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

  var descendentsCache = Map[Symbol, Set[Symbol]]()

  def getDescendents(s: Symbol): Set[Symbol] = {
    val tpesym = if (s.isType) s else s.tpe.typeSymbol
    val tpe    = s.tpe

    if (!tpesym.isClass) {
      Set[Symbol]()
    } else {
      if (!descendentsCache.contains(tpesym)) {
        val set = if (tpesym.isFinal) {
          Set[Symbol]()
        } else if (classHierarchyGraph.sToV contains tpesym) {
          classHierarchyGraph.sToV(tpesym).children.flatMap(v => getDescendents(v.symbol))
        } else {
          // We request the database
          val name    = uniqueClassName(tpesym)
          val subTree = Database.Hierarchy.subTree(name).flatMap(str => lookupClassSymbol(str))

          if (subTree.isEmpty && Database.Hierarchy.transLookup(name).isEmpty) {
            reporter.warn("Unable to obtain descendents of unvisited type: "+tpesym, Some(tpesym.pos))
            debugSymbol(tpesym)
          }

          subTree
        }
        descendentsCache += tpesym -> set
      }

      descendentsCache(tpesym)
    }
  }

}
