package insane
package hierarchy

import utils.Graphs._
import utils._
//import storage.Database
import utils.Reporters.{CompilerReporterPassThrough,posToOptPos}
import collection.mutable.Queue
import scala.reflect.internal.util.Position

trait ClassHierarchy { self: AnalysisComponent =>

  import global._

  class ClassHierarchyPhase extends SubPhase {
    val name = "Generating/Loading class hierarchy"

    def loadFromClassfiles() {
      import collection.mutable.Set

      // We traverse the symbols, for previously compiled symbols
      val oldReporter = global.reporter

      global.reporter = CompilerReporterPassThrough( (msg, pos) => settings.ifVerbose( reporter.warn(msg, pos.asInstanceOf[Position]) ))

      var seen  = Set[Symbol]()
      var lastSeen = seen;
      var i = 0;
      do {
        i += 1;

        lastSeen = seen
        seen = Set()

        var queue = Queue[Symbol](rootMirror.RootClass)
        while (!queue.isEmpty) {
          val sym = queue.dequeue
          if (sym.isClass || sym.isModule || sym.isTrait || sym.isPackage) {
            if (sym.rawInfo.isComplete || !safeFullName(sym).contains("$")) {
              val tpesym = if (sym.isType) sym else sym.tpe.typeSymbol

              if (!(seen contains tpesym) && tpesym != NoSymbol) {
                seen += tpesym

                if (!sym.isPackage) {
                  classHierarchyGraph.addSingleNode(tpesym)
                  for (parentType <- tpesym.info.parents) {
                    val parent = parentType.typeSymbol
                    if (parent != NoSymbol) {
                      classHierarchyGraph.addEdge(parent, tpesym)
                    } else {
                      reporter.debug("Woops, "+parentType+" has no typesymbol == NoSymbol");
                    }
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

      reporter.msg("Loaded "+seen.size+" symbols in "+i+" descents")

      global.reporter = oldReporter

    }

    def loadFromTrees() {
      def traverseStep(tree: Tree) = tree match {
        case cd @ ClassDef(modes, name, tparams, impl) =>
          val classSymbol = cd.symbol

          assert(classSymbol.isType, "Class symbol "+uniqueClassName(classSymbol)+" is not a type!")

          classHierarchyGraph.addSingleNode(classSymbol)
          for (parentType <- classSymbol.info.parents) {
            val parent = parentType.typeSymbol
            if (parent != NoSymbol) {
              classHierarchyGraph.addEdge(parent, classSymbol)
            } else {
              reporter.debug("Woops, "+parentType+" has no typesymbol == NoSymbol");
            }
          }
        case _ =>
      }
      for (unit <- currentRun.units) {
        new ForeachTreeTraverser(traverseStep).traverse(unit.body)
      }
    }

    def run() {
      if (settings.fillHierarchy) {
        loadFromClassfiles()
      } else {
        loadFromTrees()
      }

      if (settings.dumpClassDescendents) {
        val path = "classgraph.dot";
        reporter.msg("Dumping Class Graph to "+path)
        new DotConverter(classHierarchyGraph, "Class Graph").writeFile(path)
      }
    }
  }

  case class CHVertex(symbol: Symbol) extends VertexAbs {
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
    println(Console.CYAN+Console.BOLD+"Symbol:"+Console.RESET+" "+sym+" (ID: "+sym.id+")") 
    if (sym == NoSymbol) return;
    val isComplete = sym.rawInfo.isComplete
    println("  fullname:      "+sym.fullName)
    println("  owner:         "+sym.owner+" (ID: "+sym.owner.id+")")
    println("  isComplete:    "+isComplete)
    println("  flags:         "+sym.flagBitsToString(sym.flags))
    println("  type:          "+sym.tpe)
    println("  type (safe):   "+sym.tpe.safeToString)
  }

  var descendentsCache          = Map[Symbol, Set[Symbol]]()
  var directDescendentsCache    = Map[Symbol, Set[Symbol]]()

  def lookupClassSymbol(str: String): Option[Symbol] = {
    val ds = ClassSymbolUnSerializer(str).unserialize()

    if (ds == NoSymbol) {
      None
    } else {
      Some(ds)
    }
  }

  def getDirectDescendents(s: Symbol): Set[Symbol] = {
    val tpesym = if (s.isType) s else s.tpe.typeSymbol
    val tpe    = s.tpe
    if (!tpesym.isClass) {
      Set[Symbol]()
    } else {
      if (!directDescendentsCache.contains(tpesym)) {
        val set = if (tpesym.isFinal) {
          Set[Symbol]()
        } else if (classHierarchyGraph.sToV contains tpesym) {
          classHierarchyGraph.sToV(tpesym).children.map(_.symbol)
        //} else if (Database.active) {
        //  sys.error("Not implemented yet!");
        //  Set[Symbol]()
        } else {
          Set[Symbol]()
        }
        directDescendentsCache += tpesym -> set
        set
      } else {
        directDescendentsCache(tpesym)
      }
    }
  }

  def getDescendents(s: Symbol): Set[Symbol] = {
    var tpesym = if (s.isType) s else s.tpe.typeSymbol
    var tpe    = s.tpe

    if (!tpesym.isClass) {
      // we use the higher type bound instead:
      tpe    = tpe.bounds.hi
      tpesym = tpe.typeSymbol
    }

    if (!tpesym.isClass) {
        settings.ifDebug {
          reporter.warn("Symbol "+safeFullName(tpesym)+" is still not a class, was "+s+" first");
          debugSymbol(s)
          debugSymbol(tpesym)
        }
      Set[Symbol]()
    } else {
      if (!descendentsCache.contains(tpesym)) {
        val set = if (tpesym.isFinal) {
          Set[Symbol]()
        } else if (classHierarchyGraph.sToV contains tpesym) {
          classHierarchyGraph.sToV(tpesym).children.flatMap(v => getDescendents(v.symbol)+v.symbol)
        //} else if (Database.active) {
        //  // We request the database
        //  val name    = uniqueClassName(tpesym)
        //  val subTree = Database.Hierarchy.subTree(name).flatMap(lookupClassSymbol _)

        //  if (subTree.isEmpty && Database.Hierarchy.transLookup(name).isEmpty) {
        //    reporter.warn("Unable to obtain descendents of unvisited type: "+tpesym, Some(tpesym.pos))
        //    debugSymbol(tpesym)
        //  }

        //  subTree
        } else {
          settings.ifDebug {
            if (safeFullName(tpesym).startsWith("scala.")) {
              reporter.warn("Symbol "+safeFullName(tpesym)+" not found in the class hierarchy graph!");
            }
          }
          Set[Symbol]()
        }
        descendentsCache += tpesym -> set
        set
      } else {
        descendentsCache(tpesym)
      }

    }
  }

}
