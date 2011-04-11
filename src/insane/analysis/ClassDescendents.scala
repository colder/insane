package insane
package analysis

import utils.Graphs._
import utils._

trait ClassDescendents { self: AnalysisComponent =>

  import global._

  class ClassDescendentsPhase extends SubPhase {
    val name = "Generating class hierarchy"
    def run = {

      var seen = Set[Symbol]()
      def recurseSym(sym: Symbol): Unit = {
        val tpesym = if (sym.isType) sym else sym.tpe.typeSymbol


        if (sym.isClass || sym.isModule || sym.isTrait || sym.isPackage) {

          if (seen contains tpesym) {
            return
          } else {
            seen += tpesym
          }

          if (tpesym.name == nme.NOSYMBOL) {
            return
          }

          val parent = tpesym.superClass


          if (parent != NoSymbol) {
            classDescendentGraph.addEdge(parent, tpesym)
          } else {
            classDescendentGraph.addSingleNode(tpesym)
          }

          tpesym.tpe.members.foreach{ sym =>
            if (sym.rawInfo.isComplete) {
              recurseSym(sym)
            }
          }
        } else if (!sym.isMethod && !sym.isValue) {
          reporter.warn("Ingored "+sym)
        }
      }

      def recurseAST(t: Tree): Unit = t match {
        case PackageDef(_, stats) =>
          stats foreach (recurseAST _)
        case cd : ClassDef =>
          val tpesym = cd.symbol.tpe.typeSymbol

          val parent = tpesym.superClass

          if (parent != NoSymbol) {
            classDescendentGraph.addEdge(parent, tpesym)
          } else {
            classDescendentGraph.addSingleNode(tpesym)
          }
      }
      // First, we traverse the symbols, for previously compiled symbols
      recurseSym(definitions.RootClass)

      // Then, we complete the graph by traversing the AST
      for (unit <- currentRun.units) {
        recurseAST(unit.body)
      }

      if (settings.dumpClassDescendents) {
        val path = "classgraph.dot";
        reporter.info("Dumping Class Graph to "+path)
        new DotConverter(classDescendentGraph, "Class Graph").toFile(path)
      }
    }
  }

  case class CDVertex(val symbol: Symbol) extends VertexAbs[CDEdge] {
    val name = symbol.name.toString
    var children = Set[CDVertex]()
  }

  case class CDEdge(val v1: CDVertex, val v2: CDVertex) extends EdgeAbs[CDVertex]

  class ClassDescendentGraph extends DirectedGraphImp[CDVertex, CDEdge] {
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
  def debugSymbol(sym: Symbol) = {
    println("Symbol: "+sym+" (ID: "+sym.id+")") 
    val isComplete = sym.rawInfo.isComplete
    println("  owner:         "+sym.owner+" (ID: "+sym.owner.id+")")
    println("  ct. in o.:     "+(sym.owner.tpe.members contains sym))
    println("  ct. in o. t:   "+(sym.owner.tpe.typeSymbol.tpe.members contains sym))
    println("  isComplete:    "+isComplete)
    if (isComplete) {
      println("  isClass:       "+sym.isClass)
      println("  isModule:      "+sym.isModule)
      println("  isTrait:       "+sym.isTrait)
      println("  isfinal:       "+sym.isFinal)
      println("  isPackage:     "+sym.isPackage)

      val tpesym = if (sym.isType) sym else sym.tpe.typeSymbol
      println("  isType:        "+sym.isType)
      println("  sym==type:     "+(sym == tpesym))
      println("  Type:          "+tpesym)
      println("  TypeAncestors: "+tpesym.ancestors.mkString(", "))
      println("  Superclass:    "+tpesym.superClass)

      //println("  TypeMembers:   "+tpesym.tpe.members.mkString(", "))
      //println("  SymMembers:    "+sym.tpe.members.mkString(", "))
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
        } else if (tpesym.isSealed) {
          val exaust = tpesym.sealedDescendants.forall(_.isSealed)
          ObjectSet(tpesym.sealedDescendants.toSet + tpesym, exaust)
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
