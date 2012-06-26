package insane
package alias

//import storage.Database

import utils._
import utils.Reporters._
import GlobalCounters.withDebugCounter
import utils.Graphs.DotConverter
import CFG._

import scala.tools.nsc.symtab.Flags

trait EffectRepresentations extends PointToGraphsDefs with PointToEnvs {
  self: AnalysisComponent =>

  import global._
  import PointToGraphs._
  import Automatons._


  class SimpleEffectRepresentation(env: PTEnv) {
      
    def effects: List[String] = {
      var res: List[String] = Nil 

      val ptGraph = env.ptGraph
      val sources = (env.locState.flatMap(_._2) ++ ptGraph.V.filter(v => v.isInstanceOf[GloballyReachableNode])).toSet

      def formatField(f: Field): String = {
        f.sym.name.toString.split("\\$").toList.last.trim
      }

      def formatSource(n: Node): String = {
        n.toString
      }

      for (ie <- env.iEdges) {
        var isLooping = false;
        var found     = false;
        var path      = List[String](formatField(ie.label));


        def traverseNodeBackward(n: Node, visited : Set[Node]): Unit = {
          for (e <- ptGraph.inEdges(n)) {
            if (visited(e.v1)) {
              isLooping = true
            } else if (!found) {
              path = formatField(e.label) :: path

              if (sources(e.v1)) {
                path = formatSource(e.v1) :: path
                found = true;
              } else {
                traverseNodeBackward(e.v1, visited + e.v1)
              }
            } else {
              traverseNodeBackward(e.v1, visited + e.v1)
            }
          }
        }

        traverseNodeBackward(ie.v1, Set(ie.v1))

        if (found) {
          res = path.mkString(" --> ")+(if(isLooping) " '" else "") :: res
        }
      }

      res
    }
  }

  import language.implicitConversions

  implicit def simpleSymbolStr(s: Symbol): String = s.name.toString.split("\\$").toList.last.trim
  implicit def simpleWrapSymbolStr(s: AbsWrappedSymbol): String = s.toSimpleString
  implicit def simpleIntStr(s: Int): String = s.toString

  def dumpFA[L <% String, S <% String](atm: Automaton[L, S], dest: String) {
    reporter.debug("Dumping FA to "+dest+"...")

    new AutomatonDotConverter(atm, "Effect Automaton", "") {
      override def transitionLabel(t: Transition[L, S]): String = t.label.map(s => s : String).getOrElse("\u03B5")
      override def stateLabel(t: State[S]): String = (t.v : String)
    }.writeFile(dest)
  }

  abstract class AbsWrappedSymbol {
    def symbol: Symbol
    def toSimpleString: String
  }
  case class WrappedSymbol(s: Symbol) extends AbsWrappedSymbol {
    def symbol = s
    def toSimpleString = simpleSymbolStr(s)
  }
  case class WrappedField(r: CFG.SymbolRef) extends AbsWrappedSymbol {
    def toSimpleString = r match {
      case t: CFG.ThisRef =>
        "this"
      case s: CFG.SymbolRef =>
        simpleSymbolStr(s.symbol)
    }

    def symbol = r.symbol
  }

  class NFAEffectRepresentation(env: PTEnv) {

    type S = Int

    def newState() = State[S](Automatons.nextStateID)

    def getNFA: Automaton[AbsWrappedSymbol, Int] = { // TO FIX

      def nodeToID(n: Node): AnyRef = n match {
        // we collapse load nodes and INodes as much as possible
        case LNode(_, via, pPoint, _) =>
          (via, pPoint)
        case INode(pPoint, _, sym) =>
          (pPoint, sym)
        case _ =>
          n
      }

      val entry = newState()

      var nToS = Map[AnyRef, State[S]]()

      var entryTransitions = Set[Transition[AbsWrappedSymbol, S]]()

      for (v <- env.ptGraph.V) {
        val id = nodeToID(v)
        val state = nToS.get(id) match {
          case Some(s) => s
          case None    =>
            val s = newState()
            nToS += id -> s
            s
        }

        v match {
          case LVNode(sr: CFGTrees.SymbolRef, _) =>
            entryTransitions += Transition(entry, Label(WrappedField(sr)), state)
          case OBNode(s) =>
            entryTransitions += Transition(entry, Label(WrappedSymbol(s)), state)
          case _ =>
            ""
        }
      }

      var finals = Set[State[S]]();

      val transitions = env.ptGraph.E.collect {
        case IEdge(v1, l, v2) =>
          finals += nToS(nodeToID(v2))
          Transition(nToS(nodeToID(v1)), Label(WrappedSymbol(l.sym): AbsWrappedSymbol), nToS(nodeToID(v2)))
        case OEdge(v1, l, v2) =>
          Transition(nToS(nodeToID(v1)), Label(WrappedSymbol(l.sym): AbsWrappedSymbol), nToS(nodeToID(v2)))
      }

      var res = new Automaton(
        nToS.values,
        transitions ++ entryTransitions,
        entry,
        finals
      )

      res = res.removeDeadPaths

      res = res.collapseSimilarStates

      res
    }
  }


  class RegexEffectRepresentation(env: PTEnv) {
    import utils.RegularExpressions._

    def getRegex(): Regex[AbsWrappedSymbol] = {
      val nfa = new NFAEffectRepresentation(env).getNFA
      val dfa = nfa.determinize
      val mdfa = dfa.minimize
      RegexHelpers.nfaToRegex(mdfa)
    }

    def getStringRegex(): Regex[String] = {
      getRegex().map(_.toSimpleString)
    }

  }
}
