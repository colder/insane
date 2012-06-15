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

  def dumpFA[S <% String](atm: Automaton[S], dest: String) {
    reporter.debug("Dumping FA to "+dest+"...")

    new AutomatonDotConverter(atm, "Effect Automaton", "") {
      override def transitionLabel(t: Transition[S]): String = t.label.map(s => s : String).getOrElse("\u03B5")
    }.writeFile(dest)
  }

  class NFAEffectRepresentation(env: PTEnv) {

    def getNFA: Automaton[Symbol] = {

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

      var nToS = Map[AnyRef, State]()

      var entryTransitions = Set[Transition[Symbol]]()

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
          case LVNode(CFGTrees.SymRef(s, _, _), _) =>
            entryTransitions += Transition(entry, Label(s), state)
          case OBNode(s) =>
            entryTransitions += Transition(entry, Label(s), state)
          case _ =>
            ""
        }
      }

      var finals = Set[State]();

      val transitions = env.ptGraph.E.collect {
        case IEdge(v1, l, v2) =>
          finals += nToS(nodeToID(v2))
          Transition(nToS(nodeToID(v1)), Label(l.sym), nToS(nodeToID(v2)))
        case OEdge(v1, l, v2) =>
          Transition(nToS(nodeToID(v1)), Label(l.sym), nToS(nodeToID(v2)))
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

    def getRegex(): Regex[Symbol] = {
      val dfa = new NFAEffectRepresentation(env).getNFA.determinize.minimize
      RegexHelpers.nfaToRegex(dfa)
    }

    def getStringRegex(): Regex[String] = {
      getRegex().map(_.name.toString.split("\\$").toList.last.trim)
    }

  }
}
