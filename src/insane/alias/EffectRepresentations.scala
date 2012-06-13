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

  object EffectNFA {
    import utils.Automatons._

    final case class State(id: Int) extends StateAbs {
      val name = id.toString
    }

    final case class Transition(v1: State, label: Symbol, v2: State) extends TransitionAbs[Symbol, State]


    type Automaton = Automatons.Automaton[State, Transition, Symbol] 

    class DotConverter(atm: Automaton, title: String) extends AutomatonDotConverter(atm, title, "") {
      override def transitionLabel(t: Transition): String = t.label.name.toString.split('$').toList.last.trim
      override def stateLabel(s: State): String = s.id.toString
    }
  }

  def dumpNFA(env: EffectNFA.Automaton, dest: String) {
    reporter.debug("Dumping ENFA to "+dest+"...")
    new EffectNFA.DotConverter(env, "Effect Automaton").writeFile(dest)
  }

  object StateIDs {
    private var nextId = 0;
    def nextStateID() = {
      nextId += 1
      nextId
    }
  }

  class NFAEffectRepresentation(env: PTEnv) {

    def newState : EffectNFA.State = EffectNFA.State(StateIDs.nextStateID())

    def getNFA: EffectNFA.Automaton = {

      def nodeToID(n: Node): AnyRef = n match {
        // we collapse load nodes and INodes as much as possible
        case LNode(_, via, pPoint, _) =>
          (via, pPoint)
        case INode(pPoint, _, sym) =>
          (pPoint, sym)
        case _ =>
          n
      }

      val entry = newState

      var nToS = Map[AnyRef, EffectNFA.State]()

      var entryTransitions = Set[EffectNFA.Transition]()

      for (v <- env.ptGraph.V) {
        val id = nodeToID(v)
        val state = nToS.get(id) match {
          case Some(s) => s
          case None    =>
            val s = newState
            nToS += id -> s
            s
        }

        v match {
          case LVNode(CFGTrees.SymRef(s, _, _), _) =>
            entryTransitions += EffectNFA.Transition(entry, s, state)
          case OBNode(s) =>
            entryTransitions += EffectNFA.Transition(entry, s, state)
          case _ =>
            ""
        }
      }

      var finals = Set[EffectNFA.State]();

      val transitions = env.ptGraph.E.collect {
        case IEdge(v1, l, v2) =>
          finals += nToS(nodeToID(v2))
          EffectNFA.Transition(nToS(nodeToID(v1)), l.sym, nToS(nodeToID(v2)))
        case OEdge(v1, l, v2) =>
          EffectNFA.Transition(nToS(nodeToID(v1)), l.sym, nToS(nodeToID(v2)))
      }

      var res = new EffectNFA.Automaton(
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
      val nfaBuilder = new NFAEffectRepresentation(env)
      implicit val stateBuilder = nfaBuilder.newState _

      val nfa = nfaBuilder.getNFA
      convertNFAToRegex(nfa)
    }

    def getStringRegex(): Regex[String] = {
      getRegex().map(_.name.toString.split("\\$").toList.last.trim)
    }

  }
}
