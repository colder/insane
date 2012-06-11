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

    final case class State(id: Int, name: String) extends StateAbs

    abstract class Effect {
      val field: Symbol
    }

    final case class Read(field: Symbol)  extends Effect;
    final case class Write(field: Symbol) extends Effect;

    final case class Transition(v1: State, label: Effect, v2: State) extends TransitionAbs[Effect, State]


    type Automaton = Automatons.Automaton[State, Transition, Effect] 

    class DotConverter(atm: Automaton, title: String) extends AutomatonDotConverter(atm, title, "") {
      override def transitionOptions(t: Transition, opts: List[String]): List[String] = t.label match {
        case Read(f) =>
          "style=dashed" :: opts
        case Write(f) =>
          opts
      }
      override def transitionLabel(t: Transition): String = t.label.field.name.toString.split('$').toList.last
      override def stateLabel(s: State): String = if (s.name != "") s.name else s.id.toString
    }
  }

  def dumpNFA(env: EffectNFA.Automaton, dest: String) {
    reporter.debug("Dumping ENFA to "+dest+"...")
    new EffectNFA.DotConverter(env, "Effect Automaton").writeFile(dest)
  }

  class NFAEffectRepresentation(env: PTEnv) {
    object StateIDs {
      private var nextId = 0;
      def nextStateID() = {
        nextId += 1
        nextId
      }
    }
    def getNFA: EffectNFA.Automaton = {
      import StateIDs.nextStateID

      var r = new EffectNFA.Automaton()

      var nToS = Map[Node, EffectNFA.State]()

      for (v <- env.ptGraph.V) {
        val name = v match {
          case LVNode(r, _) =>
            r.toString
          case OBNode(s) =>
            s.toString
          case _ =>
            ""
        }
        nToS += v -> EffectNFA.State(nextStateID(), name)
      }

      val transitions = env.ptGraph.E.collect {
        case IEdge(v1, l, v2) =>
          EffectNFA.Transition(nToS(v1), EffectNFA.Write(l.sym), nToS(v2))
        case OEdge(v1, l, v2) =>
          EffectNFA.Transition(nToS(v1), EffectNFA.Read(l.sym), nToS(v2))
      }

      new EffectNFA.Automaton(
        nToS.values,
        transitions,
        (env.locState.flatMap(_._2) ++ env.ptGraph.V.filter(_.isGloballyReachable)) map nToS,
        Set()
      )
    }
  }
}
