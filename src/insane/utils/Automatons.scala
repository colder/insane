package insane
package utils

object Automatons {
  import Graphs._

  abstract class StateAbs extends VertexAbs {
    val name: String
  }

  case class Transition[S <: StateAbs, L](v1: S, label: L, v2: S) extends LabeledEdgeAbs[L, S]

  case class Automaton[S <: StateAbs, L](
    val entry: S,
    val finals: Set[S],
    val graph: LabeledImmutableDirectedGraphImp[L, S, Transition[S, L]]
  ) {
    
    def this(states: Iterable[S], transitions: Iterable[Transition[S, L]], entry: S, finals: Iterable[S]) = 
      this(entry, finals.toSet, new LabeledImmutableDirectedGraphImp[L, S, Transition[S, L]](states.toSet ++ finals + entry, transitions.toSet))

    def this(entry: S) = 
      this(entry, Set(), new LabeledImmutableDirectedGraphImp[L, S, Transition[S, L]]())

    lazy val transitions = graph.E
    lazy val states      = graph.V


    def removeTransitions(trs: Iterable[Transition[S, L]]): Automaton[S, L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g - e)
      copy(graph = newGraph)
    }

    def addTransitions(trs: Iterable[Transition[S, L]]): Automaton[S, L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g + e)
      copy(graph = newGraph)
    }

    def removeStates(sts: Iterable[S]): Automaton[S, L] = {
      assert(!sts.toSet.apply(entry), "Trying to remove entry state!")
      copy(finals = finals -- sts, graph = graph -- sts)
    }

    def removeDeadPaths: Automaton[S, L] = {
      var markedStates = Set[S](entry) ++ finals

      def visit(s: S, from: Set[S]): Unit = {
        for (in <- graph.ins(s)) {
          if (markedStates(in.v1)) {
            markedStates ++= from + s 
          } else if (!from(in.v1)) {
            visit(in.v1, from + s)
          }
        }
      }
      
      finals.foreach(visit(_, Set()))
      
      removeStates(states -- markedStates)
    }

    case class StateSig(ins: Set[(S, L)], outs: Set[(S, L)])

    object StateSig {
      def fromState(s: S): StateSig = {
        StateSig(graph.ins(s).map(t => (t.v1, t.label)).toSet, 
                 graph.outs(s).map(t => (t.v2, t.label)).toSet)
      }
    }
    def collapseSimilarStates: Automaton[S, L] = {
      // Keep the head of each kind, remove the rest
      val toRemove = states.groupBy(StateSig.fromState _).values.flatMap(_.tail)

      removeStates(toRemove)
    }
  }

  class AutomatonDotConverter[S <: StateAbs, L](atm: Automaton[S, L], _title: String, _prefix: String) extends DotConverter[S, Transition[S, L]](atm.graph, _title, _prefix) {
    import utils.DotHelpers


    def transitionOptions(t: Transition[S, L], opts: List[String]): List[String] = opts
    def transitionLabel(t: Transition[S, L]): String = t.label.toString

    def stateToString(res: StringBuffer, s: S) {
      var opts = List("fontsize=10")

      if (atm.entry == s) {
        opts = "shape=diamond" :: opts
      } else {
        if (atm.finals(s)) {
          opts = "shape=doublecircle" :: opts
        } else {
          opts = "shape=circle" :: opts
        }
      }

      opts = stateOptions(s, opts)
        
      res append DotHelpers.node(vToS(s), stateLabel(s), opts)

    }
    def stateLabel(s: S): String = s.name
    def stateOptions(s: S, opts: List[String]): List[String] = opts

    override final def edgeToString(res: StringBuffer, t: Transition[S, L]) {
      res append DotHelpers.labeledArrow(vToS(t.v1), transitionLabel(t), vToS(t.v2), transitionOptions(t, Nil))
    }

    override final def vertexToString(res: StringBuffer, v: S) {
      stateToString(res, v)
    }
  }

}
