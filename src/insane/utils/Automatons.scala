package insane
package utils

object Automatons {
  import Graphs._

  abstract class StateAbs extends VertexAbs {
    val name: String
  }

  abstract class TransitionAbs[L, S <: StateAbs] extends LabeledEdgeAbs[L, S]  {
    val v1: S
    val v2: S
  }

  case class Automaton[S <: StateAbs, T <: TransitionAbs[L, S], L](
    val entry: S,
    val finals: Set[S],
    val graph: LabeledImmutableDirectedGraphImp[L, S, T]
  ) {

    def this(states: Iterable[S], transitions: Iterable[T], entry: S, finals: Iterable[S]) = 
      this(entry, finals.toSet, new LabeledImmutableDirectedGraphImp[L, S, T](states.toSet ++ finals + entry, transitions.toSet))

    def this(entry: S) = 
      this(entry, Set(), new LabeledImmutableDirectedGraphImp[L, S, T]())

    lazy val transitions = graph.E
    lazy val states      = graph.V


    def removeTransitions(trs: Iterable[T]): Automaton[S, T, L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g - e)
      copy(graph = newGraph)
    }

    def addTransitions(trs: Iterable[T]): Automaton[S, T, L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g + e)
      copy(graph = newGraph)
    }

    def removeStates(sts: Iterable[S]): Automaton[S, T, L] = {
      assert(!sts.toSet.apply(entry), "Trying to remove entry state!")
      copy(finals = finals -- sts, graph = graph -- sts)
    }

    def removeDeadPaths: Automaton[S, T, L] = {
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
  }

  class AutomatonDotConverter[S <: StateAbs, T <: TransitionAbs[L, S], L](atm: Automaton[S, T, L], _title: String, _prefix: String) extends DotConverter[S, T](atm.graph, _title, _prefix) {
    import utils.DotHelpers


    def transitionToString(res: StringBuffer, t: T) {
      res append DotHelpers.labeledArrow(vToS(t.v1), transitionLabel(t), vToS(t.v2), transitionOptions(t, Nil))
    }
    def transitionOptions(t: T, opts: List[String]): List[String] = opts
    def transitionLabel(t: T): String = t.label.toString

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

    override final def edgeToString(res: StringBuffer, e: T) {
      transitionToString(res, e)
    }

    override final def vertexToString(res: StringBuffer, v: S) {
      stateToString(res, v)
    }
  }

}
