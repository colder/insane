package insane
package utils

object Automatons {
  import Graphs._

  final case class State(id: Int) extends VertexAbs{
    val name = id.toString
  }

  private var nextStateID = 0;
  def newState() = {
    nextStateID += 1
    State(nextStateID)
  }

  case class Transition[L](v1: State, label: L, v2: State) extends LabeledEdgeAbs[L, State]

  case class Automaton[L](
    val entry: State,
    val finals: Set[State],
    val graph: LabeledImmutableDirectedGraphImp[L, State, Transition[L]]
  ) {
    
    def this(states: Iterable[State], transitions: Iterable[Transition[L]], entry: State, finals: Iterable[State]) = 
      this(entry, finals.toSet, new LabeledImmutableDirectedGraphImp[L, State, Transition[L]](states.toSet ++ finals + entry, transitions.toSet))

    def this(entry: State) = 
      this(entry, Set(), new LabeledImmutableDirectedGraphImp[L, State, Transition[L]]())

    lazy val transitions = graph.E
    lazy val states      = graph.V


    def removeTransitions(trs: Iterable[Transition[L]]): Automaton[L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g - e)
      copy(graph = newGraph)
    }

    def addTransitions(trs: Iterable[Transition[L]]): Automaton[L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g + e)
      copy(graph = newGraph)
    }

    def removeStates(sts: Iterable[State]): Automaton[L] = {
      assert(!sts.toSet.apply(entry), "Trying to remove entry state!")
      copy(finals = finals -- sts, graph = graph -- sts)
    }

    def removeDeadPaths: Automaton[L] = {
      var markedStates = Set[State](entry) ++ finals

      def visit(s: State, from: Set[State]): Unit = {
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

    case class StateSig(ins: Set[(State, L)], outs: Set[(State, L)])

    object StateSig {
      def fromState(s: State): StateSig = {
        StateSig(graph.ins(s).map(t => (t.v1, t.label)).toSet, 
                 graph.outs(s).map(t => (t.v2, t.label)).toSet)
      }
    }
    def collapseSimilarStates: Automaton[L] = {
      // Keep the head of each kind, remove the rest
      val toRemove = states.groupBy(StateSig.fromState _).values.flatMap(_.tail)

      removeStates(toRemove)
    }
  }

  class AutomatonDotConverter[L](atm: Automaton[L], _title: String, _prefix: String) extends DotConverter[State, Transition[L]](atm.graph, _title, _prefix) {
    import utils.DotHelpers


    def transitionOptions(t: Transition[L], opts: List[String]): List[String] = opts
    def transitionLabel(t: Transition[L]): String = t.label.toString

    override final def edgeToString(res: StringBuffer, t: Transition[L]) {
      res append DotHelpers.labeledArrow(vToS(t.v1), transitionLabel(t), vToS(t.v2), transitionOptions(t, Nil))
    }

    override final def vertexToString(res: StringBuffer, s: State) {
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
        
      res append DotHelpers.node(vToS(s), s.name, opts)
    }
  }

}
