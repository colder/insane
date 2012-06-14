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

  case class Transition[L](v1: State, label: Option[L], v2: State) extends LabeledEdgeAbs[Option[L], State]

  case class Automaton[L](
    val entry: State,
    val finals: Set[State],
    val graph: LabeledImmutableDirectedGraphImp[Option[L], State, Transition[L]]
  ) {
    
    def this(states: Iterable[State], transitions: Iterable[Transition[L]], entry: State, finals: Iterable[State]) = 
      this(entry, finals.toSet, new LabeledImmutableDirectedGraphImp[Option[L], State, Transition[L]](states.toSet ++ finals + entry, transitions.toSet))

    def this(entry: State) = 
      this(entry, Set[State](), new LabeledImmutableDirectedGraphImp[Option[L], State, Transition[L]]())

    lazy val transitions = graph.E
    lazy val states      = graph.V

    lazy val isDeterministic = transitions.groupBy(t => (t.v1, t.label)).exists(_._2.size > 1)

    def removeTransitions(trs: Iterable[Transition[L]]): Automaton[L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g - e)
      copy(graph = newGraph)
    }

    def addTransitions(trs: Iterable[Transition[L]]): Automaton[L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g + e)
      copy(graph = newGraph)
    }

    def removeStates(sts: Iterable[State]): Automaton[L] = {
      assert(!sts.toSet.apply(entry),        "Trying to remove entry state!")
      assert(!(finals -- sts.toSet).isEmpty, "Removing every final states!")
      copy(finals = finals -- sts, graph = graph -- sts)
    }

    def removeDeadPaths: Automaton[L] = {
      var markedStates = Set[State](entry)

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

    def negation: Automaton[L] = {
      val dfa = if (isDeterministic) {
        this
      } else {
        this.determinize
      }

      dfa.copy(finals = dfa.states -- dfa.finals)
    }

    def determinize: Automaton[L] = {
      def f(ss: Set[State]): Set[State] = ss.flatMap(s => Set(s) ++ graph.outs(s).collect{ case Transition(_, None, to) => to})

      val alph = transitions.map(_.label).toSet


      var statesCache = Map[Set[State], State]()
      def realState(ss: Set[State]): State = statesCache.getOrElse(ss, {
        val s = newState
        statesCache += ss -> s
        s
      })

      var dStates      = Set[State]()
      var entry        = f(Set(this.entry))
      var dEntry       = realState(entry)
      var dFinals      = Set[State]()
      var dTransitions = Set[Transition[L]]()
      var todo         = Set[Set[State]](entry)
      var done         = Set[Set[State]]()

      while(!todo.isEmpty) {
        val ds = todo.head

        for ((a, tos) <- ds.flatMap(graph.outs(_).filter(!_.label.isEmpty)).groupBy(t => t.label).mapValues(_.map(_.v2))) {
          val newDs = f(tos)

          dStates       += realState(newDs)
          dTransitions  += Transition(realState(ds), a, realState(newDs))

          if (!done(newDs)) {
            todo        += newDs
          }
        }
        if (!(ds & this.finals).isEmpty) {
          dFinals += realState(ds)
        }

        todo -= ds
        done += ds
      }

      new Automaton[L](dStates, dTransitions, dEntry, dFinals)
    }

    def constructSync(that: Automaton[L]): (Map[(State, State), State], Automaton[L]) = {

      var newTransitions = Set[Transition[L]]()
      val errorState     = newState
      var newStates      = Map[(State, State), State]()

      for (s1 <- this.states; s2 <- that.states) {
        val s = newState()
        newStates += (s1, s2) -> s
      }

      for (s1 <- this.states; s2 <- that.states) {
        val outs1 = this.graph.outs(s1).groupBy(_.label)
        val outs2 = that.graph.outs(s2).groupBy(_.label)

        val s = newStates(s1, s2)

        for (a <- outs1.keySet ++ outs2.keySet) {
          val v21 = outs1.get(a).map(_.head.v2).getOrElse(errorState)
          val v22 = outs2.get(a).map(_.head.v2).getOrElse(errorState)

          newTransitions += Transition(newStates.getOrElse((s1, s2), errorState), a, newStates.getOrElse((v21, v22), errorState)) 
        }
      }

      (newStates, new Automaton[L](Set(errorState) ++ newStates.values, newTransitions, newStates((this.entry, that.entry)), Set()))
    }

    def union(that: Automaton[L]): Automaton[L] = {
      val (statesMap, atm) = constructSync(that)

      val finals = statesMap.collect{ case ((s1, s2), s) if this.finals(s1) || that.finals(s2) => s}
      atm.copy(finals = finals.toSet).removeDeadPaths
    }

    def intersection(that: Automaton[L]): Automaton[L] = {
      val (statesMap, atm) = constructSync(that)

      val finals = statesMap.collect{ case ((s1, s2), s) if this.finals(s1) && that.finals(s2) => s}
      atm.copy(finals = finals.toSet).removeDeadPaths
    }

    case class StateSig(ins: Set[(State, Option[L])], outs: Set[(State, Option[L])])

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
