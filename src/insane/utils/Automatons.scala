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

  abstract class OptLabel[+A] {
    def isEpsylon: Boolean

    def get : A

    final def getOrElse[B >: A](default: => B): B =
      if (isEpsylon) default else this.get

    final def map[B](f: A => B): OptLabel[B] = 
      if (isEpsylon) Epsylon else Label(f(this.get))
  }

  final case class Label[+A](l: A) extends OptLabel[A] {
    def isEpsylon = false

    def get = l
  }

  case object Epsylon extends OptLabel[Nothing] {
    def isEpsylon = true

    def get = throw new NoSuchElementException("Epsylon.get")
  }

  case class Transition[L](v1: State, label: OptLabel[L], v2: State) extends LabeledEdgeAbs[OptLabel[L], State]

  case class Automaton[L](
    val entry: State,
    val finals: Set[State],
    val graph: LabeledImmutableDirectedGraphImp[OptLabel[L], State, Transition[L]]
  ) {
    
    def this(states: Iterable[State], transitions: Iterable[Transition[L]], entry: State, finals: Iterable[State]) = 
      this(entry, finals.toSet, new LabeledImmutableDirectedGraphImp[OptLabel[L], State, Transition[L]](states.toSet ++ finals + entry, transitions.toSet))

    def this(entry: State) = 
      this(entry, Set[State](), new LabeledImmutableDirectedGraphImp[OptLabel[L], State, Transition[L]]())

    lazy val transitions = graph.E
    lazy val states      = graph.V
    lazy val alphabet    = graph.E.map(_.label).toSet

    lazy val isDeterministic = transitions.groupBy(t => (t.v1, t.label)).forall{ case (k, ts) => (ts.size == 1) && !k._2.isEpsylon }

    def removeTransitions(trs: Iterable[Transition[L]]): Automaton[L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g - e)
      copy(graph = newGraph)
    }

    def addTransitions(trs: Iterable[Transition[L]]): Automaton[L] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g + e)
      copy(graph = newGraph)
    }

    def isImpossible = finals.isEmpty

    def removeStates(sts: Iterable[State]): Automaton[L] = {
      assert(!sts.toSet.apply(entry),        "Trying to remove entry state!")
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

    def minimize: Automaton[L] = {
      var partitions = Set[Set[State]](finals, states -- finals)
      var todo       = Set[Set[State]](finals)

      while(!todo.isEmpty) {
        val ss = todo.head
        todo = todo.tail

        for ((a, ts) <- ss.flatMap(s => graph.ins(s)).groupBy(_.label)) {
          val inStates = ts.map(_.v1).toSet

          for (part <- partitions if !(inStates & part).isEmpty) {
            val part1 = part & inStates
            val part2 = part -- inStates
            partitions = (partitions - part) + part1 + part2
            if (todo contains part) {
              todo = (todo - part) + part1 + part2
            } else {
              if (part1.size < part2.size) {
                todo += part1
              } else {
                todo += part2
              }
            }
          }
        }
      }

      var sToPart        = Map[State, State]()
      var newStates      = Set[State]()
      var newTransitions = Set[Transition[L]]()

      for (part <- partitions) {
        val s = newState()
        newStates += s

        for (p <- part) {
          sToPart += p -> s
        }
      }

      for (t <- transitions) {
        newTransitions += Transition(sToPart(t.v1), t.label, sToPart(t.v2))
      }

      new Automaton(newStates, newTransitions, sToPart(this.entry), this.finals.map(sToPart(_)).toSet)
    }

    def -(that: Automaton[L]): Automaton[L] = {
      val alph = this.alphabet ++ that.alphabet

      (this intersection that.complement(alph))
    }

    def complement(over: Set[OptLabel[L]]): Automaton[L] = {
      val cdfa = this.complete(over)

      cdfa.copy(finals = cdfa.states -- cdfa.finals)
    }

    def determinize: Automaton[L] = {
      def f(ss: Set[State]): Set[State] = ss.flatMap(s => Set(s) ++ graph.outs(s).collect{ case Transition(_, Epsylon, to) => to})

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

        for ((a, tos) <- ds.flatMap(graph.outs(_).filter(!_.label.isEpsylon)).groupBy(t => t.label).mapValues(_.map(_.v2))) {
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

    def complete(over: Set[OptLabel[L]]): Automaton[L] = {
      val errorState = newState()
      var newStates = states + errorState
      var newTransitions = transitions

      for (s <- newStates) {
        val existingTrans = graph.outs(s).map(_.label).toSet

        for (a <- over -- existingTrans) {
          newTransitions += Transition(s, a, errorState)
        }
      }

      new Automaton[L](newStates, newTransitions, entry, finals)
    }

    def constructSync(that: Automaton[L]): (Map[(State, State), State], Automaton[L]) = {

      var newTransitions = Set[Transition[L]]()
      val errorState1    = newState
      val errorState2    = newState
      var newStates      = Map[(State, State), State]()

      for (s1 <- this.states+errorState1; s2 <- that.states+errorState2) {
        val s = newState()
        newStates += (s1, s2) -> s
      }

      for (s1 <- this.states+errorState1; s2 <- that.states+errorState2) {
        val outs1 = this.graph.outs(s1).groupBy(_.label)
        val outs2 = that.graph.outs(s2).groupBy(_.label)

        val s = newStates(s1, s2)

        for (a <- outs1.keySet ++ outs2.keySet) {
          val v21 = outs1.get(a).map(_.head.v2).getOrElse(errorState1)
          val v22 = outs2.get(a).map(_.head.v2).getOrElse(errorState2)

          newTransitions += Transition(newStates((s1, s2)), a, newStates((v21, v22))) 
        }
      }

      (newStates, new Automaton[L](newStates.values, newTransitions, newStates((this.entry, that.entry)), Set()))
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

    case class StateSig(ins: Set[(State, OptLabel[L])], outs: Set[(State, OptLabel[L])])

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
