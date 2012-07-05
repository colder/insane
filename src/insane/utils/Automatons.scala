package insane
package utils

object Automatons {
  import Graphs._

  final case class State[T](v: T) extends VertexAbs{
    val name = v.toString

    def map[B](f: T => B) = State(f(v))
  }

  private var _nextStateID = 0;

  def nextStateID = {
    _nextStateID += 1
    _nextStateID
  }

  implicit def intStateBuilder() = State(nextStateID)

  //def newState[S](v: S) = {
  //  nextStateID += 1
  //  State(nextStateID, v)
  //}

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

  case class Transition[L, S](v1: State[S], label: OptLabel[L], v2: State[S]) extends LabeledEdgeAbs[OptLabel[L], State[S]]

  case class Automaton[L, S](
    val entry: State[S],
    val finals: Set[State[S]],
    val graph: LabeledImmutableDirectedGraphImp[OptLabel[L], State[S], Transition[L, S]]
  ) {
    
    def this(states: Iterable[State[S]], transitions: Iterable[Transition[L, S]], entry: State[S], finals: Iterable[State[S]]) = 
      this(entry, finals.toSet, new LabeledImmutableDirectedGraphImp[OptLabel[L], State[S], Transition[L, S]](states.toSet ++ finals + entry, transitions.toSet))

    def this(entry: State[S]) = 
      this(entry, Set[State[S]](), new LabeledImmutableDirectedGraphImp[OptLabel[L], State[S], Transition[L, S]]())

    lazy val transitions = graph.E
    lazy val states      = graph.V
    lazy val alphabet    = graph.E.map(_.label).toSet

    lazy val isDeterministic = transitions.groupBy(t => (t.v1, t.label)).forall{ case (k, ts) => (ts.size == 1) && !k._2.isEpsylon }

    def removeTransitions(trs: Iterable[Transition[L, S]]): Automaton[L, S] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g - e)
      copy(graph = newGraph)
    }

    def addTransitions(trs: Iterable[Transition[L, S]]): Automaton[L, S] = {
      var newGraph = trs.foldLeft(graph)((g, e) => g + e)
      copy(graph = newGraph)
    }

    def isImpossible = finals.isEmpty

    def removeStates(sts: Iterable[State[S]]): Automaton[L, S] = {
      assert(!sts.toSet.apply(entry),        "Trying to remove entry state!")
      copy(finals = finals -- sts, graph = graph -- sts)
    }

    def removeDeadPaths: Automaton[L, S] = {
      var markedStates = Set[State[S]](entry)
      var visited      = Set[State[S]]()

      def visit(s: State[S], from: Set[State[S]]): Unit = {
        for (in <- graph.ins(s)) {
          if (markedStates(in.v1)) {
            markedStates ++= from + s 
          } else if (!from(in.v1)) {
            if (!visited(in.v1)) {
              visit(in.v1, from + s)
            }
          }
        }

        visited += s;
      }
      
      finals.foreach(visit(_, Set()))
      
      removeStates(states -- markedStates)
    }

    def mapTransitions[A](fTransLabel: L => A): Automaton[A, S] = {
      map(fTransLabel, identity)
    }

    def mapStates[B](fStateLabel: S => B): Automaton[L, B] = {
      map(identity, fStateLabel)
    }

    def map[A, B](fTransLabel: L => A, fStateLabel: S => B): Automaton[A, B] = {
      def mapState(s: State[S]) = State(fStateLabel(s.v))

      val newTransitions = transitions.map(t => Transition(mapState(t.v1), t.label.map(fTransLabel), mapState(t.v2)))
      val newStates      = states.map(mapState)
      val newEntry       = mapState(entry)
      val newFinals      = finals.map(mapState)

      new Automaton(newStates, newTransitions, newEntry, newFinals)
    }

    def minimize(implicit newState: () => State[S]): Automaton[L, S] = {
      var partitions = Set[Set[State[S]]](finals, states -- finals)
      var todo       = Set[Set[State[S]]](finals)

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

      var sToPart        = Map[State[S], State[S]]()
      var newStates      = Set[State[S]]()
      var newTransitions = Set[Transition[L, S]]()

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

    def -(that: Automaton[L, S])(implicit newState: () => State[S]): Automaton[L, S] = {
      val alph = this.alphabet ++ that.alphabet

      (this intersection that.complement(alph))
    }

    def complement(over: Set[OptLabel[L]])(implicit newState: () => State[S]): Automaton[L, S] = {
      val cdfa = this.complete(over)

      cdfa.copy(finals = cdfa.states -- cdfa.finals)
    }

    def determinize(implicit newState: () => State[S]): Automaton[L, S] = {
      def f(ss: Set[State[S]]): Set[State[S]] = ss.flatMap(s => Set(s) ++ graph.outs(s).collect{ case Transition(_, Epsylon, to) => to})

      val alph = transitions.map(_.label).toSet


      var statesCache = Map[Set[State[S]], State[S]]()
      def realState(ss: Set[State[S]]): State[S] = statesCache.getOrElse(ss, {
        val s = newState()
        statesCache += ss -> s
        s
      })

      var dStates      = Set[State[S]]()
      var entry        = f(Set(this.entry))
      var dEntry       = realState(entry)
      var dFinals      = Set[State[S]]()
      var dTransitions = Set[Transition[L, S]]()
      var todo         = Set[Set[State[S]]](entry)
      var done         = Set[Set[State[S]]]()

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

      new Automaton[L, S](dStates, dTransitions, dEntry, dFinals)
    }

    def complete(over: Set[OptLabel[L]])(implicit newState: () => State[S]): Automaton[L, S] = {
      val errorState = newState()
      var newStates = states + errorState
      var newTransitions = transitions

      for (s <- newStates) {
        val existingTrans = graph.outs(s).map(_.label).toSet

        for (a <- over -- existingTrans) {
          newTransitions += Transition(s, a, errorState)
        }
      }

      new Automaton[L, S](newStates, newTransitions, entry, finals)
    }

    def constructSync(that: Automaton[L, S])(implicit newState: () => State[S]): (Map[(State[S], State[S]), State[S]], Automaton[L, S]) = {

      var newTransitions = Set[Transition[L, S]]()
      val errorState1    = newState()
      val errorState2    = newState()
      var newStates      = Map[(State[S], State[S]), State[S]]()

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

      (newStates, new Automaton[L, S](newStates.values, newTransitions, newStates((this.entry, that.entry)), Set()))
    }

    def union(that: Automaton[L, S])(implicit newState: () => State[S]): Automaton[L, S] = {
      val (statesMap, atm) = constructSync(that)

      val finals = statesMap.collect{ case ((s1, s2), s) if this.finals(s1) || that.finals(s2) => s}
      atm.copy(finals = finals.toSet).removeDeadPaths
    }

    def intersection(that: Automaton[L, S])(implicit newState: () => State[S]): Automaton[L, S] = {
      val (statesMap, atm) = constructSync(that)

      val finals = statesMap.collect{ case ((s1, s2), s) if this.finals(s1) && that.finals(s2) => s}
      atm.copy(finals = finals.toSet).removeDeadPaths
    }

    case class StateSig(ins: Set[(State[S], OptLabel[L])], outs: Set[(State[S], OptLabel[L])])

    object StateSig {
      def fromState(s: State[S]): StateSig = {
        StateSig(graph.ins(s).map(t => (t.v1, t.label)).toSet, 
                 graph.outs(s).map(t => (t.v2, t.label)).toSet)
      }
    }

    def collapseSimilarStates: Automaton[L, S] = {
      // Keep the head of each kind, remove the rest
      val toRemove = states.groupBy(StateSig.fromState _).values.flatMap(_.tail)

      removeStates(toRemove)
    }
  }

  class AutomatonDotConverter[L, S](atm: Automaton[L, S], _title: String, _prefix: String) extends DotConverter[State[S], Transition[L, S]](atm.graph, _title, _prefix) {
    import utils.DotHelpers


    def transitionOptions(t: Transition[L, S], opts: List[String]): List[String] = opts
    def transitionLabel(t: Transition[L, S]): String = t.label.toString
    def stateLabel(s: State[S]): String = s.name

    override final def edgeToString(res: StringBuffer, t: Transition[L, S]) {
      res append DotHelpers.labeledArrow(vToS(t.v1), transitionLabel(t), vToS(t.v2), transitionOptions(t, Nil))
    }

    override final def vertexToString(res: StringBuffer, s: State[S]) {
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
        
      res append DotHelpers.node(vToS(s), stateLabel(s), opts)
    }
  }

}
