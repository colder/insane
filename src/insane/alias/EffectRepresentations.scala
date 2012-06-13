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


  object RegexNFA {
    import utils.Automatons._

    type State = EffectNFA.State

    abstract class Regex {
      def toStringSpecial(outer: Regex) = {
        val addParen = (this, outer) match {
          case (_: RegVar, _)   => false
          case (_: RegField, _) => false
          case (_: RegOr, _) => true 
          case (_: RegCons, _: RegOr) => false 
          case (_: RegCons, _) => true
          case (_: RegCons, _) => true
          case _ => false
        }

        if (addParen) {
          "("+toString+")"
        } else {
          toString
        }
      }

      def combOr(that: Regex): Regex = (this, that) match {
        case (RegOr(o1), RegOr(o2)) =>
          RegOr(o1 ::: o2)
        case (RegOr(o1), r2) =>
          RegOr(o1 ::: List(r2))
        case (r1, RegOr(o2)) =>
          RegOr(r1 :: o2)
        case (r1, r2) => 
          RegOr(r1 :: r2 :: Nil)
      }

      def combCons(that: Regex): Regex = (this, that) match {
        case (RegEps, r2) =>
          r2
        case (r1, RegEps) =>
          r1
        case (RegCons(c1), RegCons(c2)) =>
          RegCons(c1 ::: c2)
        case (RegCons(c1), r2) =>
          RegCons(c1 ::: List(r2))
        case (r1, RegCons(c2)) =>
          RegCons(r1 :: c2)
        case (r1, r2) => 
          RegCons(r1 :: r2 :: Nil)
      }

    }

    case object RegEps extends Regex {
      override def toString = "\u03B5"
    }

    case class RegVar(sym: Symbol) extends Regex {
      override def toString = sym.name.toString.split('$').toList.last.trim
    }

    case class RegField(sym: Symbol) extends Regex {
      override def toString = "."+sym.name.toString.split('$').toList.last.trim
    }

    case class RegOr(alternatives: List[Regex]) extends Regex {
      override def toString = alternatives.map(_.toStringSpecial(this)).mkString(" | ")
    }

    case class RegCons(chain: List[Regex]) extends Regex {
      override def toString = chain.map(_.toStringSpecial(this)).mkString("")
    }

    case class RegAst(r: Regex) extends Regex {
      override def toString = r.toStringSpecial(this)+"*"
    }

    object RegAst {
      def around(r: Regex): Regex = r match {
        case ast : RegAst =>
          ast
        case _ =>
          RegAst(r)
      } 
    }

    final case class Transition(v1: State, label: Regex, v2: State) extends TransitionAbs[Regex, State]

    type Automaton = Automatons.Automaton[State, Transition, Regex] 

    class DotConverter(atm: Automaton, title: String) extends AutomatonDotConverter(atm, title, "") {
      override def transitionLabel(t: Transition): String = t.label.toString
      override def stateLabel(s: State): String = if (s.name != "") s.name else s.id.toString
    }
  }

  class RegexEffectRepresentation(env: PTEnv) {

    def newState : RegexNFA.State = EffectNFA.State(StateIDs.nextStateID())

    def ripState(rnfa: RegexNFA.Automaton, s: RegexNFA.State): RegexNFA.Automaton = {
      val selfRegs = rnfa.graph.ins(s).filter(_.v1 == s).  // Only self loops
                      map(t => RegexNFA.RegAst.around(t.label)) // Construct self loop regexes

      val selfReg = if (selfRegs.isEmpty) {
        RegexNFA.RegEps
      } else {
        selfRegs.reduce(_ combOr _)
      }

      var newTransitions = Set[RegexNFA.Transition]()

      for (in <- rnfa.graph.ins(s); out <- rnfa.graph.outs(s) if in.v1 != s && out.v2 != s) {
        val v1 = in.v1
        val v2 = out.v2

        val reg = in.label combCons selfReg combCons out.label

        newTransitions += RegexNFA.Transition(v1, reg, v2)
      }

      rnfa.removeStates(Set(s)).addTransitions(newTransitions)
    }
    def getRegex(): RegexNFA.Regex = {
      val nfa = new NFAEffectRepresentation(env).getNFA

      // Step 1, convert ENFA into RNFA
      val finalState = newState

      val transitions = nfa.transitions.map{t =>
                          new RegexNFA.Transition(t.v1, 
                                if (t.v1 == nfa.entry)
                                  RegexNFA.RegVar(t.label)
                                else
                                  RegexNFA.RegField(t.label)
                              , t.v2)} ++
                        nfa.finals.map(s => new RegexNFA.Transition(s, RegexNFA.RegEps, finalState))

      var rnfa = new RegexNFA.Automaton(nfa.states+finalState, transitions, nfa.entry, Set(finalState))


      val removableStates = (rnfa.states -- rnfa.finals) - rnfa.entry

      for (s <- removableStates) {
        rnfa = ripState(rnfa, s)
      }

      if (rnfa.transitions.isEmpty) {
        RegexNFA.RegEps
      } else {
        rnfa.transitions.map(_.label).reduce(_ combOr _)
      }
    }

  }
}
