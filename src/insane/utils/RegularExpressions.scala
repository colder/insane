package insane
package utils

object RegularExpressions {
  import Automatons._

  abstract class Regex[T] {

    def map[B](f: T => B): Regex[B]

    def toStringSpecial(outer: Regex[T]) = {
      val addParen = (this, outer) match {
        case (_: RegLit[T], _)   => false
        case (_: RegOr[T], _) => true
        case (_: RegCons[T], _: RegOr[T]) => false
        case (_: RegCons[T], _) => true
        case (_: RegCons[T], _) => true
        case _ => false
      }

      if (addParen) {
        "("+toString+")"
      } else {
        toString
      }
    }

    def combOr(that: Regex[T]): Regex[T] = (this, that) match {
      case (RegOr(o1), RegOr(o2)) =>
        RegOr(o1 ::: o2)
      case (RegOr(o1), r2) =>
        RegOr(o1 ::: List(r2))
      case (r1, RegOr(o2)) =>
        RegOr(r1 :: o2)
      case (r1, r2) =>
        RegOr(r1 :: r2 :: Nil)
    }

    def combCons(that: Regex[T]): Regex[T] = (this, that) match {
      case (RegEps(), r2) =>
        r2
      case (r1, RegEps()) =>
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

  case class RegEps[T]() extends Regex[T] {
    override def toString = "\u03B5"
    def map[B](f: T => B): Regex[B] = RegEps()
  }

  case class RegLit[T](l: T) extends Regex[T] {
    override def toString = l.toString
    def map[B](f: T => B): Regex[B] = RegLit(f(l))
  }

  case class RegOr[T](alternatives: List[Regex[T]]) extends Regex[T] {
    override def toString = alternatives.map(_.toStringSpecial(this)).mkString(" | ")
    def map[B](f: T => B): Regex[B] = RegOr(alternatives.map(_.map(f)))
  }

  case class RegCons[T](chain: List[Regex[T]]) extends Regex[T] {
    override def toString = chain.map(_.toStringSpecial(this)).mkString(".")
    def map[B](f: T => B): Regex[B] = RegCons(chain.map(_.map(f)))
  }

  case class RegAst[T](r: Regex[T]) extends Regex[T] {
    override def toString = r.toStringSpecial(this)+"*"
    def map[B](f: T => B): Regex[B] = RegAst(r.map(f))
  }

  object RegAst {
    def around[T](r: Regex[T]): Regex[T] = r match {
      case ast : RegAst[T] =>
        ast
      case _ =>
        RegAst(r)
    }
  }


  object RegexHelpers {
    final case class RegexTransition[S <: StateAbs, W](v1: S, label: Regex[W], v2: S) extends TransitionAbs[Regex[W], S]

    type RegexAutomaton[S <: StateAbs, W] = Automatons.Automaton[S, RegexTransition[S, W], Regex[W]]

    def nfaToRegex[S <: StateAbs, T <: TransitionAbs[W, S], W](atm: Automatons.Automaton[S, T, W])(implicit stateBuilder: () => S): Regex[W] = {

      def ripState(rnfa: RegexAutomaton[S, W], s: S): RegexAutomaton[S, W] = {
        val selfRegs = rnfa.graph.ins(s).filter(_.v1 == s).  // Only self loops
                        map(t => RegAst.around(t.label)) // Construct self loop regexes

        val selfReg = if (selfRegs.isEmpty) {
          RegEps[W]()
        } else {
          selfRegs.reduce(_ combOr _)
        }

        var newTransitions = Set[RegexTransition[S, W]]()

        for (in <- rnfa.graph.ins(s); out <- rnfa.graph.outs(s) if in.v1 != s && out.v2 != s) {
          val v1 = in.v1
          val v2 = out.v2

          val reg = in.label combCons selfReg combCons out.label

          newTransitions += RegexTransition(v1, reg, v2)
        }

        rnfa.removeStates(Set(s)).addTransitions(newTransitions)
      }

      val finalState = stateBuilder()

      val transitions = atm.transitions.map{t => new RegexTransition(t.v1, new RegLit[W](t.label), t.v2)} ++
                        atm.finals.map(s => new RegexTransition(s, RegEps[W](), finalState))

      var rnfa = new RegexAutomaton[S, W](atm.states+finalState, transitions, atm.entry, Set(finalState))

      val removableStates = (rnfa.states -- rnfa.finals) - rnfa.entry

      for (s <- removableStates) {
        rnfa = ripState(rnfa, s)
      }

      if (rnfa.transitions.isEmpty) {
        RegEps()
      } else {
        rnfa.transitions.map(_.label).reduce(_ combOr _)
      }
    }

    def regexToNFA[S <: StateAbs, W](atm: Regex[W])(implicit stateBuilder: () => S): Automatons.Automaton[S, TransitionAbs[S, W], W] = {
      null
    }
  }

  import scala.util.parsing.combinator.syntactical.StandardTokenParsers

  object RegexParser extends StandardTokenParsers {
    lexical.delimiters += ("(", ")", "*", "|", ".")

    def term: Parser[Regex[String]] =
      "(" ~> regex <~ ")" |
      "." ~ ident ^^ { case "." ~ i => RegLit(i.toString) } |
      ident ^^ { i => RegLit(i.toString) }

    def star: Parser[Regex[String]] =
      term ~ "*" ^^ { case t ~ "*" => RegAst.around(t) } |
      term

    def cons: Parser[Regex[String]] =
      rep1sep(star, ".") ^^ { case r :: Nil  => r
                              case rs        => RegCons(rs) }

    def regex: Parser[Regex[String]] =
      rep1sep(cons, "|") ^^ { case r :: Nil  => r
                              case rs        => RegOr(rs) }

    def parseString(str: String): Option[Regex[String]] = {
      val s = new lexical.Scanner(str)
      val r = phrase(regex)(s)
      if (r.isEmpty) {
        None
      } else {
        Some(r.get)
      }
    }
  }
}
