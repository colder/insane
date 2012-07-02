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

    def isEmpty = false
  }

  case class RegEps[T]() extends Regex[T] {
    override def isEmpty = true
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
      case eps : RegEps[T] =>
        eps
      case ast : RegAst[T] =>
        ast
      case _ =>
        RegAst(r)
    }
  }


  object RegexHelpers {
    import Automatons._

    def nfaToRegex[W, S](atm: Automaton[W, S])(implicit newState: () => State[S]): Regex[W] = {

      def oreg(o: OptLabel[Regex[W]]): Regex[W] = o.getOrElse(RegEps())

      def ripState(rnfa: Automaton[Regex[W], S], s: State[S]): Automaton[Regex[W], S] = {
        val selfRegs = rnfa.graph.ins(s).filter(_.v1 == s).  // Only self loops
                        map(t => RegAst.around(oreg(t.label))) // Construct self loop regexes

        val selfReg = if (selfRegs.isEmpty) {
          RegEps[W]()
        } else {
          selfRegs.reduce(_ combOr _)
        }

        var newTransitions = Set[Transition[Regex[W], S]]()

        for (in <- rnfa.graph.ins(s); out <- rnfa.graph.outs(s) if in.v1 != s && out.v2 != s) {
          val v1 = in.v1
          val v2 = out.v2

          val reg = oreg(in.label) combCons selfReg combCons oreg(out.label)

          newTransitions += Transition(v1, Label(reg), v2)
        }

        rnfa.removeStates(Set(s)).addTransitions(newTransitions)
      }

      val finalState = newState()

      val transitions = atm.transitions.map{t => Transition[Regex[W], S](t.v1, t.label.map(s => new RegLit[W](s)), t.v2)} ++
                        atm.finals.map(s => Transition[Regex[W], S](s, Epsylon, finalState))

      var rnfa = new Automaton[Regex[W], S](atm.states+finalState, transitions, atm.entry, Set(finalState))

      val removableStates = (rnfa.states -- rnfa.finals) - rnfa.entry

      for (s <- removableStates) {
        rnfa = ripState(rnfa, s)
      }

      if (rnfa.transitions.isEmpty) {
        RegEps()
      } else {
        rnfa.transitions.map(t => oreg(t.label)).reduce(_ combOr _)
      }
    }

    def regexToNFA[W, S](reg: Regex[W])(implicit newState2: () => State[S]): Automaton[W, S] = {
      var states      = Set[State[S]]()
      var transitions = Set[Transition[W, S]]()

      def newState() = {
        val s = newState2()
        states += s
        s
      }

      val entryState = newState()
      val finalState = newState()

      def convertRegex(from: State[S], r: Regex[W], to: State[S]): Unit = r match {
        case RegEps() =>
          transitions += Transition(from, Epsylon, to)
        case RegCons(ls) =>
          var curBegin = from
          var curEnd   = curBegin

          for (r <- ls.dropRight(1)) {
            curEnd = newState()
            convertRegex(curBegin, r, curEnd)
            curBegin = curEnd
          }

          convertRegex(curBegin, ls.last, to)

        case RegOr(ls) =>
          for (r <- ls) {
            convertRegex(from, r, to)
          }
        case RegAst(r) =>
          var f = from
          if (from == entryState) {
            f = newState()
            transitions += Transition(from, Epsylon, f)
          }
          convertRegex(f, r, f)
          transitions += Transition(f, Epsylon, to)

        case RegLit(l) =>
          transitions += Transition(from, Label(l), to)
      }

      convertRegex(entryState, reg, finalState)

      new Automaton(states, transitions, entryState, Set(finalState))
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
