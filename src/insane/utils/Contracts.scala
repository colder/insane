package insane
package utils

import scala.tools.nsc._
import Automatons._
import RegularExpressions._

trait Contracts {
  val global: Global

  import global._

  sealed abstract class AbsContract;

  case class Requires(expr: Tree) extends AbsContract;
  case class Ensures(expr: Tree) extends AbsContract;
  abstract class Assert extends AbsContract {
    val tree: Tree
    val lhs: Tree
    val rhs: Tree
  }
  case class AssertEQ(tree: Tree, lhs: Tree, rhs: Tree) extends Assert;
  case class AssertNE(tree: Tree, lhs: Tree, rhs: Tree) extends Assert;

  abstract class EffectsContract;

  // Effect based assertions
  case class AssertUntouched(regex: Regex[String], region: Automaton[String]) extends EffectsContract {
    override def toString = ("@WillNotModify("+regex+")")
  }

  case class AssertOnlyModified(regex: Regex[String], region: Automaton[String]) extends EffectsContract {
    override def toString = ("@MayOnlyModify("+regex+")")
  }
}
