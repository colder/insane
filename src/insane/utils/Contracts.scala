package insane
package utils

import scala.tools.nsc._
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

  case class AssertUntouched(region: Regex[String]) extends EffectsContract;
}
