package treeset
import scala.collection.immutable.TreeSet
import insane.annotations.{WillNotModify, MayOnlyModify}

class El(val i: Int) {
  var visited = false
}

object ElOrdering extends Ordering[El] {
  def compare(a: El, b: El) = a.i - b.i
}

object Test {
  def run1(s: TreeSet[El], f: (Int, El) => Int) = {
    s.foldLeft(0)(f)
  }

  def run2(s: TreeSet[El]) = {
    s.foldLeft(0) { (i, el) => i + el.i }
  }

  def run3(s: TreeSet[El]) = {
    s.foldLeft(0) { (i, el) => el.visited = true; i + el.i }
  }
}
