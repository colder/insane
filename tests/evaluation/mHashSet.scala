package mhashset
import scala.collection.mutable.HashSet

class El(val i: Int) {
  var visited = false
}

object Test {
  def run1(s: HashSet[El], f: (Int, El) => Int) = {
    s.foldLeft(0)(f)
  }

  def run2(s: HashSet[El]) = {
    s.foldLeft(0) { (i, el) => i + el.i }
  }

  def run3(s: HashSet[El]) = {
    s.foldLeft(0) { (i, el) => el.visited = true; i + el.i }
  }

  def run4(s: HashSet[El], el: El) = {
    s += el
  }
}
