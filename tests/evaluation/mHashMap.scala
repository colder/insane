package mhashmap
import scala.collection.mutable.HashMap

class El(val i: Int) {
  var visited = false
}

object Test {
  def run1(s: HashMap[El, Int], f: ((El, Int)) => Boolean) = {
    s.exists(f)
  }

  def run2(s: HashMap[El, Int]) = {
    s.exists { case (el, i) => el.visited = true; i > 1  }
  }

  def run3(s: HashMap[El, Int], el: El) = {
    s += el -> 42
  }
}
