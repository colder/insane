package treeset
import scala.collection.immutable.TreeSet
import scala.collection.mutable.HashSet
import scala.collection.mutable.LinkedList

class El(val i: Int) {
  var visited = false
}

object ElOrdering extends Ordering[El] {
  def compare(a: El, b: El) = a.i - b.i
}

object Test {
  def runA1(s: LinkedList[El], f: El => Unit) = {
    s.foreach(f)
  }

  def runA2(s: LinkedList[El]) = {
    s.foreach{ (el) => () }
  }

  def runA3(s: LinkedList[El]) = {
    s.foreach{ (el) => el.visited = true }
  }

  def runA4(s: HashSet[El], el: El) = {
    s += el
  }

}
