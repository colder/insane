package treeset
import scala.collection.immutable.TreeSet
import scala.collection.mutable.HashSet
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.Queue

class El(val i: Int) {
  var visited = false
}

object ElOrdering extends Ordering[El] {
  def compare(a: El, b: El) = a.i - b.i
}

object Test {
  def runA1(s: TreeSet[El], f: El => Unit) = {
    s.foreach(f)
  }

  def runA2(s: TreeSet[El]) = {
    s.foreach{ (el) => () }
  }

  def runA3(s: TreeSet[El]) = {
    s.foreach{ (el) => el.visited = true }
  }

}
