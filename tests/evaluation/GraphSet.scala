package graphset
import scala.collection.immutable.HashSet
import insane.annotations.{WillNotModify, MayOnlyModify}

case class Vertex(i: Int) {
  var visited = false
}
case class Edge(from: Vertex, to: Vertex)

case class Graph(vs: HashSet[Vertex], ve: HashSet[Edge]) {

  @MayOnlyModify("this.ve.tl*.hd.to.visited | from.visited")
  def traverse(from: Vertex) {
    var sum = 0;

    var todo = from :: Nil;
    while(!todo.isEmpty) {
      val v = todo.head
      todo = todo.tail

      sum += v.i
      v.visited = true

      for (e <- ve ) {
        if (e.from == v && !e.to.visited) {
          todo = e.to :: todo
        }
      }
    }

    sum
  }
}
