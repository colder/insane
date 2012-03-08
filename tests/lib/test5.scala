package t5

class Data(var b: Boolean);


class Iterator(var c: List) {
  def hasNext = c != Nil

  def current = {
    c.asInstanceOf[Cons].head
  }

  def next() {
    c = c.asInstanceOf[Cons].tail
  }
}

abstract class List
class Cons(val head: Data, val tail: List) extends List
object Nil extends List


object Usage {

  def create = {
    new Cons(new Data(true), new Cons(new Data(true), new Cons(new Data(true), Nil)))
  }

  def use(l: List) = {
    val it = new Iterator(l)

    if (it.hasNext) {
      val c = it.current

      c.b = false

      it.next()
    }

    it
  }
}
