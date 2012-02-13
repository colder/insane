package t4

class Data(var b: Boolean);

abstract class M {
  def apply(a1: Data): Data
}

class MapId extends M {
  def apply(a1: Data): Data = a1
}

class MapId1 extends M {
  def apply(a1: Data): Data = a1
}
class MapId2 extends M {
  def apply(a1: Data): Data = a1
}
class MapId3 extends M {
  def apply(a1: Data): Data = a1
}
class MapId4 extends M {
  def apply(a1: Data): Data = a1
}

class AllFalse extends M {
  def apply(a1: Data): Data = new Data(false)
}

abstract class List
class Cons(val head: Data, val tail: List) extends List
object Nil extends List


object Usage {

  def create = {
    new Cons(new Data(true), new Cons(new Data(true), new Cons(new Data(true), Nil)))
  }

  def map(l: List, f: M): List = {
    if (l == Nil) {
      Nil
    } else {
      val c = l.asInstanceOf[Cons]
      new Cons(f.apply(c.head), map(c.tail, f))
    }
  }

  def append(l: List, d: Data): List = {
    if (l == Nil) {
      new Cons(d, Nil)
    } else {
      val c = l.asInstanceOf[Cons]
      new Cons(c.head, append(c.tail, d))
    }
  }

  def prepend(l: List, d: Data): List = {
    new Cons(d, l)
  }

  def use1 = {
    val list = create

    map(list, new AllFalse)
  }

  def use2 = {
    val list = create

    append(list, new Data(false))
  }

  def use3 = {
    val list = create

    prepend(list, new Data(false))
  }
}
