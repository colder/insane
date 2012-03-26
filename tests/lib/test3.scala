package t3

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

abstract class List {
  def map(f: M): List
}

class Cons(head: Data, tail: List) extends List {
  def map(f: M): List =
    new Cons(f.apply(head), tail.map(f))
}

object Nil extends List {
  def map(f: M): List = Nil
}

object Usage {

  def create = {
    new Cons(new Data(true), new Cons(new Data(true), new Cons(new Data(true), Nil)))
  }

  def use1 = {
    val list = create

    list.map(new AllFalse)
  }

  def use2 = {
    val list = create

    list.map(new MapId)
  }
}
