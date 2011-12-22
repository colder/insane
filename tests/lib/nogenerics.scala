package lib.nogenerics

class Counter(var v: Int);

class CounterRef(var c: Counter);

abstract class M {
  def apply(a1: CounterRef): CounterRef
}

class MapId extends M {
  def apply(a1: CounterRef): CounterRef = a1
}

class MapId1 extends M {
  def apply(a1: CounterRef): CounterRef = a1
}
class MapId2 extends M {
  def apply(a1: CounterRef): CounterRef = a1
}
class MapId3 extends M {
  def apply(a1: CounterRef): CounterRef = a1
}
class MapId4 extends M {
  def apply(a1: CounterRef): CounterRef = a1
}

class AllNull extends M {
  def apply(a1: CounterRef): CounterRef = new CounterRef(null)
}

abstract class P {
  def apply(a1: CounterRef): Boolean
}

abstract class List {
  def forall(f: P): Boolean
  def exists(f: P): Boolean
  def map(f: M): List
}

class Cons(head: CounterRef, tail: List) extends List {
  def forall(f: P): Boolean =
    f.apply(head) && tail.forall(f)

  def exists(f: P): Boolean =
    f.apply(head) || tail.exists(f)

  def map(f: M): List =
    new Cons(f.apply(head), tail.map(f))
}

object Nil extends List {
  def forall(f: P): Boolean = true
  def exists(f: P): Boolean = false
  def map(f: M): List = Nil
}

object Usage {

  def create = {
    new Cons(new CounterRef(new Counter(0)), new Cons(new CounterRef(new Counter(1)), new Cons(new CounterRef(new Counter(2)), Nil)))
  }


  def use1 = {
    val list = create

    list.map(new AllNull)
  }
}
