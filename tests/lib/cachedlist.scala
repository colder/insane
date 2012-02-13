package lib.cachedlist

class Counter(var v: Int);

class CounterRef(var c: Counter);

abstract class List {
  def apply(i: Int): CounterRef
}

class Cons(head: CounterRef, tail: List) extends List {
  var cachedI = -1
  var cachedV: CounterRef = null;

  def apply(i: Int): CounterRef = {
    if (i == 0) {
      head
    } else if (i == cachedI) {
      cachedV
    } else {
      val v = tail.apply(i-1)
      cachedI = i
      cachedV = v
      v
    }
  }
}

object Nil extends List {
  def apply(i: Int) = null
}

object Usage {

  def create = {
    new Cons(new CounterRef(new Counter(0)), new Cons(new CounterRef(new Counter(1)), new Cons(new CounterRef(new Counter(2)), Nil)))
  }


  def use1 = {
    val list = create

    list.apply(1);
  }
}
