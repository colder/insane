package lib.generics

abstract class F1[-T1, +R1] {
  def apply(a1: T1): R1
}

abstract class F2[-T2, -T3, +R2] {
  def apply(a1: T2, a2: T3): R2
}

abstract class List[+T4] {
  def forall(f: F1[T4, Boolean]): Boolean
  def exists(f: F1[T4, Boolean]): Boolean
  def foreach[B4](f: F1[T4, B4]): Unit
  def map[B4](f: F1[T4, B4]): List[B4]
}

class Cons[T5](head: T5, tail: List[T5]) extends List[T5] {
  def forall(f: F1[T5, Boolean]): Boolean =
    f.apply(head) && tail.forall(f)

  def exists(f: F1[T5, Boolean]): Boolean =
    f.apply(head) || tail.exists(f)


  def foreach[B5](f: F1[T5, B5]): Unit = {
    f.apply(head)
    tail.foreach(f)
  }

  def map[B7](f: F1[T5, B7]): List[B7] =
    new Cons[B7](f.apply(head), tail.map(f))
}

object Nil extends List[Nothing] {
  def forall(t: F1[Nothing, Boolean]): Boolean =
    true
  def exists(t: F1[Nothing, Boolean]): Boolean =
    false
  def foreach[B6](t: F1[Nothing, B6]): Unit =
    {}
  def map[B6](t: F1[Nothing, B6]): List[B6] =
    Nil
}

// Usage ----------------------------------------------------------------------

class SomeF1_001 extends F1[Int, Unit] {
  def apply(a1: Int): Unit = { }
}
class SomeF1_002 extends F1[Int, Unit] {
  def apply(a1: Int): Unit = { }
}
class SomeF1_003 extends F1[Int, Unit] {
  def apply(a1: Int): Unit = { }
}
class SomeF1_004 extends F1[Int, Unit] {
  def apply(a1: Int): Unit = { }
}

class MyF1 extends F1[Boolean, Unit]{
  def apply(a1: Boolean): Unit = { }
}



object Test {
  def run = {

    val l: List[Boolean] = new Cons(true, new Cons(false, Nil))

    l.foreach(new MyF1)
  }
}
