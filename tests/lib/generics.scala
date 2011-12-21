package lib.generics

abstract class F1[-T1, +R] {
  def apply(a1: T1): R
}

abstract class F2[-T1, -T2, +R] {
  def apply(a1: T1, a2: T2): R
}


abstract class List[+T] {
  def forall(f: F1[T, Boolean]): Boolean
  def exists(f: F1[T, Boolean]): Boolean
  def foreach(f: F1[T, Unit]): Unit
  def map[B](f: F1[T, B]): List[B]
}

class Cons[T](head: T, tail: List[T]) extends List[T] {
  def forall(f: F1[T, Boolean]): Boolean =
    f.apply(head) && tail.forall(f)

  def exists(f: F1[T, Boolean]): Boolean =
    f.apply(head) || tail.exists(f)


  def foreach(f: F1[T, Unit]): Unit = {
    f.apply(head)
    tail.foreach(f)
  }

  def map[B](f: F1[T, B]): List[B] =
    new Cons[B](f.apply(head), tail.map(f))
}

object Nil extends List[Nothing] {
  def forall(t: F1[Nothing, Boolean]): Boolean =
    true
  def exists(t: F1[Nothing, Boolean]): Boolean =
    false
  def foreach(t: F1[Nothing, Unit]): Unit =
    {}
  def map[B](t: F1[Nothing, B]): List[B] =
    Nil
}
