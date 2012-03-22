package evaluation

import scala._
import scala.runtime._

abstract class List[+T] {
  def forall(f: Function1[T, Boolean]): Boolean
  def exists(f: Function1[T, Boolean]): Boolean
  def filter(f: Function1[T, Boolean]): List[T]
  def foreach(f: Function1[T, Unit]): Unit
  def map[B](f: Function1[T, B]): List[B]
}

class Cons[T](head: T, tail: List[T]) extends List[T] {
  def forall(f: Function1[T, Boolean]): Boolean =
    f.apply(head) && tail.forall(f)

  def exists(f: Function1[T, Boolean]): Boolean =
    f.apply(head) || tail.exists(f)

  def filter(f: Function1[T, Boolean]): List[T] =
    if (f(head)) {
      new Cons[T](head, tail.filter(f))
    } else {
      tail.filter(f)
    }


  def foreach(f: Function1[T, Unit]): Unit = {
    f.apply(head)
    tail.foreach(f)
  }

  def map[B](f: Function1[T, B]): List[B] =
    new Cons[B](f.apply(head), tail.map(f))
}

object Nil extends List[Nothing] {
  def forall(t: Function1[Nothing, Boolean]): Boolean =
    true
  def exists(t: Function1[Nothing, Boolean]): Boolean =
    false
  def foreach(t: Function1[Nothing, Unit]): Unit =
    {}
  def map[B](t: Function1[Nothing, B]): List[B] =
    Nil

  def filter(t: Function1[Nothing, Boolean]): List[Nothing] =
    Nil
}

object Dummy {
  def getf = {
    {x: Object  => x.toString}
    {x: Object  => false}
    {x: Object  => true}
    {x: Object  => () }
  }
}
