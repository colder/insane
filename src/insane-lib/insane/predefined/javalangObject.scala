package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Object")
abstract class javalangObject {
  @AbstractsMethod("java.lang.Object.$bang$eq((x$1: java.lang.Object)Boolean)")
  def __$bang$eq(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.clone(()java.lang.Object)")
  def __clone(): java.lang.Object
  @AbstractsMethod("java.lang.Object.$eq$eq((x$1: java.lang.Object)Boolean)")
  def __$eq$eq(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.equals((x$1: java.lang.Object)Boolean)")
  def __equals(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.eq((x$1: java.lang.Object)Boolean)")
  def __eq(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.getClass(()java.lang.Class)")
  def __getClass(): java.lang.Class[_]
  @AbstractsMethod("java.lang.Object.hashCode(()Int)")
  def __hashCode(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Object.<init>(()java.lang.Object)")
  def ____init__(): javalangObject = {
    this
  }
  @AbstractsMethod("java.lang.Object.ne((x$1: java.lang.Object)Boolean)")
  def __ne(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.notifyAll(()Unit)")
  def __notifyAll(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Object.notify(()Unit)")
  def __notify(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Object.synchronized((x$1: java.lang.Object)java.lang.Object)")
  def __synchronized(x1: java.lang.Object): java.lang.Object
  @AbstractsMethod("java.lang.Object.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Object.wait(()Unit)")
  def __wait(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Object.wait((x$1: Long)Unit)")
  def __wait(x1: Long): Unit = {
    ()
  }
}
