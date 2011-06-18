package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Object")
class javalangObject {
  @AbstractsMethod("java.lang.Object.$bang$eq((x$1: java.lang.Object)Boolean)")
  def $bang$eq(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.clone(()java.lang.Object)")
  def clone(): java.lang.Object = {
    new java.lang.Object()
  }
  @AbstractsMethod("java.lang.Object.$eq$eq((x$1: java.lang.Object)Boolean)")
  def $eq$eq(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.equals((x$1: java.lang.Object)Boolean)")
  def equals(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.eq((x$1: java.lang.Object)Boolean)")
  def eq(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.getClass(()java.lang.Class)")
  def getClass(): java.lang.Class = {
    new java.lang.Class()
  }
  @AbstractsMethod("java.lang.Object.hashCode(()Int)")
  def hashCode(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Object.<init>(()java.lang.Object)")
  def __init__(): java.lang.Object = {
    new java.lang.Object()
  }
  @AbstractsMethod("java.lang.Object.ne((x$1: java.lang.Object)Boolean)")
  def ne(x1: java.lang.Object): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Object.notifyAll(()Unit)")
  def notifyAll(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Object.notify(()Unit)")
  def notify(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Object.synchronized((x$1: java.lang.Object)java.lang.Object)")
  def synchronized(x1: java.lang.Object): java.lang.Object = {
    new java.lang.Object()
  }
  @AbstractsMethod("java.lang.Object.toString(()java.lang.String)")
  def toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Object.wait(()Unit)")
  def wait(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Object.wait((x$1: Long)Unit)")
  def wait(x1: Long): Unit = {
    ()
  }
}
