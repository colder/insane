package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Throwable")
class javalangThrowable {
  @AbstractsMethod("java.lang.Throwable.fillInStackTrace(()java.lang.Throwable)")
  def fillInStackTrace(): java.lang.Throwable = {
    new java.lang.Throwable()
  }
  @AbstractsMethod("java.lang.Throwable.getCause(()java.lang.Throwable)")
  def getCause(): java.lang.Throwable = {
    new java.lang.Throwable()
  }
  @AbstractsMethod("java.lang.Throwable.getMessage(()java.lang.String)")
  def getMessage(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Throwable.getStackTrace(()Array[java.lang.StackTraceElement])")
  def getStackTrace(): Array[java.lang.StackTraceElement] = {
    new Array[java.lang.StackTraceElement](1)
  }
  @AbstractsMethod("java.lang.Throwable.<init>(()java.lang.Throwable)")
  def __init__(): javalangThrowable = {
    this
  }
  @AbstractsMethod("java.lang.Throwable.<init>((x$1: java.lang.String)java.lang.Throwable)")
  def __init__(x1: java.lang.String): javalangThrowable = {
    this
  }
  @AbstractsMethod("java.lang.Throwable.printStackTrace(()Unit)")
  def printStackTrace(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Throwable.toString(()java.lang.String)")
  def toString(): java.lang.String = {
    ""
  }
}
