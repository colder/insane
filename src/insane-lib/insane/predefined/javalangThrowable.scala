package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Throwable")
abstract class javalangThrowable {
  @AbstractsMethod("java.lang.Throwable.fillInStackTrace(()java.lang.Throwable)")
  def __fillInStackTrace(): java.lang.Throwable
  @AbstractsMethod("java.lang.Throwable.getCause(()java.lang.Throwable)")
  def __getCause(): java.lang.Throwable
  @AbstractsMethod("java.lang.Throwable.getMessage(()java.lang.String)")
  def __getMessage(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Throwable.getStackTrace(()Array[java.lang.StackTraceElement])")
  def __getStackTrace(): Array[java.lang.StackTraceElement] = {
    new Array[java.lang.StackTraceElement](1)
  }
  @AbstractsMethod("java.lang.Throwable.<init>(()java.lang.Throwable)")
  def ____init__(): javalangThrowable = {
    this
  }
  @AbstractsMethod("java.lang.Throwable.<init>((x$1: java.lang.String)java.lang.Throwable)")
  def ____init__(x1: java.lang.String): javalangThrowable = {
    this
  }
  @AbstractsMethod("java.lang.Throwable.printStackTrace(()Unit)")
  def __printStackTrace(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Throwable.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
}
