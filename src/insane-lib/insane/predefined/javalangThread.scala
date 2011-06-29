package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Thread")
abstract class javalangThread {
  @AbstractsMethod("java.lang.Thread.activeCount(()Int)")
  def __activeCount(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Thread.currentThread(()java.lang.Thread)")
  def __currentThread(): java.lang.Thread
  @AbstractsMethod("java.lang.Thread.enumerate((x$1: Array[java.lang.Thread])Int)")
  def __enumerate(x1: Array[java.lang.Thread]): Int = {
    42
  }
  @AbstractsMethod("java.lang.Thread.<init>(()java.lang.Thread)")
  def ____init__(): javalangThread = {
    this
  }
  @AbstractsMethod("java.lang.Thread.<init>((x$1: java.lang.Runnable)java.lang.Thread)")
  def ____init__(x1: java.lang.Runnable): javalangThread = {
    this
  }
  @AbstractsMethod("java.lang.Thread.<init>((x$1: java.lang.Runnable, x$2: java.lang.String)java.lang.Thread)")
  def ____init__(x1: java.lang.Runnable, x2: java.lang.String): javalangThread = {
    this
  }
  @AbstractsMethod("java.lang.Thread.<init>((x$1: java.lang.String)java.lang.Thread)")
  def ____init__(x1: java.lang.String): javalangThread = {
    this
  }
  @AbstractsMethod("java.lang.Thread.interrupt(()Unit)")
  def __interrupt(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Thread.join(()Unit)")
  def __join(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Thread.setDaemon((x$1: Boolean)Unit)")
  def __setDaemon(x1: Boolean): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Thread.setName((x$1: java.lang.String)Unit)")
  def __setName(x1: java.lang.String): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Thread.sleep((x$1: Long)Unit)")
  def __sleep(x1: Long): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Thread.start(()Unit)")
  def __start(): Unit = {
    ()
  }
}
