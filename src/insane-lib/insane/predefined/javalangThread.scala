package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Thread")
class javalangThread {
  @AbstractsMethod("java.lang.Thread.activeCount(()Int)")
  def activeCount() : Int = { 0 }

  @AbstractsMethod("java.lang.Thread.currentThread(()java.lang.Thread)")
  def currentThread() : java.lang.Thread = { new java.lang.Thread() }

  @AbstractsMethod("java.lang.Thread.enumerate((x$1:Array[java.lang.Thread])Int)")
  def enumerate(x1 : Array[java.lang.Thread]) : Int = { 0 }

  @AbstractsMethod("java.lang.Thread.<init>(()java.lang.Thread)")
  def PLOPINIT() : java.lang.Thread = { new java.lang.Thread() }

  @AbstractsMethod("java.lang.Thread.<init>((x$1:java.lang.Runnable)java.lang.Thread)")
  def PLOPINIT(x1 : java.lang.Runnable) : java.lang.Thread = { new java.lang.Thread() }

  @AbstractsMethod("java.lang.Thread.<init>((x$1:java.lang.Runnable, x$2:java.lang.String)java.lang.Thread)")
  def PLOPINIT(x1 : java.lang.Runnable, x2 : java.lang.String) : java.lang.Thread = { new java.lang.Thread() }

  @AbstractsMethod("java.lang.Thread.<init>((x$1:java.lang.String)java.lang.Thread)")
  def PLOPINIT(x1 : java.lang.String) : java.lang.Thread = { new java.lang.Thread() }

  @AbstractsMethod("java.lang.Thread.interrupt(()Unit)")
  def interrupt() : Unit = { () }

  @AbstractsMethod("java.lang.Thread.join(()Unit)")
  def join() : Unit = { () }

  @AbstractsMethod("java.lang.ThreadLocal.get(()java.lang.Object)")
  def get() : java.lang.Object = { new java.lang.Object() }

  @AbstractsMethod("java.lang.ThreadLocal.<init>(()java.lang.ThreadLocal)")
  def PLOPINIT() : java.lang.ThreadLocal = { new java.lang.ThreadLocal() }

  @AbstractsMethod("java.lang.ThreadLocal.set((x$1:java.lang.Object)Unit)")
  def set(x1 : java.lang.Object) : Unit = { () }

  @AbstractsMethod("java.lang.Thread.setDaemon((x$1:Boolean)Unit)")
  def setDaemon(x1 : Boolean) : Unit = { () }

  @AbstractsMethod("java.lang.Thread.setName((x$1:java.lang.String)Unit)")
  def setName(x1 : java.lang.String) : Unit = { () }

  @AbstractsMethod("java.lang.Thread.sleep((x$1:Long)Unit)")
  def sleep(x1 : Long) : Unit = { () }

  @AbstractsMethod("java.lang.Thread.start(()Unit)")
  def start() : Unit = { () }

}
