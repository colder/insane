package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.LinkedBlockingQueue")
class javautilconcurrentLinkedBlockingQueue {
  @AbstractsMethod("java.util.concurrent.LinkedBlockingQueue.<init>(()java.util.concurrent.LinkedBlockingQueue)")
  def __init__(): javautilconcurrentLinkedBlockingQueue = {
    this
  }
  @AbstractsMethod("java.util.concurrent.LinkedBlockingQueue.<init>((x$1: Int)java.util.concurrent.LinkedBlockingQueue)")
  def __init__(x1: Int): javautilconcurrentLinkedBlockingQueue = {
    this
  }
  @AbstractsMethod("java.util.concurrent.LinkedBlockingQueue.peek(()java.lang.Object)")
  def peek(): java.lang.Object = {
    new java.lang.Object()
  }
  @AbstractsMethod("java.util.concurrent.LinkedBlockingQueue.put((x$1: java.lang.Object)Unit)")
  def put(x1: java.lang.Object): Unit = {
    ()
  }
  @AbstractsMethod("java.util.concurrent.LinkedBlockingQueue.take(()java.lang.Object)")
  def take(): java.lang.Object = {
    new java.lang.Object()
  }
}
