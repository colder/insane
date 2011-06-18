package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.atomic.AtomicInteger")
class javautilconcurrentatomicAtomicInteger {
  @AbstractsMethod("java.util.concurrent.atomic.AtomicInteger.compareAndSet((x$1: Int, x$2: Int)Boolean)")
  def compareAndSet(x1: Int, x2: Int): Boolean = {
    true
  }
  @AbstractsMethod("java.util.concurrent.atomic.AtomicInteger.get(()Int)")
  def get(): Int = {
    42
  }
  @AbstractsMethod("java.util.concurrent.atomic.AtomicInteger.<init>((x$1: Int)java.util.concurrent.atomic.AtomicInteger)")
  def __init__(x1: Int): javautilconcurrentatomicAtomicInteger = {
    this
  }
  @AbstractsMethod("java.util.concurrent.atomic.AtomicInteger.set((x$1: Int)Unit)")
  def set(x1: Int): Unit = {
    ()
  }
}
