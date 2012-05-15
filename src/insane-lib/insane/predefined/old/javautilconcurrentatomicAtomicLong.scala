package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.atomic.AtomicLong")
class javautilconcurrentatomicAtomicLong {
  @AbstractsMethod("java.util.concurrent.atomic.AtomicLong.incrementAndGet(()Long)")
  def __incrementAndGet(): Long = {
    42L
  }
  @AbstractsMethod("java.util.concurrent.atomic.AtomicLong.<init>((x$1: Long)java.util.concurrent.atomic.AtomicLong)")
  def ____init__(x1: Long): javautilconcurrentatomicAtomicLong = {
    this
  }
}
