package insane
package predefined

import annotations._

@AbstractsClass("java.util.Iterator")
class javautilIterator {
  @AbstractsMethod("java.util.Iterator.hasNext(()Boolean)")
  def hasNext() : Boolean = { true }

  @AbstractsMethod("java.util.Iterator.next(()java.lang.Object)")
  def next() : java.lang.Object = { new java.lang.Object() }

}
