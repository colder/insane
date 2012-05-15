package insane
package predefined

import annotations._

@AbstractsClass("java.util.Iterator")
abstract class javautilIterator {
  @AbstractsMethod("java.util.Iterator.hasNext(()Boolean)")
  def __hasNext(): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Iterator.next(()java.lang.Object)")
  def __next(): java.lang.Object
}
