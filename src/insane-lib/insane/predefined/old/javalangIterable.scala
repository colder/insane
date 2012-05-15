package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Iterable")
abstract class javalangIterable {
  @AbstractsMethod("java.lang.Iterable.iterator(()java.util.Iterator)")
  def __iterator(): java.util.Iterator[_]
}
