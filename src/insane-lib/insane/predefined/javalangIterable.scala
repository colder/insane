package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Iterable")
class javalangIterable {
  @AbstractsMethod("java.lang.Iterable.iterator(()java.util.Iterator)")
  def iterator(): java.util.Iterator = {
    new java.util.Iterator()
  }
}
