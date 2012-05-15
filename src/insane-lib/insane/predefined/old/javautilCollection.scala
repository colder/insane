package insane
package predefined

import annotations._

@AbstractsClass("java.util.Collection")
abstract class javautilCollection {
  @AbstractsMethod("java.util.Collection.isEmpty(()Boolean)")
  def __isEmpty(): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Collection.iterator(()java.util.Iterator)")
  def __iterator(): java.util.Iterator[_]
  @AbstractsMethod("java.util.Collection.size(()Int)")
  def __size(): Int = {
    42
  }
}
