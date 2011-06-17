package insane
package predefined

import annotations._

@AbstractsClass("java.util.Collection")
class javautilCollection {
  @AbstractsMethod("java.util.Collection.isEmpty(()Boolean)")
  def isEmpty() : Boolean = { true }

  @AbstractsMethod("java.util.Collection.iterator(()java.util.Iterator)")
  def iterator() : java.util.Iterator = { new java.util.Iterator() }

  @AbstractsMethod("java.util.Collection.size(()Int)")
  def size() : Int = { 0 }

}
