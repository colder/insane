package insane
package predefined

import annotations._

@AbstractsClass("java.util.Map$Entry")
class javautilMap$Entry {
  @AbstractsMethod("java.util.Map$Entry.getKey(()java.lang.Object)")
  def getKey() : java.lang.Object = { new java.lang.Object() }

  @AbstractsMethod("java.util.Map$Entry.getValue(()java.lang.Object)")
  def getValue() : java.lang.Object = { new java.lang.Object() }

}
