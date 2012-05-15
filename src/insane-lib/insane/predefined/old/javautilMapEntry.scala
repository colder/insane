package insane
package predefined

import annotations._

@AbstractsClass("java.util.Map$Entry")
abstract class javautilMapEntry {
  @AbstractsMethod("java.util.Map$Entry.getKey(()java.lang.Object)")
  def __getKey(): java.lang.Object
  @AbstractsMethod("java.util.Map$Entry.getValue(()java.lang.Object)")
  def __getValue(): java.lang.Object
}
