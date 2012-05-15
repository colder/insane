package insane
package predefined

import annotations._

@AbstractsClass("java.util.Enumeration")
abstract class javautilEnumeration {
  @AbstractsMethod("java.util.Enumeration.hasMoreElements(()Boolean)")
  def __hasMoreElements(): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Enumeration.nextElement(()java.lang.Object)")
  def __nextElement(): java.lang.Object
}
