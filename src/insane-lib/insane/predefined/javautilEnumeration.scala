package insane
package predefined

import annotations._

@AbstractsClass("java.util.Enumeration")
class javautilEnumeration {
  @AbstractsMethod("java.util.Enumeration.hasMoreElements(()Boolean)")
  def hasMoreElements() : Boolean = { true }

  @AbstractsMethod("java.util.Enumeration.nextElement(()java.lang.Object)")
  def nextElement() : java.lang.Object = { new java.lang.Object() }

}
