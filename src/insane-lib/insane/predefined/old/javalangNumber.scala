package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Number")
class javalangNumber {
  @AbstractsMethod("java.lang.Number.byteValue(()Byte)")
  def __byteValue(): Byte = {
    (42 : Byte)
  }
  @AbstractsMethod("java.lang.Number.doubleValue(()Double)")
  def __doubleValue(): Double = {
    42.0d
  }
  @AbstractsMethod("java.lang.Number.floatValue(()Float)")
  def __floatValue(): Float = {
    42.0f
  }
  @AbstractsMethod("java.lang.Number.intValue(()Int)")
  def __intValue(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Number.longValue(()Long)")
  def __longValue(): Long = {
    42L
  }
  @AbstractsMethod("java.lang.Number.shortValue(()Short)")
  def __shortValue(): Short = {
    (42 : Short)
  }
}
