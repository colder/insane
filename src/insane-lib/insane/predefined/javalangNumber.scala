package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Number")
class javalangNumber {
  @AbstractsMethod("java.lang.Number.byteValue(()Byte)")
  def byteValue(): Byte = {
    (42 : Byte)
  }
  @AbstractsMethod("java.lang.Number.doubleValue(()Double)")
  def doubleValue(): Double = {
    42.0d
  }
  @AbstractsMethod("java.lang.Number.floatValue(()Float)")
  def floatValue(): Float = {
    42.0f
  }
  @AbstractsMethod("java.lang.Number.intValue(()Int)")
  def intValue(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Number.longValue(()Long)")
  def longValue(): Long = {
    42L
  }
  @AbstractsMethod("java.lang.Number.shortValue(()Short)")
  def shortValue(): Short = {
    (42 : Short)
  }
}
