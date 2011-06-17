package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Number")
class javalangNumber {
  @AbstractsMethod("java.lang.Number.byteValue(()Byte)")
  def byteValue() : Byte = { 0 }

  @AbstractsMethod("java.lang.Number.doubleValue(()Double)")
  def doubleValue() : Double = { 0.0d }

  @AbstractsMethod("java.lang.Number.floatValue(()Float)")
  def floatValue() : Float = { 0.0f }

  @AbstractsMethod("java.lang.NumberFormatException.<init>((x$1:java.lang.String)java.lang.NumberFormatException)")
  def PLOPINIT(x1 : java.lang.String) : java.lang.NumberFormatException = { new java.lang.NumberFormatException() }

  @AbstractsMethod("java.lang.Number.intValue(()Int)")
  def intValue() : Int = { 0 }

  @AbstractsMethod("java.lang.Number.longValue(()Long)")
  def longValue() : Long = { 0L }

  @AbstractsMethod("java.lang.Number.shortValue(()Short)")
  def shortValue() : Short = { 0 }

}
