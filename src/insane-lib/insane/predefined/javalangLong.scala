package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Long")
class javalangLong {
  @AbstractsMethod("java.lang.Long.hashCode(()Int)")
  def __hashCode(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Long.<init>((x$1: Long)java.lang.Long)")
  def ____init__(x1: Long): javalangLong = {
    this
  }
  @AbstractsMethod("java.lang.Long.longValue(()Long)")
  def __longValue(): Long = {
    42L
  }
  @AbstractsMethod("java.lang.Long.parseLong((x$1: java.lang.String)Long)")
  def __parseLong(x1: java.lang.String): Long = {
    42L
  }
  @AbstractsMethod("java.lang.Long.toBinaryString((x$1: Long)java.lang.String)")
  def __toBinaryString(x1: Long): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Long.toHexString((x$1: Long)java.lang.String)")
  def __toHexString(x1: Long): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Long.toOctalString((x$1: Long)java.lang.String)")
  def __toOctalString(x1: Long): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Long.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Long.valueOf((x$1: Long)java.lang.Long)")
  def __valueOf(x1: Long): java.lang.Long = {
    new java.lang.Long(42L)
  }
}
