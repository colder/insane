package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Long")
class javalangLong {
  @AbstractsMethod("java.lang.Long.hashCode(()Int)")
  def hashCode(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Long.longValue(()Long)")
  def longValue(): Long = {
    42L
  }
  @AbstractsMethod("java.lang.Long.parseLong((x$1: java.lang.String)Long)")
  def parseLong(x1: java.lang.String): Long = {
    42L
  }
  @AbstractsMethod("java.lang.Long.toBinaryString((x$1: Long)java.lang.String)")
  def toBinaryString(x1: Long): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Long.toHexString((x$1: Long)java.lang.String)")
  def toHexString(x1: Long): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Long.toOctalString((x$1: Long)java.lang.String)")
  def toOctalString(x1: Long): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Long.toString(()java.lang.String)")
  def toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Long.valueOf((x$1: Long)java.lang.Long)")
  def valueOf(x1: Long): java.lang.Long = {
    new java.lang.Long()
  }
}
