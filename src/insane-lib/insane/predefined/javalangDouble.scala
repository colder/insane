package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Double")
class javalangDouble {
  @AbstractsMethod("java.lang.Double.compare((x$1: Double, x$2: Double)Int)")
  def compare(x1: Double, x2: Double): Int = {
    42
  }
  @AbstractsMethod("java.lang.Double.doubleValue(()Double)")
  def doubleValue(): Double = {
    42.0d
  }
  @AbstractsMethod("java.lang.Double.hashCode(()Int)")
  def hashCode(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Double.isInfinite((x$1: Double)Boolean)")
  def isInfinite(x1: Double): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Double.isNaN(()Boolean)")
  def isNaN(): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Double.longBitsToDouble((x$1: Long)Double)")
  def longBitsToDouble(x1: Long): Double = {
    42.0d
  }
  @AbstractsMethod("java.lang.Double.parseDouble((x$1: java.lang.String)Double)")
  def parseDouble(x1: java.lang.String): Double = {
    42.0d
  }
  @AbstractsMethod("java.lang.Double.toString((x$1: Double)java.lang.String)")
  def toString(x1: Double): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Double.valueOf((x$1: Double)java.lang.Double)")
  def valueOf(x1: Double): java.lang.Double = {
    new java.lang.Double(42d)
  }
}
