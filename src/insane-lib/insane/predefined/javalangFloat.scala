package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Float")
class javalangFloat {
  @AbstractsMethod("java.lang.Float.compare((x$1: Float, x$2: Float)Int)")
  def __compare(x1: Float, x2: Float): Int = {
    42
  }
  @AbstractsMethod("java.lang.Float.floatValue(()Float)")
  def __floatValue(): Float = {
    42.0f
  }
  @AbstractsMethod("java.lang.Float.hashCode(()Int)")
  def __hashCode(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Float.<init>((x$1: Float)java.lang.Float)")
  def ____init__(x1: Float): javalangFloat = {
    this
  }
  @AbstractsMethod("java.lang.Float.intBitsToFloat((x$1: Int)Float)")
  def __intBitsToFloat(x1: Int): Float = {
    42.0f
  }
  @AbstractsMethod("java.lang.Float.isInfinite((x$1: Float)Boolean)")
  def __isInfinite(x1: Float): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Float.isNaN(()Boolean)")
  def __isNaN(): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Float.parseFloat((x$1: java.lang.String)Float)")
  def __parseFloat(x1: java.lang.String): Float = {
    42.0f
  }
  @AbstractsMethod("java.lang.Float.valueOf((x$1: Float)java.lang.Float)")
  def __valueOf(x1: Float): java.lang.Float = {
    new java.lang.Float(42.0f)
  }
}
