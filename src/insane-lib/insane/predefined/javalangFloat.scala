package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Float")
class javalangFloat {
  @AbstractsMethod("java.lang.Float.compare((x$1:Float, x$2:Float)Int)")
  def compare(x1 : Float, x2 : Float) : Int = { 0 }

  @AbstractsMethod("java.lang.Float.floatValue(()Float)")
  def floatValue() : Float = { 0.0f }

  @AbstractsMethod("java.lang.Float.hashCode(()Int)")
  def hashCode() : Int = { 0 }

  @AbstractsMethod("java.lang.Float.intBitsToFloat((x$1:Int)Float)")
  def intBitsToFloat(x1 : Int) : Float = { 0.0f }

  @AbstractsMethod("java.lang.Float.isInfinite((x$1:Float)Boolean)")
  def isInfinite(x1 : Float) : Boolean = { true }

  @AbstractsMethod("java.lang.Float.isNaN(()Boolean)")
  def isNaN() : Boolean = { true }

  @AbstractsMethod("java.lang.Float.parseFloat((x$1:java.lang.String)Float)")
  def parseFloat(x1 : java.lang.String) : Float = { 0.0f }

  @AbstractsMethod("java.lang.Float.valueOf((x$1:Float)java.lang.Float)")
  def valueOf(x1 : Float) : java.lang.Float = { new java.lang.Float() }

}
