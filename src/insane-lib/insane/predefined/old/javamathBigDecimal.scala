package insane
package predefined

import annotations._

@AbstractsClass("java.math.BigDecimal")
abstract class javamathBigDecimal {
  @AbstractsMethod("java.math.BigDecimal.abs((x$1: java.math.MathContext)java.math.BigDecimal)")
  def __abs(x1: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.add((x$1: java.math.BigDecimal, x$2: java.math.MathContext)java.math.BigDecimal)")
  def __add(x1: java.math.BigDecimal, x2: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.byteValueExact(()Byte)")
  def __byteValueExact(): Byte = {
    (42 : Byte)
  }
  @AbstractsMethod("java.math.BigDecimal.compareTo((x$1: java.math.BigDecimal)Int)")
  def __compareTo(x1: java.math.BigDecimal): Int = {
    42
  }
  @AbstractsMethod("java.math.BigDecimal.divideAndRemainder((x$1: java.math.BigDecimal, x$2: java.math.MathContext)Array[java.math.BigDecimal])")
  def __divideAndRemainder(x1: java.math.BigDecimal, x2: java.math.MathContext): Array[java.math.BigDecimal] = {
    new Array[java.math.BigDecimal](1)
  }
  @AbstractsMethod("java.math.BigDecimal.divideToIntegralValue((x$1: java.math.BigDecimal, x$2: java.math.MathContext)java.math.BigDecimal)")
  def __divideToIntegralValue(x1: java.math.BigDecimal, x2: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.divide((x$1: java.math.BigDecimal, x$2: java.math.MathContext)java.math.BigDecimal)")
  def __divide(x1: java.math.BigDecimal, x2: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.doubleValue(()Double)")
  def __doubleValue(): Double = {
    42.0d
  }
  @AbstractsMethod("java.math.BigDecimal.floatValue(()Float)")
  def __floatValue(): Float = {
    42.0f
  }
  @AbstractsMethod("java.math.BigDecimal.<init>((x$1: java.lang.String, x$2: java.math.MathContext)java.math.BigDecimal)")
  def ____init__(x1: java.lang.String, x2: java.math.MathContext): javamathBigDecimal = {
    this
  }
  @AbstractsMethod("java.math.BigDecimal.<init>((x$1: java.math.BigInteger, x$2: Int, x$3: java.math.MathContext)java.math.BigDecimal)")
  def ____init__(x1: java.math.BigInteger, x2: Int, x3: java.math.MathContext): javamathBigDecimal = {
    this
  }
  @AbstractsMethod("java.math.BigDecimal.<init>((x$1: java.math.BigInteger, x$2: java.math.MathContext)java.math.BigDecimal)")
  def ____init__(x1: java.math.BigInteger, x2: java.math.MathContext): javamathBigDecimal = {
    this
  }
  @AbstractsMethod("java.math.BigDecimal.<init>((x$1: Long, x$2: java.math.MathContext)java.math.BigDecimal)")
  def ____init__(x1: Long, x2: java.math.MathContext): javamathBigDecimal = {
    this
  }
  @AbstractsMethod("java.math.BigDecimal.intValueExact(()Int)")
  def __intValueExact(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigDecimal.intValue(()Int)")
  def __intValue(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigDecimal.longValueExact(()Long)")
  def __longValueExact(): Long = {
    42L
  }
  @AbstractsMethod("java.math.BigDecimal.longValue(()Long)")
  def __longValue(): Long = {
    42L
  }
  @AbstractsMethod("java.math.BigDecimal.max((x$1: java.math.BigDecimal)java.math.BigDecimal)")
  def __max(x1: java.math.BigDecimal): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.min((x$1: java.math.BigDecimal)java.math.BigDecimal)")
  def __min(x1: java.math.BigDecimal): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.multiply((x$1: java.math.BigDecimal, x$2: java.math.MathContext)java.math.BigDecimal)")
  def __multiply(x1: java.math.BigDecimal, x2: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.negate((x$1: java.math.MathContext)java.math.BigDecimal)")
  def __negate(x1: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.pow((x$1: Int, x$2: java.math.MathContext)java.math.BigDecimal)")
  def __pow(x1: Int, x2: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.precision(()Int)")
  def __precision(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigDecimal.remainder((x$1: java.math.BigDecimal, x$2: java.math.MathContext)java.math.BigDecimal)")
  def __remainder(x1: java.math.BigDecimal, x2: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.round((x$1: java.math.MathContext)java.math.BigDecimal)")
  def __round(x1: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.scale(()Int)")
  def __scale(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigDecimal.setScale((x$1: Int)java.math.BigDecimal)")
  def __setScale(x1: Int): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.setScale((x$1: Int, x$2: Int)java.math.BigDecimal)")
  def __setScale(x1: Int, x2: Int): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.shortValueExact(()Short)")
  def __shortValueExact(): Short = {
    (42 : Short)
  }
  @AbstractsMethod("java.math.BigDecimal.signum(()Int)")
  def __signum(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigDecimal.subtract((x$1: java.math.BigDecimal, x$2: java.math.MathContext)java.math.BigDecimal)")
  def __subtract(x1: java.math.BigDecimal, x2: java.math.MathContext): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.toBigIntegerExact(()java.math.BigInteger)")
  def __toBigIntegerExact(): java.math.BigInteger
  @AbstractsMethod("java.math.BigDecimal.toBigInteger(()java.math.BigInteger)")
  def __toBigInteger(): java.math.BigInteger
  @AbstractsMethod("java.math.BigDecimal.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.math.BigDecimal.ulp(()java.math.BigDecimal)")
  def __ulp(): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.valueOf((x$1: Double)java.math.BigDecimal)")
  def __valueOf(x1: Double): java.math.BigDecimal
  @AbstractsMethod("java.math.BigDecimal.valueOf((x$1: Long)java.math.BigDecimal)")
  def __valueOf(x1: Long): java.math.BigDecimal
}
