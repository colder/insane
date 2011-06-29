package insane
package predefined

import annotations._

@AbstractsClass("java.math.BigInteger")
abstract class javamathBigInteger {
  @AbstractsMethod("java.math.BigInteger.abs(()java.math.BigInteger)")
  def __abs(): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.add((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __add(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.andNot((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __andNot(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.and((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __and(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.bitCount(()Int)")
  def __bitCount(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigInteger.bitLength(()Int)")
  def __bitLength(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigInteger.clearBit((x$1: Int)java.math.BigInteger)")
  def __clearBit(x1: Int): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.compareTo((x$1: java.math.BigInteger)Int)")
  def __compareTo(x1: java.math.BigInteger): Int = {
    42
  }
  @AbstractsMethod("java.math.BigInteger.divideAndRemainder((x$1: java.math.BigInteger)Array[java.math.BigInteger])")
  def __divideAndRemainder(x1: java.math.BigInteger): Array[java.math.BigInteger] = {
    new Array[java.math.BigInteger](1)
  }
  @AbstractsMethod("java.math.BigInteger.divide((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __divide(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.doubleValue(()Double)")
  def __doubleValue(): Double = {
    42.0d
  }
  @AbstractsMethod("java.math.BigInteger.flipBit((x$1: Int)java.math.BigInteger)")
  def __flipBit(x1: Int): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.floatValue(()Float)")
  def __floatValue(): Float = {
    42.0f
  }
  @AbstractsMethod("java.math.BigInteger.gcd((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __gcd(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.getLowestSetBit(()Int)")
  def __getLowestSetBit(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigInteger.<init>((x$1: Array[Byte])java.math.BigInteger)")
  def ____init__(x1: Array[Byte]): javamathBigInteger = {
    this
  }
  @AbstractsMethod("java.math.BigInteger.<init>((x$1: Int, x$2: Array[Byte])java.math.BigInteger)")
  def ____init__(x1: Int, x2: Array[Byte]): javamathBigInteger = {
    this
  }
  @AbstractsMethod("java.math.BigInteger.<init>((x$1: Int, x$2: Int, x$3: java.util.Random)java.math.BigInteger)")
  def ____init__(x1: Int, x2: Int, x3: java.util.Random): javamathBigInteger = {
    this
  }
  @AbstractsMethod("java.math.BigInteger.<init>((x$1: Int, x$2: java.util.Random)java.math.BigInteger)")
  def ____init__(x1: Int, x2: java.util.Random): javamathBigInteger = {
    this
  }
  @AbstractsMethod("java.math.BigInteger.<init>((x$1: java.lang.String)java.math.BigInteger)")
  def ____init__(x1: java.lang.String): javamathBigInteger = {
    this
  }
  @AbstractsMethod("java.math.BigInteger.<init>((x$1: java.lang.String, x$2: Int)java.math.BigInteger)")
  def ____init__(x1: java.lang.String, x2: Int): javamathBigInteger = {
    this
  }
  @AbstractsMethod("java.math.BigInteger.intValue(()Int)")
  def __intValue(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigInteger.isProbablePrime((x$1: Int)Boolean)")
  def __isProbablePrime(x1: Int): Boolean = {
    true
  }
  @AbstractsMethod("java.math.BigInteger.longValue(()Long)")
  def __longValue(): Long = {
    42L
  }
  @AbstractsMethod("java.math.BigInteger.max((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __max(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.min((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __min(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.modInverse((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __modInverse(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.modPow((x$1: java.math.BigInteger, x$2: java.math.BigInteger)java.math.BigInteger)")
  def __modPow(x1: java.math.BigInteger, x2: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.mod((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __mod(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.multiply((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __multiply(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.negate(()java.math.BigInteger)")
  def __negate(): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.not(()java.math.BigInteger)")
  def __not(): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.or((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __or(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.pow((x$1: Int)java.math.BigInteger)")
  def __pow(x1: Int): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.probablePrime((x$1: Int, x$2: java.util.Random)java.math.BigInteger)")
  def __probablePrime(x1: Int, x2: java.util.Random): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.remainder((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __remainder(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.setBit((x$1: Int)java.math.BigInteger)")
  def __setBit(x1: Int): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.shiftLeft((x$1: Int)java.math.BigInteger)")
  def __shiftLeft(x1: Int): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.shiftRight((x$1: Int)java.math.BigInteger)")
  def __shiftRight(x1: Int): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.signum(()Int)")
  def __signum(): Int = {
    42
  }
  @AbstractsMethod("java.math.BigInteger.subtract((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __subtract(x1: java.math.BigInteger): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.testBit((x$1: Int)Boolean)")
  def __testBit(x1: Int): Boolean = {
    true
  }
  @AbstractsMethod("java.math.BigInteger.toByteArray(()Array[Byte])")
  def __toByteArray(): Array[Byte] = {
    new Array[Byte](1)
  }
  @AbstractsMethod("java.math.BigInteger.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.math.BigInteger.toString((x$1: Int)java.lang.String)")
  def __toString(x1: Int): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.math.BigInteger.valueOf((x$1: Long)java.math.BigInteger)")
  def __valueOf(x1: Long): java.math.BigInteger
  @AbstractsMethod("java.math.BigInteger.xor((x$1: java.math.BigInteger)java.math.BigInteger)")
  def __xor(x1: java.math.BigInteger): java.math.BigInteger
}
