package insane
package predefined

import annotations._

@AbstractsClass("java.math.BigInteger")
class javamathBigInteger {
  @AbstractsMethod("java.math.BigInteger.abs(()java.math.BigInteger)")
  def abs(): java.math.BigInteger = {
    new java.math.BigInteger("42")
  }

  // ...

  @AbstractsMethod("java.math.BigInteger.valueOf((x$1: Long)java.math.BigInteger)")
  def valueOf(x1: Long): java.math.BigInteger = {
    new java.math.BigInteger(x1)
  }
}
