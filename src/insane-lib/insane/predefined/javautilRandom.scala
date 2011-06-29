package insane
package predefined

import annotations._

@AbstractsClass("java.util.Random")
class javautilRandom {
  @AbstractsMethod("java.util.Random.<init>(()java.util.Random)")
  def ____init__(): javautilRandom = {
    this
  }
  @AbstractsMethod("java.util.Random.<init>((x$1: Long)java.util.Random)")
  def ____init__(x1: Long): javautilRandom = {
    this
  }
  @AbstractsMethod("java.util.Random.nextBoolean(()Boolean)")
  def __nextBoolean(): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Random.nextBytes((x$1: Array[Byte])Unit)")
  def __nextBytes(x1: Array[Byte]): Unit = {
    ()
  }
  @AbstractsMethod("java.util.Random.nextDouble(()Double)")
  def __nextDouble(): Double = {
    42.0d
  }
  @AbstractsMethod("java.util.Random.nextFloat(()Float)")
  def __nextFloat(): Float = {
    42.0f
  }
  @AbstractsMethod("java.util.Random.nextGaussian(()Double)")
  def __nextGaussian(): Double = {
    42.0d
  }
  @AbstractsMethod("java.util.Random.nextInt(()Int)")
  def __nextInt(): Int = {
    42
  }
  @AbstractsMethod("java.util.Random.nextInt((x$1: Int)Int)")
  def __nextInt(x1: Int): Int = {
    42
  }
  @AbstractsMethod("java.util.Random.nextLong(()Long)")
  def __nextLong(): Long = {
    42L
  }
  @AbstractsMethod("java.util.Random.setSeed((x$1: Long)Unit)")
  def __setSeed(x1: Long): Unit = {
    ()
  }
}
