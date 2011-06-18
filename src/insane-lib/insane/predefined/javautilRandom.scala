package insane
package predefined

import annotations._

@AbstractsClass("java.util.Random")
class javautilRandom {
  @AbstractsMethod("java.util.Random.<init>(()java.util.Random)")
  def __init__(): java.util.Random = {
    new java.util.Random()
  }
  @AbstractsMethod("java.util.Random.<init>((x$1: Long)java.util.Random)")
  def __init__(x1: Long): java.util.Random = {
    new java.util.Random()
  }
  @AbstractsMethod("java.util.Random.nextBoolean(()Boolean)")
  def nextBoolean(): Boolean = {
    true
  }
  @AbstractsMethod("java.util.Random.nextBytes((x$1: Array[Byte])Unit)")
  def nextBytes(x1: Array[Byte]): Unit = {
    ()
  }
  @AbstractsMethod("java.util.Random.nextDouble(()Double)")
  def nextDouble(): Double = {
    42.0d
  }
  @AbstractsMethod("java.util.Random.nextFloat(()Float)")
  def nextFloat(): Float = {
    42.0f
  }
  @AbstractsMethod("java.util.Random.nextGaussian(()Double)")
  def nextGaussian(): Double = {
    42.0d
  }
  @AbstractsMethod("java.util.Random.nextInt(()Int)")
  def nextInt(): Int = {
    42
  }
  @AbstractsMethod("java.util.Random.nextInt((x$1: Int)Int)")
  def nextInt(x1: Int): Int = {
    42
  }
  @AbstractsMethod("java.util.Random.nextLong(()Long)")
  def nextLong(): Long = {
    42L
  }
  @AbstractsMethod("java.util.Random.setSeed((x$1: Long)Unit)")
  def setSeed(x1: Long): Unit = {
    ()
  }
}
