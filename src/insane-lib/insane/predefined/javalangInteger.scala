package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Integer")
class javalangInteger {
  @AbstractsMethod("java.lang.Integer.bitCount((x$1: Int)Int)")
  def __bitCount(x1: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.Integer.intValue(()Int)")
  def __intValue(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Integer.parseInt((x$1: java.lang.String)Int)")
  def __parseInt(x1: java.lang.String): Int = {
    42
  }
  @AbstractsMethod("java.lang.Integer.parseInt((x$1: java.lang.String, x$2: Int)Int)")
  def __parseInt(x1: java.lang.String, x2: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.Integer.reverseBytes((x$1: Int)Int)")
  def __reverseBytes(x1: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.Integer.rotateLeft((x$1: Int, x$2: Int)Int)")
  def __rotateLeft(x1: Int, x2: Int): Int = {
    42
  }
  @AbstractsMethod("java.lang.Integer.toBinaryString((x$1: Int)java.lang.String)")
  def __toBinaryString(x1: Int): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Integer.toHexString((x$1: Int)java.lang.String)")
  def __toHexString(x1: Int): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Integer.toOctalString((x$1: Int)java.lang.String)")
  def __toOctalString(x1: Int): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Integer.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Integer.valueOf((x$1: Int)java.lang.Integer)")
  def __valueOf(x1: Int): java.lang.Integer = {
    new java.lang.Integer(42)
  }
}
