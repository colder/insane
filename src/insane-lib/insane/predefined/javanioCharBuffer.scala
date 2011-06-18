package insane
package predefined

import annotations._

@AbstractsClass("java.nio.CharBuffer")
class javanioCharBuffer {
  @AbstractsMethod("java.nio.CharBuffer.get((x$1: Array[Char])java.nio.CharBuffer)")
  def get(x1: Array[Char]): java.nio.CharBuffer = {
    new java.nio.CharBuffer()
  }
  @AbstractsMethod("java.nio.CharBuffer.wrap((x$1: java.lang.CharSequence)java.nio.CharBuffer)")
  def wrap(x1: java.lang.CharSequence): java.nio.CharBuffer = {
    new java.nio.CharBuffer()
  }
}
