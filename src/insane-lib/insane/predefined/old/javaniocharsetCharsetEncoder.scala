package insane
package predefined

import annotations._

@AbstractsClass("java.nio.charset.CharsetEncoder")
abstract class javaniocharsetCharsetEncoder {
  @AbstractsMethod("java.nio.charset.CharsetEncoder.onMalformedInput((x$1: java.nio.charset.CodingErrorAction)java.nio.charset.CharsetEncoder)")
  def __onMalformedInput(x1: java.nio.charset.CodingErrorAction): java.nio.charset.CharsetEncoder
  @AbstractsMethod("java.nio.charset.CharsetEncoder.onUnmappableCharacter((x$1: java.nio.charset.CodingErrorAction)java.nio.charset.CharsetEncoder)")
  def __onUnmappableCharacter(x1: java.nio.charset.CodingErrorAction): java.nio.charset.CharsetEncoder
  @AbstractsMethod("java.nio.charset.CharsetEncoder.replaceWith((x$1: Array[Byte])java.nio.charset.CharsetEncoder)")
  def __replaceWith(x1: Array[Byte]): java.nio.charset.CharsetEncoder
}
