package insane
package predefined

import annotations._

@AbstractsClass("java.nio.charset.CharsetDecoder")
abstract class javaniocharsetCharsetDecoder {
  @AbstractsMethod("java.nio.charset.CharsetDecoder.charset(()java.nio.charset.Charset)")
  def __charset(): java.nio.charset.Charset
  @AbstractsMethod("java.nio.charset.CharsetDecoder.onMalformedInput((x$1: java.nio.charset.CodingErrorAction)java.nio.charset.CharsetDecoder)")
  def __onMalformedInput(x1: java.nio.charset.CodingErrorAction): java.nio.charset.CharsetDecoder
  @AbstractsMethod("java.nio.charset.CharsetDecoder.onUnmappableCharacter((x$1: java.nio.charset.CodingErrorAction)java.nio.charset.CharsetDecoder)")
  def __onUnmappableCharacter(x1: java.nio.charset.CodingErrorAction): java.nio.charset.CharsetDecoder
  @AbstractsMethod("java.nio.charset.CharsetDecoder.replaceWith((x$1: java.lang.String)java.nio.charset.CharsetDecoder)")
  def __replaceWith(x1: java.lang.String): java.nio.charset.CharsetDecoder
}
