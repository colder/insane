package insane
package predefined

import annotations._

@AbstractsClass("java.nio.charset.Charset")
abstract class javaniocharsetCharset {
  @AbstractsMethod("java.nio.charset.Charset.decode((x$1: java.nio.ByteBuffer)java.nio.CharBuffer)")
  def decode(x1: java.nio.ByteBuffer): java.nio.CharBuffer
  @AbstractsMethod("java.nio.charset.Charset.defaultCharset(()java.nio.charset.Charset)")
  def defaultCharset(): java.nio.charset.Charset
  @AbstractsMethod("java.nio.charset.Charset.encode((x$1: java.nio.CharBuffer)java.nio.ByteBuffer)")
  def encode(x1: java.nio.CharBuffer): java.nio.ByteBuffer
  @AbstractsMethod("java.nio.charset.Charset.forName((x$1: java.lang.String)java.nio.charset.Charset)")
  def forName(x1: java.lang.String): java.nio.charset.Charset
  @AbstractsMethod("java.nio.charset.Charset.name(()java.lang.String)")
  def name(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.nio.charset.Charset.newDecoder(()java.nio.charset.CharsetDecoder)")
  def newDecoder(): java.nio.charset.CharsetDecoder
  @AbstractsMethod("java.nio.charset.Charset.newEncoder(()java.nio.charset.CharsetEncoder)")
  def newEncoder(): java.nio.charset.CharsetEncoder
}
