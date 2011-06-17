package insane
package predefined

import annotations._

@AbstractsClass("java.nio.charset.Charset")
class javaniocharsetCharset {
  @AbstractsMethod("java.nio.charset.CharsetDecoder.charset(()java.nio.charset.Charset)")
  def charset() : java.nio.charset.Charset = { new java.nio.charset.Charset() }

  @AbstractsMethod("java.nio.charset.CharsetDecoder.onMalformedInput((x$1:java.nio.charset.CodingErrorAction)java.nio.charset.CharsetDecoder)")
  def onMalformedInput(x1 : java.nio.charset.CodingErrorAction) : java.nio.charset.CharsetDecoder = { new java.nio.charset.CharsetDecoder() }

  @AbstractsMethod("java.nio.charset.CharsetDecoder.onUnmappableCharacter((x$1:java.nio.charset.CodingErrorAction)java.nio.charset.CharsetDecoder)")
  def onUnmappableCharacter(x1 : java.nio.charset.CodingErrorAction) : java.nio.charset.CharsetDecoder = { new java.nio.charset.CharsetDecoder() }

  @AbstractsMethod("java.nio.charset.CharsetDecoder.replaceWith((x$1:java.lang.String)java.nio.charset.CharsetDecoder)")
  def replaceWith(x1 : java.lang.String) : java.nio.charset.CharsetDecoder = { new java.nio.charset.CharsetDecoder() }

  @AbstractsMethod("java.nio.charset.Charset.decode((x$1:java.nio.ByteBuffer)java.nio.CharBuffer)")
  def decode(x1 : java.nio.ByteBuffer) : java.nio.CharBuffer = { new java.nio.CharBuffer() }

  @AbstractsMethod("java.nio.charset.Charset.defaultCharset(()java.nio.charset.Charset)")
  def defaultCharset() : java.nio.charset.Charset = { new java.nio.charset.Charset() }

  @AbstractsMethod("java.nio.charset.CharsetEncoder.onMalformedInput((x$1:java.nio.charset.CodingErrorAction)java.nio.charset.CharsetEncoder)")
  def onMalformedInput(x1 : java.nio.charset.CodingErrorAction) : java.nio.charset.CharsetEncoder = { new java.nio.charset.CharsetEncoder() }

  @AbstractsMethod("java.nio.charset.CharsetEncoder.onUnmappableCharacter((x$1:java.nio.charset.CodingErrorAction)java.nio.charset.CharsetEncoder)")
  def onUnmappableCharacter(x1 : java.nio.charset.CodingErrorAction) : java.nio.charset.CharsetEncoder = { new java.nio.charset.CharsetEncoder() }

  @AbstractsMethod("java.nio.charset.CharsetEncoder.replaceWith((x$1:Array[Byte])java.nio.charset.CharsetEncoder)")
  def replaceWith(x1 : Array[Byte]) : java.nio.charset.CharsetEncoder = { new java.nio.charset.CharsetEncoder() }

  @AbstractsMethod("java.nio.charset.Charset.encode((x$1:java.nio.CharBuffer)java.nio.ByteBuffer)")
  def encode(x1 : java.nio.CharBuffer) : java.nio.ByteBuffer = { new java.nio.ByteBuffer() }

  @AbstractsMethod("java.nio.charset.Charset.forName((x$1:java.lang.String)java.nio.charset.Charset)")
  def forName(x1 : java.lang.String) : java.nio.charset.Charset = { new java.nio.charset.Charset() }

  @AbstractsMethod("java.nio.charset.Charset.name(()java.lang.String)")
  def name() : java.lang.String = { "" }

  @AbstractsMethod("java.nio.charset.Charset.newDecoder(()java.nio.charset.CharsetDecoder)")
  def newDecoder() : java.nio.charset.CharsetDecoder = { new java.nio.charset.CharsetDecoder() }

  @AbstractsMethod("java.nio.charset.Charset.newEncoder(()java.nio.charset.CharsetEncoder)")
  def newEncoder() : java.nio.charset.CharsetEncoder = { new java.nio.charset.CharsetEncoder() }

}
