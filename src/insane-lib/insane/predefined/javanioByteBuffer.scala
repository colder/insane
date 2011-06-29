package insane
package predefined

import annotations._

@AbstractsClass("java.nio.ByteBuffer")
abstract class javanioByteBuffer {
  @AbstractsMethod("java.nio.ByteBuffer.get((x$1: Array[Byte])java.nio.ByteBuffer)")
  def __get(x1: Array[Byte]): java.nio.ByteBuffer
  @AbstractsMethod("java.nio.ByteBuffer.wrap((x$1: Array[Byte])java.nio.ByteBuffer)")
  def __wrap(x1: Array[Byte]): java.nio.ByteBuffer
}
