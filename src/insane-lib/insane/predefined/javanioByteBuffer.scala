package insane
package predefined

import annotations._

@AbstractsClass("java.nio.ByteBuffer")
class javanioByteBuffer {
  @AbstractsMethod("java.nio.ByteBuffer.get((x$1:Array[Byte])java.nio.ByteBuffer)")
  def get(x1 : Array[Byte]) : java.nio.ByteBuffer = { new java.nio.ByteBuffer() }

  @AbstractsMethod("java.nio.ByteBuffer.wrap((x$1:Array[Byte])java.nio.ByteBuffer)")
  def wrap(x1 : Array[Byte]) : java.nio.ByteBuffer = { new java.nio.ByteBuffer() }

}
