package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Byte")
class javalangByte {
  @AbstractsMethod("java.lang.Byte.byteValue(()Byte)")
  def byteValue(): Byte = {
    42
  }
  @AbstractsMethod("java.lang.Byte.parseByte((x$1: java.lang.String)Byte)")
  def parseByte(x1: java.lang.String): Byte = {
    42
  }
  @AbstractsMethod("java.lang.Byte.valueOf((x$1: Byte)java.lang.Byte)")
  def valueOf(x1: Byte): java.lang.Byte = {
    new java.lang.Byte()
  }
}
