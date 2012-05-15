package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Byte")
class javalangByte {
  @AbstractsMethod("java.lang.Byte.byteValue(()Byte)")
  def __byteValue(): Byte = {
    (42 : Byte)
  }
  @AbstractsMethod("java.lang.Byte.<init>((x$1: Byte)java.lang.Byte)")
  def ____init__(x1: Byte): javalangByte = {
    this
  }
  @AbstractsMethod("java.lang.Byte.parseByte((x$1: java.lang.String)Byte)")
  def __parseByte(x1: java.lang.String): Byte = {
    (42 : Byte)
  }
  @AbstractsMethod("java.lang.Byte.valueOf((x$1: Byte)java.lang.Byte)")
  def __valueOf(x1: Byte): java.lang.Byte = {
    new java.lang.Byte(42 : Byte)
  }
}
