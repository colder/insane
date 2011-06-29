package insane
package predefined

import annotations._

@AbstractsClass("java.io.ObjectOutputStream")
class javaioObjectOutputStream {
  @AbstractsMethod("java.io.ObjectOutputStream.close(()Unit)")
  def __close(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.ObjectOutputStream.defaultWriteObject(()Unit)")
  def __defaultWriteObject(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.ObjectOutputStream.<init>((x$1: java.io.OutputStream)java.io.ObjectOutputStream)")
  def ____init__(x1: java.io.OutputStream): javaioObjectOutputStream = {
    this
  }
  @AbstractsMethod("java.io.ObjectOutputStream.writeBoolean((x$1: Boolean)Unit)")
  def __writeBoolean(x1: Boolean): Unit = {
    ()
  }
  @AbstractsMethod("java.io.ObjectOutputStream.writeInt((x$1: Int)Unit)")
  def __writeInt(x1: Int): Unit = {
    ()
  }
  @AbstractsMethod("java.io.ObjectOutputStream.writeObject((x$1: java.lang.Object)Unit)")
  def __writeObject(x1: java.lang.Object): Unit = {
    ()
  }
}
