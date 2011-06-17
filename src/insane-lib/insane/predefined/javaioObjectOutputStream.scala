package insane
package predefined

import annotations._

@AbstractsClass("java.io.ObjectOutputStream")
class javaioObjectOutputStream {
  @AbstractsMethod("java.io.ObjectOutputStream.close(()Unit)")
  def close() : Unit = { () }

  @AbstractsMethod("java.io.ObjectOutputStream.defaultWriteObject(()Unit)")
  def defaultWriteObject() : Unit = { () }

  @AbstractsMethod("java.io.ObjectOutputStream.<init>((x$1:java.io.OutputStream)java.io.ObjectOutputStream)")
  def PLOPINIT(x1 : java.io.OutputStream) : java.io.ObjectOutputStream = { new java.io.ObjectOutputStream() }

  @AbstractsMethod("java.io.ObjectOutputStream.writeBoolean((x$1:Boolean)Unit)")
  def writeBoolean(x1 : Boolean) : Unit = { () }

  @AbstractsMethod("java.io.ObjectOutputStream.writeInt((x$1:Int)Unit)")
  def writeInt(x1 : Int) : Unit = { () }

  @AbstractsMethod("java.io.ObjectOutputStream.writeObject((x$1:java.lang.Object)Unit)")
  def writeObject(x1 : java.lang.Object) : Unit = { () }

}
