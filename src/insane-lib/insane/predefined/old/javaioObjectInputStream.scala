package insane
package predefined

import annotations._

@AbstractsClass("java.io.ObjectInputStream")
abstract class javaioObjectInputStream {
  @AbstractsMethod("java.io.ObjectInputStream.close(()Unit)")
  def __close(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.ObjectInputStream.defaultReadObject(()Unit)")
  def __defaultReadObject(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.ObjectInputStream.<init>((x$1: java.io.InputStream)java.io.ObjectInputStream)")
  def ____init__(x1: java.io.InputStream): javaioObjectInputStream = {
    this
  }
  @AbstractsMethod("java.io.ObjectInputStream.readBoolean(()Boolean)")
  def __readBoolean(): Boolean = {
    true
  }
  @AbstractsMethod("java.io.ObjectInputStream.readInt(()Int)")
  def __readInt(): Int = {
    42
  }
  @AbstractsMethod("java.io.ObjectInputStream.readObject(()java.lang.Object)")
  def __readObject(): java.lang.Object
}
