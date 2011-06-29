package insane
package predefined

import annotations._

@AbstractsClass("java.io.FileInputStream")
class javaioFileInputStream {
  @AbstractsMethod("java.io.FileInputStream.close(()Unit)")
  def __close(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.FileInputStream.<init>((x$1: java.io.FileDescriptor)java.io.FileInputStream)")
  def ____init__(x1: java.io.FileDescriptor): javaioFileInputStream = {
    this
  }
  @AbstractsMethod("java.io.FileInputStream.<init>((x$1: java.io.File)java.io.FileInputStream)")
  def ____init__(x1: java.io.File): javaioFileInputStream = {
    this
  }
  @AbstractsMethod("java.io.FileInputStream.<init>((x$1: java.lang.String)java.io.FileInputStream)")
  def ____init__(x1: java.lang.String): javaioFileInputStream = {
    this
  }
}
