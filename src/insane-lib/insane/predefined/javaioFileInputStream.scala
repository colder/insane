package insane
package predefined

import annotations._

@AbstractsClass("java.io.FileInputStream")
class javaioFileInputStream {
  @AbstractsMethod("java.io.FileInputStream.close(()Unit)")
  def close() : Unit = { () }

  @AbstractsMethod("java.io.FileInputStream.<init>((x$1:java.io.FileDescriptor)java.io.FileInputStream)")
  def PLOPINIT(x1 : java.io.FileDescriptor) : java.io.FileInputStream = { new java.io.FileInputStream() }

  @AbstractsMethod("java.io.FileInputStream.<init>((x$1:java.io.File)java.io.FileInputStream)")
  def PLOPINIT(x1 : java.io.File) : java.io.FileInputStream = { new java.io.FileInputStream() }

  @AbstractsMethod("java.io.FileInputStream.<init>((x$1:java.lang.String)java.io.FileInputStream)")
  def PLOPINIT(x1 : java.lang.String) : java.io.FileInputStream = { new java.io.FileInputStream() }

}
