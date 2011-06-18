package insane
package predefined

import annotations._

@AbstractsClass("java.io.FileOutputStream")
class javaioFileOutputStream {
  @AbstractsMethod("java.io.FileOutputStream.close(()Unit)")
  def close(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.FileOutputStream.getChannel(()java.nio.channels.FileChannel)")
  def getChannel(): java.nio.channels.FileChannel = {
    new java.nio.channels.FileChannel()
  }
  @AbstractsMethod("java.io.FileOutputStream.<init>((x$1: java.io.File)java.io.FileOutputStream)")
  def __init__(x1: java.io.File): java.io.FileOutputStream = {
    new java.io.FileOutputStream()
  }
  @AbstractsMethod("java.io.FileOutputStream.<init>((x$1: java.io.File, x$2: Boolean)java.io.FileOutputStream)")
  def __init__(x1: java.io.File, x2: Boolean): java.io.FileOutputStream = {
    new java.io.FileOutputStream()
  }
  @AbstractsMethod("java.io.FileOutputStream.<init>((x$1: java.lang.String)java.io.FileOutputStream)")
  def __init__(x1: java.lang.String): java.io.FileOutputStream = {
    new java.io.FileOutputStream()
  }
}
