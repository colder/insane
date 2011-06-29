package insane
package predefined

import annotations._

@AbstractsClass("java.io.FileOutputStream")
abstract class javaioFileOutputStream {
  @AbstractsMethod("java.io.FileOutputStream.close(()Unit)")
  def __close(): Unit = {
    ()
  }
  @AbstractsMethod("java.io.FileOutputStream.getChannel(()java.nio.channels.FileChannel)")
  def __getChannel(): java.nio.channels.FileChannel
  @AbstractsMethod("java.io.FileOutputStream.<init>((x$1: java.io.File)java.io.FileOutputStream)")
  def ____init__(x1: java.io.File): javaioFileOutputStream = {
    this
  }
  @AbstractsMethod("java.io.FileOutputStream.<init>((x$1: java.io.File, x$2: Boolean)java.io.FileOutputStream)")
  def ____init__(x1: java.io.File, x2: Boolean): javaioFileOutputStream = {
    this
  }
  @AbstractsMethod("java.io.FileOutputStream.<init>((x$1: java.lang.String)java.io.FileOutputStream)")
  def ____init__(x1: java.lang.String): javaioFileOutputStream = {
    this
  }
}
