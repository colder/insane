package insane
package predefined

import annotations._

@AbstractsClass("java.io.File")
class javaioFile {
  @AbstractsMethod("java.io.File.createNewFile(()Boolean)")
  def createNewFile() : Boolean = { true }

  @AbstractsMethod("java.io.File.delete(()Boolean)")
  def delete() : Boolean = { true }

  @AbstractsMethod("java.io.File.exists(()Boolean)")
  def exists() : Boolean = { true }

  @AbstractsMethod("java.io.File.getAbsolutePath(()java.lang.String)")
  def getAbsolutePath() : java.lang.String = { "" }

  @AbstractsMethod("java.io.File.getName(()java.lang.String)")
  def getName() : java.lang.String = { "" }

  @AbstractsMethod("java.io.File.getParent(()java.lang.String)")
  def getParent() : java.lang.String = { "" }

  @AbstractsMethod("java.io.File.<init>((x$1:java.lang.String)java.io.File)")
  def PLOPINIT(x1 : java.lang.String) : java.io.File = { new java.io.File() }

  @AbstractsMethod("java.io.File.<init>((x$1:java.lang.String, x$2:java.lang.String)java.io.File)")
  def PLOPINIT(x1 : java.lang.String, x2 : java.lang.String) : java.io.File = { new java.io.File() }

  @AbstractsMethod("java.io.File.<init>((x$1:java.net.URI)java.io.File)")
  def PLOPINIT(x1 : java.net.URI) : java.io.File = { new java.io.File() }

  @AbstractsMethod("java.io.FileInputStream.close(()Unit)")
  def close() : Unit = { () }

  @AbstractsMethod("java.io.FileInputStream.<init>((x$1:java.io.FileDescriptor)java.io.FileInputStream)")
  def PLOPINIT(x1 : java.io.FileDescriptor) : java.io.FileInputStream = { new java.io.FileInputStream() }

  @AbstractsMethod("java.io.FileInputStream.<init>((x$1:java.io.File)java.io.FileInputStream)")
  def PLOPINIT(x1 : java.io.File) : java.io.FileInputStream = { new java.io.FileInputStream() }

  @AbstractsMethod("java.io.FileInputStream.<init>((x$1:java.lang.String)java.io.FileInputStream)")
  def PLOPINIT(x1 : java.lang.String) : java.io.FileInputStream = { new java.io.FileInputStream() }

  @AbstractsMethod("java.io.File.lastModified(()Long)")
  def lastModified() : Long = { 0L }

  @AbstractsMethod("java.io.FileOutputStream.close(()Unit)")
  def close() : Unit = { () }

  @AbstractsMethod("java.io.FileOutputStream.getChannel(()java.nio.channels.FileChannel)")
  def getChannel() : java.nio.channels.FileChannel = { new java.nio.channels.FileChannel() }

  @AbstractsMethod("java.io.FileOutputStream.<init>((x$1:java.io.File)java.io.FileOutputStream)")
  def PLOPINIT(x1 : java.io.File) : java.io.FileOutputStream = { new java.io.FileOutputStream() }

  @AbstractsMethod("java.io.FileOutputStream.<init>((x$1:java.io.File, x$2:Boolean)java.io.FileOutputStream)")
  def PLOPINIT(x1 : java.io.File, x2 : Boolean) : java.io.FileOutputStream = { new java.io.FileOutputStream() }

  @AbstractsMethod("java.io.FileOutputStream.<init>((x$1:java.lang.String)java.io.FileOutputStream)")
  def PLOPINIT(x1 : java.lang.String) : java.io.FileOutputStream = { new java.io.FileOutputStream() }

  @AbstractsMethod("java.io.FileReader.<init>((x$1:java.io.File)java.io.FileReader)")
  def PLOPINIT(x1 : java.io.File) : java.io.FileReader = { new java.io.FileReader() }

}
