package insane
package predefined

import annotations._

@AbstractsClass("java.io.File")
class javaioFile {
  @AbstractsMethod("java.io.File.createNewFile(()Boolean)")
  def __createNewFile(): Boolean = {
    true
  }
  @AbstractsMethod("java.io.File.delete(()Boolean)")
  def __delete(): Boolean = {
    true
  }
  @AbstractsMethod("java.io.File.exists(()Boolean)")
  def __exists(): Boolean = {
    true
  }
  @AbstractsMethod("java.io.File.getAbsolutePath(()java.lang.String)")
  def __getAbsolutePath(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.io.File.getName(()java.lang.String)")
  def __getName(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.io.File.getParent(()java.lang.String)")
  def __getParent(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.io.File.<init>((x$1: java.lang.String)java.io.File)")
  def ____init__(x1: java.lang.String): javaioFile = {
    this
  }
  @AbstractsMethod("java.io.File.<init>((x$1: java.lang.String, x$2: java.lang.String)java.io.File)")
  def ____init__(x1: java.lang.String, x2: java.lang.String): javaioFile = {
    this
  }
  @AbstractsMethod("java.io.File.<init>((x$1: java.net.URI)java.io.File)")
  def ____init__(x1: java.net.URI): javaioFile = {
    this
  }
  @AbstractsMethod("java.io.File.lastModified(()Long)")
  def __lastModified(): Long = {
    42L
  }
}
