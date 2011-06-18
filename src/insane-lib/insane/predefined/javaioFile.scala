package insane
package predefined

import annotations._

@AbstractsClass("java.io.File")
class javaioFile {
  @AbstractsMethod("java.io.File.createNewFile(()Boolean)")
  def createNewFile(): Boolean = {
    true
  }
  @AbstractsMethod("java.io.File.delete(()Boolean)")
  def delete(): Boolean = {
    true
  }
  @AbstractsMethod("java.io.File.exists(()Boolean)")
  def exists(): Boolean = {
    true
  }
  @AbstractsMethod("java.io.File.getAbsolutePath(()java.lang.String)")
  def getAbsolutePath(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.io.File.getName(()java.lang.String)")
  def getName(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.io.File.getParent(()java.lang.String)")
  def getParent(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.io.File.<init>((x$1: java.lang.String)java.io.File)")
  def __init__(x1: java.lang.String): javaioFile = {
    this
  }
  @AbstractsMethod("java.io.File.<init>((x$1: java.lang.String, x$2: java.lang.String)java.io.File)")
  def __init__(x1: java.lang.String, x2: java.lang.String): javaioFile = {
    this
  }
  @AbstractsMethod("java.io.File.<init>((x$1: java.net.URI)java.io.File)")
  def __init__(x1: java.net.URI): javaioFile = {
    this
  }
  @AbstractsMethod("java.io.File.lastModified(()Long)")
  def lastModified(): Long = {
    42L
  }
}
