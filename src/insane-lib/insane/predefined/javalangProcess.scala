package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Process")
class javalangProcess {
  @AbstractsMethod("java.lang.Process.destroy(()Unit)")
  def destroy(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Process.exitValue(()Int)")
  def exitValue(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Process.getErrorStream(()java.io.InputStream)")
  def getErrorStream(): java.io.InputStream = {
    new java.io.InputStream()
  }
  @AbstractsMethod("java.lang.Process.getInputStream(()java.io.InputStream)")
  def getInputStream(): java.io.InputStream = {
    new java.io.InputStream()
  }
  @AbstractsMethod("java.lang.Process.getOutputStream(()java.io.OutputStream)")
  def getOutputStream(): java.io.OutputStream = {
    new java.io.OutputStream()
  }
  @AbstractsMethod("java.lang.Process.waitFor(()Int)")
  def waitFor(): Int = {
    42
  }
}
