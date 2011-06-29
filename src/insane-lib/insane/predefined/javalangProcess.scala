package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Process")
abstract class javalangProcess {
  @AbstractsMethod("java.lang.Process.destroy(()Unit)")
  def __destroy(): Unit = {
    ()
  }
  @AbstractsMethod("java.lang.Process.exitValue(()Int)")
  def __exitValue(): Int = {
    42
  }
  @AbstractsMethod("java.lang.Process.getErrorStream(()java.io.InputStream)")
  def __getErrorStream(): java.io.InputStream
  @AbstractsMethod("java.lang.Process.getInputStream(()java.io.InputStream)")
  def __getInputStream(): java.io.InputStream
  @AbstractsMethod("java.lang.Process.getOutputStream(()java.io.OutputStream)")
  def __getOutputStream(): java.io.OutputStream
  @AbstractsMethod("java.lang.Process.waitFor(()Int)")
  def __waitFor(): Int = {
    42
  }
}
