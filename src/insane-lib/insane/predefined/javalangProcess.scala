package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Process")
class javalangProcess {
  @AbstractsMethod("java.lang.ProcessBuilder.command(()java.util.List)")
  def command() : java.util.List = { new java.util.List() }

  @AbstractsMethod("java.lang.ProcessBuilder.directory((x$1:java.io.File)java.lang.ProcessBuilder)")
  def directory(x1 : java.io.File) : java.lang.ProcessBuilder = { new java.lang.ProcessBuilder() }

  @AbstractsMethod("java.lang.ProcessBuilder.environment(()java.util.Map)")
  def environment() : java.util.Map = { new java.util.Map() }

  @AbstractsMethod("java.lang.ProcessBuilder.<init>((x$1:Array[java.lang.String])java.lang.ProcessBuilder)")
  def PLOPINIT(x1 : Array[java.lang.String]) : java.lang.ProcessBuilder = { new java.lang.ProcessBuilder() }

  @AbstractsMethod("java.lang.ProcessBuilder.redirectErrorStream(()Boolean)")
  def redirectErrorStream() : Boolean = { true }

  @AbstractsMethod("java.lang.ProcessBuilder.start(()java.lang.Process)")
  def start() : java.lang.Process = { new java.lang.Process() }

  @AbstractsMethod("java.lang.Process.destroy(()Unit)")
  def destroy() : Unit = { () }

  @AbstractsMethod("java.lang.Process.exitValue(()Int)")
  def exitValue() : Int = { 0 }

  @AbstractsMethod("java.lang.Process.getErrorStream(()java.io.InputStream)")
  def getErrorStream() : java.io.InputStream = { new java.io.InputStream() }

  @AbstractsMethod("java.lang.Process.getInputStream(()java.io.InputStream)")
  def getInputStream() : java.io.InputStream = { new java.io.InputStream() }

  @AbstractsMethod("java.lang.Process.getOutputStream(()java.io.OutputStream)")
  def getOutputStream() : java.io.OutputStream = { new java.io.OutputStream() }

  @AbstractsMethod("java.lang.Process.waitFor(()Int)")
  def waitFor() : Int = { 0 }

}
