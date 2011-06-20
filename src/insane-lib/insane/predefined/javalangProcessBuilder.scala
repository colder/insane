package insane
package predefined

import annotations._

@AbstractsClass("java.lang.ProcessBuilder")
abstract class javalangProcessBuilder {
  @AbstractsMethod("java.lang.ProcessBuilder.command(()java.util.List)")
  def command(): java.util.List[_]
  @AbstractsMethod("java.lang.ProcessBuilder.directory((x$1: java.io.File)java.lang.ProcessBuilder)")
  def directory(x1: java.io.File): java.lang.ProcessBuilder
  @AbstractsMethod("java.lang.ProcessBuilder.environment(()java.util.Map)")
  def environment(): java.util.Map[_,_]
  @AbstractsMethod("java.lang.ProcessBuilder.<init>((x$1: Array[java.lang.String])java.lang.ProcessBuilder)")
  def __init__(x1: Array[java.lang.String]): javalangProcessBuilder = {
    this
  }
  @AbstractsMethod("java.lang.ProcessBuilder.redirectErrorStream(()Boolean)")
  def redirectErrorStream(): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.ProcessBuilder.start(()java.lang.Process)")
  def start(): java.lang.Process
}
