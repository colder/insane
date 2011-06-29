package insane
package predefined

import annotations._

@AbstractsClass("java.lang.management.RuntimeMXBean")
abstract class javalangmanagementRuntimeMXBean {
  @AbstractsMethod("java.lang.management.RuntimeMXBean.getInputArguments(()java.util.List)")
  def __getInputArguments(): java.util.List[_]
}
