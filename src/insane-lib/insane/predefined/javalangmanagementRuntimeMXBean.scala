package insane
package predefined

import annotations._

@AbstractsClass("java.lang.management.RuntimeMXBean")
class javalangmanagementRuntimeMXBean {
  @AbstractsMethod("java.lang.management.RuntimeMXBean.getInputArguments(()java.util.List)")
  def getInputArguments() : java.util.List = { new java.util.List() }

}
