package insane
package predefined

import annotations._

@AbstractsClass("java.lang.management.ManagementFactory")
class javalangmanagementManagementFactory {
  @AbstractsMethod("java.lang.management.ManagementFactory.getRuntimeMXBean(()java.lang.management.RuntimeMXBean)")
  def getRuntimeMXBean(): java.lang.management.RuntimeMXBean = {
    new java.lang.management.RuntimeMXBean()
  }
}
