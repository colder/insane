package insane
package predefined

import annotations._

@AbstractsClass("java.nio.Buffer")
class javanioBuffer {
  @AbstractsMethod("java.nio.Buffer.remaining(()Int)")
  def remaining(): Int = {
    42
  }
}
