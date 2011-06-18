package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Appendable")
class javalangAppendable {
  @AbstractsMethod("java.lang.Appendable.append((x$1: java.lang.CharSequence)java.lang.Appendable)")
  def append(x1: java.lang.CharSequence): java.lang.Appendable = {
    new java.lang.Appendable()
  }
}
