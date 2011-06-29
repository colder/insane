package insane
package predefined

import annotations._

@AbstractsClass("java.util.Stack")
abstract class javautilStack {
  @AbstractsMethod("java.util.Stack.<init>(()java.util.Stack)")
  def ____init__(): javautilStack = {
    this
  }
  @AbstractsMethod("java.util.Stack.peek(()java.lang.Object)")
  def __peek(): java.lang.Object
  @AbstractsMethod("java.util.Stack.pop(()java.lang.Object)")
  def __pop(): java.lang.Object
  @AbstractsMethod("java.util.Stack.push((x$1: java.lang.Object)java.lang.Object)")
  def __push(x1: java.lang.Object): java.lang.Object
}
