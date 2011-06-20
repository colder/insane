package insane
package predefined

import annotations._

@AbstractsClass("scala.math.ScalaNumber")
abstract class scalamathScalaNumber {
  @AbstractsMethod("scala.math.ScalaNumber.<init>(()scala.math.ScalaNumber)")
  def __init__(): scalamathScalaNumber = {
    this
  }
  @AbstractsMethod("scala.math.ScalaNumber.isWhole(()Boolean)")
  def isWhole(): Boolean = {
    true
  }
  @AbstractsMethod("scala.math.ScalaNumber.underlying(()java.lang.Object)")
  def underlying(): java.lang.Object
}
