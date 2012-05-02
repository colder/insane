package insane
package predefined

import annotations._

@AbstractsClass("scala.Array")
class scalaArray[T] extends java.io.Serializable with java.lang.Cloneable {
  @GhostField
  var l     : Int = 0;

  @GhostField
  var store : T = _

  @AbstractsMethod("scala.Array.<init>((_length: Int)Array[T])")
  def ____init__(_length: Int): scalaArray[T]= {
    l = _length
    this
  }

  @AbstractsMethod("scala.Array.length(()Int)")
  def __length: Int= {
    l
  }

  @AbstractsMethod("scala.Array.update((i: Int, x: T)Unit)")
  def __update(i: Int, x: T) = {
    if (i > 42) {
      store = x
    }
  }
  @AbstractsMethod("scala.Array.apply((i: Int)T)")
  def __apply(i: Int): T = store

}
