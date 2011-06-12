package insane
package utils

case class UniqueID(ids: List[Int]) {
  def this(i: Int) = this(List(i))

  def add(that: UniqueID) = UniqueID(ids ::: that.ids)

  def safeAdd(that: UniqueID) = {
    if ((that.ids.toSet & this.ids.toSet).isEmpty) {
      add(that)
    } else {
      this
    }
  }

  override def toString = ids.mkString("[", ",", "]")
}
