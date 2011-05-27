package insane
package utils

abstract class UniqueID {
  def add(that: UniqueID): UniqueID
}


case class IntUniqueID(i: Int) extends UniqueID {
  def add(that: UniqueID) = CompoundUniqueID(that, this)

  override def toString = i.toString
}
case class CompoundUniqueID(ids: UniqueID*) extends UniqueID {
  def add(that: UniqueID) = CompoundUniqueID(that +: ids : _*)

  override def toString = ids.map(_.toString).mkString("[", ",", "]")
}
case class ObjUniqueID(o: AnyRef) extends UniqueID {
  def add(that: UniqueID) = CompoundUniqueID(that, this)

  override def toString = "#"+o.toString
}
