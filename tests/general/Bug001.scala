package tests.general

class Bug001 {
  def zz = {
    val a = Set(CC(1), CC(2), CC(3))
    val b = a.filter(_.i < 3)
  }
}

case class CC(i: Int);
