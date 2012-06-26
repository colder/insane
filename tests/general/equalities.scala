package testequality
case class AAA(i: Int) {
  override def equals(a: Any) = {
    false
  }
}

object Test2 {
  def test() = {
    (AAA(1) == AAA(2)) && (AAA(1) eq AAA(2))
  }

  def test2() = {
    AAA(1) == AAA(2)
  }
}
