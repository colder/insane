class Plop {
  var f1: Plop = null

  def test(o: Plop) = {
    if (o.f1 == null) {
      o.f1 = null
    }
  }

  def test2 = {
    val myo = new Plop

    test(myo)
  }
}
