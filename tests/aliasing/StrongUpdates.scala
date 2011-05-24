class Plop {
  var f1: Plop = null
  var f2: Plop = null

  def test() = {
    val o  = new Plop
    val o2 = new Plop

    if (o.f1 == null) {
      o.f1 = o2
    } else {
      o.f2 = o2
    }
  }
}
