class Plop {
  var f1 = 1
  var f2 = 2

  def test() = {
    val a = new Plop

    if (a.f1 < 2) {
      a.f1 = 3
    } else {
      a.f2 = 4
    }
  }
}
