package tests.aliasing

class Aliasing005() {
  private[this] var pr1 = 1
  private[this] var pr2 = 2

  var pu3 = 3
  var pu4 = 4

  def test1() = {
    this.pr1 = 42
  }

  def test2() = {
    val a = this.pr2
  }

  def test3() = {
    val a = this.pr2

    this.pr1 = a;
  }

  def test4() = {
    val a = this.pu3

    this.pu4 = a;
  }
}
