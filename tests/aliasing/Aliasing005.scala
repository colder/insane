package tests.aliasing

class A {
  var next: A = null
}

class Aliasing005() {
  private[this] var pr1 = new A;
  private[this] var pr2 = new A;

  private[this] val o1 = new A;
  private[this] val o2 = new A;

  var pu3 = new A;
  var pu4 = new A;

  def test(): A = {
    val a = this.pr2
    this.pr1 = a;

    val b = new A
    this.pr2 = b

    val c = new A
    c.next = b

    c
  }
}
