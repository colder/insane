package tests
class A;

class Test1 {
  def a(v1: Test1, v2: Test1) = {
    var a   = new A();
    var d   = new A();
    var tmp = a;
    val b   = new A();
    val c   = b;

    a = c;


    if (2 > 3) {
      a = b;
    } else {
      a = d;
    }

    d = if (2 > 3) {
      c
    } else {
      a
    }

    d = a match {
      case _ if a eq d =>
        c
      case _ =>
        a
    }
  }
}
