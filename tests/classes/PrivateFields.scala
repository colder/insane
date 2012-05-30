class A { 
  private[this] var a = 1;

  def meth() = {
    val b = a;
    b
  }
}

class B extends A {
}

object T {
  def run(b: B) = {
    b.meth()
    b
  }
}
