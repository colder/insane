package testsuper;

class A { 
  var a = 1;
  var b = 1;
  var c = 1;

  def meth() = {
    this.f();
  }

  def f() = {
    a = 2
  }
}

class B extends A {
  override def meth() = {
    super.meth()
  }

  override def f() = {
    b = 2;
  }
}

class C extends A {
  override def f() = {
    c = 2;
  }
}

object T {
  def run(b: B) = {
    b.meth()
    b
  }
}
