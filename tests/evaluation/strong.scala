class A {
  var f : A = null;

  def foo(a1: A, a2: A) = {
    a1.f = new C;
    val ret = a2.f
    a1.f = new B;
    ret
  }

  def bar() = {
    foo(this, this)
  }
}

class B extends A
class C extends A





