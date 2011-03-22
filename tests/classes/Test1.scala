package tests.classes

class Test1 {
  sealed class A {
    def foo() {

    }
  }
  class B extends A {
    override def foo() {

    }
  }
  class C extends A {

  }
  class D extends C {
    override def foo() {

    }
  }

  def plop = {
    main(new A)(new C)
  }


  def main(arg_a: A)(arg_c: C) {
    var a: A = new B
    var i = 1;

    arg_a.foo();
    arg_c.foo();

    (new C).foo();

    a.foo();

    arg_a match {
      case a: C =>
        a.foo()
        Predef.error("test")
      case _ =>
    }

    plop

    while(i < 2) {
      if (i > 1) {
        a = new D
      } else {

      }

      i += 1
    }

    a.foo();
  }

}
