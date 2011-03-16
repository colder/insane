package tests.classes

class Test1 {
  class A {
    def foo() {

    }
  }
  class B extends A {
    override def foo() {

    }
  }
  class C extends A {
    override def foo() {

    }
  }
  class D extends C {
    override def foo() {

    }
  }


  def main() {
    var a: A = new B
    var i = 1;

    a.foo();

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
