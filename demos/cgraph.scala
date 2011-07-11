object Test {
  def run1(obj: A) {
    obj.f()
  }
  def run2() {
    val obj = new A
    obj.f()
  }
}

class A {
  def f() {}
}

class B extends A {
  override def f() {}
}
