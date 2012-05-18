package p1

class A[T](a: T) {
  def test: T  = a
}

class B(i: Int) extends A[Int](i) {

}


class Test {
  def run = {
    val b= new B(42);
    b.test
  }
}
