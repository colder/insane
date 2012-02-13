package t7

abstract class T {
  var v1: T = null
  var v2: T = null

  def foo(): Unit
}

class T2 extends T {
  def foo() {
    this.v2 = this
  }
}
class T1 extends T {
  def foo() {
    this.v1 = this
  }
}

class Usage {
  def run = {
    var a: T = new T1

    while(a != null) {
      a.foo()
      a = new T2
    }

    a
  }
}


