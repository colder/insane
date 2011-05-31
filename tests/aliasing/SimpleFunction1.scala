class A {
  def test() {

  }

  def applyFE1(l: MyList) = {
    val newC = new C
    myHOF(l, { b => b.content = new C })
  }

  def applyFE2(l: MyList) = {
    myHOF(l, { b => b.content = null })
  }

  def myHOF(l: MyList, fun: B => Unit): Unit = {
    fun(l.value)
  }
}

class MyList(var next: MyList, var value: B)

class B(var content: C)

class C
