class A {
  def test() {

  }

  def applyFE1(l: MyList) = {
    val newC = new C
    myforeach(l, { b => b.content = new C })
  }

  def applyFE2(l: MyList) = {
    myforeach(l, { b => b.content = null })
  }

  def myforeach(l: MyList, fun: B => Unit): Unit = {
    if (l != null) {
      fun(l.value)
      myforeach(l.next, fun)
    }
  }
}

class MyList(var next: MyList, var value: B)

class B(var content: C)

class C
