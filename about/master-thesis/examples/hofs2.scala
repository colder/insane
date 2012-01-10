class Test {
    var a: Int = 42

    def test1() = {
        plop(_ + 1)
    }

    def test2() = {
        plop(x => {a += 1; a})
    }

    def plop(f: Int => Int) = f(42)
}
