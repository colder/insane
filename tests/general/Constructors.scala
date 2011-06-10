class A(a: Int) {
   def this(dim1: Int, dim2: Int) = {
     this(dim1)
     throw new RuntimeException()
   }
}
