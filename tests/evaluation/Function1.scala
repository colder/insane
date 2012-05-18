package scala

trait Function1[-T1, +R] extends AnyRef { self =>
  /** Apply the body of this function to the argument.
   *  @return   the result of function application.
   */
  def apply(v1: T1): R

  override def toString() = "<function1>"
}
