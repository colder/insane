package insane
package utils

class Settings {
  var verbosity = Verbosity.Normal

  var dumpcfg = Seq[String]() 
}

object Verbosity extends Enumeration {
  val Quiet      = Value("Quiet",      1)
  val Normal     = Value("Normal",     2)
  val Verbose    = Value("Verbose",    3)
  val Pleonastic = Value("Pleonastic", 4)

  class VerbVal(name: String, val level: Int) extends Val(nextId, name) {
    def <  (that: VerbVal): Boolean = (this compareTo that) <  0
    def >  (that: VerbVal): Boolean = (this compareTo that) >  0
    def <= (that: VerbVal): Boolean = (this compareTo that) <= 0
    def >= (that: VerbVal): Boolean = (this compareTo that) >= 0

    def compareTo(that: VerbVal) =
      this.level - that.level
  }

  def Value(name: String, level: Int) = new VerbVal(name, level)
}
