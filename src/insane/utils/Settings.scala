package insane
package utils

class Settings {
  var verbosity                 = Verbosity.Normal

  def displayFixPoint           = verbosity > Verbosity.Normal
  def displayProgress           = verbosity > Verbosity.Normal

  var dumpcfg              = Seq[String]() 
  var displayclassanalyses = Seq[String]() 


  def displayClassAnalysis(toMatch: String) = {
    displayclassanalyses.exists(strMatch(toMatch, _))
  }
  def dumpCFG(toMatch: String) = {
    dumpcfg.exists(strMatch(toMatch, _))
  }


  def strMatch(haystack: String, needle: String): Boolean = {
    (haystack == needle) || (needle == "_")
  }
}

object Verbosity extends Enumeration {
  val Quiet      = Value("Quiet",      1)
  val Normal     = Value("Normal",     2)
  val Verbose    = Value("Verbose",    3)
  val Pleonastic = Value("Pleonastic", 4)

  class VerbVal(name: String, val level: Int) extends Val(nextId, name) with Ordered[Value] {
    def compare(that: VerbVal) = level compare that.level
  }

  def Value(name: String, level: Int) = new VerbVal(name, level)
}
