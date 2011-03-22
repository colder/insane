package insane
package utils

object DotHelpers {
  private var _nextName   = 0
  private var _nextColor  = 0

  def nextName = {
    _nextName += 1
    "v"+_nextName
  }

  val bgColors = List("bisque", "khaki", "mistyrose", "lightcyan", "mediumorchid", "aquamarine", "antiquewhite")

  def nextColor = {
    _nextColor += 1
    val colornumber: String = if((_nextColor/bgColors.size)%3 == 0) "" else ((_nextColor/bgColors.size)%3)+"";
    bgColors(_nextColor%bgColors.size)+colornumber
  }

  def escape(s: String) =
    s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\"", "\\\\\"").replaceAll("\\\n", "\\\\n").replaceAll("[^<>a-zA-Z0-9;$.,!# \t=^:_\\\\\"'*+/&()\\[\\]{}-]", "?")

  def arrow(x: String, y: String) = {
    "  "+x+" -> "+y+";\n"
  }
  def box(id : String, name : String) = {
    "  "+id+" [shape=box,color=lightblue,style=filled,label=\""+escape(name)+"\"];\n"
  }
}
