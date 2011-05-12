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
    s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\"", "\\\\\"").replaceAll("\\\n", "\\\\n").replaceAll("[^<>@a-zA-Z0-9;$.,!# \t=^:_\\\\\"'*+/&()\\[\\]{}-]", "?")

  def labeledArrow(x: String, label: String, y: String, options: List[String] = Nil) =
    arrow(x, y, "label=\""+escape(label)+"\"" :: options)

  def labeledDashedArrow(x: String, label: String, y: String, options: List[String] = Nil) =
    arrow(x, y, "label=\""+escape(label)+"\"" :: "style=dashed" :: options)

  def arrow(x: String, y: String, options: List[String] = Nil) = {
    "  "+x+" -> "+y+options.mkString(" [", " ", "]")+";\n"
  }

  def box(id : String, name : String, options: List[String] = Nil) = {
    node(id, name, "shape=box" :: "color=lightblue" :: "style=filled" :: options)
  }

  def invisNode(id : String, name : String, options: List[String] = Nil) = {
    node(id, name, "shape=none" :: options)
  }

  def dashedNode(id : String, name : String, options: List[String] = Nil) = {
    node(id, name, "style=dashed" :: options)
  }

  def node(id: String, name: String, options: List[String] = Nil) = {
    id +("label=\""+escape(name)+"\"" :: options).mkString(" [", ", ", "]")+";\n"
  }
}
