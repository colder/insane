#!/home/colder/bin/scala
::!#

import scala.io.Source
import java.io.{File, FileWriter}

val argsList = args.toList

val plopPath = argsList.headOption.getOrElse(sys.error("No file provided!"))
val savePath = argsList.tail.headOption.getOrElse(sys.error("Provide path to the class directory!"))

val methods = Source.fromFile(plopPath).getLines.toList

def className(sign: String) =
  sign.split('(').head.split('.').dropRight(1).mkString(".")

for ((cl, methods) <- methods.groupBy(className _)) {

  val safeClass = cl.replace(".", "").replace("$", "")

  val path = savePath+"/"+safeClass+".scala"
  println("Processing "+cl+" to "+path+"...")

  val f = new FileWriter(new File(path))

  f.write("package insane\n")
  f.write("package predefined\n")
  f.write("\n")
  f.write("import annotations._\n")
  f.write("\n")
  f.write("@AbstractsClass(\""+cl+"\")\n")
  f.write("class "+safeClass+" {\n")

  for (m <- methods) {
    val name = m.split('(').head.split('.').last
    val signature = m.split("\\(", 3).tail.tail.head.split(')')

    val retType = signature.last
    val args = signature.dropRight(1).mkString(")").replace("x$", "x")

    val retVal = retType match {
      case "Boolean" => "true"
      case "Char" => "'c'"
      case "Int" => "42"
      case "Byte" => "42"
      case "Short" => "42"
      case "Long" => "42L"
      case "Double" => "42.0d"
      case "Float" => "42.0f"
      case "Unit" => "()"
      case "java.lang.String" => "\"\""
      case t if t contains "Array[" =>
        "new "+t+"(1)"
      case t =>
        "new "+t+"()"
    }

    f.write("  @AbstractsMethod(\""+m+"\")\n")
    f.write("  def "+name+"("+args+"): "+retType+" = {\n")
    f.write("    "+retVal+"\n")
    f.write("  }\n")

  }

  f.write("}\n")
  f.close
}
