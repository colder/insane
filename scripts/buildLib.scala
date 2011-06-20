// #!/home/colder/bin/scala
// ::!#
 
import scala.io.Source
import java.io.{File, FileWriter}
import java.lang.StringBuffer

val argsList = args.toList

val plopPath = argsList.headOption.getOrElse(sys.error("No file provided!"))
val savePath = argsList.tail.headOption.getOrElse(sys.error("Provide path to the class directory!"))

val methods = Source.fromFile(plopPath).getLines.toList

def className(sign: String) = sign.split('(').head.split('.').dropRight(1).mkString(".")

for ((cl, methods) <- methods.groupBy(className _)) {

  val safeClass = cl.replace(".", "").replace("$", "")

  val path = savePath+"/"+safeClass+".scala"
  println("Processing "+cl+" to "+path+"...")

  val f = new FileWriter(new File(path))
  val b = new StringBuffer()

  var allConcrete = true

  for (m <- methods) {
    val isConstructor = m.contains("<init>")
    val name = m.split('(').head.split('.').last.replace("<init>", "__init__")
    val signature = m.split("\\(", 3).tail.tail.head.split(')')

    var retType = signature.last
    val args = signature.dropRight(1).mkString(")").replace("x$", "x")

    val retVal = if (isConstructor) {
      retType = safeClass
      Some("this")
    } else {
      constructValueFromType(retType)
    }

    val keepAbstract = retVal.isEmpty

    b.append("  @AbstractsMethod(\"" + m + "\")\n")
    methodImplementations(cl, name) match {
      case None => {
        if(keepAbstract) {
          allConcrete = false
          b.append("  def " + name + "(" + args + "): " + parametrizeType(retType) + "\n")
        } else {
          b.append("  def " + name + "(" + args + "): " + parametrizeType(retType) + " = {\n")
          b.append("    " + retVal.get + "\n")
          b.append("  }\n")
        }
      }
      case Some(impl) => {
        b.append("  def " + name + "(" + args + "): " + parametrizeType(retType) + " = {\n")
        b.append("    " + impl + "\n")
        b.append("  }\n")
      }
    }
  }

  f.write("package insane\n")
  f.write("package predefined\n")
  f.write("\n")
  f.write("import annotations._\n")
  f.write("\n")
  f.write("@AbstractsClass(\""+cl+"\")\n")
  if(allConcrete) {
    f.write("class "+safeClass+" {\n")
  } else {
    f.write("abstract class "+safeClass+" {\n")
  }

  f.write(b.toString)

  f.write("}\n")
  f.close
}

def constructValueFromType(tpe : String) : Option[String]= {
  val res = tpe match {
    case "Boolean" => "true"
    case "Byte" => "(42 : Byte)"
    case "Char" => "'c'"
    case "Double" => "42.0d"
    case "Float" => "42.0f"
    case "Int" => "42"
    case "Long" => "42L"
    case "Short" => "(42 : Short)"
    case "Unit" => "()"
    case "java.lang.Boolean" => "new " + tpe + "(true)"
    case "java.lang.Byte" => "new " + tpe + "(42 : Byte)"
    case "java.lang.Character" => "new " + tpe + "('c')"
    case "java.lang.Double" => "new " + tpe + "(42.0d)"
    case "java.lang.Float" => "new " + tpe + "(42.0f)"
    case "java.lang.Integer" => "new " + tpe + "(42)"
    case "java.lang.Long" => "new " + tpe + "(42L)"
    case "java.lang.Short" => "new " + tpe + "(42 : Short)"
    case "java.lang.String" => "\"\""
    case t if t contains "Array[" =>
      "new " + parametrizeType(t) + "(1)"
    case _ => "NOPE"
  }
  if(res == "NOPE") None else Some(res)    
}

def typeParameterCount(tpe : String) : Int = tpe match {
  case "scala.concurrent.forkjoin.ForkJoinTask" => 1
  case "java.lang.Class" => 1
  case "java.util.Iterator" => 1
  case "java.util.List" => 1
  case "java.util.Map" => 2
  case "java.lang.ref.ReferenceQueue" => 1
  case "java.lang.ref.Reference" => 1
  case "java.util.Comparator" => 1
  case "java.util.concurrent.Future" => 1
  case "java.util.concurrent.BlockingQueue" => 1
  case "java.util.Enumeration" => 1
  case "java.util.Set" => 1
  case _ => 0
}

def parametrizeType(tpe : String) : String = {
  if(tpe.startsWith("Array[")) {
    "Array[" + parametrizeType(tpe.substring(6, tpe.length - 1)) + "]"
  } else {
    val c = typeParameterCount(tpe)
    if(c == 0) tpe else tpe + List.fill(c)("_").mkString("[",",","]")
  }
}

// perhaps we don't really want that...
def methodImplementations(tpe : String, mname : String) : Option[String] = (tpe, mname) match {
  // case ("java.lang.Integer", "valueOf") => Some("new java.lang.Integer(x1)")
  case _ => None
}

// Junk:
// case "java.lang.Package" => tpe + ".getPackage(\"java\")"
// case "java.io.InputStream" 
// case "java.io.OutputStream"
// case "java.lang.reflect.Method" => "(" + constructValueFromType("java.lang.class") + ").getMethod(\"hashCode\")"
// case "java.lang.reflect.Field" => "(" + constructValueFromType("java.lang.class") + ").getField("\"SIZE\")"
// case "java.lang.Process"
// case "java.lang.Runtime"
// case "java.lang.Appendable"
// case "java.nio.channels.FileChannel" 
// case "java.math.BigInteger" | "java.math.BigDecimal" => "new " + tpe + "(\"0\")"
// case "java.nio.ByteBuffer" | "java.nio.CharBuffer" => tpe + ".allocate(0)"
// case "java.nio.charset.Charset" => tpe + ".forName(\"ISO-8859-1\")"
// case "java.nio.charset.CharsetDecoder" => "(" + constructValueFromType("java.nio.charset.Charset") + ").newDecoder()"
// case "java.nio.charset.CharsetEncoder" => "(" + constructValueFromType("java.nio.charset.Charset") + ").newEncoder()"
// case "java.util.regex.Matcher" => "(" + constructValueFromType("java.util.regex.Pattern") + ").matcher(\"x\")"
// case "java.util.regex.Pattern" => tpe + ".compile(\"x\")"
// case "javax.xml.parsers.SAXParser" => "(" + constructValueFromType("javax.xml.parsers.SAXParserFactory") + ").newSAXParser()"
// case "javax.xml.parsers.SAXParserFactory" => tpe + ".newInstance()"
// case "org.xml.sax.EntityResolver" => "new " + tpe + " { override def resolveEntity(p : String, s : String) = new org.xml.sax.InputSource() }"
// case "org.xml.sax.XMLReader" => "org.xml.sax.helpers.XMLReaderFactory.createXMLReader()"
// case "java.util.concurrent.ExecutorService" => "java.util.concurrent.Executors.newSingleThreadExecutor()"
// case "java.lang.management.RuntimeMXBean" => "java.lang.management.ManagementFactory.getRuntimeMXBean()"
