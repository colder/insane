package insane
package storage

import java.io.InputStream
import java.io.OutputStream
import java.lang.StringBuffer

trait SerializationHelpers {
  self: AnalysisComponent =>

  import global._

  type RealSymbol = Symbol
  type RealType   = Type
  type RealPTEnv  = PTEnv

  abstract class Serialization {
    var is: InputStream
    var os: OutputStream

    class Helpers {
      def write(str: String) {
        os.write(str.getBytes("UTF-8"))
      }

      def read(len: Int): String = {
        val arr = new Array[Byte](len)

        val gotLen = is.read(arr, 0, len)

        if (gotLen == len) {
          new String(arr)
        } else {
          sys.error("Unserialization error,  expected "+len+" chars, got "+gotLen)
        }
      }

      def readUntil(delim: Byte): String = {
        var res = new StringBuffer
        var c = is.read()

        while(c != delim && c >= 0) {
          res append c
          c = is.read()
        }

        res.toString
      }

      def consume(b: Byte) = {
        var c = is.read()

        if (c != b) {
          sys.error("Serialization error, expected "+b.toChar+", found "+c.toChar)
        }
      }
    }

    object Symbol extends Helpers {
      def readClassSymbol(): RealSymbol = {
        read(3) match {
          case "cl:" =>
            // Class Symbol
            definitions.getClass(readUntil(':')) 
          case "mc:" =>
            // ModuleClass Symbol
            definitions.getModule(readUntil(':')) 
          case _ =>
            sys.error("Unnexpected class symbol type!")
        }
      }

      def readSymbol(): RealSymbol = {
        read(3) match {
          case "cl:" =>
            // Class Symbol
            definitions.getClass(readUntil(':')) 
          case "mc:" =>
            // ModuleClass Symbol
            definitions.getModule(readUntil(':')) 
          case "te:" =>
            // Term Symbol
            val cl = readClassSymbol
            cl.tpe.decls.lookup(readUntil(':'))
        }
      }

      def writeSymbol(s: RealSymbol) {
        if (s.isModuleClass) {
          write("mc:"+uniqueClassName(s)+":")
        } else if (s.isClass) {
          write("cl:"+uniqueClassName(s)+":")
        } else if (s.isTerm) {
          write("te:"+uniqueClassName(s.owner)+":"+s.name+":")
        } else {
          debugSymbol(s)
          sys.error("Unnexpected kind of symbol here!")
        }
      }
    }

    object Type extends Helpers {
      def readType(): RealType = {
        read(3) match {
          case "at:" =>
            // Array Type
            arrayType(readType())
          case "st:" =>
            // Simple Type
            Symbol.readClassSymbol().tpe
        }
      }

      def writeType(t: RealType) {
        t match {
          case TypeRef(NoPrefix, definitions.ArrayClass, List(tpe)) =>
            write("at:")
            writeType(tpe)
          case tpe =>
            write("st:"+Symbol.writeSymbol(tpe.typeSymbol))
        }
      }
    }

    object PTEnv extends Helpers {
      import PointToGraphs._

      def readEnv(): RealPTEnv = {
        val graph = readGraph()
        sys.error("todo")
      }

      def writeEnv(env: RealPTEnv) {
        sys.error("todo")
      }

      def readGraph(): PointToGraph = {
        sys.error("todo")
      }

      def writeGraph(env: PointToGraph) {
        sys.error("todo")
      }

      def readNode(): Node = {
        sys.error("todo")
      }

      def writeNode(n: Node) {
        sys.error("todo")
      }

      def readEdge(): Edge = {
        sys.error("todo")
      }

      def writeEdge(e: Edge) {
        sys.error("todo")
      }
    }
  }
}
