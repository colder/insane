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
    var os: InputStream

    class Helpers {
      def readUntil(delim: Byte): String = {
        var res = new StringBuffer
        var c = is.read()

        while(c != delim) {
          res append c
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
      def read(): RealSymbol = {
        sys.error("todo")
      }

      def write(s: RealSymbol) {
        sys.error("todo")
      }
    }

    object Type extends Helpers {
      def read(): RealType = {
        sys.error("todo")
      }

      def write(t: RealType) {
        sys.error("todo")
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
