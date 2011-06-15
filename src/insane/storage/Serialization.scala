package insane
package storage

import utils.UniqueID

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

      def readList[B](readFunc: () => B): List[B] = {
        var l = List[B]()

        while(is.read() == ',') {
          l = readFunc() :: l
        }

        l.reverse
      }

      def writeList[A](list: Traversable[A], writeFunc: (A) => Unit) {
        for (s <- list) {
          write(",")
          writeFunc(s)
        }
        write(";")
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

      def readInt(): Int   = readUntil(':').toInt
      def writeInt(i: Int) = write(i+":")

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

      var nodesToIds = Map[Node, Int]()
      var idsToNodes = Map[Int, Node]()

      def readEnv(): RealPTEnv = {
        val isBottom = read(2) == "B;"

        if (isBottom) {
          BottomPTEnv
        } else {
          val graph = readGraph()
          val rNodes = readList(() => readInt()) map idsToNodes

          new RealPTEnv(
            graph,
            Map(),
            graph.E.collect { case i: IEdge => i },
            graph.E.collect { case o: OEdge => o },
            rNodes.toSet,
            Set(),
            false)
        }
      }

      def writeEnv(env: RealPTEnv) {
        if (env.isBottom) {
          write("B;")
        } else {
          write("E:")
          writeGraph(env.ptGraph)
          writeList(env.rNodes map nodesToIds, writeInt _)
        }
      }

      def readGraph(): PointToGraph = {
        // First, we load the nodeMap
        idsToNodes = readList(() => readNodeId).toMap

        // We read edges
        val edges = readList(() => readEdge).toSet
        val nodes = idsToNodes.values.toSet


        new PointToGraph(nodes, edges)
      }

      def writeGraph(graph: PointToGraph) {
        nodesToIds = graph.V.zipWithIndex.toMap

        writeList(graph.V, writeNode _)
        writeList(graph.E, writeEdge _)
      }

      def readNodeId(): (Int, Node) = {
        (readInt(), readNode())
      }

      def writeNodeId(n: Node, id: Int) {
        writeInt(id)
        writeNode(n)
      }

      def readNode(): Node = {
        read(3) match {
          case "PN:" =>
            val pId = readInt()
            val types = readTypes()

            PNode(pId, types)
          case "IN:" =>
            val sgt = read(1) == "T"
            val pPoint = readUniqueID()
            val types = readTypes()
            INode(pPoint, sgt, types)

          case "LN:" =>
            val fromNode = idsToNodes(readInt())
            val pPoint = readUniqueID()
            val via = readField()
            LNode(fromNode, via, pPoint)

          case "GB;" =>
            GBNode
          case "NN;" =>
            NNode
          case "SG;" =>
            StringLitNode
          case "LG;" =>
            LongLitNode
          case "IT;" =>
            IntLitNode
          case "FT;" =>
            FloatLitNode
          case "BT;" =>
            ByteLitNode
          case "CR;" =>
            CharLitNode
          case "DB;" =>
            DoubleLitNode
          case "BL;" =>
            BooleanLitNode
        }
      }

      def writeNode(n: Node) {
        n match {
          case PNode(pId, types) =>
            write("PN:")
            writeInt(pId)
            writeTypes(types)
          case INode(pPoint, sgt, types) =>
            write("IN:"+(if(sgt) "T" else "F"))
            writeUniqueID(pPoint)
            writeTypes(types)
          case LNode(fromNode, via, pPoint) =>
            write("LN:")
            writeInt(nodesToIds(fromNode))
            writeUniqueID(pPoint)
            writeField(via)
          case GBNode =>
            write("GB;")
          case NNode =>
            write("NN;")
          case StringLitNode =>
            write("SG;")
          case LongLitNode =>
            write("LG;")
          case IntLitNode =>
            write("IT;")
          case FloatLitNode =>
            write("FT;")
          case ByteLitNode =>
            write("BT;")
          case CharLitNode =>
            write("CR;")
          case DoubleLitNode =>
            write("DB;")
          case BooleanLitNode =>
            write("BL;")
        }
      }

      def writeTypes(types: ObjectSet) {
        writeList(types.subtypesOf, Type.writeType(_: RealType))
        writeList(types.exactTypes, Type.writeType(_: RealType))
      }

      def readTypes(): ObjectSet = {
        val subTypesOf = readList(() => Type.readType()).toSet
        val exactTypes = readList(() => Type.readType()).toSet

        ObjectSet(subTypesOf, exactTypes)
      }

      def writeUniqueID(uid: UniqueID) {
        writeList(uid.ids, writeInt _)
      }

      def readUniqueID(): UniqueID = {
        UniqueID(readList(() => readInt()))
      }

      def readEdge(): Edge = {
        val tpe   = read(2)

        val from  = idsToNodes(readInt())
        val field = readField()
        val to    = idsToNodes(readInt())

        tpe match {
          case "i:" =>
            IEdge(from, field, to)
          case "o:" =>
            OEdge(from, field, to)
        }
      }

      def writeEdge(e: Edge) {
        e match {
          case IEdge(from, label, to) =>
            write("i:")
            writeInt(nodesToIds(from))
            writeField(label)
            writeInt(nodesToIds(to))
          case OEdge(from, label, to) =>
            write("o:")
            writeInt(nodesToIds(from))
            writeField(label)
            writeInt(nodesToIds(to))
          case _ =>
            sys.error("Unnexpected edge to serialize: "+e)
        }
      }

      def writeField(f: Field) {
        Symbol.writeSymbol(f.symbol)        
      }

      def readField(): Field = {
        Field(Symbol.readSymbol())
      }

    }
  }
}
