package insane
package storage

import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.lang.StringBuffer

import scala.tools.nsc.symtab.Flags

trait SerializationHelpers {
  self: AnalysisComponent =>

  import global._

  type RealSymbol = Symbol
  type RealType   = Type
  type RealPTEnv  = PTEnv

  abstract class Serialization {
    var is: ByteArrayInputStream  = null
    var os: ByteArrayOutputStream = null

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
        res append c.toChar
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

  trait Serializer {
    self: Serialization =>

    def doSerialize()

    def serialize(): String = {
      os = new ByteArrayOutputStream
      doSerialize()
      os.toString("UTF-8")
    }
  }

  trait Unserializer[T] {
    self: Serialization =>

    val str: String

    def doUnSerialize(): T

    def unserialize(): T = {
      is = new ByteArrayInputStream(str.getBytes("UTF-8"))
      doUnSerialize()
    }
  }

  abstract class CompilerSerialization extends Serialization {
    def explicitFullName(s: String): Name = {
      newTypeName(if (s contains ".") {
        s
      } else {
        "<empty>."+s
      })
    }

    def readClassSymbol(): RealSymbol = {
      try {
        read(3) match {
          case "cl:" =>
            // Class Symbol
            definitions.getClassIfDefined(explicitFullName(readUntil(':'))) 
          case "mc:" =>
            // ModuleClass Symbol
            definitions.getModule(explicitFullName(readUntil(':'))).moduleClass
          case t =>
            reporter.error("Unnexpected class symbol type: "+t)
            NoSymbol
        }
      } catch {
        case e => 
          reporter.error("Unable to unserialize classsymbol: "+e.getMessage)
          NoSymbol
      }
    }

    def getField(cl: Symbol, fieldName: String): Symbol = {
      cl.tpe.findMember(newTermName(fieldName), Flags.METHOD, 0, false)
    }

    def readSymbol(): RealSymbol = {
      try {
        read(3) match {
          case "cl:" =>
            // Class Symbol
            definitions.getClassIfDefined(explicitFullName(readUntil(':'))) 
          case "mc:" =>
            // ModuleClass Symbol
            definitions.getModule(explicitFullName(readUntil(':'))).moduleClass
          case "te:" =>
            // Term Symbol
            val cl = readClassSymbol
            getField(cl, readUntil(':'))

          case "tp:" =>
            // Type Parameter Symbol
            val cl = readClassSymbol
            val name = readUntil(':')
            cl.typeParams.find(s => s.name.toString == name).getOrElse(NoSymbol)

          case "ns:" =>
            NoSymbol
          case "er:" =>
            val name = readUntil(':')
            reporter.error("Cannot recover from erroneous symbol at unserialization: "+name)
            NoSymbol
        }
      } catch {
        case e =>
          reporter.error("Unable to unserialize symbol: "+e.getMessage)
          NoSymbol
      }
    }

    def writeSymbol(s: RealSymbol) {
      try {
        if (s.isModuleClass) {
          write("mc:"+s.fullName+":")
        } else if (s.isClass) {
          write("cl:"+s.fullName+":")
        } else if (s.isTerm) {
          write("te:")
          writeSymbol(s.owner)
          write(s.name.toString+":")
        } else if (s.isType) {
          write("tp:")
          writeSymbol(s.owner)
          write(s.name.toString+":")
        } else if (s == NoSymbol) {
          write("ns:")
        } else {
          debugSymbol(s)
          sys.error("Unnexpected kind of symbol here!")
        }
      } catch {
        case e =>
          write("er:"+s.name+":")
          sys.error("Error while writing symbol: "+e.getMessage)
      }
    }
    def readType(): RealType = {
      read(3) match {
        case "at:" =>
          // Array Type
          arrayType(readType())
        case "st:" =>
          // Simple Type
          readClassSymbol().tpe
      }
    }

    def writeType(t: RealType) {
      t match {
        case TypeRef(NoPrefix, definitions.ArrayClass, List(tpe)) =>
          write("at:")
          writeType(tpe)
        case tpe =>
          write("st:")
          writeSymbol(tpe.typeSymbol)
      }
    }
  }

  abstract class PTEnvSerialization extends CompilerSerialization {
    import PointToGraphs._

    var nodesToIds = Map[Node, Int]()
    var idsToNodes = Map[Int, Node]()

    var lNodesToFix = Map[LNode, Int]()

    def readEnv(): RealPTEnv = {
      val isBottom = read(2) == "B;"

      if (isBottom) {
        BottomPTEnv
      } else {
        val graph = readGraph()
        // TODO handle dangling calls
        val isPartial = read(2) == "P,"

        new RealPTEnv(
          graph,
          Map().withDefaultValue(Set()),
          // Map().withDefaultValue(None),
          graph.E.collect { case i: IEdge => i },
          graph.E.collect { case o: OEdge => o },
          // Set(),
          Map(),
          false,
          false)
      }
    }

    def writeEnv(env: RealPTEnv) {
      if (env.isBottom) {
        write("B;")
      } else {
        write("E:")
        if (env.isPartial) {
          write("P,")
        } else {
          write("C,")
        }
        writeGraph(env.ptGraph)
      }
    }

    def readGraph(): PointToGraph = {
      // First, we load the nodeMap
      idsToNodes = readList(() => readNodeId).toMap

      // We fix nodes
      for ((n, id) <- lNodesToFix) {
        n.fromNode = idsToNodes(id)
      }

      // We read edges
      val edges = readList(() => readEdge).toSet
      val nodes = idsToNodes.values.toSet


      new PointToGraph(nodes, edges)
    }

    def writeGraph(graph: PointToGraph) {

      nodesToIds = graph.V.zipWithIndex.toMap

      writeList(nodesToIds, (t: (Node, Int)) => writeNodeId(t._1, t._2))
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
/*        case "LV:" =>
          val sym = readSymbol()
          val types = readTypes()

          PNode(pId, types)
*/
        case "IN:" =>
          val sgt = read(1) == "T"
          val pPoint = readUniqueID()
          val sym = readSymbol()
          INode(pPoint, sgt, sym)

        case "LN:" =>
          val fromNodeID = readInt()
          val pPoint = readUniqueID()
          val via = readField()
          val types = readTypes()
          val ln = LNode(null, via, pPoint, null)

          lNodesToFix += ln -> fromNodeID

          ln
          
        case "OB:" =>
          val sym = readSymbol()
          OBNode(sym)

        case "GB;" =>
          GBNode
        case "NN;" =>
          NNode
        case "SG;" =>
          StringLitNode
        case "LG;" =>
          AnyLongLitNode
        case "IT;" =>
          AnyIntLitNode
        case "FT;" =>
          FloatLitNode
        case "BT;" =>
          ByteLitNode
        case "CR;" =>
          CharLitNode
        case "DB;" =>
          DoubleLitNode
        case "BL;" =>
          AnyBooleanLitNode
        case "SL;" =>
          ShortLitNode
      }
    }

    def writeNode(n: Node) {
      n match {
/*
        case PNode(pId, types) =>
          write("PN:")
          writeInt(pId)
          writeTypes(types)
*/
        case INode(pPoint, sgt, sym) =>
          write("IN:"+(if(sgt) "T" else "F"))
          writeUniqueID(pPoint)
          writeSymbol(sym)
        case LNode(fromNode, via, pPoint, types) =>
          write("LN:")
          writeInt(nodesToIds(fromNode))
          writeUniqueID(pPoint)
          writeField(via)
//          writeTypes(types)
        case OBNode(sym) =>
          write("OB:")
          writeSymbol(sym)

        case GBNode =>
          write("GB;")
        case NNode =>
          write("NN;")
        case StringLitNode =>
          write("SG;")
        case LongLitNode(_) | AnyLongLitNode =>
          write("LG;")
        case IntLitNode(_) | AnyIntLitNode =>
          write("IT;")
        case FloatLitNode =>
          write("FT;")
        case ByteLitNode =>
          write("BT;")
        case CharLitNode =>
          write("CR;")
        case DoubleLitNode =>
          write("DB;")
        case BooleanLitNode(_) | AnyBooleanLitNode => // TODO divise
          write("BL;")
        case ShortLitNode =>
          write("SL;")
      }
    }

    def writeTypes(info: TypeInfo) {
      //TODO
    }

    def readTypes(): TypeInfo = {
      //TODO
      TypeInfo.empty
    }

    def writeUniqueID(uid: UniqueID) {
      var list = List[Int]()

      for ((i,n) <- uid.ids; k <- 1 to n) {
        list = i :: list
      }

      writeList(list, writeInt _)
    }

    def readUniqueID(): UniqueID = {
      val list = readList(() => readInt())

      var map = Map[Int, Int]().withDefaultValue(0)
      for (l <- list) {
        map += l -> (map(l) + 1)
      }

      UniqueID(map)
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
      write(f.fullName+":"+f.name+":")        
    }

    def readField(): Field = {
      val fullName = readUntil(':')
      val name     = readUntil(':')
      Field(fullName, name)
    }
  }

  case class EnvSerializer(ptEnv: PTEnv) extends PTEnvSerialization with Serializer {
    def doSerialize() {
      writeEnv(ptEnv)
    }
  }

  case class EnvUnSerializer(str: String) extends PTEnvSerialization  with Unserializer[PTEnv] {
    def doUnSerialize() = {
      readEnv()
    }
  }

  case class ClassSymbolSerializer(cs: Symbol) extends PTEnvSerialization with Serializer {
    def doSerialize() {
      writeSymbol(cs)
    }
  }
  case class ClassSymbolUnSerializer(str: String) extends PTEnvSerialization with Unserializer[Symbol] {
    def doUnSerialize() = {
      readSymbol()
    }
  }
}
