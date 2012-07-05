package insane
package CFG

import AST.ASTBindings

import scala.tools.nsc._
import utils._
import utils.Graphs.{DotConverter, GraphCopier}

trait CFGTreesDef extends ASTBindings { self: AnalysisComponent =>
  val global: Global

  trait PosBound {
    import global.{Position, NoPosition}

    var opos: Option[Position] = None

    def setPos(p: Position): this.type = {
      if (opos != None && opos != Some(p)) {
        reporter.warn("Position already set on "+this)
      }

      opos = Some(p)
      this
    }

    def setPosFrom(t: PosBound): this.type = {
      t.opos match {
        case Some(p) =>
          setPos(p)
        case None =>
      }

      this
    }

    def getPos: Position = opos match {
      case Some(p) =>
        p
      case None =>
        NoPosition
    }
  }

  object CFGTrees {
    import global._
    import utils.GlobalCounters._
    def nextID = {
      new UniqueID(getCFGCounter)
    }

    sealed abstract class Tree extends ASTBound with PosBound {
      val uniqueID = nextID

      lazy val pos = tree match {
        case Some(t) => t.pos
        case None    => getPos
      }

      def setInfoFrom(t: Tree): this.type = {
        this setTreeFrom t
        this setPosFrom t
        this
      }

      override def toString = stringRepr(this)
    }

    sealed abstract class Statement extends Tree

    class BasicBlock(val stmts: Seq[Statement])                                    extends Statement
    class AssignCast(val r: Ref, val rhs: Ref, val tpe: Type)                      extends Statement
    class AssignTypeCheck(val r: Ref, val lhs: Ref, val tpe: Type)                 extends Statement
    class AssignVal(val r: Ref, val v: SimpleValue)                                extends Statement
    class AssignFieldRead(val r: Ref, val obj: Ref, val field: Symbol)             extends Statement
    class AssignFieldWrite(val obj: Ref, val field: Symbol, val rhs: SimpleValue)  extends Statement
    class AssignNew(val r: Ref, val tpe: Type)                                     extends Statement
    sealed abstract class CallStyle {
      def desc(call: AssignApplyMeth): String;
    }
    case object DynamicCall  extends CallStyle {
      def desc(call: AssignApplyMeth) = "@dyn"
    }
    case object StaticCall    extends CallStyle {
      def desc(call: AssignApplyMeth) = "@static<"+call.meth.fullName+">"
    }
    case object VirtualCall   extends CallStyle {
      def desc(call: AssignApplyMeth) = ""
    }
    class AssignApplyMeth(val r: Ref,
                          val obj: SimpleValue,
                          val meth: Symbol,
                          val args: Seq[SimpleValue],
                          val typeArgs: Seq[global.Tree] = Seq(),
                          val style: CallStyle = VirtualCall,
                          val excludedSymbols: Set[Symbol] = Set(),
                          val inlinedIn: Set[(Symbol, TypeSignature)] = Set()) extends Statement {

      def excludeSymbols(syms: Set[Symbol]) = {
        val newExcludedSymbols = excludedSymbols ++ syms.map(s => lookupFunction(s).flatMap(_.implOfMethod).getOrElse(s))
        if (newExcludedSymbols != excludedSymbols) {
          new AssignApplyMeth(r, obj, meth, args, typeArgs, style, newExcludedSymbols, inlinedIn) setTreeFrom this
        } else {
          this
        }
      }
    }
    class AssertEQ(val lhs: SimpleValue, val rhs: SimpleValue)                     extends Statement
    class AssertNE(val lhs: SimpleValue, val rhs: SimpleValue)                     extends Statement
    class Branch(val cond: BranchCondition)                                        extends Statement
    class Effect(val env: PTEnv, val name: String)                                 extends Statement
    object Skip                                                                    extends Statement {
      setTree(EmptyTree)
    }


    sealed abstract class SimpleValue extends Tree {
      def tpe: Type;
    }

    sealed abstract class Ref extends SimpleValue

    sealed trait TypedSymbolRef {
      this: {val symbol: Symbol} =>

      def tpe = symbol.tpe;
    }

    sealed trait SymbolRef {
      this: Ref =>

      def symbol: Symbol
    }

    case class ObjRef(symbol: Symbol, tpe: Type)                        extends Ref with SymbolRef
    case class SymRef(symbol: Symbol, version: UniqueID, tpe: Type)     extends Ref with SymbolRef
    case class TempRef(name: String, version: UniqueID, tpe: Type)      extends Ref

    // Mutable only during CFG Generation
    case class ThisRef(var symbol: Symbol, version: UniqueID, tpe: Type) extends Ref with SymbolRef

    class Null extends SimpleValue {
      override def tpe = NoType
    }

    sealed abstract class LiteralValue extends SimpleValue {
      override def tpe: Type = this match {
        case _: CFG.StringLit     | _: CFG.AnyStringLit => 
          definitions.StringClass.tpe
        case _: CFG.BooleanLit    | _: CFG.AnyBooleanLit => 
          definitions.BooleanClass.tpe
        case _: CFG.LongLit       | _: CFG.AnyLongLit => 
          definitions.LongClass.tpe
        case _: CFG.IntLit        | _: CFG.AnyIntLit => 
          definitions.IntClass.tpe
        case _: CFG.CharLit       | _: CFG.AnyCharLit => 
          definitions.CharClass.tpe
        case _: CFG.ByteLit       | _: CFG.AnyByteLit =>
          definitions.ByteClass.tpe
        case _: CFG.FloatLit      | _: CFG.AnyFloatLit =>
          definitions.FloatClass.tpe
        case _: CFG.DoubleLit     | _: CFG.AnyDoubleLit => 
          definitions.DoubleClass.tpe
        case _: CFG.ShortLit      | _: CFG.AnyShortLit => 
          definitions.ShortClass.tpe
      }
    }

    class StringLit(val v: String)     extends LiteralValue
    class AnyStringLit                 extends LiteralValue
    class BooleanLit(val v: Boolean)   extends LiteralValue
    class AnyBooleanLit                extends LiteralValue
    class LongLit(val v: Long)         extends LiteralValue
    class AnyLongLit                   extends LiteralValue
    class IntLit(val v: Int)           extends LiteralValue
    class AnyIntLit                    extends LiteralValue
    class ShortLit(val v: Short)       extends LiteralValue
    class AnyShortLit                  extends LiteralValue
    class ByteLit(val v: Byte)         extends LiteralValue
    class AnyByteLit                   extends LiteralValue
    class CharLit(val v: Char)         extends LiteralValue
    class AnyCharLit                   extends LiteralValue
    class FloatLit(val v: Float)       extends LiteralValue
    class AnyFloatLit                  extends LiteralValue
    class DoubleLit(val v: Double)     extends LiteralValue
    class AnyDoubleLit                 extends LiteralValue
    class Unit                         extends LiteralValue
    class ClassLit(override val tpe: Type)      extends LiteralValue
    class EnumLit       extends LiteralValue


    sealed abstract class BranchCondition extends Tree
    class IfTrue(val sv: SimpleValue) extends BranchCondition
    class IfFalse(val sv: SimpleValue) extends BranchCondition
    class IfEqual(val rhs: SimpleValue, val lhs: SimpleValue) extends BranchCondition
    class IfNotEqual(val rhs: SimpleValue, val lhs: SimpleValue) extends BranchCondition


    def strVersion(version: UniqueID): String = {
      if (version != NoUniqueID && CFGTreesDef.this.settings.isDebug) {
        "@"+version
      } else {
        ""
      }
    }
    def strType(tpe: Type): String = {
      //"["+tpe+"]"
      ""
    }

    def stringRepr(tr: Tree): String = tr match {
      case bb: BasicBlock =>
        bb.stmts.map(stringRepr _).mkString("[[", " => ", "]]")
      case t: AssignCast =>
        stringRepr(t.r) +" = "+stringRepr(t.rhs)+" asInstanceOf "+t.tpe.toString
      case t: AssignTypeCheck =>
        stringRepr(t.r) +" = "+stringRepr(t.lhs)+" isInstanceOf "+t.tpe.toString+" ?"
      case t: AssignVal =>
        stringRepr(t.r) +" = "+stringRepr(t.v)
      case t: AssignFieldRead =>
        stringRepr(t.r) +" = "+stringRepr(t.obj)+"."+t.field.name
      case t: AssignFieldWrite =>
        stringRepr(t.obj) +"."+t.field.name+" = "+stringRepr(t.rhs)
      case t: AssignApplyMeth =>
      stringRepr(t.r) +" = "+stringRepr(t.obj)+"."+t.meth.name+t.style.desc(t)+(if (t.typeArgs.isEmpty) "" else t.typeArgs.mkString("[", ", ", "]"))+t.args.map(stringRepr).mkString("(", ", ", ")")
      case t: AssignNew =>
        stringRepr(t.r) +" = new "+t.tpe
      case t: AssertEQ =>
        "assert("+stringRepr(t.lhs)+" eq "+stringRepr(t.rhs)+")"
      case t: AssertNE =>
        "assert("+stringRepr(t.lhs)+" ne "+stringRepr(t.rhs)+")"
      case t: Branch =>
        "["+stringRepr(t.cond)+"]"
      case Skip =>
        "skip"
      case r: ObjRef =>
        r.symbol.name.toString()+strType(r.tpe)
      case r: SymRef =>
        r.symbol.name.toString()+strVersion(r.version)+strType(r.tpe)
      case r: TempRef =>
        r.name+strVersion(r.version)+strType(r.tpe)
      case r: ThisRef =>
        "this"+strVersion(r.version)+strType(r.tpe)
      case t: AnyStringLit =>
        "?string?"
      case t: AnyByteLit =>
        "?byte?"
      case t: AnyShortLit =>
        "?short?"
      case t: AnyCharLit =>
        "?char?"
      case t: AnyIntLit =>
        "?int?"
      case t: AnyFloatLit =>
        "?float?"
      case t: AnyLongLit =>
        "?long?"
      case t: AnyBooleanLit =>
        "?boolean?"
      case t: StringLit =>
        "\""+t.v+"\""
      case t: ByteLit =>
        t.v.toString
      case t: ShortLit =>
        t.v.toString
      case t: CharLit =>
        t.v.toString
      case t: IntLit =>
        t.v.toString
      case t: FloatLit =>
        t.v.toString
      case t: LongLit =>
        t.v.toString
      case t: ClassLit =>
        "classOf["+t.tpe+"]"
      case t: EnumLit =>
        "enum?"
      case t: DoubleLit =>
        t.v.toString
      case n: Null =>
        "null"
      case t: Unit =>
        "unit"
      case t: BooleanLit =>
        t.v.toString
      case e: Effect =>
        "some effect"
      case t: IfTrue =>
        stringRepr(t.sv)
      case t: IfFalse =>
        "!"+stringRepr(t.sv)
      case t: IfEqual =>
        stringRepr(t.lhs)+" == "+stringRepr(t.rhs)
      case t: IfNotEqual =>
        stringRepr(t.lhs)+" != "+stringRepr(t.rhs)
    }
  }


  def dumpCFG(cfg: FunctionCFG, dest: String) {
    reporter.debug("Dumping CFG to "+dest+"...")
    new CFGDotConverter(cfg, "CFG of "+cfg.symbol.fullName).writeFile(dest)
  }

  class CFGDotConverter(cfg: FunctionCFG, _title: String, _prefix: String = "") extends DotConverter(cfg.graph, _title, _prefix) {
    import utils.DotHelpers

    override def vertexToString(res: StringBuffer, v: CFGVertex) {
        if (v == cfg.entry) {
            res append (v.dotName +" [label=\""+DotHelpers.escape(v.name)+"\", style=filled, color=\"green\"];\n")
        } else if (v == cfg.exit) {
            res append (v.dotName +" [label=\""+DotHelpers.escape(v.name)+"\", style=filled, color=\"red\"];\n")
        } else {
            res append (v.dotName +" [label=\""+DotHelpers.escape(v.name+"#"+v.id)+"\"];\n")
        }
    }

    override def drawGraph(res: StringBuffer) {
      res append " compound=true;\n"
      super.drawGraph(res)
    }

    override def edgeToString(res: StringBuffer, le: CFGEdge[CFGTrees.Statement]) {

      le.label match {
        case aam: CFGTrees.AssignApplyMeth =>
          res append DotHelpers.arrow(le.v1.dotName, le.dotName)
          res append DotHelpers.arrow(le.dotName, le.v2.dotName)
          var methDesc = le.label.toString+"\\n("+aam.meth.fullName+")"

          if (!aam.excludedSymbols.isEmpty) {
            methDesc += "\\n"+aam.excludedSymbols.map("- "+_.fullName).mkString("\\n")
          }

          res append DotHelpers.box(le.dotName, methDesc)

        case bb: CFGTrees.BasicBlock =>
          res append DotHelpers.arrow(le.v1.dotName, le.dotName)
          res append DotHelpers.arrow(le.dotName, le.v2.dotName)
          res append DotHelpers.box(le.dotName, bb.stmts.map(_.toString).mkString("\\n\\n"))

        case e: CFGTrees.Effect =>
          val id = e.uniqueID.ids.map{ case (i,n) => i+"_"+n }.mkString("")

          val clusterName = "cluster"+id;
          val invisName   = "invis"+id;

          res append "subgraph "+clusterName+" {\n"
          res append "  label=\""+DotHelpers.escape(e.name)+"\";\n"
          res append "  color=\"gray\";\n"

          res append "  "+invisName+" [color=white, fontcolor=white]; \n"

          if (e.env.category.isBottom) {
            res append "  bottom"+id+" [label=\"(Bottom)\", color=white]; "
          } else if (e.env.category.isTop) {
            res append "  top"+id+" [label=\"(Top)\", color=white]; "
          } else {
            val ptdot = new PointToGraphs.PTDotConverter(e.env, "Effects", "x"+id+prefix)
            ptdot.drawGraph(res)
          }

          res append "}\n"

          res append DotHelpers.arrow(le.v1.dotName, invisName, "lhead="+clusterName :: Nil)
          res append DotHelpers.arrow(invisName, le.v2.dotName, "ltail="+clusterName :: Nil)
        case _ =>
          res append DotHelpers.arrow(le.v1.dotName, le.dotName)
          res append DotHelpers.arrow(le.dotName, le.v2.dotName)
          res append DotHelpers.box(le.dotName, le.label.toString)
      }
    }
  }

  abstract class CFGCopier extends GraphCopier[CFGTrees.Statement, CFGVertex, CFGEdge[CFGTrees.Statement]] {

     def copyStmt(stmt: CFGTrees.Statement): CFGTrees.Statement

     override def copyEdge(e: CFGEdge[CFGTrees.Statement]) = CFGEdge(e.v1, copyStmt(e.label), e.v2)
  }
}

