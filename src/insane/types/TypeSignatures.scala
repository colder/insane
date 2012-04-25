package insane
package types

import utils.Reporters._

trait TypeSignatures { self: AnalysisComponent =>

  import global._

  object SigEntry {
    def fromTypeInfo(info: TypeInfo): SigEntry = {
      SimpleSigEntry(info)
    }
  }

  abstract class SigEntry(val info: TypeInfo) {
    def limitDepth(depth: Int): SigEntry

    def withInfo(info: TypeInfo): SigEntry
  }

  case class SimpleSigEntry(_info: TypeInfo) extends SigEntry(_info) {
    def limitDepth(depth: Int): SigEntry = {
      this
    }

    def withInfo(info: TypeInfo): SigEntry = {
      SimpleSigEntry(info)
    }

    override def toString() = info.toString
  }

  case class FieldsSigEntry(_info: TypeInfo, fields: Map[Symbol, SigEntry]) extends SigEntry(_info) {
    def limitDepth(depth: Int): SigEntry = {
      if (depth == 0) {
        SimpleSigEntry(_info)
      } else {
        FieldsSigEntry(_info, fields.mapValues(_.limitDepth(depth-1)))
      }
    }

    def withInfo(info: TypeInfo): SigEntry = {
      FieldsSigEntry(info, fields)
    }

    override def toString() = info.toString+" with "+fields.map{ case (s, se) => s + " -> " +se }.mkString("{", ", ", "}")
  }

  case class TypeSignature(rec: SigEntry, args: Seq[SigEntry], tm: DualTypeMap) {

    def clampAccordingTo(fun: AbsFunction): TypeSignature = {
      clampAccordingTo(fun.symbol)
    }

    def clampAccordingTo(meth: Symbol): TypeSignature = {
      val resTpe = canBeSubtypeOf(rec.info.tpe, meth.owner.tpe)

      if (resTpe.isEmpty) {
        TypeSignature(rec.withInfo(TypeInfo.subtypeOf(meth.owner.tpe)), args, tm)
      } else {
        TypeSignature(rec.withInfo(TypeInfo.subtypeOf(resTpe.get)), args, tm)
      }
    }


    override def toString = {
      "("+rec+"; "+args.mkString(", ")+")"+tm
    }
  }

  object TypeSignature {
    def fromDeclaration(fun: AbsFunction): TypeSignature = {
      TypeSignature(TypeInfo.subtypeOf(fun.symbol.owner.tpe),
                    fun.args.map(a => TypeInfo.subtypeOf(a.tpt.tpe)),
                    DualTypeMap.empty)
    }

    def apply(rec: TypeInfo, args: Seq[TypeInfo], tm: DualTypeMap): TypeSignature = {
      TypeSignature(SimpleSigEntry(rec), args.map(a => SimpleSigEntry(a)), tm)
    }
  }
}
