package insane
package types

import utils.Reporters._

trait TypeSignatures { self: AnalysisComponent =>

  import global._

  case class TypeSignature(rec: TypeInfo, args: Seq[TypeInfo], tm: DualTypeMap) {

    def clampAccordingTo(fun: AbsFunction): TypeSignature = {
      clampAccordingTo(fun.symbol)
    }

    def clampAccordingTo(meth: Symbol): TypeSignature = {
      val resTpe = canBeSubtypeOf(rec.tpe, meth.owner.tpe)

      if (resTpe.isEmpty) {
        TypeSignature(TypeInfo.subtypeOf(meth.owner.tpe), args, tm)
      } else {
        TypeSignature(TypeInfo.subtypeOf(resTpe.get), args, tm)
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
  }
}
