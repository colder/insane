package insane
package types

import utils.Reporters._

trait TypeSignatures { self: AnalysisComponent =>

  import global._

  case class TypeSignature(rec: ObjectSet, args: Seq[ObjectSet], tm: DualTypeMap) {

    def clampAccordingTo(fun: AbsFunction): TypeSignature = {
      clampAccordingTo(fun.symbol)
    }

    def clampAccordingTo(meth: Symbol): TypeSignature = {
      val resTypes = rec.exactTypes flatMap {t => canBeSubtypeOf(t, meth.owner.tpe)}
      
      if (resTypes.isEmpty) {
        TypeSignature(ObjectSet.subtypesOf(meth.owner.tpe), args, tm)
      } else {
        TypeSignature(ObjectSet(resTypes, false), args, tm)
      }
    }


    override def toString = {
      "("+rec+"; "+args.mkString(", ")+")"+tm
    }
  }

  object TypeSignature {
    def fromDeclaration(fun: AbsFunction): TypeSignature = {
      TypeSignature(ObjectSet.subtypesOf(fun.symbol.owner.tpe),
                    fun.args.map(a => ObjectSet.subtypesOf(a.tpt.tpe)),
                    DualTypeMap.empty)
    }
  }
}
