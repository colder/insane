package insane
package types

import utils.Reporters._

trait TypeSignatures { self: AnalysisComponent =>

  import global._

  object SigEntry {
    def fromTypeInfo(info: TypeInfo): SigEntry = {
      SimpleSigEntry(info)
    }

    def empty: SigEntry = EmptySigEntry
  }

  abstract class SigEntry(val info: TypeInfo) {
    def withInfo(info: TypeInfo): SigEntry

    def toStringDepth(d: Int): String;

    def preciseSigFor(field: Field): Option[SigEntry];

    def union(that: SigEntry): SigEntry = {
      (this, that) match {
        case (EmptySigEntry, b) =>
          b
        case (a, EmptySigEntry) =>
          a
        case (RecursiveSigEntry(ra), RecursiveSigEntry(rb)) if ra == rb =>
          this
        case (FieldsSigEntry(ia, fa), FieldsSigEntry(ib, fb)) =>
          val fu = (fb.keySet++fb.keySet).map{
            k => k -> (fa.getOrElse(k, EmptySigEntry) union fb.getOrElse(k, EmptySigEntry))
          }
          FieldsSigEntry(ia union ib, fu.toMap)
        case (a, b) =>
          SigEntry.fromTypeInfo(a.info union b.info)
      }
    }

  }


  case class SimpleSigEntry(_info: TypeInfo) extends SigEntry(_info) {
    def withInfo(info: TypeInfo): SigEntry = {
      SigEntry.fromTypeInfo(info)
    }

    override def toStringDepth(f: Int) = {
      info.toString
    }

    override def toString = toStringDepth(1)

    def preciseSigFor(field: Field): Option[SigEntry] = None
  }

  object EmptySigEntry extends SimpleSigEntry(TypeInfo.empty) {
    override def toStringDepth(f: Int) = {
      "?empty?"
    }
  }

  case class FieldsSigEntry(_info: TypeInfo, fields: Map[Field, SigEntry]) extends SigEntry(_info) {
    def withInfo(info: TypeInfo): SigEntry = {
      FieldsSigEntry(info, fields)
    }

    override def toStringDepth(d: Int) = {
      info.toString+" with "+fields.map{ case (s, se) => s.strName + " -> " +se.toStringDepth(d) }.mkString("{", ", ", "}")
    }

    override def toString = toStringDepth(1)

    def preciseSigFor(field: Field): Option[SigEntry] = fields.get(field)
  }

  case class RecursiveSigEntry(to: FieldsSigEntry) extends SigEntry(to.info) {
    def withInfo(info: TypeInfo): SigEntry = {
      to.withInfo(info)
    }

    override def toStringDepth(d: Int) = {
      if (d == 0) {
        ".o."
      } else {
        to.toStringDepth(d-1)
      }
    }

    override def toString = toStringDepth(1)

    def preciseSigFor(field: Field): Option[SigEntry] = to.preciseSigFor(field)
  }

  case class TypeSignature(rec: SigEntry, args: Seq[SigEntry], tm: DualTypeMap) {

    def clampAccordingTo(fun: AbsFunction): TypeSignature = {
      clampAccordingTo(fun.symbol)
    }

    // Make this call signature compatible with proxy's signature
    // TODO handle possible arguments, not only receiver as currently
    def convertForProxy(stub: Symbol, impl: AbsFunction): TypeSignature = {
      this.copy(rec = SigEntry.fromTypeInfo(TypeInfo.subtypeOf(impl.symbol.owner.tpe)))
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

    def fromDeclaration(sym: Symbol): TypeSignature = {
      sym.tpe match {
        case MethodType(params, _) =>
          TypeSignature(TypeInfo.subtypeOf(sym.owner.tpe),
                        params.map(p => TypeInfo.subtypeOf(p.tpe)),
                        DualTypeMap.empty)
        case _ =>
          reporter.fatal("Unable to obtain typesignature from method"+sym.fullName)
      }
    }

    def apply(rec: TypeInfo, args: Seq[TypeInfo], tm: DualTypeMap): TypeSignature = {
      TypeSignature(SigEntry.fromTypeInfo(rec), args.map(a => SigEntry.fromTypeInfo(a)), tm)
    }
  }
}
