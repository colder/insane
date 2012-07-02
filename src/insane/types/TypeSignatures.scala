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


  // Helper function to traverse graph and compute type signature from nodes
  def typeSignatureFromNodes(env: PTEnv, nodes: Iterable[PointToGraphs.Node], depth: Int): SigEntry = {
    //withDebugCounter { cnt =>
    //  reporter.debug("Computing TypeSig From Nodes: "+nodes+" in:")
    //  dumpPTE(env, "sig-"+cnt+".dot")
    //}

    val info = (TypeInfo.empty /: nodes) (_ union _.types)
    val sig  = (SigEntry.empty /: nodes) (_ union _.sig)

    if (depth == 0) {
        sig
    } else {
      val fieldsSig = info.tpe.decls.filter(sym => !sym.isMethod && !sym.isMutable).flatMap{ sym =>
        val field = Field(sym)
        // Static type of the field
        val tpe       = TypeInfo.subtypeOf(info.tpe.memberType(sym))
        val staticSig = SigEntry.fromTypeInfo(tpe)

        var abort = false;
        val allTargets = for (n <- nodes if !abort) yield {
          val targets = env.getWriteOrElseReadTargets(Set(n), field);

          if (targets.isEmpty) {
            // One node does not contain an edge on that field
            abort = true;
          }
          targets
        }

        val targets = allTargets.flatten

        if (abort || targets.isEmpty) {
          // Some targets may have been found, but not for all nodes
          sig.preciseSigFor(field).map(sig => field -> sig)
        } else {
          val nodesSig = typeSignatureFromNodes(env, allTargets.flatten, depth-1)

          if (nodesSig != staticSig) {
            Some(field -> nodesSig)
          } else {
            None
          }
        }
      }

      val res = if (!fieldsSig.isEmpty) {
        // We have at least one field that is precise
        FieldsSigEntry(info, fieldsSig.toMap)
      } else {
        sig
      }

      //reporter.debug("Computed: "+res)

      res
    }
  }

  abstract class SigEntry(val info: TypeInfo) {
    def limitDepth(d: Int): SigEntry

    def withInfo(info: TypeInfo): SigEntry

    def toStringDepth(d: Int): String;

    def preciseSigFor(field: Field): Option[SigEntry];

    def union(that: SigEntry): SigEntry = {
      (this, that) match {
        case (EmptySigEntry, b) =>
          b
        case (a, EmptySigEntry) =>
          a
        //case (RecursiveSigEntry(ra), RecursiveSigEntry(rb)) if ra == rb =>
        //  this
        case (FieldsSigEntry(ia, fa), FieldsSigEntry(ib, fb)) =>
          val fu = (fb.keySet++fb.keySet).map{
            k => k -> (fa.getOrElse(k, EmptySigEntry) union fb.getOrElse(k, EmptySigEntry))
          }
          FieldsSigEntry(ia union ib, fu.toMap)
        case (a, b) =>
          SigEntry.fromTypeInfo(a.info union b.info)
      }
    }
  
    def lessPreciseThan(that: SigEntry): Boolean  = {
      (this, that) match {
        case (FieldsSigEntry(info1, fields1), FieldsSigEntry(info2, fields2)) =>
         (info2 isMorePreciseThan info1) && (fields1.keySet & fields2.keySet).forall(k =>  fields1(k) lessPreciseThan fields2(k) )
        case (FieldsSigEntry(info1, fields1), SimpleSigEntry(info2)) =>
          false
        case (SimpleSigEntry(info1), FieldsSigEntry(info2, fields2)) =>
          info2 isMorePreciseThan info1
        case (SimpleSigEntry(info1), SimpleSigEntry(info2)) =>
          info2 isMorePreciseThan info1
      }
    }

  }


  case class SimpleSigEntry(_info: TypeInfo) extends SigEntry(_info) {
    def limitDepth(d: Int): SigEntry = this

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
    def limitDepth(d: Int): SigEntry = if (d == 0) {
      SimpleSigEntry(_info)
    } else {
      FieldsSigEntry(_info, fields.mapValues(_.limitDepth(d-1)))
    }

    def withInfo(info: TypeInfo): SigEntry = {
      FieldsSigEntry(info, fields)
    }

    override def toStringDepth(d: Int) = {
      info.toString+" with "+fields.map{ case (s, se) => s.name + " -> " +se.toStringDepth(d) }.mkString("{", ", ", "}")
    }

    override def toString = toStringDepth(1)

    def preciseSigFor(field: Field): Option[SigEntry] = fields.get(field)
  }

  /*
  case class RecursiveSigEntry(to: SigEntry) extends SigEntry(to.info) {
    def withInfo(info: TypeInfo): SigEntry = {
      to.withInfo(info)
    }

    def limitDepth(d: Int): SigEntry = if (d == 0) {
      SimpleSigEntry(info)
    } else {
      RecursiveSigEntry(to.limitDepth(d-1))
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
  */

  case class TypeSignature(rec: SigEntry, args: Seq[SigEntry], tm: DualTypeMap) {

    def limitDepth(d: Int): TypeSignature = {
      TypeSignature(rec.limitDepth(d), args.map(_.limitDepth(d)), tm)
    }
    def lessPreciseThan(other: TypeSignature) = {
      ((rec +: args) zip (other.rec +: other.args)).forall{ case (s1, s2) => s1 lessPreciseThan s2 }
    }

    def clampAccordingTo(fun: AbsFunction): TypeSignature = {
      clampAccordingTo(fun.symbol)
    }

    // Make this call signature compatible with proxy's signature
    // TODO handle possible arguments, not only receiver as currently
    def convertForProxy(stub: Symbol, impl: AbsFunction): TypeSignature = {
      this.copy(rec = SigEntry.fromTypeInfo(TypeInfo.subtypeOf(impl.symbol.owner.tpe)))
    }

    def clampAccordingTo(meth: Symbol): TypeSignature = {
      val methodRec = TypeInfo.subtypeOf(meth.owner.tpe)

      if (rec.info.orSubtypes) {
        if (rec.info isMorePreciseThan methodRec) {
          // Call is to a parent method, we keep the precise receiver
          this
        } else {
          TypeSignature(rec.withInfo(methodRec), args, tm)
        }
      } else {
        // rec is already fixed to a specific object, no point in trying to refine it
        this
      }
    }

    override def toString = {
      "("+rec+"; "+args.mkString(", ")+")"+tm
    }

    def combine(that: TypeSignature) = {
      if (this == that) {
        this
      } else {
        assert(that.args.size == this.args.size) 

        TypeSignature(this.rec union that.rec, (this.args zip that.args) map { case (t1, t2) => t1 union t2 }, this.tm)
      }
    }
  }

  object TypeSignature {
    def fromDeclaration(fun: AbsFunction): TypeSignature = {
      TypeSignature(TypeInfo.subtypeOf(fun.symbol.owner.tpe),
                    fun.args.map(sym => TypeInfo.subtypeOf(sym.tpe)),
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
