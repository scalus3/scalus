package scalus.compiler.sir

import java.util

trait SIRHashCodeInRec[T] {

    def apply(t: T, trace: java.util.IdentityHashMap[SIRType, Int]): Int

    def applyEq(a: T, b: T, trace: java.util.IdentityHashMap[SIRType, List[SIRType]]): Boolean

    def newRec(t: T): Int = {
        val trace = new util.IdentityHashMap[SIRType, Int]()
        apply(t, trace)
    }

    def newRecEq(a: T, b: T): Boolean = {
        val trace = new util.IdentityHashMap[SIRType, List[SIRType]]()
        applyEq(a, b, trace)
    }

}

object SIRHashCodeInRec {

    final val enabled = false

    given SIRHashCodeInRec[SIRType] with {

        def apply(t: SIRType, trace: java.util.IdentityHashMap[SIRType, Int]): Int = {
            t match {
                case p: SIRType.Primitive => p.hashCode()
                case f: SIRType.Fun =>
                    summon[SIRHashCodeInRec[SIRType.Fun]](f, trace)
                case sc: SIRType.SumCaseClass =>
                    summon[SIRHashCodeInRec[SIRType.SumCaseClass]](sc, trace)
                case cc: SIRType.CaseClass =>
                    summon[SIRHashCodeInRec[SIRType.CaseClass]](cc, trace)
                case tl: SIRType.TypeLambda =>
                    summon[SIRHashCodeInRec[SIRType.TypeLambda]](tl, trace)
                case lb: SIRType.TypeProxy =>
                    summon[SIRHashCodeInRec[SIRType.TypeProxy]](lb, trace)
                case other =>
                    other.hashCode()
            }
        }

        def applyEq(
            a: SIRType,
            y: SIRType,
            trace: java.util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {
            (a, y) match
                case (a: SIRType.Primitive, y: SIRType.Primitive) => a == y
                case (a: SIRType.Fun, y: SIRType.Fun) =>
                    summon[SIRHashCodeInRec[SIRType.Fun]].applyEq(a, y, trace)
                case (a: SIRType.SumCaseClass, y: SIRType.SumCaseClass) =>
                    summon[SIRHashCodeInRec[SIRType.SumCaseClass]].applyEq(a, y, trace)
                case (a: SIRType.CaseClass, y: SIRType.CaseClass) =>
                    summon[SIRHashCodeInRec[SIRType.CaseClass]].applyEq(a, y, trace)
                case (a: SIRType.TypeLambda, y: SIRType.TypeLambda) =>
                    summon[SIRHashCodeInRec[SIRType.TypeLambda]].applyEq(a, y, trace)
                case (a: SIRType.TypeProxy, y: SIRType.TypeProxy) =>
                    summon[SIRHashCodeInRec[SIRType.TypeProxy]].applyEq(a, y, trace)
                case (a: SIRType.TypeProxy, b: SIRType) =>
                    if a.ref == null then false else applyEq(a.ref, b, trace)
                case (a: SIRType, b: SIRType.TypeProxy) =>
                    if b.ref == null then false else applyEq(a, b.ref, trace)
                case _ =>
                    a == y
        }

    }

    given SIRHashCodeInRec[SIRType.Fun] with {

        def apply(f: SIRType.Fun, trace: java.util.IdentityHashMap[SIRType, Int]): Int = {
            import scala.util.hashing.MurmurHash3
            var h = MurmurHash3.productSeed
            trace.put(f, 0)
            h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[SIRType]](f.in, trace))
            h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[SIRType]](f.out, trace))
            h = MurmurHash3.finalizeHash(h, 2)
            trace.put(f, h)
            h
        }

        def applyEq(
            a: SIRType.Fun,
            b: SIRType.Fun,
            trace: java.util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {
            val inTrace = trace.get(a)
            if (inTrace eq null) || (!inTrace.contains(b)) then
                summon[SIRHashCodeInRec[SIRType]].applyEq(a.in, b.in, trace) &&
                summon[SIRHashCodeInRec[SIRType]].applyEq(a.out, b.out, trace)
            else true
        }

    }

    given SIRHashCodeInRec[SIRType.SumCaseClass] with {

        def apply(sc: SIRType.SumCaseClass, trace: java.util.IdentityHashMap[SIRType, Int]): Int = {
            import scala.util.hashing.MurmurHash3
            var h = MurmurHash3.productSeed
            val found = trace.get(sc)
            if !(found eq null) then found
            else
                trace.put(sc, 0)
                h = MurmurHash3.mix(h, "SumCaseClass".hashCode())
                h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[DataDecl]](sc.decl, trace))
                h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[List[SIRType]]](sc.typeArgs, trace))
                h = MurmurHash3.finalizeHash(h, 2)
                trace.put(sc, h)
                h
        }

        def applyEq(
            a: SIRType.SumCaseClass,
            b: SIRType.SumCaseClass,
            trace: java.util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {
            val inTrace = trace.get(a)
            if (inTrace eq null) || !inTrace.contains(b) then
                val nSameAsA = if inTrace eq null then List(b) else b :: inTrace
                trace.put(a, nSameAsA)
                summon[SIRHashCodeInRec[DataDecl]].applyEq(a.decl, b.decl, trace) &&
                summon[SIRHashCodeInRec[List[SIRType]]].applyEq(a.typeArgs, b.typeArgs, trace)
            else true
        }

    }

    given SIRHashCodeInRec[SIRType.CaseClass] with {

        def apply(cc: SIRType.CaseClass, trace: java.util.IdentityHashMap[SIRType, Int]): Int = {
            import scala.util.hashing.MurmurHash3
            val found = trace.get(cc)
            if !(found eq null) then found
            else
                trace.put(cc, 0)
                var h = MurmurHash3.productSeed
                h = MurmurHash3.mix(h, "CaseClass".hashCode())
                h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[ConstrDecl]](cc.constrDecl, trace))
                h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[List[SIRType]]](cc.typeArgs, trace))
                h = MurmurHash3.finalizeHash(h, 2)
                trace.put(cc, h)
                h
        }

        def applyEq(
            a: SIRType.CaseClass,
            b: SIRType.CaseClass,
            trace: java.util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {
            val inTrace = trace.get(a)
            if (inTrace eq null) || !inTrace.contains(b) then
                val nSameAsA = if inTrace eq null then List(b) else b :: inTrace
                trace.put(a, nSameAsA)
                summon[SIRHashCodeInRec[ConstrDecl]].applyEq(a.constrDecl, b.constrDecl, trace) &&
                summon[SIRHashCodeInRec[List[SIRType]]].applyEq(a.typeArgs, b.typeArgs, trace)
            else true
        }

    }

    given SIRHashCodeInRec[TypeBinding] with {

        def apply(tb: TypeBinding, trace: java.util.IdentityHashMap[SIRType, Int]): Int = {
            import scala.util.hashing.MurmurHash3
            var h = MurmurHash3.productSeed
            h = MurmurHash3.mix(h, "TypeBinding".hashCode())
            h = MurmurHash3.mix(h, tb.name.hashCode())
            h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[SIRType]](tb.tp, trace))
            h = MurmurHash3.finalizeHash(h, 2)
            h
        }

        override def applyEq(
            a: TypeBinding,
            b: TypeBinding,
            trace: util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {
            a.name == b.name &&
            summon[SIRHashCodeInRec[SIRType]].applyEq(a.tp, b.tp, trace)
        }

    }

    given SIRHashCodeInRec[SIRType.TypeLambda] with {

        override def apply(
            t: SIRType.TypeLambda,
            trace: util.IdentityHashMap[SIRType, Int]
        ): Int = {
            import scala.util.hashing.MurmurHash3
            val found = trace.get(t)
            if !(found eq null) then found
            else
                trace.put(t, 0)
                var h = MurmurHash3.productSeed
                h = MurmurHash3.mix(h, "TypeLambda".hashCode())
                h = MurmurHash3.mix(h, t.params.hashCode())
                h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[SIRType]](t.body, trace))
                h = MurmurHash3.finalizeHash(h, 2)
                trace.put(t, h)
                h
        }

        override def applyEq(
            a: SIRType.TypeLambda,
            b: SIRType.TypeLambda,
            trace: util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {
            val inTrace = trace.get(a)
            if (inTrace eq null) || !inTrace.contains(b) then
                val nSameAsA = if inTrace eq null then List(b) else b :: inTrace
                trace.put(a, nSameAsA)
                a.params == b.params &&
                summon[SIRHashCodeInRec[SIRType]].applyEq(a.body, b.body, trace)
            else true
        }

    }

    given SIRHashCodeInRec[SIRType.TypeProxy] with {

        override def apply(t: SIRType.TypeProxy, trace: util.IdentityHashMap[SIRType, Int]): Int = {
            if t.ref == null then 0
            else summon[SIRHashCodeInRec[SIRType]](t.ref, trace)
        }

        override def applyEq(
            a: SIRType.TypeProxy,
            b: SIRType.TypeProxy,
            trace: util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {
            if a.ref == null then b.ref == null
            else summon[SIRHashCodeInRec[SIRType]].applyEq(a.ref, b.ref, trace)
        }

    }

    given SIRHashCodeInRec[DataDecl] with {

        override def apply(t: DataDecl, trace: util.IdentityHashMap[SIRType, Int]): Int = {
            import scala.util.hashing.MurmurHash3
            var h = MurmurHash3.productSeed
            h = MurmurHash3.mix(h, "DataDecl".hashCode())
            h = MurmurHash3.mix(h, t.name.hashCode())
            h = MurmurHash3.mix(
              h,
              summon[SIRHashCodeInRec[List[ConstrDecl]]](t.constructors, trace)
            )
            h = MurmurHash3.finalizeHash(h, 2)
            h
        }

        override def applyEq(
            a: DataDecl,
            b: DataDecl,
            trace: util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {
            a.name == b.name &&
            summon[SIRHashCodeInRec[List[ConstrDecl]]]
                .applyEq(a.constructors, b.constructors, trace)
        }

    }

    given SIRHashCodeInRec[ConstrDecl] with {

        override def apply(t: ConstrDecl, trace: util.IdentityHashMap[SIRType, Int]): Int = {
            import scala.util.hashing.MurmurHash3
            var h = MurmurHash3.productSeed
            h = MurmurHash3.mix(h, "ConstrDecl".hashCode())
            h = MurmurHash3.mix(h, t.name.hashCode())
            h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[List[TypeBinding]]](t.params, trace))
            h = MurmurHash3.mix(h, t.typeParams.hashCode())
            h = MurmurHash3.finalizeHash(h, 4)
            h
        }

        override def applyEq(
            a: ConstrDecl,
            b: ConstrDecl,
            trace: util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {
            a.name == b.name &&
            summon[SIRHashCodeInRec[List[TypeBinding]]].applyEq(a.params, b.params, trace) &&
            a.typeParams == b.typeParams
        }

    }

    given list[T: SIRHashCodeInRec]: SIRHashCodeInRec[List[T]] with {

        override def apply(t: List[T], trace: util.IdentityHashMap[SIRType, Int]): Int = {
            import scala.util.hashing.MurmurHash3
            var h = MurmurHash3.seqSeed
            var n = 0;
            var c = t;
            while c.nonEmpty do {
                h = MurmurHash3.mix(h, summon[SIRHashCodeInRec[T]](c.head, trace))
                n += 1
                c = c.tail
            }
            h = MurmurHash3.finalizeHash(h, n)
            h
        }

        override def applyEq(
            a: List[T],
            b: List[T],
            trace: util.IdentityHashMap[SIRType, List[SIRType]]
        ): Boolean = {

            @annotation.tailrec
            def loop(a: List[T], b: List[T]): Boolean = {
                (a, b) match {
                    case (Nil, Nil) => true
                    case (ah :: at, bh :: bt) =>
                        if !summon[SIRHashCodeInRec[T]].applyEq(ah, bh, trace) then false
                        else loop(at, bt)
                    case _ => false
                }
            }

            loop(a, b)

        }

    }

}
