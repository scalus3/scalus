package scalus.uplc.jit

import scalus.cardano.ledger.Word64
import scalus.uplc.builtin.Data
import scalus.uplc.eval.*
import scalus.uplc.{Constant, DefaultUni, Term}

/** Precompiled code that generated code calls: the failures of applying and forcing, and casing on
  * values.
  *
  * Each method replaces what would otherwise be generated at every site. One static call per site
  * keeps the generated methods small; HotSpot inlines these small methods into the generated code.
  * Not the calls of UPLC functions themselves: a call inside a shared method has one receiver
  * profile for all sites, so HotSpot inlines none of them. Those calls are generated at each site.
  * Each one fails where `Cek.scala` fails, after the same spends and with the same exception class:
  * the generated code computes every operand first, in CEK order, and only then calls in here. The
  * environment of each error is empty: compiled code keeps none.
  */
object JitRuntime {

    /** The failure of applying `f`, which is not a `LamValue`, as `Cek.scala` `applyEvaluate`
      * fails: a builtin that expects a force, or any other non-function. Both operands were
      * computed before, so their steps are spent before the failure, as in the CEK machine.
      */
    def cannotApply(f: UplcValue): Nothing = f match
        case _: BuiltinDelayValue =>
            // a builtin that expects a force; the JIT does not keep its term, so `error` stands in
            throw new UnexpectedBuiltinTermArgumentMachineError(Term.Error(), Nil)
        case other => throw new NonFunctionalApplicationMachineError(toCekValue(other), Nil)

    /** The failure of forcing `t`, which is not a `DelayValue`, as `Cek.scala` `forceEvaluate`
      * fails: a builtin that expects a term argument, or any other non-delay.
      */
    def cannotForce(t: UplcValue): Nothing = t match
        case _: BuiltinLamValue =>
            // a builtin that expects a term argument; `error` stands in for its term
            throw new BuiltinTermArgumentExpectedMachineError(Term.Error(), Nil)
        case other => throw new NonPolymorphicInstantiationMachineError(toCekValue(other), Nil)

    /** The largest constructor tag that can select a branch: `Cek.scala` `maxIndexableTag`. */
    private val MaxIndexableTag = Int.MaxValue.toLong

    /** The branch of `(case s b0 .. b(n-1))` that `s` selects, with the checks and in the order of
      * `Cek.scala` `returnCek` for `FrameCases`, at van Rossem (PV11: case on builtins on, case on
      * `Data` off):
      *   - `ConstrValue`: its tag, if below `n` (the tag is unsigned);
      *   - integer `i`: `i`, if `0 <= i < n`;
      *   - bool: `False` is 0, `True` is 1, with 1 or 2 branches;
      *   - unit: 0, with exactly 1 branch;
      *   - list constant: a non-empty list is 0 (cons), the empty list 1 (nil), with 1 or 2
      *     branches;
      *   - pair constant: 0, with exactly 1 branch.
      *
      * Anything else fails: a `Data` constant (case on `Data` is a PV12 feature, see
      * [[caseIndexOnData]]), any other constant, a `ListValue` (the CEK machine does not case on a
      * `VList`) and a function. The fields that the branch takes are applied by [[applyFields]].
      *
      * Code compiled for PV11 calls this method; code compiled for PV12 calls [[caseIndexOnData]]
      * (`JIT.genCaseDispatch` chooses at JIT-compile time).
      */
    def caseIndex(s: UplcValue, n: Int): Int = s match
        case ConstrValue(tag, _) =>
            if java.lang.Long.compareUnsigned(tag, MaxIndexableTag) > 0 || tag >= n then
                throw new MissingCaseBranch(Word64(tag), Nil)
            tag.toInt
        case Constant.Integer(i) =>
            if i >= 0 && i < n then i.toInt
            else throw new CaseIndexOutOfBounds(i, n, Nil)
        case Constant.Bool(b) =>
            val index = if b then 1 else 0
            if n == 0 || n > 2 || index >= n then throw new CaseBoolBranchMissing(b, n, Nil)
            index
        case Constant.Unit =>
            if n != 1 then throw new CaseUnitBranchMissing(n, Nil)
            0
        case Constant.List(_, elems) =>
            if n == 0 || n > 2 then throw new CaseListBranchError(n, Nil)
            if elems.nonEmpty then 0
            else if n < 2 then throw new CaseListBranchError(n, Nil)
            else 1
        case Constant.Pair(_, _) =>
            if n != 1 then throw new CasePairBranchError(n, Nil)
            0
        case Constant.Data(_) =>
            // Case on Data is allowed from Dijkstra (PV12); at PV11 the CEK machine rejects every
            // Data scrutinee, a Data.Constr and a Data.I alike (`Cek.scala`,
            // `CaseDataNotSupportedError`).
            throw new CaseDataNotSupportedError(Nil)
        case other =>
            throw new NonConstrScrutinized(toCekValue(other), Nil)

    /** [[caseIndex]] at Dijkstra (PV12), where `case` also scrutinizes a `Data` constant (Plutus
      * `caseBuiltinData`, `Cek.scala` with `caseOnDataEnabled`): a `Data.Constr` selects the branch
      * of its tag, if `0 <= tag < n`, else it fails with `CaseIndexOutOfBounds`; a `Map`, `List`,
      * `I` or `B` fails with `CaseDataNonConstrError`. Every other scrutinee selects as in
      * [[caseIndex]]. The branch gets the fields as one `(list data)`, see [[applyFields]].
      *
      * Only code compiled for PV12 calls this method: the JIT generates code for one protocol
      * version, so no flag is checked at evaluation time.
      * {{{
      * caseIndexOnData(Constant.Data(Data.Constr(1, Nil)), 2)   // 1
      * caseIndexOnData(Constant.Data(Data.Constr(2, Nil)), 2)   // CaseIndexOutOfBounds
      * caseIndexOnData(Constant.Data(Data.I(5)), 2)             // CaseDataNonConstrError
      * caseIndexOnData(Constant.Integer(1), 2)                  // 1, as caseIndex
      * }}}
      */
    def caseIndexOnData(s: UplcValue, n: Int): Int = s match
        case Constant.Data(Data.Constr(tag, _)) =>
            // the tag is a BigInt: compared before `toInt`, as in `Cek.scala`
            if tag >= 0 && tag < n then tag.toInt
            else throw new CaseIndexOutOfBounds(tag, n, Nil)
        case Constant.Data(other) => throw new CaseDataNonConstrError(other, Nil)
        case other                => caseIndex(other, n)

    /** Applies `branch`, the value of the branch [[caseIndex]] or [[caseIndexOnData]] chose, to the
      * fields of `s`, as the CEK machine pushes them (`FrameAwaitFunValue`): the arguments of a
      * `ConstrValue` in order; the head and the tail of a non-empty list constant; both elements of
      * a pair; for a `Data.Constr`, one argument: its fields as one `(list data)` constant (Plutus
      * `caseBuiltinData`, not one argument per field). Other scrutinees have no fields. Applying a
      * field spends nothing, as in the CEK machine.
      *
      * For `(case (con data (Constr 0 [I 1, B #00])) (lam fs fs))` the branch gets one argument:
      * {{{
      * call(stack, branch, Constant.List(DefaultUni.Data, List(Constant.Data(I(1)), Constant.Data(B(#00)))))
      * }}}
      *
      * Every call is depth-counted in `stack`, like an application: a loop through `case` alone
      * recurses through these calls, and an uncounted one overflowed the JVM stack. The calls stay
      * in this shared method: generated at every branch (one call site per branch, as for `Apply`),
      * a case-heavy Scalus script (midgard) ran 14 % slower, from the larger code.
      */
    def applyFields(
        stack: nativestack.NativeStackContext,
        branch: UplcValue,
        s: UplcValue
    ): UplcValue = s match
        case ConstrValue(_, args) =>
            // args.foldLeft(branch)(call(stack, _, _)), as a loop: no closure allocation per `case`
            var f = branch
            var rest = args
            while rest.nonEmpty do
                f = call(stack, f, rest.head)
                rest = rest.tail
            f
        case Constant.List(elemType, head :: tail) =>
            call(stack, call(stack, branch, head), Constant.List(elemType, tail))
        case Constant.Pair(a, b)                   => call(stack, call(stack, branch, a), b)
        case Constant.Data(Data.Constr(_, fields)) =>
            // reached only from code compiled for PV12: caseIndex, which code compiled for PV11
            // calls, rejects every Data scrutinee before the branch is applied
            val list = Constant.List(DefaultUni.Data, fields.toScalaList.map(Constant.Data(_)))
            call(stack, branch, list)
        case _ => branch

    /** One depth-counted call of a field, for [[applyFields]]. */
    private def call(
        stack: nativestack.NativeStackContext,
        f: UplcValue,
        arg: UplcValue
    ): UplcValue = f match
        case l: LamValue =>
            stack.incr()
            val r = l.apply(arg)
            stack.decr()
            r
        case other => cannotApply(other)

    /** The `CekValue` the CEK machine would hold for `v`.
      *
      * A constant becomes `VCon`, a `ConstrValue` `VConstr`, a `ListValue` `VList`. A function has
      * no `CekValue` form: a `LamValue` becomes a `VLamAbs` and a `DelayValue` a `VDelay` of the
      * `error` term, stand-ins with the same memory usage (1, `MemoryUsage.memoryUsage`) and the
      * same failure when a builtin unlifts them as a constant. Used by the bridge for the arguments
      * of a builtin, and for the values in error messages.
      */
    def toCekValue(v: UplcValue): CekValue = v match
        case c: Constant          => CekValue.VCon(c)
        case ConstrValue(tag, as) => CekValue.VConstr(Word64(tag), as.map(toCekValue))
        case ListValue(elems)     => CekValue.VList(elems.map(toCekValue))
        case _: LamValue          => CekValue.VLamAbs("jit", Term.Error(), Nil)
        case _: DelayValue        => CekValue.VDelay(Term.Error(), Nil)

    /** A short description of a value for an error message. */
    def describe(v: UplcValue): String = v match
        case c: Constant      => c.toString.take(80)
        case c: ConstrValue   => s"constr ${c.tag} with ${c.args.size} fields"
        case ListValue(elems) => s"list of ${elems.size} values"
        case _: LamValue      => "a function"
        case _: DelayValue    => "a delayed term"
}
