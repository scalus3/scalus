package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v3.{ScriptContext, TxOutRef}
import scalus.uplc.Constant
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.verify.{FunctionTable, LeanPredicate, ProofKind, Prop, VerificationResult}
import scalus.verify.Props.*
import scalus.verify.lean.LeanTypeBinding
import scalus.verify.uplcblaster.MappingPlan.{Mapping, Reason}

import scala.concurrent.duration.*

/** How a tactic's [[VariableMapping]] maps a variable: the rules, a choice for every type, one for
  * a type, and a Lean type where one exists ([[LeanTypeBinding]]); and, with Lean, a statement over
  * a script context of CardanoLedgerApi's type, proved and refuted, its counterexample encoded by
  * Lean and replayed.
  */
class VariableMappingTest extends AnyFunSuite with LeanProofsSuite {
    import LeanPredicateTest.{unbalanced, validSpending}
    import proofs.*

    // The checks of this suite prefer Lean types.
    override protected def variableMapping: VariableMapping =
        VariableMapping.rules.preferLeanTypes

    private val leanTypes = VariableMapping.rules.preferLeanTypes

    private def lowered(prop: Prop, mapping: VariableMapping): UplcBlaster.Lowered =
        UplcBlaster
            .lower(prop, FunctionTable.empty, MappingPlan.Fixed.none, rejecting = true, mapping)
            .fold(fail(_), identity)

    private def moduleText(goal: UplcBlaster.Lowered): String =
        UplcBlaster
            .statementModule(
              goal,
              600,
              Some(ProofKind.Blaster),
              400000,
              "Probe.statement",
              // The module names where its programs would be, and writes nothing there.
              "programs"
            )
            .text

    private val fee = forAllData[ScriptContext](ctx => ctx.txInfo.fee >= BigInt(0))

    /** [[fee]] of the valid contexts: a statement that assumes `isValid` of its variable. */
    private val validFee =
        forAllData[ScriptContext](ctx => isValid(ctx) ==>: (ctx.txInfo.fee >= BigInt(0)))

    test("by the rules, a script context is one Data, restricted by its encoding's premise") {
        val goal = lowered(fee, VariableMapping.rules)
        assert(
          goal.plan.nodes.map(node => node.mapping -> node.reason) == List(
            Mapping.AsData -> Reason.Larger
          )
        )
        assert(goal.typed == Vector(None))
        assert(goal.predicates.head.isDefined)
        assert(moduleText(goal).contains("(x0 : Data)"))
    }

    test(
      "preferring Lean types, a script context is one of CardanoLedgerApi's, without a premise"
    ) {
        val goal = lowered(validFee, leanTypes)
        assert(
          goal.plan.nodes.map(node => node.mapping -> node.reason) ==
              List(
                Mapping.AsLeanType(
                  "CardanoLedgerApi.V3.Contexts.ScriptContext"
                ) -> Reason.ByPreference
              )
        )
        assert(goal.typed == Vector(Some(LeanTypeBinding.scriptContextV3)))
        assert(goal.predicates == Vector(None))
        // The programs are those of the rules': the value reaches them as its Data.
        assert(goal.programHashes == lowered(validFee, VariableMapping.rules).programHashes)
        val text = moduleText(goal)
        assert(text.startsWith("import ScalusProofs.Run\n\n"), text)
        assert(text.contains("(x0 : CardanoLedgerApi.V3.Contexts.ScriptContext)"), text)
        assert(
          text.contains("Term.Const (Const.Data (CardanoLedgerApi.IsData.Class.IsData.toData x0))"),
          text
        )
        assert(!text.contains("(x0 : Data)"), text)
        // A type for one type is the same as one for every type that has one.
        val forScriptContext =
            VariableMapping.rules.forType[ScriptContext](VariableMapping.Choice.LeanType)
        assert(lowered(validFee, forScriptContext).plan == goal.plan)
        // Without `isValid` too: the flag decides, and its assumption is the user's.
        assert(lowered(fee, leanTypes).typed == Vector(Some(LeanTypeBinding.scriptContextV3)))
        // The rules keep the context Data.
        assert(lowered(fee, VariableMapping.rules).typed == Vector(None))
    }

    test("Auto, the default, makes a context typed by the statement's form, and Data elsewhere") {
        assert(VariableMapping.default == VariableMapping.auto)
        def typedBy(prop: Prop, refuted: Boolean): Vector[Option[LeanTypeBinding]] =
            UplcBlaster
                .lower(
                  prop,
                  FunctionTable.empty,
                  MappingPlan.Fixed.none,
                  rejecting = true,
                  VariableMapping.auto,
                  expectsRefutation = refuted
                )
                .fold(fail(_), identity)
                .typed
        val typed = Vector(Some(LeanTypeBinding.scriptContextV3))
        // With isValid of the context as a premise: the statement over the Lean type is exactly
        // the one over its Data.
        assert(typedBy(validFee, refuted = false) == typed)
        // A negative control: its refutation is of a value of the Scala type whatever the mapping.
        assert(typedBy(fee, refuted = true) == typed)
        // Neither: Data, which assumes nothing.
        assert(typedBy(fee, refuted = false) == Vector(None))
        // isValid in a claim is no premise: Data.
        val claimed =
            forAllData[ScriptContext](ctx => (ctx.txInfo.fee > BigInt(0)) ==>: isValid(ctx))
        assert(typedBy(claimed, refuted = false) == Vector(None))
        // A variable with limits likewise.
        val limitedValid =
            forAllDataLimited[ScriptContext](ctx => ctx.txInfo.signatories.length <= 2)(ctx =>
                isValid(ctx) ==>: (ctx.txInfo.fee >= BigInt(0))
            )
        val limitedPlain =
            forAllDataLimited[ScriptContext](ctx => ctx.txInfo.signatories.length <= 2)(ctx =>
                ctx.txInfo.fee >= BigInt(0)
            )
        assert(typedBy(limitedValid, refuted = false) == typed)
        assert(typedBy(limitedPlain, refuted = false) == Vector(None))
        assert(typedBy(limitedPlain, refuted = true) == typed)
    }

    test("isValid of a variable of CardanoLedgerApi's type is its own predicate of the value") {
        val goal = lowered(
          forAllData[ScriptContext](ctx => isValid(ctx) ==>: (ctx.txInfo.fee > BigInt(0))),
          leanTypes
        )
        val text = moduleText(goal)
        assert(text.contains("(CardanoLedgerApi.V3.Contexts.validScriptContext x0 = true) →"), text)
        assert(!text.contains("ScalusProofs.Ledger.validScriptContextV3"), text)
    }

    test("a choice for a type, by fields or as Data, is taken there, and the rules elsewhere") {
        val reference = forAll[TxOutRef](ref => ref.idx >= BigInt(0))
        def mapping(choice: VariableMapping.Choice) =
            lowered(reference, VariableMapping.rules.forType[TxOutRef](choice)).plan.nodes.head
        assert(mapping(VariableMapping.Choice.Rules).mapping == Mapping.ByFields)
        val asData = mapping(VariableMapping.Choice.AsData)
        assert(asData.mapping == Mapping.AsData && asData.reason == Reason.ByPreference)
        // No Lean type exists for it: the rules map it.
        assert(mapping(VariableMapping.Choice.LeanType).reason != Reason.ByPreference)
        // A choice for every type is asked of every node, the context's by fields among them.
        val byFields = lowered(fee, VariableMapping(VariableMapping.Choice.ByFields, Map.empty))
        assert(byFields.plan.nodes.head.mapping == Mapping.ByFields)
        assert(byFields.binders.size > MappingPlan.mostByFields)
    }

    test(
      "a variable that asks for a witness is mapped by the rules, as a Lean type may have more values"
    ) {
        val witness = exists[ScriptContext](ctx => ctx.txInfo.fee == BigInt(5))
        UplcBlaster.lower(
          witness,
          FunctionTable.empty,
          MappingPlan.Fixed.none,
          rejecting = true,
          leanTypes
        ) match
            case Right(goal) =>
                assert(goal.plan.nodes.forall(_.reason != Reason.ByPreference), goal.plan.show)
            case Left(_) => () // refused for other reasons, as without the preference
    }

    test("a variable with limits is of the Lean type, and its guard walks its Data") {
        val limited = forAllDataLimited[ScriptContext](ctx => ctx.txInfo.signatories.length <= 2)(
          ctx => isValid(ctx) ==>: (ctx.txInfo.fee >= BigInt(0))
        )
        val goal = lowered(limited, leanTypes)
        assert(
          goal.plan.nodes.map(node => node.mapping -> node.reason) ==
              List(
                Mapping.AsLeanType("CardanoLedgerApi.V3.Contexts.ScriptContext") -> Reason.Limited
              )
        )
        assert(goal.typed == Vector(Some(LeanTypeBinding.scriptContextV3)))
        assert(goal.predicates == Vector(None))
        // The guard is still a test of the program, of the value's Data.
        assert(goal.programHashes == lowered(limited, VariableMapping.rules).programHashes)
    }

    test("a typed counterexample that is not read ends the search, and fails nothing") {
        val goal = lowered(validFee, leanTypes)
        def probed(value: String): Unit =
            UplcBlaster.probed(
              goal,
              400,
              errors = false,
              s"❌ Falsified\n- x0: $value",
              UplcBlaster.Evaluation.none
            ) match
                // Another budget would read it no better: the result is no failure, and no
                // falsification without values, which would double the budget.
                case UplcBlaster.Probe.Decided(VerificationResult.Inconclusive(reason)) =>
                    // The variable is named as the statement names it.
                    assert(
                      reason.matches("Lean's counterexample is not read: ctx_\\d+: .*"),
                      reason
                    )
                case other => fail(s"expected an inconclusive probe, got $other")
        // A term of a sort the reader does not know.
        probed("((as const (Array Int Int)) 0)")
        // A term it reads, which no Lean encodes here.
        probed(
          "(CardanoLedgerApi.V3.Tx.TxOutRef.mk " +
              "(PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk \"A\") " +
              "(as Option.none (@Option Int)) (as List.nil (@List (@Prod Data Data))))"
        )
    }

    test("the solver's term of a typed value is written as Lean's, and Lean's Data text is read") {
        val term =
            """(let ((a!1 (List.cons (Prod.mk (PlutusCore.Data.PlutusCore.DataInternal.Data.B
              |  (PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk ""))
              |  (PlutusCore.Data.PlutusCore.DataInternal.Data.I (- 3)))
              |  (as List.nil (@List (@Prod Data Data))))))
              |(CardanoLedgerApi.V3.Tx.TxOutRef.mk (PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk "A")
              |  (as Option.none (@Option Int)) a!1))""".stripMargin
        assert(
          SmtValues.leanTerm(term, SmtValues.Renaming.none) == Right(
            "(CardanoLedgerApi.V3.Tx.TxOutRef.mk " +
                "(PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk \"\\x41\") none " +
                "(((PlutusCore.Data.PlutusCore.DataInternal.Data.B " +
                "(PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk \"\")), " +
                "(PlutusCore.Data.PlutusCore.DataInternal.Data.I (-3))) :: []))"
          )
        )
        // A part left unconstrained is any value of its type.
        assert(SmtValues.leanTerm("$0", SmtValues.Renaming.none) == Right("default"))
        // A byte string's character that is no byte is read as the counterexample's others are.
        val above =
            "(PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk \"\\u{100}\")"
        assert(
          SmtValues.leanTerm(above, SmtValues.Renaming(Map(0x100 -> 0xfe), Map.empty)) ==
              Right("(PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk \"\\xfe\")")
        )
        assert(
          UplcBlaster.readDataText("(C 0 (I -5) (B 0aff) (B ) (L (I 1)) (M (P (B 01) (I 2))))") ==
              Right(
                Data.Constr(
                  0,
                  prelude.List(
                    Data.I(-5),
                    Data.B(ByteString.fromHex("0aff")),
                    Data.B(ByteString.empty),
                    Data.List(prelude.List(Data.I(1))),
                    Data.Map(prelude.List(Data.B(ByteString.fromHex("01")) -> Data.I(2)))
                  )
                )
              )
        )
        assert(
          UplcBlaster
              .readDataText("(B 0!!1)")
              .left
              .exists(_.isInstanceOf[SmtValues.Unreadable.OutsideType])
        )
        assert(UplcBlaster.readDataText("(C 0 (I 1)").isLeft)
    }

    test("a value with parts no value of the type has is repaired to one of the type") {
        // CardanoLedgerApi keeps the validity range, the eighth field of a transaction, as any
        // Data: a typed counterexample can have one that is no interval.
        val encoding = scalus.verify.DataEncoding.of[ScriptContext]
        val broken = validSpending.toData match
            case Data.Constr(0, prelude.List.Cons(Data.Constr(0, fields), rest)) =>
                val list = fields.toScalaList
                val withRange = list.updated(7, Data.I(5))
                Data.Constr(
                  0,
                  prelude.List.Cons(Data.Constr(0, prelude.List.from(withRange)), rest)
                )
            case other => fail(s"a context's Data is $other")
        assert(encoding.outside(broken).isDefined)
        val fixed = encoding.repaired(broken).getOrElse(fail("not repaired"))
        assert(encoding.outside(fixed).isEmpty)
        // Only the range is another: the rest is as it was.
        val context = fixed.to[ScriptContext]
        assert(
          context.copy(txInfo =
              context.txInfo.copy(validRange = validSpending.txInfo.validRange)
          ) == validSpending
        )
        // A value of the type is its own repair.
        assert(encoding.repaired(validSpending.toData) == Some(validSpending.toData))
        // A range a program reads by position, of another tag and with a field more, keeps what
        // such a program reads: its bounds.
        val range = validSpending.txInfo.validRange.toData
        val positional = range match
            case Data.Constr(_, fields) =>
                Data.Constr(3, prelude.List.from(fields.toScalaList :+ Data.I(1)))
            case other => fail(s"a range's Data is $other")
        val rangeEncoding = scalus.verify.DataEncoding.of[scalus.cardano.onchain.plutus.v1.Interval]
        assert(rangeEncoding.repaired(positional) == Some(range))
    }

    test("Lean encodes a term of CardanoLedgerApi's script context as Scalus does") {
        requireLean()
        val evaluation = UplcBlaster.Evaluation.in(leanServer, 400000, Some(5.minutes.fromNow))
        // A term of the context: CardanoLedgerApi's reading of its Data, which Lean decides is one.
        val term =
            s"((CardanoLedgerApi.IsData.Class.IsData.fromData ${UplcBlaster.leanData(validSpending.toData)} : " +
                "Option CardanoLedgerApi.V3.Contexts.ScriptContext).get (by native_decide))"
        assert(
          evaluation.encode(LeanTypeBinding.scriptContextV3, term) == Right(validSpending.toData)
        )
    }

    test(
      "a statement over CardanoLedgerApi's script context is proved, and refuted with a replayed context"
    ) {
        requireLean()
        // Of every valid context: the transaction pays a fee.
        proven(
          forAllData[ScriptContext](ctx => isValid(ctx) ==>: (ctx.txInfo.fee > BigInt(0))),
          600
        )
        // Of every valid context: false, and Lean's counterexample, a typed value, is encoded by
        // Lean and replayed as Data, a context the ledger accepts.
        val counterexample = refuted(
          forAllData[ScriptContext](ctx => isValid(ctx) ==>: (ctx.txInfo.fee > BigInt(1000000))),
          600
        )
        counterexample.values.toList match
            case List(Constant.Data(data)) =>
                val context = data.to[ScriptContext]
                assert(context.txInfo.fee <= BigInt(1000000), context)
            case other => fail(s"the counterexample is $other")
        assert(unbalanced.txInfo.fee > BigInt(0))
        assert(
          LeanPredicate.validScriptContextV3.typed.contains(
            LeanTypeBinding.scriptContextV3.leanType
          )
        )
    }
}
