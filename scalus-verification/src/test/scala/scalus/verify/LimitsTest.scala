package scalus.verify

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.{ScriptContext, ScriptInfo, TxId, TxInInfo, TxInfo, TxOutRef}
import scalus.compiler.sir.SIRType
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.verify.Props.*

class LimitsTest extends AnyFunSuite {
    import LimitsTest.*

    test("a limit is on the lists and maps of a value, not on the fields of its case classes") {
        // A transaction has 16 fields, and one of each collection: one of everything meets it.
        assert(Limits.of(Limit.MaxCollectionLength(1)).outside(context.toData, contextType).isEmpty)
        // None of anything: the first collection of the context is its inputs.
        assert(
          Limits
              .of(Limit.MaxCollectionLength(0))
              .outside(context.toData, contextType)
              .contains(
                "txInfo.inputs has 1 element, not from 0 to 0"
              )
        )
        // A value is its map, the one field it wraps: its policies are a collection, and the
        // tokens of each policy another.
        val twoPolicies = context.copy(txInfo =
            context.txInfo.copy(mint =
                Value(policy, hex("01"), BigInt(1)) + Value(hex("55" * 28), hex("01"), BigInt(1))
            )
        )
        assert(
          Limits
              .of(Limit.MaxCollectionLength(1))
              .outside(twoPolicies.toData, contextType)
              .contains("txInfo.mint.toSortedMap has 2 elements, not from 0 to 1")
        )
        // A list in a field of type `Data` is no collection of the type.
        val datum = OutputDatum.OutputDatum(Data.List(prelude.List(Data.I(1), Data.I(2))))
        val withDatum = context.copy(txInfo =
            context.txInfo.copy(outputs = prelude.List(output.copy(datum = datum)))
        )
        assert(
          Limits.of(Limit.MaxCollectionLength(1)).outside(withDatum.toData, contextType).isEmpty
        )
    }

    test("a limit of one collection is by its path") {
        val inputs = List(Step.Field("txInfo"), Step.Field("inputs"))
        assert(
          Limits
              .of(Limit.Length(inputs, 2, Some(3)))
              .outside(context.toData, contextType)
              .contains("txInfo.inputs has 1 element, not from 2 to 3")
        )
        // Through the elements of a list: the tokens of the value of each input's output.
        val tokens = inputs ++ List(
          Step.Elements,
          Step.Field("resolved"),
          Step.Field("value"),
          Step.Field("toSortedMap"),
          Step.Values
        )
        assert(
          Limits.of(Limit.Length(tokens, 0, Some(1))).outside(context.toData, contextType).isEmpty
        )
        assert(
          Limits
              .of(Limit.Length(tokens, 2, None))
              .outside(context.toData, contextType)
              .contains(
                "txInfo.inputs.*.resolved.value.toSortedMap.values has 1 element, not from 2 to " +
                    "any number"
              )
        )
        // The bound of a collection is the least of its own and of every collection's.
        val limits = Limits.of(Limit.MaxCollectionLength(2), Limit.Length(inputs, 1, Some(5)))
        assert(limits.bounds(Some(inputs)) == (1, Some(2)))
        assert(limits.bounds(Some(tokens)) == (0, Some(2)))
        assert(limits.bounds(None) == (0, Some(2)))
        assert(Limits.none.bounds(Some(inputs)) == (0, None))
    }

    test("the largest value that meets the limits fills each limited collection to its bound") {
        val inputs = List(Step.Field("txInfo"), Step.Field("inputs"))
        val policies = List(Step.Field("txInfo"), Step.Field("mint"), Step.Field("toSortedMap"))
        val limits = Limits.of(
          Limit.Length(inputs, 0, Some(2)),
          Limit.Length(policies, 0, Some(3)),
          Limit.Length(policies :+ Step.Values, 1, None)
        )
        val largest = limits.largest(DataLayout.of(contextType)).getOrElse(fail("no largest value"))
        // It is a context, of the limits, and the most they allow: so are its keys distinct.
        assert(limits.outside(largest, contextType).isEmpty, largest)
        assert(Limits.of(Limit.Length(inputs, 2, Some(2))).outside(largest, contextType).isEmpty)
        assert(Limits.of(Limit.Length(policies, 3, Some(3))).outside(largest, contextType).isEmpty)
        // A collection that no limit bounds is empty.
        val outputs = List(Step.Field("txInfo"), Step.Field("outputs"))
        assert(Limits.of(Limit.Length(outputs, 0, Some(0))).outside(largest, contextType).isEmpty)
    }

    test("limits that do not fit the type are refused") {
        def unfit(limits: Limit*): Option[String] = Limits(limits.toList).unfit(contextType)
        def length(path: String*) = Limit.Length(path.map(Step.Field(_)).toList, 0, Some(1))
        assert(unfit(length("txInfo", "inputs")).isEmpty)
        assert(unfit(length("txInfo", "nothing")).contains("txInfo has no field nothing"))
        assert(unfit(length("txInfo", "fee")).contains("txInfo.fee is no list or map"))
        assert(unfit(length()).contains("the value is no list or map"))
        assert(
          unfit(length("scriptInfo", "policyId")).contains(
            "scriptInfo is of several constructors, into which a limit does not reach yet"
          )
        )
        assert(
          unfit(length("redeemer", "items")).contains(
            "redeemer is any Data, which has no collection of its type"
          )
        )
        val inputs = List(Step.Field("txInfo"), Step.Field("inputs"))
        assert(
          unfit(Limit.Length(inputs :+ Step.Keys, 0, Some(1)))
              .contains("txInfo.inputs has no keys")
        )
        // Bounds that no number of elements meets.
        assert(
          unfit(Limit.Length(inputs, 3, Some(2))).contains(
            "txInfo.inputs has at least 3 elements and at most 2, which no number of elements is"
          )
        )
        assert(unfit(Limit.MaxCollectionLength(1), Limit.Length(inputs, 2, None)).nonEmpty)
        assert(unfit(Limit.MaxCollectionLength(-1)).nonEmpty)
        assert(
          unfit(Limit.Length(inputs, -1, None)).contains(
            "txInfo.inputs has from -1 to any number elements, below none"
          )
        )
        // A value is checked against limits that fit its type.
        assert(
          Limits
              .of(length("txInfo", "fee"))
              .outside(context.toData, contextType)
              .contains("txInfo.fee is no list or map")
        )
    }

    test("forAllDataLimited keeps the limits its limit says of the variable") {
        val most = 3
        val prop = forAllDataLimited[ScriptContext](ctx =>
            maxCollectionLength(ctx) <= 2 && ctx.txInfo.inputs.nonEmpty &&
                ctx.txInfo.signatories.length < most && BigInt(5) >= ctx.txInfo.outputs.length &&
                ctx.txInfo.withdrawals.isEmpty &&
                ctx.txInfo.inputs.forall(input =>
                    input.resolved.value.toSortedMap.length == BigInt(1)
                ) &&
                ctx.txInfo.mint.toSortedMap.values.forall(tokens => tokens.length <= 3)
        )(ctx => ctx.txInfo.fee >= BigInt(0))
        def field(names: String*) = names.map(Step.Field(_)).toList
        val inputs = field("txInfo", "inputs")
        prop match
            case Prop.Forall(ident, _) =>
                val limits = DataEncoding.limits(ident.tp)
                assert(
                  limits == Limits.of(
                    Limit.MaxCollectionLength(2),
                    Limit.Length(inputs, 1, None),
                    Limit.Length(field("txInfo", "signatories"), 0, Some(2)),
                    Limit.Length(field("txInfo", "outputs"), 0, Some(5)),
                    Limit.Length(field("txInfo", "withdrawals"), 0, Some(0)),
                    Limit.Length(
                      (inputs :+ Step.Elements) ++ field("resolved", "value", "toSortedMap"),
                      1,
                      Some(1)
                    ),
                    Limit.Length(field("txInfo", "mint", "toSortedMap") :+ Step.Values, 0, Some(3))
                  )
                )
                assert(limits.unfit(DataEncoding.valueType(ident.tp)).isEmpty)
                assert(Limits.fromData(limits.toData) == limits)
            case other => fail(s"expected a universal statement, got $other")
        // The limits are part of what the statement is.
        val unlimited = forAllData[ScriptContext](ctx => ctx.txInfo.fee >= BigInt(0))
        assert(Verifier.empty.statement(prop).name != Verifier.empty.statement(unlimited).name)
    }

    test("UplcBlaster takes a statement with limits as one whose guard is the premise") {
        val limited = forAllDataLimited[ScriptContext](ctx => ctx.txInfo.inputs.length <= 2)(ctx =>
            ctx.txInfo.fee >= BigInt(0)
        )
        uplcblaster.UplcBlaster.lower(limited, FunctionTable.empty) match
            case Right(lowered) =>
                // The guard is a test of its own, and the premise of the statement's test.
                lowered.body match
                    case uplcblaster.UplcBlaster.LeafFormula.Forall(
                          List(0),
                          uplcblaster.UplcBlaster.LeafFormula.Implies(
                            uplcblaster.UplcBlaster.LeafFormula.Test(guard),
                            uplcblaster.UplcBlaster.LeafFormula.Test(test)
                          )
                        ) =>
                        assert(guard != test)
                    case other => fail(s"expected the guard as the premise, got $other")
            case Left(reason) => fail(reason)
    }

    test("a limit of another kind is a compile error") {
        val forms = "a limit of forAllDataLimited bounds the length of the variable's collections"
        // A condition that is not about the length of a collection.
        val fee = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.Props.*
             import scalus.cardano.onchain.plutus.v3.ScriptContext
             forAllDataLimited[ScriptContext](ctx => ctx.txInfo.fee > BigInt(0))(ctx => true)"""
        )
        assert(fee.exists(_.message.contains(forms)), fee)
        // A disjunction of limits, which no shape is.
        val either = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.Props.*
             import scalus.cardano.onchain.plutus.v3.ScriptContext
             forAllDataLimited[ScriptContext](ctx =>
                 ctx.txInfo.inputs.isEmpty || ctx.txInfo.outputs.isEmpty
             )(ctx => true)"""
        )
        assert(either.exists(_.message.contains(forms)), either)
        // A bound of the variable.
        val relative = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.Props.*
             import scalus.cardano.onchain.plutus.v3.ScriptContext
             forAllDataLimited[ScriptContext](ctx =>
                 ctx.txInfo.inputs.length <= ctx.txInfo.outputs.length
             )(ctx => true)"""
        )
        assert(relative.exists(_.message.contains(forms)), relative)
        // Every collection has at most so many elements, and no fewer.
        val least = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.Props.*
             import scalus.cardano.onchain.plutus.v3.ScriptContext
             forAllDataLimited[ScriptContext](ctx => maxCollectionLength(ctx) >= 1)(ctx => true)"""
        )
        assert(least.exists(_.message.contains(forms)), least)
        // Every collection of the variable, and not of a part of it: that is no limit here.
        val ofPart = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.Props.*
             import scalus.cardano.onchain.plutus.v3.ScriptContext
             forAllDataLimited[ScriptContext](ctx =>
                 maxCollectionLength(ctx.txInfo.inputs) <= 1
             )(ctx => true)"""
        )
        assert(ofPart.exists(_.message.contains(forms)), ofPart)
        // A bound is computed where the statement is built, and uses nothing of the limit.
        val ofOuter = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.Props.*
             import scalus.cardano.onchain.plutus.v3.ScriptContext
             forAllDataLimited[ScriptContext](ctx =>
                 ctx.txInfo.inputs.forall(input =>
                     input.resolved.value.toSortedMap.length <= ctx.txInfo.outputs.length
                 )
             )(ctx => true)"""
        )
        assert(ofOuter.exists(_.message.contains(forms)), ofOuter)
    }

    test("UplcBlaster refuses limits it does not check: too large a bound, and an exists") {
        val large = forAllDataLimited[prelude.List[BigInt]](xs => xs.length <= 65)(xs =>
            xs.length >= BigInt(0)
        )
        uplcblaster.UplcBlaster.lower(large, FunctionTable.empty) match
            case Left(reason) => assert(reason.contains("more than the 64 a guard walks"), reason)
            case Right(_)     => fail("expected a bound of 65 to be refused")
        // A binder with limits in an exists, which no syntax writes, but a statement may.
        val limited = forAllDataLimited[prelude.List[BigInt]](xs => xs.length <= 2)(xs =>
            xs.length >= BigInt(0)
        )
        limited match
            case Prop.Forall(ident, body) =>
                uplcblaster.UplcBlaster.lower(
                  Props.existsSIR(ident, None, body),
                  FunctionTable.empty
                ) match
                    case Left(reason) => assert(reason.contains("a binder with limits"), reason)
                    case Right(_) =>
                        fail("expected an exists of a binder with limits to be refused")
            case other => fail(s"expected a universal statement, got $other")
    }
}

object LimitsTest {

    private def hex(text: String): ByteString = ByteString.fromHex(text)

    private val contextType: SIRType =
        Props.variableType(scalus.compiler.compile((value: ScriptContext) => value))

    private val owner = PubKeyHash(hex("11" * 28))
    private val policy = hex("33" * 28)

    private val output = TxOut(
      Address(Credential.PubKeyCredential(owner), prelude.Option.None),
      Value.lovelace(BigInt(10)),
      OutputDatum.OutputDatum(Data.I(5))
    )

    /** A context of a minting, with an input, an output, a signatory and a withdrawal. */
    private val context = ScriptContext(
      TxInfo(
        inputs = prelude.List(TxInInfo(TxOutRef(TxId(hex("22" * 32)), BigInt(1)), output)),
        outputs = prelude.List(output),
        fee = BigInt(2),
        mint = Value(policy, hex("01"), BigInt(1)),
        withdrawals = SortedMap.singleton(Credential.PubKeyCredential(owner), BigInt(3)),
        signatories = prelude.List(owner),
        id = TxId(hex("44" * 32))
      ),
      scriptInfo = ScriptInfo.MintingScript(policy)
    )
}
