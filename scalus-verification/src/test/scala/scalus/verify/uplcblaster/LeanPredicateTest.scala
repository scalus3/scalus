package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.{AssocMap, SortedMap}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, Interval, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.{ScriptContext, ScriptInfo, ScriptPurpose, TxId, TxInInfo, TxInfo, TxOutRef}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.verify.{FunctionTable, Kept, LeanPredicate, Prop, VerificationResult}
import scalus.verify.Props.*
import scalus.verify.lean.LeanServer.{Message, Result, Severity}

import java.nio.file.Files
import scala.concurrent.duration.*

/** The Lean predicates of a statement ([[Prop.Satisfies]]), as [[scalus.verify.Props.isValid]]
  * states the ledger's validity of a script context with IOG's CardanoLedgerApi: how they are
  * lowered and written, and, with Lean, that CardanoLedgerApi reads the `Data` Scalus gives a
  * script context as the same context.
  */
class LeanPredicateTest extends AnyFunSuite with LeanProofsSuite {
    import LeanPredicateTest.*
    import proofs.*

    /** `prop` lowered by the rules, which pass a script context as its one `Data`: the form of a
      * Lean predicate of a `Data`. A context of the library's type is `VariableMappingTest`'s.
      */
    private def lowered(prop: Prop): UplcBlaster.Lowered =
        UplcBlaster
            .lower(prop, FunctionTable.empty, MappingPlan.Fixed.none, true, VariableMapping.rules)
            .fold(fail(_), identity)

    /** The Lean module of `prop`, whose text is that of its check without the Blaster command. */
    private def moduleText(prop: Prop): String =
        // The module names where its programs would be, and writes nothing there.
        UplcBlaster.statementModule(lowered(prop), 600, 400000, "Probe.statement", "programs").text

    test("isValid of a script context variable is a premise of Lean on its one Data") {
        val statement =
            forAllData[ScriptContext](ctx => isValid(ctx) ==>: (ctx.txInfo.fee > BigInt(0)))
        val goal = lowered(statement)
        // The context is one Data variable, of which Lean's predicate is.
        assert(goal.binders.size == 1, goal.binders)
        val satisfies = UplcBlaster.LeafFormula.Satisfies(
          LeanPredicate.validScriptContextV3,
          UplcBlaster.LeanArgument.Binder(0)
        )
        goal.body match
            case UplcBlaster.LeafFormula.Forall(
                  List(0),
                  UplcBlaster.LeafFormula.Implies(`satisfies`, UplcBlaster.LeafFormula.Test(0))
                ) =>
                ()
            case other => fail(s"lowered to $other")
        // One program, the test of the fee: the predicate is no program.
        assert(goal.leaves.size == 1)
        val text = moduleText(statement)
        // The library's run imports the ledger's rules: the check imports it alone.
        assert(text.startsWith("import ScalusProofs.Run\n\n"), text)
        assert(text.contains("(ScalusProofs.Ledger.validScriptContextV3 x0 = true) →"), text)
    }

    test("a refutation Lean evaluated the predicates of is not restored with another side") {
        val goal =
            lowered(
              forAllData[ScriptContext](ctx => isValid(ctx) ==>: (ctx.txInfo.fee > BigInt(0)))
            )
        val notes = Map("budget" -> "400", "kind" -> "Blaster")
        // The ledger's rules are of the old side, and may give another verdict with the new one.
        val refuted = Kept(
          Kept.Result.Refuted,
          notes + ("lean-predicates" -> "true"),
          Map("ctx" -> "{\"int\":0}")
        )
        assert(
          UplcBlaster.restoredOnAnotherSide(goal, refuted) ==
              Left("its Lean predicates were evaluated with another side of the backend")
        )
        // A result that rests on no evaluation of Lean's is restored as with the same side.
        val proved = Kept(Kept.Result.Proved, notes, Map.empty)
        assert(
          UplcBlaster
              .restoredOnAnotherSide(goal, proved)
              .exists(_.isInstanceOf[VerificationResult.Proven])
        )
    }

    test("every check has one header, the library's run, which imports its ledger module") {
        // A server loads a header once: checks with and without a Lean predicate have the same.
        val text = moduleText(forAllData[ScriptContext](ctx => ctx.txInfo.fee == ctx.txInfo.fee))
        assert(text.startsWith("import ScalusProofs.Run\n\n"), text)
        // A workspace builds what its root imports, and every root Scalus makes imports the run:
        // so the run imports the ledger's rules, and a workspace that builds it builds them.
        val run = Files.readString(LeanProofs.librarySources.resolve("ScalusProofs/Run.lean"))
        assert(run.linesIterator.contains("import ScalusProofs.Ledger"), run)
    }

    test("isValid of a value without variables is a predicate of its Data, a constant") {
        val statement: Prop = isValid(validSpending)
        statement match
            case Prop.Satisfies(LeanPredicate.validScriptContextV3, _) => ()
            case other                                                 => fail(s"built $other")
        // A closed statement: Lean evaluates it, and runs no program.
        val goal = lowered(statement)
        assert(goal.binders.isEmpty && goal.leaves.isEmpty)
        goal.body match
            case UplcBlaster.LeafFormula.Satisfies(_, UplcBlaster.LeanArgument.Constant(data)) =>
                assert(data == validSpending.toData)
            case other => fail(s"lowered to $other")
    }

    test("isValid is of a variable itself, or of a value without variables") {
        val errors = scala.compiletime.testing.typeCheckErrors(
          "forAllData[ScriptContext](ctx => isValid(ctx.copy(redeemer = Data.I(0))))"
        )
        assert(errors.exists(_.message.contains("of a statement variable itself")), errors)
    }

    test("Lean's evaluation of predicates is read in order, and an error is none") {
        def info(text: String) = Message(10, Severity.Information, text)
        assert(UplcBlaster.evaluated(2, List(info("[true, false]"))) == Right(List(true, false)))
        assert(UplcBlaster.evaluated(1, List(info("[true]"))) == Right(List(true)))
        // A list wider than Lean's line is broken over lines.
        val many = List.tabulate(30)(_ % 2 == 0)
        assert(
          UplcBlaster.evaluated(30, List(info(many.mkString("[", ",\n ", "]")))) == Right(many)
        )
        assert(UplcBlaster.evaluated(2, List(info("[true]"))).isLeft)
        assert(
          UplcBlaster
              .evaluated(1, List(info("[true]"), Message(3, Severity.Error, "unknown constant")))
              .isLeft
        )
        val text = UplcBlaster.evaluationSource(
          List(LeanPredicate.validScriptContextV3 -> Data.I(0)),
          400000
        )
        assert(text.startsWith("import ScalusProofs.Run\n"), text)
        assert(text.contains("#eval [ScalusProofs.Ledger.validScriptContextV3 (Data.I 0)]"), text)
    }

    test("without Lean, a statement with a Lean predicate is not evaluated on values") {
        val statement =
            forAllData[ScriptContext](ctx => isValid(ctx) ==>: (ctx.txInfo.fee > BigInt(0)))
        val held = UplcBlaster.holdsAsImplicationOn(
          statement,
          FunctionTable.empty,
          List(scalus.uplc.Constant.Data(validSpending.toData))
        )
        assert(held.left.exists(_.contains("only Lean evaluates")), held)
    }

    test("CardanoLedgerApi reads the Data Scalus gives a script context as the same context") {
        requireLean()
        // Of each: CardanoLedgerApi's `fromData` reads it, and its `toData` gives it back. The
        // layout of a context is the one both have, as Plutus's.
        val contexts = List(
          "the valid spending" -> validSpending,
          "a minting with a withdrawal" -> mintingWithWithdrawal,
          "a spending without its input" -> unbalanced
        )
        val examples = contexts.map { (name, context) =>
            val data = UplcBlaster.leanData(context.toData)
            s"-- $name\nexample : (match (IsData.fromData $data : Option " +
                s"CardanoLedgerApi.V3.Contexts.ScriptContext) with\n" +
                s"  | some c => IsData.toData c == $data | none => false) = true := by native_decide"
        }
        val verdicts = List(
          s"example : ScalusProofs.Ledger.validScriptContextV3 ${UplcBlaster.leanData(validSpending.toData)} = true := by native_decide",
          // Without the input it spends, with no inputs at all: no transaction has it.
          s"example : ScalusProofs.Ledger.validScriptContextV3 ${UplcBlaster.leanData(unbalanced.toData)} = false := by native_decide",
          "example : ScalusProofs.Ledger.validScriptContextV3 (Data.I 0) = false := by native_decide"
        )
        val text = (header :: examples ++ verdicts).mkString("\n\n") + "\n"
        leanServer.check(text, Some(5.minutes)) match
            case Result.Finished(messages) =>
                val errors = messages.filter(_.severity == Severity.Error)
                assert(errors.isEmpty, errors.map((m: Message) => s"${m.line}: ${m.text}"))
            case other => fail(s"Lean did not check the contexts: $other")
    }

    test("Lean evaluates a predicate of a counterexample's Data") {
        requireLean()
        val evaluation = UplcBlaster.Evaluation.in(leanServer, 400000, Some(5.minutes.fromNow))
        assert(
          evaluation(
            List(
              LeanPredicate.validScriptContextV3 -> validSpending.toData,
              LeanPredicate.validScriptContextV3 -> unbalanced.toData
            )
          ) == Right(List(true, false))
        )
    }
}

object LeanPredicateTest {

    /** What a check of CardanoLedgerApi's reading starts with. */
    private val header = List(
      "import ScalusProofs.Run",
      "open PlutusCore.Integer (Integer)",
      "open PlutusCore.ByteString (ByteString)",
      "open PlutusCore.Data (Data)",
      "open CardanoLedgerApi.IsData.Class (IsData)"
    ).mkString("\n")

    private def hex(text: String): ByteString = ByteString.fromHex(text)

    private val owner = PubKeyHash(hex("11" * 28))
    private val script = hex("44" * 28)
    private val policy = hex("33" * 28)
    private val spent = TxOutRef(TxId(hex("22" * 32)), BigInt(0))
    private val redeemer = Data.Constr(0, prelude.List.Nil)

    /** A spending of an output at a script, of a transaction the ledger accepts: its input, an
      * output to the owner, a fee that balances them, and the redeemer of the spend.
      */
    val validSpending: ScriptContext = ScriptContext(
      TxInfo(
        inputs = prelude.List(
          TxInInfo(
            spent,
            TxOut(
              Address(Credential.ScriptCredential(script), prelude.Option.None),
              Value.lovelace(BigInt(5_000_000)),
              OutputDatum.OutputDatum(Data.I(7))
            )
          )
        ),
        outputs = prelude.List(
          TxOut(
            Address(Credential.PubKeyCredential(owner), prelude.Option.None),
            Value.lovelace(BigInt(4_800_000))
          )
        ),
        fee = BigInt(200_000),
        validRange = Interval.after(BigInt(1000)),
        signatories = prelude.List(owner),
        redeemers = AssocMap.singleton(ScriptPurpose.Spending(spent), redeemer),
        id = TxId(hex("55" * 32))
      ),
      redeemer,
      ScriptInfo.SpendingScript(spent, prelude.Option.Some(Data.I(7)))
    )

    /** A minting with an input, an output with a token, a withdrawal and a datum: of more of the
      * context's parts than [[validSpending]].
      */
    val mintingWithWithdrawal: ScriptContext = ScriptContext(
      TxInfo(
        inputs = prelude.List(
          TxInInfo(
            spent,
            TxOut(
              Address(Credential.PubKeyCredential(owner), prelude.Option.None),
              Value.lovelace(BigInt(10))
            )
          )
        ),
        outputs = prelude.List(
          TxOut(
            Address(Credential.PubKeyCredential(owner), prelude.Option.None),
            Value.lovelace(BigInt(10)) + Value(policy, hex("01"), BigInt(1)),
            OutputDatum.OutputDatumHash(hex("66" * 32))
          )
        ),
        fee = BigInt(2),
        mint = Value(policy, hex("01"), BigInt(1)),
        withdrawals = SortedMap.singleton(Credential.PubKeyCredential(owner), BigInt(3)),
        signatories = prelude.List(owner),
        data = SortedMap.singleton(hex("66" * 32), Data.B(ByteString.empty)),
        id = TxId(hex("44" * 32))
      ),
      Data.I(1),
      ScriptInfo.MintingScript(policy)
    )

    /** [[validSpending]] without its inputs: the transaction spends nothing and pays a fee. */
    val unbalanced: ScriptContext =
        validSpending.copy(txInfo = validSpending.txInfo.copy(inputs = prelude.List.Nil))
}
