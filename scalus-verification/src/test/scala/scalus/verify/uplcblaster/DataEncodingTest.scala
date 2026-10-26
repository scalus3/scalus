package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.{ScriptContext, ScriptInfo, TxId, TxInInfo, TxInfo, TxOutRef}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.verify.{DataEncoding, FunctionTable}
import scalus.verify.Props.*
import scalus.verify.lean.LeanServer.{Message, Result, Severity}

import scala.concurrent.duration.*

class DataEncodingTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*
    import DataEncodingTest.*

    test("the encoding of a type is the Data that its values are") {
        assert(DataEncoding.of[BigInt] == DataEncoding.Integer)
        assert(DataEncoding.of[ByteString] == DataEncoding.Bytes)
        assert(DataEncoding.of[Boolean] == DataEncoding.Bool)
        assert(DataEncoding.of[Data] == DataEncoding.AnyData)
        // A type that wraps one field is that field.
        assert(DataEncoding.of[PubKeyHash] == DataEncoding.Bytes)
        assert(DataEncoding.of[prelude.List[BigInt]] == DataEncoding.Items(DataEncoding.Integer))
        val outRef = DataEncoding.of[TxOutRef]
        assert(outRef.exact, outRef)
        assert(outRef.outside(sampleOutRef.toData).isEmpty)
        assert(outRef.outside(Data.Constr(0, prelude.List(Data.I(1), Data.I(1)))).nonEmpty)
        // An enum is a constructor of each of its cases.
        val option = DataEncoding.of[prelude.Option[BigInt]]
        assert(option.outside(prelude.Option.Some(BigInt(5)).toData).isEmpty)
        assert(option.outside((prelude.Option.None: prelude.Option[BigInt]).toData).isEmpty)
        assert(option.outside(Data.Constr(2, prelude.List.Nil)).nonEmpty)
    }

    test("the encoding of a ledger's script context holds of a script context") {
        val encoding = DataEncoding.of[ScriptContext]
        assert(encoding.outside(sampleContext.toData).isEmpty)
        assert(encoding.outside(Data.I(0)).nonEmpty)
        // A script info of no constructor of its type.
        val data = sampleContext.toData match
            case Data.Constr(tag, prelude.List.Cons(info, prelude.List.Cons(redeemer, _))) =>
                Data.Constr(tag, prelude.List(info, redeemer, Data.Constr(9, prelude.List.Nil)))
            case other => fail(s"a script context is $other")
        assert(encoding.outside(data).nonEmpty)
        // The keys of its maps of credentials and of voters are in an order that is not checked:
        // one key is in order, two are not told.
        assert(!encoding.exact)
        val withdrawals = SortedMap.fromList(
          prelude.List(
            (Credential.PubKeyCredential(owner), BigInt(3)),
            (Credential.ScriptCredential(policy), BigInt(4))
          )
        )
        val twoWithdrawals =
            sampleContext.copy(txInfo = sampleContext.txInfo.copy(withdrawals = withdrawals))
        assert(
          encoding.outside(twoWithdrawals.toData) ==
              Some("the order of the keys is not checked here")
        )
    }

    test("forAllData of a script context reads the Data the node gives a script") {
        // Its decoder comes with the data declarations of the context's types.
        val fee = forAllData[ScriptContext](ctx => ctx.txInfo.fee == ctx.txInfo.fee)
        UplcBlaster.lower(fee, FunctionTable.empty) match
            case Right(lowered) => assert(lowered.binders.size == 1, lowered.binders)
            case Left(reason)   => fail(reason)
        proven(fee, 600)
    }

    test("the Lean definition of an encoding is Lean's, and holds of the same Data") {
        requireLean()
        val definitions = List(
          DataEncoding.leanDefinitions,
          DataEncoding.of[ScriptContext].leanDefinition("isScriptContext"),
          DataEncoding.of[SortedMap[ByteString, BigInt]].leanDefinition("isTokens")
        ).mkString("\n\n")
        val ascending = Data.Map(
          prelude.List((Data.B(hex("aa")), Data.I(1)), (Data.B(hex("bb")), Data.I(2)))
        )
        val descending = Data.Map(
          prelude.List((Data.B(hex("bb")), Data.I(2)), (Data.B(hex("aa")), Data.I(1)))
        )
        val examples = List(
          s"example : isScriptContext ${UplcBlaster.leanData(sampleContext.toData)} = true",
          "example : isScriptContext (Data.I 0) = false",
          s"example : isTokens ${UplcBlaster.leanData(ascending)} = true",
          s"example : isTokens ${UplcBlaster.leanData(descending)} = false"
        ).map(_ + " := by native_decide")
        val text = (header :: definitions :: examples).mkString("\n\n") + "\n"
        leanServer.check(text, Some(5.minutes)) match
            case Result.Finished(messages) =>
                val errors = messages.filter(_.severity == Severity.Error)
                assert(errors.isEmpty, errors.map((m: Message) => s"${m.line}: ${m.text}"))
            case other => fail(s"Lean did not check the definitions: $other")
    }
}

object DataEncodingTest {

    private def hex(text: String): ByteString = ByteString.fromHex(text)

    /** What a check of the definitions starts with: PlutusCore's `Data`, as `Data`. */
    private val header = List(
      "import ScalusProofs.Run",
      "open PlutusCore.Integer (Integer)",
      "open PlutusCore.ByteString (ByteString)",
      "open PlutusCore.Data (Data)"
    ).mkString("\n")

    private val owner = PubKeyHash(hex("11" * 28))
    private val policy = hex("33" * 28)
    private val sampleOutRef = TxOutRef(TxId(hex("22" * 32)), BigInt(1))

    private val output = TxOut(
      Address(Credential.PubKeyCredential(owner), prelude.Option.None),
      Value.lovelace(BigInt(10)),
      OutputDatum.OutputDatum(Data.I(5))
    )

    /** A context of a minting, with an input, an output, a signatory and a withdrawal. */
    private val sampleContext = ScriptContext(
      TxInfo(
        inputs = prelude.List(TxInInfo(sampleOutRef, output)),
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
