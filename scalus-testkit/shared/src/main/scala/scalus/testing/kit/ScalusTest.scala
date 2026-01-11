package scalus.testing.kit

import org.scalacheck.Arbitrary
import scalus.*
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.cardano.ledger.TransactionInput
import scalus.ledger.api.v1.Credential.PubKeyCredential
import scalus.ledger.api.v1.Credential.ScriptCredential
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.compiler.Options
import scalus.compiler.sir.SIR
import scalus.uplc.*
import scalus.uplc.eval.*
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import java.security.SecureRandom

@deprecated("Use TestUtil instead", "0.14.2")
object Mock:
    def rootKeyHash: ByteString = TestUtil.rootKeyHash
    def rootTxHash: ByteString = TestUtil.rootTxHash
    def mockPubKeyHash(variation: BigInt): PubKeyHash = TestUtil.mockPubKeyHash(variation)
    def mockScriptHash(variation: BigInt): ValidatorHash = TestUtil.mockScriptHash(variation)
    def mockTxOutRef(variation: BigInt, idx: BigInt): TxOutRef =
        TestUtil.mockTxOutRef(variation, idx)
    def mockTxInput(variation: BigInt, idx: BigInt): TransactionInput =
        TestUtil.mockTxInput(variation, idx)

trait ScalusTest extends ArbitraryInstances {
    protected def plutusVM: PlutusVM = PlutusVM.makePlutusV3VM()
    protected given PlutusVM = plutusVM

    extension (self: SIR)
        def runScript(using
            scalusOptions: Options = Options.default
        )(
            scriptContext: ScriptContext,
            param: Option[Data] = None
        ): Result =
            // UPLC program: (ScriptContext as Data) -> ()
            val script = self.toUplc().plutusV3
            // println(s"uplc: ${script.pretty.render(100)}")
            val appliedScript = param.map(script $ _).getOrElse(script) $ scriptContext.toData
            appliedScript.evaluateDebug

        def scriptV3(using
            scalusOptions: Options = Options.default
        )(errorTraces: Boolean = true): Program =
            self.toUplc(generateErrorTraces = errorTraces).plutusV3

    extension (self: Program)
        def runWithDebug(scriptContext: ScriptContext): Result =
            val appliedScript = self $ scriptContext.toData
            appliedScript.evaluateDebug

        def hash: ValidatorHash = blake2b_224(ByteString.fromArray(3 +: self.cborEncoded))

    protected def random[A: Arbitrary]: A = {
        Arbitrary.arbitrary[A].sample.get
    }

    protected def makeSpendingScriptContext(
        datum: Data,
        redeemer: Redeemer,
        signatories: List[PubKeyHash]
    ): ScriptContext = {
        val ownInput =
            TxInInfo(
              outRef = random[TxOutRef],
              resolved = TxOut(
                address = Address(
                  Credential.ScriptCredential(genByteStringOfN(28).sample.get),
                  Option.None
                ),
                value = Value.zero
              )
            )
        ScriptContext(
          txInfo = TxInfo(
            inputs = List(ownInput),
            fee = 188021,
            signatories = signatories,
            id = random[TxId]
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = ownInput.outRef,
            datum = Option.Some(datum)
          )
        )
    }

    protected def makePubKeyHashInput(pkh: Hash, value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(random[TxId], 0),
          resolved = TxOut(
            address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    protected def makeScriptHashInput(scriptHash: ValidatorHash, value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(random[TxId], 0),
          resolved = TxOut(
            address = Address(ScriptCredential(scriptHash), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    protected def makePubKeyHashOutput(
        pkh: Hash,
        value: BigInt,
        datum: OutputDatum = OutputDatum.NoOutputDatum
    ): TxOut = {
        TxOut(
          address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
          value = Value.lovelace(value),
          datum = datum
        )
    }

    protected def makeScriptHashOutput(
        scriptHash: ValidatorHash,
        value: BigInt,
        datum: OutputDatum = OutputDatum.NoOutputDatum
    ): TxOut = {
        TxOut(
          address = Address(ScriptCredential(scriptHash), Option.None),
          value = Value.lovelace(value),
          datum = datum
        )
    }

    final protected def failure(message: String): (String, Option[ExUnits]) =
        (message, Option.None)
    final protected def failure(message: String, budget: ExUnits): (String, Option[ExUnits]) =
        (message, Option.Some(budget))
    protected val success: (Unit, Option[ExUnits]) = ((), Option.None)
    final protected def success(budget: ExUnits): (Unit, Option[ExUnits]) =
        ((), Option.Some(budget))

    protected def checkResult(
        expected: (String | Unit, Option[ExUnits]),
        actual: Result
    ): Unit = {
        expected._1 match
            case errorMsg: String =>
                assert(
                  actual.isFailure,
                  s"Expected failure with: $errorMsg, but got success"
                )
                // If a specific error message is provided, check it matches
                assert(
                  actual.logs.exists(_.contains(errorMsg)),
                  s"Expected error containing: $errorMsg, but got: ${actual.logs.mkString(", ")}"
                )
            case () =>
                actual match
                    case Result.Failure(ex, budget, cost, logs) =>
                        ex match
                            case be: scalus.uplc.eval.BuiltinError =>
                                be.cause.printStackTrace()
                            case _ =>
                    case _ =>
                assert(
                  actual.isSuccess,
                  s"Expected success, but got: ${actual.toString}, logs0: ${actual.logs.mkString(", ")}"
                )

        expected._2 match
            case Option.Some(budget) if budget != ExUnits(0, 0) =>
                assert(
                  actual.budget == budget,
                  s"Expected budget: $budget, but got: ${actual.budget}"
                )
            case _ =>
    }

    def compareBudgetWithReferenceValue(
        testName: String,
        scalusBudget: ExUnits,
        refBudget: ExUnits,
        isPrintComparison: Boolean = false
    ): Unit = {
        import ScalusTest.BenchmarkConfig
        extension (scalus: Long)
            def comparisonAsJsonString(ref: Long): String = {
                val comparison = f"${scalus.toDouble / ref.toDouble * 100}%.2f"
                s"{scalus: $scalus, ref: $ref, comparison: $comparison%}"
            }

        end extension

        if isPrintComparison || BenchmarkConfig.isPrintAllComparisonsOfBudgetWithReferenceValue then
            println(
              s"${BenchmarkConfig.logPrefix}[$testName]: {" +
                  s"cpu: ${scalusBudget.steps.comparisonAsJsonString(refBudget.steps)}, " +
                  s"memory: ${scalusBudget.memory.comparisonAsJsonString(refBudget.memory)}" +
                  "}"
            )
    }

    protected def generateKeyPair(): (ByteString, ByteString) = {
        val asymmetricCipherKeyPair: AsymmetricCipherKeyPair = keyPairGenerator.generateKeyPair()
        val privateKeyParams: Ed25519PrivateKeyParameters =
            asymmetricCipherKeyPair.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters]
        val publicKeyParams: Ed25519PublicKeyParameters =
            asymmetricCipherKeyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters]
        val privateKey: ByteString = ByteString.fromArray(privateKeyParams.getEncoded)
        val publicKey: ByteString = ByteString.fromArray(publicKeyParams.getEncoded)
        (privateKey, publicKey)
    }

    private val keyPairGenerator = {
        val keyPairGenerator = new Ed25519KeyPairGenerator()
        keyPairGenerator.init(new Ed25519KeyGenerationParameters(new SecureRandom()))
        keyPairGenerator
    }
}

object ScalusTest {
    private object BenchmarkConfig {
        inline val logPrefix = "BenchmarkComparison"
        val isPrintAllComparisonsOfBudgetWithReferenceValue: Boolean = false
    }
}
