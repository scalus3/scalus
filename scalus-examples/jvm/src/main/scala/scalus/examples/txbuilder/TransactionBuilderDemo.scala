package scalus.examples.txbuilder

import com.bloxbean.cardano.client.util.HexUtil
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder, TransactionUnspentOutput}
import scalus.examples.txbuilder.Generators.*
import scalus.uplc.eval.ExBudget

import scala.sys.process.*

class TransactionBuilderDemo extends AnyFunSuite {

    test("Empty tx") {
        trace {
            TransactionBuilder.build(Mainnet, List.empty)
        }
    }

    private def genUtxos =
        val utxos = Gen.listOfN(2, genAdaUtxo()).sample.get
        val additionalUtxo = genAdaUtxoAt(utxos.head.output.address).sample.get
        utxos :+ additionalUtxo

    private def stage1 = {
        val inputs = genUtxos
        val output = TransactionOutput.apply(
          address = genPubKeyAddr().sample.get,
          value = inputs.foldLeft(Value.zero)((acc, o) => acc + o.output.value)
        )
        TransactionBuilder.build(Mainnet, inputs.map(Spend(_, PubKeyWitness)) :+ Send(output))
    }

    test("Start building") {
        trace {
            stage1
        }
    }

    private def stage2 = {
        val ctx = stage1.getOrElse(???)
        val feeUtxo = genAdaUtxo().sample.get
        TransactionBuilder.modify(ctx, List(Spend(feeUtxo, PubKeyWitness), Send(feeUtxo.output)))
    }

    test("Use modify for further building stages") {
        trace {
            stage2
        }
    }

    private def balanced = {
        val ctx: TransactionBuilder.Context = stage2.getOrElse(???)
        ctx.balance(
          ChangeOutputDiffHandler(testProtocolParams, 1).changeOutputDiffHandler,
          testProtocolParams,
          testEvaluator
        )
    }

    test("Balancing") {
        trace {
            balanced
        }
    }

    val testValidators: Seq[Validator] =
        // These validators are used to check an unsigned transaction
        List(
          EmptyInputsValidator,
          InputsAndReferenceInputsDisjointValidator,
          AllInputsMustBeInUtxoValidator,
          ValueNotConservedUTxOValidator,
          // VerifiedSignaturesInWitnessesValidator,
          // MissingKeyHashesValidator,
          // MissingOrExtraScriptHashesValidator,
          TransactionSizeValidator,
          FeesOkValidator,
          OutputsHaveNotEnoughCoinsValidator,
          OutputsHaveTooBigValueStorageSizeValidator,
          OutsideValidityIntervalValidator,
          OutsideForecastValidator
        )

    test("Validate balanced tx") {
        trace {
            val ctx = balanced.getOrElse(???)
            ctx.validate(testValidators :+ MissingKeyHashesValidator, testProtocolParams)

        }
    }

    // ===================================
    // helpers
    // ===================================
    def trace(testFun: => Either[Any, TransactionBuilder.Context]): Any = {
        testFun match {
            case Left(any) =>
                pprint.pprintln(any)
                assert(false)
            case Right(ctx) =>
                dumpTx(ctx.transaction)
                dumpCtx(ctx)
        }
    }

    private def dumpTx(transaction: Transaction): Unit = {
        val cborHex = HexUtil.encodeHexString(transaction.toCbor)
        println(s"CBOR Hex: $cborHex")
        dumpCborDiag(cborHex)
    }

    private def dumpCborDiag(cborHex: String): Unit = {
        try {
            val result = (s"echo $cborHex" #| "/home/euonymos/.cargo/bin/cbor-diag").!!
            println
            println("Diagnostic notation:")
            println(result)
        } catch {
            case e: Exception =>
                println(s"Failed to run cbor-diag: ${e.getMessage}")
        }
    }

    private def dumpCtx(ctx: TransactionBuilder.Context): Unit = {
        val indentedPrinter = pprint.PPrinter(defaultIndent = 4)

        println("Context.expectedSigners:")
        indentedPrinter.pprintln(ctx.expectedSigners)
        // println("Context.resolvedUtxos:")
        // indentedPrinter.pprintln(ctx.resolvedUtxos)    }
    }
}

object Generators {

    import scalus.cardano.address.ShelleyPaymentPart.Key

    def genAdaUtxo(
        network: Network = Mainnet
    ): Gen[TransactionUnspentOutput] = for {
        address <- genPubKeyAddr(network)
        res <- genAdaUtxoAt(address)
    } yield res

    def genAdaUtxoAt(address: Address): Gen[TransactionUnspentOutput] = for {
        utxoId <- arbitrary[TransactionInput]
        coin <- arbitrary[Coin]
    } yield TransactionUnspentOutput(
      utxoId,
      ensureMinAda(
        TransactionOutput.apply(
          address = address,
          value = Value(coin)
        ),
        testProtocolParams
      )
    )

    def genPubKeyAddr(
        network: Network = Mainnet,
        delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
    ): Gen[ShelleyAddress] =
        arbitrary[AddrKeyHash].flatMap(akh =>
            ShelleyAddress(network = network, payment = Key(akh), delegation = delegation)
        )
}

val testProtocolParams = CardanoInfo.mainnet.protocolParams

val testEvaluator = PlutusScriptEvaluator(
  slotConfig = SlotConfig.Mainnet,
  initialBudget = ExBudget.enormous,
  protocolMajorVersion = MajorProtocolVersion.plominPV,
  costModels = testProtocolParams.costModels
)
