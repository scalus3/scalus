package scalus.examples.paymentsplitter

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.{TwoArgumentPlutusScriptWitness, TxBuilder}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.testing.kit.{Party, ScalusTest, TestUtil}
import scalus.utils.await

class PaymentSplitterTxBuilderTest
    extends AnyFunSuite
    with ScalusTest
    with PaymentSplitterTestCases {

    given CardanoInfo = CardanoInfo.mainnet

    case class TxResult(
        success: Boolean,
        error: Option[String],
        fee: Coin,
        exUnits: Map[RedeemerTag, Seq[ExUnits]],
        outputs: Seq[TransactionOutput] = Seq.empty
    ) {
        def totalExUnits(tag: RedeemerTag): ExUnits =
            exUnits
                .getOrElse(tag, Seq.empty)
                .foldLeft(ExUnits(0, 0))((a, e) => ExUnits(a.memory + e.memory, a.steps + e.steps))
    }

    private val genesisHash = TestUtil.genesisHash

    // Scale factor to convert test case amounts to realistic lovelace values
    // Test cases use small numbers (30, 31, etc.) which we scale to ADA
    private val ScaleFactor = 1_000_000L

    /** Map Payee to Party - gives us consistent addresses and signing keys */
    private def payeeParty(p: Payee): Party = p match
        case Payee.A => Party.Alice
        case Payee.B => Party.Bob
        case Payee.C => Party.Charles
        case Payee.D => Party.Dave
        case Payee.E => Party.Eve
        case _       => Party.Alice

    private def scriptAddr(scriptHash: ScriptHash) = ShelleyAddress(
      summon[CardanoInfo].network,
      ShelleyPaymentPart.Script(scriptHash),
      ShelleyDelegationPart.Null
    )

    private def applyParam[A](contract: scalus.uplc.PlutusV3[A], param: Data): Script.PlutusV3 = {
        // Use withErrorTraces for better error messages during debugging
        val applied = contract.withErrorTraces.program $ param
        Script.PlutusV3(applied.cborByteString)
    }

    private def buildUtxos(tc: PaymentSplitterTestCase, scriptHash: ScriptHash) = {
        val addr = scriptAddr(scriptHash)
        val feePayerParty = payeeParty(tc.feePayerInput._1)
        // Scale contract inputs to realistic lovelace amounts
        val contractUtxos = tc.contractInputs.zipWithIndex.map { case (amt, idx) =>
            TransactionInput(genesisHash, idx) ->
                TransactionOutput(addr, Value.lovelace(amt.toLong * ScaleFactor), Data.unit)
        }
        // Fee payer UTxO - scaled amount plus extra for fees
        val feePayerUtxo = TransactionInput(genesisHash, 100) ->
            TransactionOutput(
              feePayerParty.address,
              Value.lovelace(tc.feePayerInput._2.toLong * ScaleFactor + 100_000_000L)
            )
        // Collateral uses fee payer's address
        val collateralUtxo = TransactionInput(genesisHash, 200) ->
            TransactionOutput(feePayerParty.address, Value.lovelace(50_000_000L))
        (contractUtxos, feePayerUtxo, collateralUtxo)
    }

    private def extractExUnits(tx: Transaction): Map[RedeemerTag, Seq[ExUnits]] = {
        val redeemers: Seq[Redeemer] = tx.witnessSet.redeemers.toSeq.flatMap(_.value.toSeq)
        redeemers.groupBy(_.tag).view.mapValues(_.map(_.exUnits)).toMap
    }

    private def runNaive(tc: PaymentSplitterTestCase): TxResult = {
        // Use Party PKHs for the validator parameter (matches Party addresses we'll use for UTxOs)
        val payeePkhs: scalus.cardano.onchain.plutus.prelude.List[scalus.uplc.builtin.ByteString] =
            tc.payees.map(p => payeeParty(p).addrKeyHash)
        val paramScript = applyParam(NaivePaymentSplitterContract, payeePkhs.toData)
        val (contractUtxos, feePayerUtxo, collateralUtxo) = buildUtxos(tc, paramScript.scriptHash)
        val emulator = Emulator((contractUtxos :+ feePayerUtxo :+ collateralUtxo).toMap)

        val feePayerParty = payeeParty(tc.feePayerInput._1)
        val feePayerPayee = tc.feePayerInput._1

        // Calculate actual splitPerPayee from scaled inputs (must match what validator calculates)
        val totalContractLovelace = tc.contractInputs.sum * ScaleFactor
        val nPayees = tc.payees.asScala.size
        val splitPerPayeeLong = (totalContractLovelace / nPayees).toLong

        var builder = TxBuilder(summon[CardanoInfo])
        contractUtxos.foreach { case (input, output) =>
            builder = builder.spend(Utxo(input, output), Data.unit, paramScript)
        }
        // Explicitly add fee payer's UTxO - validator requires an input from a payee
        builder = builder.spend(Utxo(feePayerUtxo))

        // Add outputs: non-change payees get exactly splitPerPayee, change payee gets rest via changeTo
        tc.payees.asScala.foreach { payee =>
            val outputAddr = payeeParty(payee).address
            if payee == feePayerPayee then
                // Fee payer output receives change - minimum is splitPerPayee
                builder = builder.changeTo(
                  TransactionOutput(outputAddr, Value.lovelace(splitPerPayeeLong))
                )
            else
                // Other payees get exactly splitPerPayee
                builder = builder.payTo(outputAddr, Value.lovelace(splitPerPayeeLong))
        }

        try {
            val tx = builder
                .complete(emulator, feePayerParty.address)
                .await()
                .sign(feePayerParty.signer)
                .transaction
            val result = emulator.submit(tx).await()
            TxResult(
              result.isRight,
              result.left.toOption.map(_.toString),
              tx.body.value.fee,
              extractExUnits(tx),
              tx.body.value.outputs.map(_.value).toSeq
            )
        } catch {
            case e: scalus.cardano.txbuilder.TxBuilderException.BalancingException =>
                System.err.println(s"=== SCRIPT EVALUATION FAILED ===")
                System.err.println(s"Error: ${e.getMessage}")
                System.err.println(
                  s"Script logs: ${e.scriptLogs.getOrElse(Seq.empty).mkString("\n")}"
                )
                throw e
            case e: Throwable =>
                System.err.println(s"=== OTHER ERROR: ${e.getClass.getName} ===")
                System.err.println(s"Message: ${e.getMessage}")
                throw e
        }
    }

    private def runOptimized(tc: PaymentSplitterTestCase): TxResult = {
        import TwoArgumentPlutusScriptWitness.*

        // Use Party PKHs for the validator parameter (matches Party addresses we'll use for UTxOs)
        // AddrKeyHash is an opaque type extending ByteString, so it can be used directly
        val payeePkhs: scalus.cardano.onchain.plutus.prelude.List[scalus.uplc.builtin.ByteString] =
            tc.payees.map(p => payeeParty(p).addrKeyHash)
        val paramScript = applyParam(OptimizedPaymentSplitterContract, payeePkhs.toData)
        val (contractUtxos, feePayerUtxo, collateralUtxo) = buildUtxos(tc, paramScript.scriptHash)
        // Extra UTxO for stake registration transaction
        val stakeRegUtxo = TransactionInput(genesisHash, 300) ->
            TransactionOutput(payeeParty(tc.feePayerInput._1).address, Value.lovelace(10_000_000L))
        val emulator =
            Emulator((contractUtxos :+ feePayerUtxo :+ collateralUtxo :+ stakeRegUtxo).toMap)

        val feePayerParty = payeeParty(tc.feePayerInput._1)
        val feePayerPayee = tc.feePayerInput._1

        // Calculate from scaled contract inputs (must match what buildUtxos creates)
        val totalContractLovelace = tc.contractInputs.sum * ScaleFactor
        val nPayees = tc.payees.asScala.size
        val splitPerPayee = totalContractLovelace / nPayees
        val splitPerPayeeLong = splitPerPayee.toLong

        val verification = SplitVerificationRedeemer(
          PubKeyHash(feePayerParty.addrKeyHash),
          totalContractLovelace,
          splitPerPayee,
          BigInt(nPayees)
        )

        val scriptStakeAddress =
            StakeAddress(summon[CardanoInfo].network, StakePayload.Script(paramScript.scriptHash))

        // Step 1: Register stake credential in a separate transaction
        // The zero-withdraw trick requires the stake address to be registered first
        val regTx = TxBuilder(summon[CardanoInfo])
            .registerStake(scriptStakeAddress, attached(paramScript, Data.unit))
            .complete(emulator, feePayerParty.address)
            .await()
            .sign(feePayerParty.signer)
            .transaction
        val regResult = emulator.submit(regTx).await()
        assert(regResult.isRight, s"Registration tx should succeed: $regResult")

        // Step 2: Now do the payment split with zero-withdraw trick
        var builder = TxBuilder(summon[CardanoInfo])
        // Use dynamic redeemer builder to compute correct input index from final transaction
        contractUtxos.foreach { case (input, output) =>
            builder = builder.spend(
              Utxo(input, output),
              (tx: Transaction) => {
                  val inputIndex = tx.body.value.inputs.toSeq.indexOf(input)
                  SpendRedeemer(BigInt(inputIndex)).toData
              },
              paramScript
            )
        }
        // Explicitly add fee payer's UTxO - validator requires an input from a payee
        builder = builder.spend(Utxo(feePayerUtxo))
        builder = builder.withdrawRewards(
          scriptStakeAddress,
          Coin(0),
          attached(paramScript, verification)
        )

        // Add outputs: non-change payees get exactly splitPerPayee, change payee gets rest via changeTo
        tc.payees.asScala.foreach { payee =>
            val outputAddr = payeeParty(payee).address
            if payee == feePayerPayee then
                // Fee payer output receives change - minimum is splitPerPayee
                builder = builder.changeTo(
                  TransactionOutput(outputAddr, Value.lovelace(splitPerPayeeLong))
                )
            else
                // Other payees get exactly splitPerPayee
                builder = builder.payTo(outputAddr, Value.lovelace(splitPerPayeeLong))
        }

        try {
            val tx = builder
                .complete(emulator, feePayerParty.address)
                .await()
                .sign(feePayerParty.signer)
                .transaction
            val result = emulator.submit(tx).await()
            TxResult(
              result.isRight,
              result.left.toOption.map(_.toString),
              tx.body.value.fee,
              extractExUnits(tx),
              tx.body.value.outputs.map(_.value).toSeq
            )
        } catch {
            case e: scalus.cardano.txbuilder.TxBuilderException.BalancingException =>
                System.err.println(s"=== OPTIMIZED SCRIPT EVALUATION FAILED ===")
                System.err.println(s"Error: ${e.getMessage}")
                System.err.println(
                  s"Script logs: ${e.scriptLogs.getOrElse(Seq.empty).mkString("\n")}"
                )
                throw e
        }
    }

    testCases.filter(_.expectedSuccess).foreach { tc =>
        test(s"TxBuilder Naive: ${tc.name}") {
            val r = runNaive(tc)
            assert(r.success, s"Expected success: ${r.error.getOrElse("")}")
        }
        test(s"TxBuilder Optimized: ${tc.name}") {
            val r = runOptimized(tc)
            assert(r.success, s"Expected success: ${r.error.getOrElse("")}")
        }
    }

    test("TxBuilder: Cost comparison naive vs optimized (3 UTxOs)") {
        val tc = testCases.find(_.name.contains("multiple contract UTxOs")).get
        val n = runNaive(tc)
        val o = runOptimized(tc)
        val numUtxos = tc.contractInputs.size

        val naiveSpend = n.totalExUnits(RedeemerTag.Spend)
        val optReward = o.totalExUnits(RedeemerTag.Reward)
        val optSpend = o.totalExUnits(RedeemerTag.Spend)

        println(s"\n=== Cost Comparison ($numUtxos UTxOs) ===")
        println(
          f"Naive:     fee=${n.fee.value}%,d  spend: mem=${naiveSpend.memory}%,d cpu=${naiveSpend.steps}%,d"
        )
        println(
          f"Optimized: fee=${o.fee.value}%,d  reward: mem=${optReward.memory}%,d cpu=${optReward.steps}%,d"
        )
        println(f"                      spend: mem=${optSpend.memory}%,d cpu=${optSpend.steps}%,d")
        val memSave = 100 - (optReward.memory + optSpend.memory) * 100 / naiveSpend.memory
        val cpuSave = 100 - (optReward.steps + optSpend.steps) * 100 / naiveSpend.steps
        println(f"Savings: mem=$memSave%%  cpu=$cpuSave%%\n")
    }

    test("TxBuilder: verify remainder goes to fee payer when sum doesn't divide evenly") {
        // Test case: contractInputs = 31, 3 payees → splitPerPayee = 10, remainder = 1
        val tc = testCases.find(_.name.contains("remainder compensates fee - o1")).get

        val totalContractLovelace = tc.contractInputs.sum * ScaleFactor // 31_000_000
        val nPayees = tc.payees.asScala.size // 3
        val splitPerPayee = totalContractLovelace / nPayees // 10_333_333
        val remainder = totalContractLovelace % nPayees // 1

        val resultNaive = runNaive(tc)
        val resultOptimized = runOptimized(tc)

        assert(resultNaive.success, s"Naive should succeed: ${resultNaive.error.getOrElse("")}")
        assert(
          resultOptimized.success,
          s"Optimized should succeed: ${resultOptimized.error.getOrElse("")}"
        )

        // Find fee payer's output by address
        val feePayerAddr = payeeParty(tc.feePayerInput._1).address

        def verifyFeePayerTransactionOutput(result: TxResult, label: String): Unit = {
            val feePayerOutput = result.outputs.find(_.address == feePayerAddr)
            assert(feePayerOutput.isDefined, s"$label: Fee payer output not found")

            val feePayerLovelace = feePayerOutput.get.value.coin.value
            val minExpected = splitPerPayee + remainder // At least their share + remainder

            assert(
              feePayerLovelace >= minExpected.toLong,
              s"$label: Fee payer got $feePayerLovelace lovelace, expected at least $minExpected (splitPerPayee=$splitPerPayee + remainder=$remainder)"
            )

            println(
              f"$label: Fee payer received $feePayerLovelace%,d lovelace (min expected: $minExpected%,d)"
            )
        }

        verifyFeePayerTransactionOutput(resultNaive, "Naive")
        verifyFeePayerTransactionOutput(resultOptimized, "Optimized")
    }
}
