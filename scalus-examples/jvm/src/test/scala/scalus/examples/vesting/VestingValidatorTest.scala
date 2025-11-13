package scalus.examples.vesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.IntervalBoundType.*
import scalus.ledger.api.v1.{Address, PubKeyHash}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.testing.kit.{Mock, ScalusTest}
import scalus.uplc.eval.*
import scalus.Compiler.compileWithOptions

import scala.language.implicitConversions

class VestingValidatorTest extends AnyFunSuite, ScalusTest {
    private val ownerPKH: PubKeyHash = Mock.mockPubKeyHash(0)
    private val beneficiaryPKH: PubKeyHash = Mock.mockPubKeyHash(1)
    private val contractHash: ValidatorHash = Mock.mockScriptHash(0)

    private val defaultStartTime: PosixTime = BigInt(1609459200000L)
    private val defaultDuration: PosixTime = BigInt(31536000000L)
    private val defaultInitialAmount: Lovelace = BigInt(20_000_000L)
    private val defaultFee: Lovelace = BigInt(1_000_000L)

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    private inline def compiled(using options: scalus.Compiler.Options) = {
        compileWithOptions(options, VestingValidator.validate)
    }

    case class TestCase(
        signatories: List[PubKeyHash],
        interval: Interval,
        vestingDatum: Config,
        redeemer: Action,
        beneficiaryInputAmount: Lovelace = BigInt(0),
        fee: Lovelace = defaultFee
    )

    def checkTestCase(testCase: TestCase): Result = {
        val vestingDatum = testCase.vestingDatum
        val signatories = testCase.signatories
        val interval = testCase.interval
        val redeemer = testCase.redeemer
        val beneficiaryInputAmount = testCase.beneficiaryInputAmount
        val fee = testCase.fee

        val inputs = List(
          makeScriptHashInput(
            contractHash,
            vestingDatum.initialAmount
          ),
          makePubKeyHashInput(
            beneficiaryPKH.hash,
            beneficiaryInputAmount
          )
        )

        val amountToWidthdraw = redeemer.amount
        val outputs = List(
          makePubKeyHashOutput(
            beneficiaryPKH.hash,
            amountToWidthdraw
          ),
          TxOut(
            address = Address(ScriptCredential(contractHash), Option.None),
            value = Value.lovelace(vestingDatum.initialAmount - amountToWidthdraw),
            datum = OutputDatum.OutputDatum(vestingDatum.toData)
          )
        )

        val txInfo = TxInfo(
          inputs = inputs,
          id = random[TxId],
          signatories = signatories,
          outputs = outputs,
          validRange = interval,
          fee = fee
        )

        val scriptContext = ScriptContext(
          txInfo = txInfo,
          redeemer = toData(redeemer),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.head.outRef,
            datum = Some(vestingDatum.toData)
          )
        )

        // debugPrint(txInfo, vestingDatum, redeemer)
        compiled.runScript(scriptContext)
    }

    // Success cases

    test("Successful full withdrawal at/after vesting period ends") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration)
        val redeemer = Action(vestingDatum.initialAmount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee // Benefitiary paid the fee
          )
        )

        // println(result)
        assert(result.isSuccess, "Script execution should succeed")
    }

    test("Successful partial 50% withdrawal") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration / 2)
        val redeemer = Action(vestingDatum.initialAmount / 2)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee
          )
        )

        // println(result)
        assert(result.isSuccess, "Script execution should succeed for partial withdrawal")
    }

    test("Successful Partial withdrawal at 25% of vesting period") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)
        // 25% of vesting period
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration / 4)
        val withdrawalAmount = vestingDatum.initialAmount / 4
        val redeemer = Action(withdrawalAmount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee
          )
        )

        assert(result.isSuccess, "Partial withdrawal should succeed at 25% of vesting period")
    }

    test("Successful Withdrawal right after vesting starts (should get minimal amount)") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = BigInt(31_536_000L) // This ensures at least 1 lovelace per second
        )
        val signatories = List(beneficiaryPKH)
        // 1 second after
        val interval = Interval.after(vestingDatum.startTimestamp + 1000)
        val withdrawalAmount = vestingDatum.initialAmount * 1000 / vestingDatum.duration
        val redeemer = Action(withdrawalAmount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee
          )
        )

        assert(withdrawalAmount > 0, "Withdrawal amount should be greater than 0")
        assert(result.isSuccess, "Minimal withdrawal should succeed")
    }

    test("Successful Withdrawal with very large vesting duration") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = BigInt(315360000000000L), // 10 000 years
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)
        // 1 year after start
        val interval = Interval.after(vestingDatum.startTimestamp + BigInt(31536000000L))
        val expectedAmount =
            (vestingDatum.initialAmount * BigInt(31536000000L)) / vestingDatum.duration
        val redeemer = Action(expectedAmount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee
          )
        )

        assert(
          result.isSuccess,
          "Partial withdrawal should succeed with very large vesting duration"
        )
    }

    test("Successful Withdrawal with very small vesting amount") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = BigInt(1000)
        )
        val signatories = List(beneficiaryPKH)
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration)
        val redeemer = Action(BigInt(1000))

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee
          )
        )

        assert(result.isSuccess, "Full withdrawal should succeed with small vesting amount")
    }

    test("Successful Multiple partial withdrawals.") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)
        // Set time to 75% of vesting period
        val interval = Interval.after(vestingDatum.startTimestamp + (vestingDatum.duration * 3) / 4)

        // Simulate that 25% was already withdrawn
        val remainingInContract = (vestingDatum.initialAmount * 3) / 4
        val alreadyVested = (vestingDatum.initialAmount * 3) / 4
        val alreadyWithdrawn = vestingDatum.initialAmount / 4
        val availableForWithdrawal = alreadyVested - alreadyWithdrawn

        val redeemer = Action(availableForWithdrawal)

        val contractInput = makeScriptHashInput(contractHash, remainingInContract)
        val beneficiaryInput = makePubKeyHashInput(beneficiaryPKH.hash, defaultFee)
        val inputs = List(contractInput, beneficiaryInput)

        val beneficiaryOutputAmount = availableForWithdrawal
        val beneficiaryOutput = makePubKeyHashOutput(beneficiaryPKH.hash, beneficiaryOutputAmount)
        val contractOutput = TxOut(
          address = Address(ScriptCredential(contractHash), Option.None),
          value = Value.lovelace(remainingInContract - availableForWithdrawal),
          datum = OutputDatum.OutputDatum(vestingDatum.toData)
        )

        val outputs = List(beneficiaryOutput, contractOutput)

        val txInfo = TxInfo(
          inputs = inputs,
          id = random[TxId],
          signatories = signatories,
          outputs = outputs,
          validRange = interval,
          fee = defaultFee
        )

        val scriptContext = ScriptContext(
          txInfo = txInfo,
          redeemer = toData(redeemer),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.head.outRef,
            datum = Some(vestingDatum.toData)
          )
        )

        val result = compiled.runScript(scriptContext)
        assert(
          result.isSuccess,
          "Second partial withdrawal should succeed at 75% of vesting period"
        )
    }

    // Fail cases

    test("Fail Full withdrawal before vesting period ends") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration / 2)
        val redeemer = Action(vestingDatum.initialAmount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee
          )
        )

        // println(result)
        assert(result.isFailure, "Script execution should fail before the vesting period ends")
    }

    test("Fail 50% Withdrawal which exceeds available 25%") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)
        // 25% of vesting period
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration / 4)
        val excessiveAmount = vestingDatum.initialAmount / 2
        val redeemer = Action(excessiveAmount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee
          )
        )

        assert(
          result.isFailure,
          "Withdrawal should fail when amount exceeds available vested amount"
        )
    }

    test("Fail Withdrawal with no beneficiary signature") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(ownerPKH)
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration)
        val redeemer = Action(vestingDatum.initialAmount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee
          )
        )

        // println(result)
        assert(result.isFailure, "Script execution should fail because of no signatures")
    }

    test("Fail Withdrawal because no fee is paid") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration)
        val redeemer = Action(vestingDatum.initialAmount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = 0
          )
        )

        // println(result)
        assert(result.isFailure, "Script execution should fail because no fee is paid")
    }

    test("Fail: Withdrawal amount is zero") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration)
        val redeemer = Action(BigInt(0))

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer,
            beneficiaryInputAmount = defaultFee
          )
        )

        assert(result.isFailure, "Withdrawal should fail when amount is zero")
    }

    test("2 ScriptContexts") {
        val vestingDatum = Config(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          initialAmount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH)

        // 25% of vesting period
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration / 4)
        val withdrawalAmount = vestingDatum.initialAmount / 4
        val redeemer = Action(withdrawalAmount)

        val inputs = List(
          makeScriptHashInput(
            contractHash,
            vestingDatum.initialAmount
          ),
          makePubKeyHashInput(
            beneficiaryPKH.hash,
            defaultFee
          )
        )

        val outputs = List(
          makePubKeyHashOutput(
            beneficiaryPKH.hash,
            withdrawalAmount
          ),
          makePubKeyHashOutput(
            beneficiaryPKH.hash,
            withdrawalAmount
          ),
          TxOut(
            address = Address(ScriptCredential(contractHash), Option.None),
            value = Value.lovelace(vestingDatum.initialAmount - withdrawalAmount),
            datum = OutputDatum.OutputDatum(vestingDatum.toData)
          )
        )

        val txInfo = TxInfo(
          inputs = inputs,
          id = random[TxId],
          signatories = signatories,
          outputs = outputs,
          validRange = interval,
          fee = defaultFee
        )

        val scriptContext = ScriptContext(
          txInfo = txInfo,
          redeemer = toData(redeemer),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.head.outRef,
            datum = Some(vestingDatum.toData)
          )
        )

        val scriptContext2 = ScriptContext(
          txInfo = txInfo,
          redeemer = toData(redeemer),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.tail.head.outRef,
            datum = Some(vestingDatum.toData)
          )
        )
        // debugPrint(txInfo, vestingDatum, redeemer)

        val firstResult = compiled.runScript(scriptContext)
        assert(firstResult.isFailure, "First withdrawal should succeed")

        val secondResult = compiled.runScript(scriptContext2)
        assert(secondResult.isFailure, "Second withdrawal should fail")
    }

    private def debugPrint(
        txInfo: TxInfo,
        vestingDatum: Config,
        redeemer: Action
    ): Unit = {
        printAdaInInputs(txInfo, vestingDatum, redeemer)
        printContractOutput(txInfo)
        printAvailableAmout(txInfo, vestingDatum)
    }

    private def printAdaInInputs(
        txInfo: TxInfo,
        vestingDatum: Config,
        redeemer: Action
    ): Unit = {
        println("All outputs: " + txInfo.outputs)
        val beneficiaryOutputs = txInfo.outputs.filter(txOut =>
            txOut.address.credential match
                case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
                case _                                => false
        )
        println("Beneficiary Outputs: " + beneficiaryOutputs)
        val adaInOutputs = beneficiaryOutputs
            .map(txOut => txOut.value.getLovelace)
            .foldLeft(BigInt(0))(_ + _)
        println("ADA in Outputs: " + adaInOutputs)

        println("All inputs: " + txInfo.inputs)
        val beneficiaryInputs = txInfo.inputs.filter(txInInfo =>
            txInInfo.resolved.address.credential match
                case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
                case _                                => false
        )
        println("Beneficiary Inputs: " + beneficiaryInputs)
        val adaInInputs = beneficiaryInputs
            .map(txInInfo => txInInfo.resolved.value.getLovelace)
            .foldLeft(BigInt(0))(_ + _)
        println("Reddemer amount: " + redeemer.amount)
        println("ADA in Inputs: " + adaInInputs)
        println("Fee: " + txInfo.fee)
    }

    def printContractOutput(txInfo: TxInfo): Unit = {
        val ownInput = txInfo.findOwnInputOrFail(txInfo.inputs.head.outRef).resolved
        println("Own Input: " + ownInput)
        val contractAddress = ownInput.address
        println("Contract Address: " + contractAddress)

        println("TxInfo.outputs: " + txInfo.outputs)
        val contractOutputs = txInfo.outputs.filter(txOut => txOut.address === contractAddress)
        println("Contract Outputs: " + contractOutputs)

        val contractOutput = contractOutputs.head
        println("Contract Output: " + contractOutput)
        println("Contract Output Datum: " + contractOutput.datum)
    }

    def printAvailableAmout(txInfo: TxInfo, vestingDatum: Config): Unit = {
        def linearVesting(timestamp: BigInt): BigInt = {
            val min = vestingDatum.startTimestamp
            val max = vestingDatum.startTimestamp + vestingDatum.duration
            println("min: " + min + ", max: " + max)
            println("timestamp: " + timestamp)
            println("timestamp < min: " + (timestamp < min))
            println("timestamp >= max: " + (timestamp >= max))

            if timestamp < min then 0
            else if timestamp >= max then vestingDatum.initialAmount
            else
                vestingDatum.initialAmount * (timestamp - vestingDatum.startTimestamp) / vestingDatum.duration
        }
        val txEarliestTime = txInfo.validRange.from.boundType match
            case Finite(t) => t
            case _         => BigInt(0)
        val linearVestingTime = linearVesting(txEarliestTime)
        println("Linear Vesting Time: " + linearVestingTime)
        val ownInput = txInfo.findOwnInputOrFail(txInfo.inputs.head.outRef).resolved
        val contractAmount = ownInput.value.getLovelace
        println("Contract Amount: " + contractAmount)
        val released = vestingDatum.initialAmount - contractAmount
        println("Released Amount: " + released)
        val availableAmount = linearVestingTime - released
        println("Available Amount: " + availableAmount)
    }

}
