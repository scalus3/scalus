package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.toData
import scalus.builtin.{Builtins, ByteString, Data}
import scalus.cardano.ledger.ExUnits
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.testing.kit.ScalusTest

import scala.util.Try

class HtlcValidatorTest extends AnyFunSuite with ScalusTest {
    import HtlcValidatorTest.*

    test("committer successfully unlocks HTLC after timeout") {
        TestCase(
          inputs = List(Committer gives 100),
          outputs = List(Committer gets 1050),
          value = 1000,
          fee = 50,
          timeout = 500,
          validFrom = 1000,
          signatories = List(Committer),
          action = Action.Timeout,
          preimage = generatePreimage(),
          expected = success
        ).runWithDebug()
    }

    test("receiver successfully unlocks HTLC with valid preimage before timeout") {
        val preimage = generatePreimage()
        TestCase(
          inputs = List(Receiver gives 100),
          outputs = List(Receiver gets 1050),
          value = 1000,
          fee = 50,
          timeout = 1000,
          validFrom = 500,
          signatories = List(Receiver),
          action = Action.Reveal(preimage),
          preimage = preimage,
          expected = success
        ).runWithDebug()
    }

    test("committer can't unlock HTLC before timeout") {
        TestCase(
          inputs = List(Committer gives 100),
          outputs = List(Committer gets 1050),
          value = 1000,
          fee = 50,
          timeout = 500,
          validFrom = 10,
          signatories = List(Committer),
          action = Action.Timeout,
          preimage = generatePreimage(),
          expected = failure(InvalidCommitterTimePoint)
        ).runWithDebug()
    }

    test("receiver can't unlock HTLC after timeout") {
        val preimage = generatePreimage()
        TestCase(
          inputs = List(Receiver gives 100),
          outputs = List(Receiver gets 1050),
          value = 1000,
          fee = 50,
          timeout = 1000,
          validFrom = 1500,
          signatories = List(Receiver),
          action = Action.Reveal(preimage),
          preimage = preimage,
          expected = failure(InvalidReceiverTimePoint)
        ).runWithDebug()
    }

    test("receiver can't unlock HTLC with invalid preimage") {
        val validPreimage = generatePreimage()
        val invalidPreimage = generateInvalidPreimage()
        TestCase(
          inputs = List(Receiver gives 100),
          outputs = List(Receiver gets 1050),
          value = 1000,
          fee = 50,
          timeout = 1000,
          validFrom = 500,
          signatories = List(Receiver),
          action = Action.Reveal(invalidPreimage),
          preimage = validPreimage,
          expected = failure(InvalidReceiverPreimage)
        ).runWithDebug()
    }

    test("committer fails to unlock HTLC without signature") {
        TestCase(
          inputs = List(Committer gives 100),
          outputs = List(Committer gets 1050),
          value = 1000,
          fee = 50,
          timeout = 500,
          validFrom = 1000,
          signatories = List(WrongCommitter),
          action = Action.Timeout,
          preimage = generatePreimage(),
          expected = failure(UnsignedCommitterTransaction)
        ).runWithDebug()
    }

    test("receiver fails to unlock HTLC without signature") {
        val preimage = generatePreimage()
        TestCase(
          inputs = List(Receiver gives 100),
          outputs = List(Receiver gets 1050),
          value = 1000,
          fee = 50,
          timeout = 1000,
          validFrom = 500,
          signatories = List(WrongReceiver),
          action = Action.Reveal(preimage),
          preimage = preimage,
          expected = failure(UnsignedReceiverTransaction)
        ).runWithDebug()
    }
}

object HtlcValidatorTest extends ScalusTest {
    export HtlcValidator.*
    export Person.*

    enum Person(val pkh: Hash):
        case Committer extends Person(genByteStringOfN(28).sample.get)
        case Receiver extends Person(genByteStringOfN(28).sample.get)
        case WrongCommitter
            extends Person(
              Builtins.xorByteString(false, Person.Committer.pkh, Person.Committer.pkh)
            )
        case WrongReceiver
            extends Person(Builtins.xorByteString(false, Person.Receiver.pkh, Person.Receiver.pkh))

        infix def gives(value: BigInt): Input = Input(this, value)
        infix def gets(value: BigInt): Output = Output(this, value)

    case class Input(person: Person, value: BigInt)
    case class Output(person: Person, value: BigInt)

    case class TestCase(
        inputs: List[Input] = List.empty,
        outputs: List[Output] = List.empty,
        value: BigInt = 0,
        fee: BigInt = 0,
        timeout: BigInt = 0,
        validFrom: BigInt = 0,
        signatories: List[Person] = List.empty,
        action: Action,
        preimage: ByteString,
        expected: (String | Unit, Option[ExUnits]) = success
    ):
        def runWithDebug(): Unit = {
            val contractDatum = ContractDatum(
              committer = Committer.pkh,
              receiver = Receiver.pkh,
              image = sha3_256(preimage),
              timeout = timeout
            )

            val context = makeSpendingScriptContext(
              inputs = inputs
                  .map(input => makePubKeyHashInput(input.person.pkh, input.value))
                  .prepended(makeScriptHashInput(scriptHash, value)),
              outputs = outputs
                  .map(output => makePubKeyHashOutput(output.person.pkh, output.value)),
              fee = fee,
              validRange = Interval.after(validFrom),
              signatories = signatories.map(_.pkh),
              action = action,
              contractDatum = contractDatum
            )

            val actual = Try(HtlcValidator.validate(context.toData))
            (expected, actual) match
                case ((msg: String, _), scala.util.Failure(exception)) =>
                case (((), _), scala.util.Success(_))                  =>
                case (_, actual) => fail(s"Expected: $expected, but got: $actual")

            checkResult(expected = expected, actual = program.runWithDebug(context))
        }

    private def generatePreimage(): Preimage = genByteStringOfN(32).sample.get
    private def generateInvalidPreimage(): Preimage = genByteStringOfN(12).sample.get

    private def makeSpendingScriptContext(
        inputs: List[TxInInfo],
        outputs: List[TxOut],
        fee: BigInt,
        validRange: Interval,
        signatories: List[ByteString],
        action: Action,
        contractDatum: ContractDatum
    ): ScriptContext = {
        ScriptContext(
          txInfo = TxInfo(
            inputs = inputs,
            outputs = outputs,
            fee = fee,
            validRange = validRange,
            signatories = signatories.map(PubKeyHash(_)),
            id = random[TxId]
          ),
          redeemer = action.toData,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.at(0).outRef,
            datum = Option.Some(contractDatum.toData)
          )
        )
    }

    private lazy val program = HtlcContract.debugCompiledContract.program
    private lazy val scriptHash = HtlcContract.debugCompiledContract.script.scriptHash
}
