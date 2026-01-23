package scalus.examples.paymentsplitter

import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.testing.kit.ScalusTest

/** Shared test cases for PaymentSplitter validators.
  *
  * This trait provides a common set of test case definitions that can be used by both
  * NaivePaymentSplitterValidator and OptimizedPaymentSplitterValidator tests.
  */
trait PaymentSplitterTestCases { self: ScalusTest =>

    enum Payee(val pkh: ByteString):
        case A extends Payee(genByteStringOfN(28).sample.get)
        case B extends Payee(genByteStringOfN(28).sample.get)
        case C extends Payee(genByteStringOfN(28).sample.get)
        case D extends Payee(genByteStringOfN(28).sample.get)
        case E extends Payee(genByteStringOfN(28).sample.get)
        case F extends Payee(genByteStringOfN(28).sample.get)
        case G extends Payee(genByteStringOfN(28).sample.get)
        case H extends Payee(genByteStringOfN(28).sample.get)

    case class Input(payee: Payee, amount: BigInt)
    case class Output(payee: Payee, amount: BigInt)

    extension (payee: Payee)
        inline infix def gives(amount: BigInt): Input = Input(payee, amount)
        inline infix def gets(amount: BigInt): Output = Output(payee, amount)

    /** Test case definition for payment splitter validation.
      *
      * @param payees
      *   List of payees who should receive the split
      * @param contractInputs
      *   List of amounts from contract UTxOs being spent
      * @param feePayerInput
      *   The payee who pays the fee and their input amount
      * @param outputs
      *   Expected outputs to payees
      * @param fee
      *   Transaction fee
      * @param expectedSuccess
      *   true if the test should succeed, false otherwise
      * @param expectedError
      *   Expected error regex pattern if expectedSuccess is false
      */
    case class PaymentSplitterTestCase(
        name: String,
        payees: SList[Payee],
        contractInputs: scala.List[BigInt],
        feePayerInput: (Payee, BigInt),
        outputs: SList[Output],
        fee: BigInt,
        expectedSuccess: Boolean,
        expectedError: Option[String] = None
    ) {
        def splitPerPayee: BigInt = {
            val total = contractInputs.sum
            val n = payees.asScala.size
            if n > 0 then total / n else total
        }

        def matchesError(logs: Seq[String]): Boolean =
            expectedError.forall { pattern =>
                val regex = s"(?s).*($pattern).*".r
                logs.exists(log => regex.matches(log))
            }
    }

    import Payee.*

    /** All test cases for PaymentSplitter validation */
    lazy val testCases: scala.List[PaymentSplitterTestCase] = scala.List(
      PaymentSplitterTestCase(
        name = "success when payments are correctly split for a single payee",
        payees = SList(A),
        contractInputs = scala.List(BigInt(30)),
        feePayerInput = (A, BigInt(10)),
        outputs = SList(A gets 38),
        fee = BigInt(2),
        expectedSuccess = true
      ),
      PaymentSplitterTestCase(
        name = "success when payments are correctly split between 2 payees",
        payees = SList(A, B),
        contractInputs = scala.List(BigInt(30)),
        feePayerInput = (A, BigInt(10)),
        outputs = SList(A gets 23, B gets 15),
        fee = BigInt(2),
        expectedSuccess = true
      ),
      PaymentSplitterTestCase(
        name = "success when payments are correctly split between 3 payees",
        payees = SList(A, B, C),
        contractInputs = scala.List(BigInt(30)),
        feePayerInput = (A, BigInt(10)),
        outputs = SList(A gets 18, B gets 10, C gets 10),
        fee = BigInt(2),
        expectedSuccess = true
      ),
      PaymentSplitterTestCase(
        name = "success when split equally and remainder compensates fee - o1",
        payees = SList(A, B, C),
        contractInputs = scala.List(BigInt(31)),
        feePayerInput = (A, BigInt(3)),
        outputs = SList(A gets 12, B gets 10, C gets 10),
        fee = BigInt(2),
        expectedSuccess = true
      ),
      PaymentSplitterTestCase(
        name = "success when split equally and remainder compensates fee - o2",
        payees = SList(A, B, C),
        contractInputs = scala.List(BigInt(31)),
        feePayerInput = (A, BigInt(3)),
        outputs = SList(A gets 11, B gets 10, C gets 10),
        fee = BigInt(3),
        expectedSuccess = true
      ),
      PaymentSplitterTestCase(
        name = "success when split equally and remainder compensates fee - o3",
        payees = SList(A, B, C),
        contractInputs = scala.List(BigInt(31)),
        feePayerInput = (A, BigInt(3)),
        outputs = SList(A gets 10, B gets 10, C gets 10),
        fee = BigInt(4),
        expectedSuccess = true
      ),
      PaymentSplitterTestCase(
        name = "success between 5 payees",
        payees = SList(A, B, C, D, E),
        contractInputs = scala.List(BigInt(15000000)),
        feePayerInput = (A, BigInt(41961442)),
        outputs = SList(
          A gets (3000000 + 41115417),
          B gets 3000000,
          C gets 3000000,
          D gets 3000000,
          E gets 3000000
        ),
        fee = BigInt(846025),
        expectedSuccess = true
      ),
      PaymentSplitterTestCase(
        name = "success with multiple contract UTxOs",
        payees = SList(A, B, C),
        contractInputs = scala.List(BigInt(30), BigInt(20), BigInt(10)), // 3 UTxOs = 60 total
        feePayerInput = (A, BigInt(10)),
        outputs = SList(A gets 28, B gets 20, C gets 20),
        fee = BigInt(2),
        expectedSuccess = true
      ),
      PaymentSplitterTestCase(
        name = "failure when a payee is not present in the inputs",
        payees = SList(A, B),
        contractInputs = scala.List(BigInt(30)),
        feePayerInput = (A, BigInt(0)), // Will be excluded from inputs
        outputs = SList(A gets 14, B gets 14),
        fee = BigInt(2),
        expectedSuccess = false,
        expectedError = Some("Fee payer not found")
      ),
      PaymentSplitterTestCase(
        name = "failure when a payee is not payed out (1 payee)",
        payees = SList(A),
        contractInputs = scala.List(BigInt(30)),
        feePayerInput = (A, BigInt(10)),
        outputs = SList.empty,
        fee = BigInt(2),
        expectedSuccess = false,
        expectedError = Some("Not all payees were paid")
      ),
      PaymentSplitterTestCase(
        name = "failure when one of the payees is not payed out",
        payees = SList(A, B),
        contractInputs = scala.List(BigInt(30)),
        feePayerInput = (A, BigInt(10)),
        outputs = SList(A gets 38),
        fee = BigInt(2),
        expectedSuccess = false,
        expectedError = Some("Not all payees were paid")
      ),
      PaymentSplitterTestCase(
        name = "failure when payee not in contract is to be payed",
        payees = SList(A, B),
        contractInputs = scala.List(BigInt(30)),
        feePayerInput = (A, BigInt(10)),
        outputs = SList(A gets 18, B gets 10, C gets 10),
        fee = BigInt(2),
        expectedSuccess = false,
        expectedError = Some("(?i)payee|split") // case-insensitive "payee" or "split"
      ),
      PaymentSplitterTestCase(
        name = "failure when inflated fee reduces the split payout",
        payees = SList(A, B, C),
        contractInputs = scala.List(BigInt(31)),
        feePayerInput = (A, BigInt(3)),
        outputs = SList(A gets 8, B gets 8, C gets 8),
        fee = BigInt(10),
        expectedSuccess = false,
        expectedError = Some("value to be payed|split")
      )
    )
}
