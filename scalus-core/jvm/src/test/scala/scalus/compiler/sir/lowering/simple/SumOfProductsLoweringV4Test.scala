package scalus.compiler.sir.lowering
package simple

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.*
import scalus.uplc.Term
import scalus.uplc.eval.PlutusVM

/** Tests for SumOfProductsLowering with van Rossem hard fork features.
  *
  * SumOfProducts encoding always uses Constr/Case for ADTs. This test class explicitly uses
  * targetProtocolVersion = vanRossemPV to test Case on Data instruction for Data type matching, as
  * opposed to the default chooseData builtin approach.
  */
class SumOfProductsLoweringV4Test extends SimpleLoweringTestBase:

    // Implement lower method for Sum of Products with van Rossem Data matching
    override def lower(sir: SIR): Term =
        SumOfProductsLowering(
          sir,
          generateErrorTraces = false,
          targetProtocolVersion = MajorProtocolVersion.vanRossemPV
        ).lower()

    // Provide PlutusVM for evaluation (vanRossemPV for Constr/Case and Case on Data support)
    override given vm: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
