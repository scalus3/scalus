package scalus.compiler.sir.lowering
package simple

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.*
import scalus.uplc.Term
import scalus.uplc.eval.PlutusVM

/** Tests for ScottEncodingLowering with van Rossem hard fork features.
  *
  * Protocol version 11 uses Case on Data instruction instead of chooseData builtin. This test class
  * verifies that the Scott encoding lowering works correctly with protocol version >= vanRossemPV.
  */
class ScottEncodingLoweringV4Test extends SimpleLoweringTestBase:

    // Implement lower method for Scott encoding with van Rossem target
    override def lower(sir: SIR): Term =
        ScottEncodingLowering(
          sir,
          generateErrorTraces = false,
          targetProtocolVersion = MajorProtocolVersion.vanRossemPV
        ).lower()

    // Provide PlutusVM for evaluation (vanRossemPV for Case on Data support)
    override given vm: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
