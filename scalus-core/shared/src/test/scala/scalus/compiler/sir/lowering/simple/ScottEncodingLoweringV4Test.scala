package scalus.compiler.sir.lowering
package simple

import scalus.cardano.ledger.Language
import scalus.compiler.sir.*
import scalus.uplc.Term
import scalus.uplc.eval.PlutusVM

/** Tests for ScottEncodingLowering with PlutusV4.
  *
  * PlutusV4 uses Case on Data instruction instead of chooseData builtin. This test class verifies
  * that the Scott encoding lowering works correctly with the V4 backend.
  */
class ScottEncodingLoweringV4Test extends SimpleLoweringTestBase:

    // Implement lower method for Scott encoding with V4 target
    override def lower(sir: SIR): Term =
        ScottEncodingLowering(
          sir,
          generateErrorTraces = false,
          targetLanguage = Language.PlutusV4
        ).lower()

    // Provide PlutusVM for evaluation (V4 for Case on Data support)
    override given vm: PlutusVM = PlutusVM.makePlutusV4VM()
