package scalus.compiler.sir.lowering
package simple

import scalus.cardano.ledger.Language
import scalus.compiler.sir.*
import scalus.uplc.Term
import scalus.uplc.eval.PlutusVM

/** Tests for SumOfProductsLowering with explicit PlutusV4 Data matching.
  *
  * SumOfProducts encoding always uses Constr/Case for ADTs (requires V4). This test class
  * explicitly uses targetLanguage = PlutusV4 to test Case on Data instruction for Data type
  * matching, as opposed to the default chooseData builtin approach.
  */
class SumOfProductsLoweringV4Test extends SimpleLoweringTestBase:

    // Implement lower method for Sum of Products with explicit V4 Data matching
    override def lower(sir: SIR): Term =
        SumOfProductsLowering(
          sir,
          generateErrorTraces = false,
          targetLanguage = Language.PlutusV4
        ).lower()

    // Provide PlutusVM for evaluation (V4 for Constr/Case and Case on Data support)
    override given vm: PlutusVM = PlutusVM.makePlutusV4VM()
