package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.{BuiltinList, ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.compiler.{compile, offsetOf, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.cardano.ledger.Language
import scalus.cardano.onchain.plutus.v1.{PubKeyHash, TxId}
import scalus.cardano.onchain.plutus.v2.*
import scalus.cardano.onchain.plutus.prelude.List
import scalus.uplc.*
import scalus.uplc.eval.*
import scalus.cardano.ledger.{ExUnits, ExUnitPrices, NonNegativeInterval}
import scala.language.implicitConversions

/** Low-level assembler variant using dropList (V4 builtin) instead of the field macro's chained
  * tailList.
  */
@Compile
object OptimizedPreimageValidatorV4 {
    import scalus.cardano.onchain.plutus.prelude.require
    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
        val pair = datum.toConstr.snd
        inline def hash = pair.head.toByteString
        val pkh = pair.tail.head
        inline def preimage = redeemer.toByteString
        def checkSignatories(sigs: BuiltinList[Data]): Unit =
            if sigs.head == pkh then ()
            else checkSignatories(sigs.tail)
        // V4: use dropList + offsetOf to skip fields in V2 TxInfo to reach signatories
        val txInfoFields = ctxData.toConstr.snd.head.toConstr.snd
        inline def sigs = dropList(offsetOf[TxInfo](_.signatories), txInfoFields).head.toList
        checkSignatories(sigs)
        require(sha2_256(preimage) == hash)
    }
}

/** Low-level assembler using dropList (V4) and BuiltinList pattern match (Case on List).
  * Uses @unchecked to omit the Nil branch -- VM throws CaseListBranchError on empty list.
  */
@Compile
object PreimageValidatorWithListMatch {
    import scalus.cardano.onchain.plutus.prelude.require
    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
        val pair = datum.toConstr.snd
        inline def hash = pair.head.toByteString
        val pkh = pair.tail.head
        inline def preimage = redeemer.toByteString
        def checkSignatories(sigs: BuiltinList[Data]): Unit =
            (sigs: @unchecked) match
                case BuiltinList.Cons(h, t) =>
                    if h == pkh then () else checkSignatories(t)
        val txInfoFields = ctxData.toConstr.snd.head.toConstr.snd
        inline def sigs = dropList(offsetOf[TxInfo](_.signatories), txInfoFields).head.toList
        checkSignatories(sigs)
        require(sha2_256(preimage) == hash)
    }
}

/** Assembler with signatory index in redeemer -- O(1) lookup instead of linear search.
  * Redeemer = Constr(0, [B(preimage), I(signatoryIndex)])
  * New TxBuilder can set the index when building the transaction.
  */
@Compile
object PreimageValidatorWithSigIndex {
    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
        val pair = datum.toConstr.snd
        inline def hash = pair.head.toByteString
        val pkh = pair.tail.head
        val redeemerFields = redeemer.toConstr.snd
        inline def preimage = redeemerFields.head.toByteString
        inline def sigIndex = redeemerFields.tail.head.toBigInt
        // V4: dropList to skip fields + dropList to index into signatories
        val txInfoFields = ctxData.toConstr.snd.head.toConstr.snd
        inline def sigs = dropList(offsetOf[TxInfo](_.signatories), txInfoFields).head.toList
        dropList(sigIndex, sigs).head == pkh || (throw new RuntimeException("Not signed"))
        sha2_256(preimage) == hash || (throw new RuntimeException("Wrong"))
    }
}

/** Direct UPLC construction -- no compiler plugin, maximum control.
  * Uses V4 Case on List (Cons-only) and Case on Bool.
  */
object DirectPreimageValidator {
    import scalus.uplc.Term
    import scalus.uplc.Term.{asTerm, λ}
    import scalus.uplc.TermDSL.given
    import scalus.uplc.DefaultFun.*
    import scalus.uplc.{Constant => C}

    private def pfix(f: Term => Term): Term = λ { r => r $ r } $ λ { r => f(r $ r) }

    val preimageValidator: Term = λ { datum => λ { redeemer => λ { ctxData =>
        // let pair = snd(unConstr(datum))
        (λ { pair =>
            // let pkh = head(tail(pair)) — kept as Data for equalsData
            (λ { pkh =>
                // sigs = unListData(head(dropList(8, snd(unConstr(head(snd(unConstr(ctxData))))))))
                val txInfoFields = !(!SndPair) $ (UnConstrData $ (!(HeadList) $ (!(!SndPair) $ (UnConstrData $ ctxData))))
                val sigs = UnListData $ (!(HeadList) $ (!(DropList) $ BigInt(8).asTerm $ txInfoFields))
                // recursive signatory check: Case on List (Cons-only), Case on Bool
                val checkSigs = pfix { recur =>
                    λ { s =>
                        Term.Case(s, scala.List(
                            λ { h => λ { t =>
                                Term.Case(EqualsData $ h $ pkh, scala.List(
                                    recur $ t,             // False(0): keep searching
                                    Term.Const(C.Unit)     // True(1): found
                                ))
                            }}
                        ))
                    }
                }
                // checkSigs(sigs); then hash check
                (λ { _ =>
                    Term.Case(
                        EqualsByteString $ (Sha2_256 $ (UnBData $ redeemer)) $ (UnBData $ (!(HeadList) $ pair)),
                        scala.List(
                            Term.Error,            // False(0): wrong preimage
                            Term.Const(C.Unit)     // True(1): success
                        )
                    )
                }) $ (checkSigs $ sigs)
            }) $ (!(HeadList) $ (!(TailList) $ pair))  // pkh
        }) $ (!(!SndPair) $ (UnConstrData $ datum))     // pair
    }}}
}

/** Budget comparison of three PreimageValidator variants:
  *   1. Low-level assembler with `field` macro (ScottEncodingLowering)
  *   2. Low-level assembler with `dropList` (SirToUplcV3Lowering + PlutusV4)
  *   3. High-level style (SirToUplcV3Lowering)
  */
class PreimageBudgetComparisonTest extends AnyFunSuite {

    private val preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8"))
    private val hash = hex"36c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49"
    private val pubKeyHash =
        PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6")

    private val scriptContext = ScriptContext(
      TxInfo(
        inputs = scalus.cardano.onchain.plutus.prelude.List.Nil,
        referenceInputs = scalus.cardano.onchain.plutus.prelude.List.Nil,
        outputs = scalus.cardano.onchain.plutus.prelude.List.Nil,
        fee = Value.lovelace(BigInt("188021")),
        mint = Value.lovelace(BigInt("188021")),
        dcert = scalus.cardano.onchain.plutus.prelude.List.Nil,
        withdrawals = scalus.cardano.onchain.plutus.prelude.SortedMap.empty,
        validRange = Interval.always,
        signatories = List(pubKeyHash),
        redeemers = scalus.cardano.onchain.plutus.prelude.SortedMap.empty,
        data = scalus.cardano.onchain.plutus.prelude.SortedMap.empty,
        id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
      ),
      ScriptPurpose.Spending(
        scalus.cardano.onchain.plutus.v1.TxOutRef(
          TxId(hex"1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"),
          0
        )
      )
    )

    private val datum = (hash, pubKeyHash).toData
    private val redeemer = preimage.toData
    // Redeemer for variant 6: (preimage, signatoryIndex)
    private val redeemerWithIndex = (preimage, BigInt(0)).toData
    private val ctxData = scriptContext.toData

    // Mainnet execution unit prices
    private val exPrices = ExUnitPrices(
      priceMemory = NonNegativeInterval(0.0577, precision = 15),
      priceSteps = NonNegativeInterval(0.0000721, precision = 15)
    )

    private def evalAndReport(name: String, program: Program, vm: PlutusVM): Unit = {
        val result = program.deBruijnedProgram.evaluateDebug(using vm)
        result match
            case Result.Success(_, budget, _, _) =>
                val flatSize = program.flatEncoded.length
                val fee = ExUnits(budget.memory, budget.steps).fee(exPrices)
                info(f"$name%-50s flat=$flatSize%4d bytes  cpu=${budget.steps}%,12d  mem=${budget.memory}%,10d  fee=${fee.value}%,8d lovelace")
            case Result.Failure(err, budget, _, logs) =>
                fail(s"$name failed: ${err.getMessage}\nLogs: ${logs.mkString("\n")}")
    }

    // 10 signatories, target at index 9 (last)
    private val dummyPkh = (1 to 9).map(i =>
        PubKeyHash(hex"00000000000000000000000000000000000000000000000000000000" ++ ByteString.fromArray(Array(i.toByte)))
    )
    private val manySignatories = scalus.cardano.onchain.plutus.prelude.List.from(dummyPkh :+ pubKeyHash)
    private val scriptContext10 = ScriptContext(
      TxInfo(
        inputs = scalus.cardano.onchain.plutus.prelude.List.Nil,
        referenceInputs = scalus.cardano.onchain.plutus.prelude.List.Nil,
        outputs = scalus.cardano.onchain.plutus.prelude.List.Nil,
        fee = Value.lovelace(BigInt("188021")),
        mint = Value.lovelace(BigInt("188021")),
        dcert = scalus.cardano.onchain.plutus.prelude.List.Nil,
        withdrawals = scalus.cardano.onchain.plutus.prelude.SortedMap.empty,
        validRange = Interval.always,
        signatories = manySignatories,
        redeemers = scalus.cardano.onchain.plutus.prelude.SortedMap.empty,
        data = scalus.cardano.onchain.plutus.prelude.SortedMap.empty,
        id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
      ),
      ScriptPurpose.Spending(
        scalus.cardano.onchain.plutus.v1.TxOutRef(
          TxId(hex"1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"),
          0
        )
      )
    )
    private val ctxData10 = scriptContext10.toData
    private val redeemerWithIndex9 = (preimage, BigInt(9)).toData

    test("Budget with 10 signatories (target at index 9)") {
        // Variant 5: linear search through 10 signatories
        locally {
            given Options = Options(
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
              targetLanguage = Language.PlutusV4,
              generateErrorTraces = false,
              optimizeUplc = true
            )
            given PlutusVM = PlutusVM.makePlutusV4VM()
            val compiled = compile(PreimageValidatorWithListMatch.preimageValidator)
            val program = compiled.toUplc(generateErrorTraces = false).plutusV3
            evalAndReport("5. ListMatch, 10 sigs, search", program $ datum $ redeemer $ ctxData10, summon[PlutusVM])
        }

        // Variant 6: direct index lookup
        locally {
            given Options = Options(
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
              targetLanguage = Language.PlutusV4,
              generateErrorTraces = false,
              optimizeUplc = true
            )
            given PlutusVM = PlutusVM.makePlutusV4VM()
            val compiled = compile(PreimageValidatorWithSigIndex.preimageValidator)
            val program = compiled.toUplc(generateErrorTraces = false).plutusV3
            evalAndReport("6. sigIndex, 10 sigs, index=9", program $ datum $ redeemerWithIndex9 $ ctxData10, summon[PlutusVM])
        }
    }

    test("Budget comparison: three PreimageValidator variants") {
        // 1. Low-level assembler with field macro (ScottEncodingLowering)
        locally {
            given Options = Options(
              targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering,
              generateErrorTraces = false,
              optimizeUplc = true
            )
            given PlutusVM = PlutusVM.makePlutusV3VM()
            val compiled = compile(OptimizedPreimageValidator.preimageValidator)
            val program = compiled.toUplc(generateErrorTraces = false).plutusV3
            evalAndReport("1. Assembler + field macro (ScottEncoding, V3)", program $ datum $ redeemer $ ctxData, summon[PlutusVM])
        }

        // 2. Low-level assembler with dropList (SirToUplcV3Lowering + PlutusV4)
        locally {
            given Options = Options(
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
              targetLanguage = Language.PlutusV4,
              generateErrorTraces = false,
              optimizeUplc = true
            )
            given PlutusVM = PlutusVM.makePlutusV4VM()
            val compiled = compile(OptimizedPreimageValidatorV4.preimageValidator)
            val program = compiled.toUplc(generateErrorTraces = false).plutusV3
            info("V2 UPLC:\n" + program.term.pretty.render(120))
            evalAndReport("2. Assembler + dropList (V3Lowering, V4)", program $ datum $ redeemer $ ctxData, summon[PlutusVM])
        }

        // 3. High-level style (SirToUplcV3Lowering, V3)
        locally {
            given Options = Options(
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
              generateErrorTraces = false,
              optimizeUplc = true
            )
            given PlutusVM = PlutusVM.makePlutusV3VM()
            val compiled = compile(PreimageValidator.preimageValidator)
            val program = compiled.toUplc(generateErrorTraces = false).plutusV2
            evalAndReport("3. High-level (V3Lowering, V3)", program $ datum $ redeemer $ ctxData, summon[PlutusVM])
        }

        // 4. High-level style (SirToUplcV3Lowering, V4)
        locally {
            given Options = Options(
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
              targetLanguage = Language.PlutusV4,
              generateErrorTraces = false,
              optimizeUplc = true
            )
            given PlutusVM = PlutusVM.makePlutusV4VM()
            val compiled = compile(PreimageValidator.preimageValidator)
            val program = compiled.toUplc(generateErrorTraces = false).plutusV2
            evalAndReport("4. High-level (V3Lowering, V4)", program $ datum $ redeemer $ ctxData, summon[PlutusVM])
        }

        // 5. Low-level assembler with dropList + BuiltinList match (V4 Case on List)
        locally {
            given Options = Options(
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
              targetLanguage = Language.PlutusV4,
              generateErrorTraces = false,
              optimizeUplc = true
            )
            given PlutusVM = PlutusVM.makePlutusV4VM()
            val compiled = compile(PreimageValidatorWithListMatch.preimageValidator)
            val program = compiled.toUplc(generateErrorTraces = false).plutusV3
            evalAndReport("5. Assembler + dropList + ListMatch (V4)", program $ datum $ redeemer $ ctxData, summon[PlutusVM])
        }

        // 6. Low-level assembler with signatory index in redeemer (V4, no search)
        locally {
            given Options = Options(
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
              targetLanguage = Language.PlutusV4,
              generateErrorTraces = false,
              optimizeUplc = true
            )
            given PlutusVM = PlutusVM.makePlutusV4VM()
            val compiled = compile(PreimageValidatorWithSigIndex.preimageValidator)
            val program = compiled.toUplc(generateErrorTraces = false).plutusV3
            evalAndReport("6. Assembler + sigIndex in redeemer (V4)", program $ datum $ redeemerWithIndex $ ctxData, summon[PlutusVM])
        }

        // 7. Direct UPLC construction (V4, no compiler plugin)
        locally {
            given PlutusVM = PlutusVM.makePlutusV4VM()
            val program = DirectPreimageValidator.preimageValidator.plutusV3
            info("V7 UPLC:\n" + program.term.pretty.render(120))
            evalAndReport("7. Direct UPLC (V4)", program $ datum $ redeemer $ ctxData, summon[PlutusVM])
        }

        // 7b. Direct UPLC with CaseConstrApply optimization
        locally {
            given PlutusVM = PlutusVM.makePlutusV4VM()
            val optimized = scalus.uplc.transform.CaseConstrApply(DirectPreimageValidator.preimageValidator)
            val program = optimized.plutusV3
            evalAndReport("7b. Direct UPLC + CaseConstrApply (V4)", program $ datum $ redeemer $ ctxData, summon[PlutusVM])
        }
    }
}
