package scalus.examples.usdcx

import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData

/** The USDCx minting policy, recreated in Scalus from its compiled script.
  *
  * The original is a Plinth script, `mkUSDCXMintingPolicy`, known here only as UPLC: the
  * `minting_policy.flat` of IOG's CardanoLedgerApiBlaster tests, whose `Properties.lean` states
  * what it does. It follows the programmable-token pattern of CIP-113: the policy itself checks
  * almost nothing and delegates to a "minting logic" script, run as a withdrawal of zero.
  *
  * The policy is parameterized by the currency symbol of the protocol parameters NFT. It accepts a
  * transaction that
  *   - has a reference input holding that NFT, with an inline datum whose first field is the hash
  *     of the minting logic script,
  *   - withdraws from the credential of that script, so that the ledger runs it,
  *   - runs the policy as a minting script.
  */
@Compile
object UsdcxMintingPolicy {

    /** The policy.
      *
      * @param pparamsCs
      *   the currency symbol of the protocol parameters NFT, as `Data`, as the original takes it
      * @param ctxData
      *   the script context
      */
    def validate(pparamsCs: Data)(ctxData: Data): Unit = {
        val ctx = ctxData.to[ScriptContext]
        val txInfo = ctx.txInfo
        // The minting logic is looked up once, first, as IOG's statements of the policy read it.
        // The original looks it up when it compares a withdrawal, again for each: so where there
        // is no withdrawal it never looks, and traces where this fails without a trace. Looked
        // up first, the statements are checked in minutes; looked up only where there is a
        // withdrawal, or in the loop as the original does, they ran for hours.
        val mintingLogic = mintingLogicCredential(pparamsCs, txInfo.referenceInputs)
        // Both checks are evaluated, as in the original, so a failing transaction traces both.
        val inWithdrawals = traceIfFalse(NoMintingLogicInWithdrawals)(
          txInfo.withdrawals.exists { case (credential, _) => credential.toData == mintingLogic }
        )
        val isMinting = traceIfFalse(NotMinting)(ctx.scriptInfo match
            case ScriptInfo.MintingScript(_) => true
            case _                           => false)
        if inWithdrawals && isMinting then () else fail()
    }

    /** The credential of the minting logic script, as `Data`: the first field of the inline datum
      * of the first reference input that holds a token of `pparamsCs`. Fails when there is no such
      * input, or its datum is not inline.
      */
    def mintingLogicCredential(pparamsCs: Data, referenceInputs: List[TxInInfo]): Data =
        referenceInputs match
            case List.Cons(input, rest) =>
                if hasCurrencySymbol(pparamsCs, input.resolved.value) then
                    input.resolved.datum match
                        case OutputDatum.OutputDatum(datum) =>
                            // `ScriptCredential` of the hash, built from the field as it is.
                            constrData(1, mkCons(datum.toConstr.snd.head, mkNilData()))
                        case _ => fail()
                else mintingLogicCredential(pparamsCs, rest)
            case List.Nil => fail()

    /** Whether `value` has a token of the policy `cs`. */
    def hasCurrencySymbol(cs: Data, value: Value): Boolean =
        value.toSortedMap.exists { case (policyId, _) => policyId.toData == cs }

    inline def traceIfFalse(inline message: String)(b: Boolean): Boolean =
        if b then true else trace(message)(false)

    inline val NoMintingLogicInWithdrawals =
        "mkUSDCXMintingPolicy: minting logic credential not found in withdrawals"
    inline val NotMinting = "mkUSDCXMintingPolicy: script purpose should be Minting"
}

object UsdcxMintingPolicyContract {

    /** The options closest to the original's: protocol version 10, as it uses neither `case` on
      * builtins nor `dropList`, its traces kept (the default), and no error traces of its own.
      */
    private given Options = Options.plomin.copy(generateErrorTraces = false, optimizeUplc = true)

    lazy val compiled: PlutusV3[Data => Data => Unit] =
        PlutusV3.compile(UsdcxMintingPolicy.validate)
}
