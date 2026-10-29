import CardanoLedgerApi.IsData.Class
import CardanoLedgerApi.V3
import UsdcxMintingPolicy.MintingPolicy
import Blaster

/-!
The statements IOG's CardanoLedgerApiBlaster makes of the Plinth USDCx minting policy
(`Tests/Scripts/MintingPolicy/Properties.lean`, Apache-2.0), unchanged, of the script
`UsdcxMintingPolicy` compiles to.

Each run is cut at 1200 steps of the CEK machine, as in the original, and a run cut short ends
in `State.Error`. So the second statement says nothing of a run longer than that; the first
does not rest on it, and the counterexample of the third is a run that succeeds within it.
-/

namespace UsdcxMintingPolicy
open CardanoLedgerApi.IsData.Class
open CardanoLedgerApi.V3
open PlutusCore.UPLC.Utils
open PlutusCore.ByteString (ByteString)
open PlutusCore.Data (Data)

set_option warn.sorry false

def findMLogiScripHashInReferenceInputs (pparamsCs : ByteString) (inputs : List TxInInfo) : Option Credential :=
  match inputs with
  | [] => none
  | x :: xs =>
     if hasCurrencySymbol pparamsCs x.txInInfoResolved.txOutValue
     then
       match (txOutInlineDatum x.txInInfoResolved) with
       | some (Data.Constr _ (Data.B bsh :: _)) => some (.ScriptCredential bsh)
       | _ => none
     else findMLogiScripHashInReferenceInputs pparamsCs xs

def mintingLogicInWithdrawalMap (pparamsCs : ByteString) (ctx : ScriptContext) : Bool :=
  match findMLogiScripHashInReferenceInputs pparamsCs ctx.scriptContextTxInfo.txInfoReferenceInputs with
  | some cred => credentialInWithdrawals cred ctx.scriptContextTxInfo.txInfoWdrl
  | none => false

/-- Minting Policy successful → scriptInfo = minting logic is in withdrawal map -/
theorem minting_policy_success_imp_mintScript_info_withdrawal :
  ∀ (pparamsCs : ByteString) (ctx : ScriptContext),
      validMintingContext ctx →
      isSuccessful (appliedMintingPolicy.prop pparamsCs ctx) →
      mintingLogicInWithdrawalMap pparamsCs ctx := by blaster (random-seed: 1)

/-- Minting logic not present in withdrawal map → Minting Policy must fail -/
theorem logic_not_in_withdrawal_imp_minting_policy_fail :
  ∀ (pparamsCs : ByteString) (ctx : ScriptContext),
     validMintingContext ctx →
     ¬ mintingLogicInWithdrawalMap pparamsCs ctx →
     isUnsuccessful (appliedMintingPolicy.prop pparamsCs ctx) := by blaster (random-seed: 1)

/-- Counterexample expected if minting logic not in withdrawal map when minting policy is successful -/
def minting_policy_success_imp_logic_not_in_withdrawal : Prop :=
  ∀ (pparamsCs : ByteString) (ctx : ScriptContext),
     validMintingContext ctx →
     isSuccessful (appliedMintingPolicy.prop pparamsCs ctx) →
     ¬ mintingLogicInWithdrawalMap pparamsCs ctx

#blaster (gen-cex: 0) (solve-result: 1) [minting_policy_success_imp_logic_not_in_withdrawal]



end UsdcxMintingPolicy
