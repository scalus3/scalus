import PlutusCore.UPLC
import CardanoLedgerApi.V3
import Blaster

/-!
The script `UsdcxMintingPolicy` compiles to, applied as the ledger applies a minting policy: to
the currency symbol of the protocol parameters NFT, then to the script context. It is the
harness of IOG's CardanoLedgerApiBlaster `Tests/Scripts/MintingPolicy/MintingPolicy.lean`
(Apache-2.0), of this script instead of the Plinth one. `UsdcxMintingPolicyTest` checks that
the script read here is the one Scalus compiles.
-/

namespace UsdcxMintingPolicy

open CardanoLedgerApi.IsData.Class (toTerm)
open CardanoLedgerApi.V3 (ScriptContext mintingInputs)
open PlutusCore.ByteString (ByteString)
open PlutusCore.UPLC.Term (Term)

-- The path is relative to this workspace.
#import_uplc mintingPolicy PlutusV3 single_cbor_hex "../../resources/scalus/examples/usdcx/minting_policy_scalus.hex"

def mintingPolicyInputs (paramsCs : ByteString) (ctx : ScriptContext) : List Term :=
  toTerm paramsCs :: mintingInputs ctx

#prep_uplc appliedMintingPolicy mintingPolicy mintingPolicyInputs 1200

end UsdcxMintingPolicy
