import ScalusProofs.Generated.Targets

/-! The two trivial scripts, as an end-to-end check that the whole pipeline is wired up. -/

namespace ScalusProofs.Sanity

open PlutusCore.Data (Data)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open ScalusProofs.Generated

set_option warn.sorry false

def dataArg (d : Data) : List Term := [Term.Const $ Const.Data d]

#prep_uplc pAlwaysOk   alwaysOk   dataArg 100
#prep_uplc pAlwaysFail alwaysFail dataArg 100

theorem always_ok_succeeds : ∀ (d : Data), isSuccessful (pAlwaysOk.prop d) := by blaster

theorem always_fail_never_succeeds :
    ∀ (d : Data), ¬ isSuccessful (pAlwaysFail.prop d) := by blaster

/-- Negative control: this is false, and Blaster must say so rather than prove it. -/
def bogus_always_fail_succeeds : Prop := ∀ (d : Data), isSuccessful (pAlwaysFail.prop d)
#blaster (gen-cex: 0) (solve-result: 1) [bogus_always_fail_succeeds]

end ScalusProofs.Sanity
