import ScalusProofs.Generated.Targets
import ScalusProofs.Run

/-! The two trivial scripts, as an end-to-end check that the whole pipeline is wired up. -/

namespace ScalusProofs.Sanity

open PlutusCore.Data (Data)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open ScalusProofs.Generated
open ScalusProofs.Run

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

/-! `runFor` keeps a failing program apart from an exhausted budget: `UplcBlaster` relies on it to
    prove that a program fails. -/

#prep_uplc_run rAlwaysFail     alwaysFail dataArg 100
#prep_uplc_run rAlwaysFailTiny alwaysFail dataArg 1

theorem always_fail_fails : ∀ (d : Data), failed (rAlwaysFail d) = true := by blaster

/-- Negative control: one step does not reach the failure, and an exhausted budget must not be
    read as one. -/
def bogus_exhausted_is_failure : Prop := ∀ (d : Data), failed (rAlwaysFailTiny d) = true
#blaster (gen-cex: 0) (solve-result: 1) [bogus_exhausted_is_failure]

end ScalusProofs.Sanity
