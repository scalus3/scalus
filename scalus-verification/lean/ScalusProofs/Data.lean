import ScalusProofs.Generated.Targets

/-! Properties of prelude `Option` and `List`. At PV11 these lower to real UPLC `constr` and
    `case`, so this file is also the regression test for the upstream Frame.CaseScrutinee fix
    (input-output-hk/PlutusCoreBlaster#40): without it, none of these terminate. -/

namespace ScalusProofs.Data

open PlutusCore.Integer (Integer)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open ScalusProofs.Prelude
open ScalusProofs.Generated

set_option warn.sorry false

def i1 (x : Integer) : List Term := ints [x]
def i2 (x y : Integer) : List Term := ints [x, y]

#prep_uplc pOpt optDoubleOrDefault i1 120
#prep_uplc pSum listSum2           i2 260

/-! ### anti-vacuity guards

    As in `Math.lean`: each check runs a program at exactly the budget its theorems use, on
    that program's longest measured branch path, so a codegen change that outgrows the budget
    fails loudly instead of making the theorems below vacuously true.

    Measured worst-case step counts per path: optDoubleOrDefault 86 (x > 0) and 38 (x <= 0),
    listSum2 209. -/

example : runInts optDoubleOrDefault [5]     120 = some 10   := by native_decide
example : runInts optDoubleOrDefault [(-5)]  120 = some (-1) := by native_decide
example : runInts listSum2           [3, 4]  260 = some 7    := by native_decide

/-! ### Option match

    Source: `val o = if x > 0 then Some(x) else None; o match { case Some(v) => v * 2; case None => -1 }` -/

theorem opt_positive : ∀ (x r : Integer), x > 0 →
    (fromFrameToInt $ pOpt.prop x) = some r → r = 2 * x := by blaster

theorem opt_nonpositive : ∀ (x r : Integer), x ≤ 0 →
    (fromFrameToInt $ pOpt.prop x) = some r → r = -1 := by blaster

theorem opt_total : ∀ (x : Integer), isSuccessful (pOpt.prop x) := by blaster

/-! ### List fold

    Source: `List.Cons(a, List.Cons(b, List.Nil)).foldLeft(0)(_ + _)` -/

theorem list_fold_is_sum : ∀ (a b r : Integer),
    (fromFrameToInt $ pSum.prop a b) = some r → r = a + b := by blaster

theorem list_fold_total : ∀ (a b : Integer), isSuccessful (pSum.prop a b) := by blaster

/-! ### negative control -/

def bogus_fold_is_product : Prop := ∀ (a b r : Integer),
    (fromFrameToInt $ pSum.prop a b) = some r → r = a * b
#blaster (gen-cex: 0) (solve-result: 1) [bogus_fold_is_product]

end ScalusProofs.Data
