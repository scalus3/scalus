import ScalusProofs.Generated.Targets

/-! Properties of `scalus.cardano.onchain.plutus.prelude.Math`, proved against the compiled
    UPLC rather than against the Scala source.

    Budgets are deliberately tight. Proof cost grows superlinearly in the budget, so each
    budget sits just above its own target's measured worst path: `abs` 26 steps at budget 40,
    `min`/`max` 23 at budget 40, `clamp` 39 at budget 60, `exp2` 70 at budget 150. At these
    small sizes the cost curve is still flat, so that headroom is free; push a budget far past
    its target's worst path and cost explodes instead (see the README's budget rule, where the
    explosion is measured on `gcd`, which is not proved in this file). Lower a budget if a
    proof is slow; never raise it "to be safe". -/

namespace ScalusProofs.Math

open PlutusCore.Integer (Integer)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open ScalusProofs.Prelude
open ScalusProofs.Generated

set_option warn.sorry false

def i1 (x : Integer) : List Term := ints [x]
def i2 (x y : Integer) : List Term := ints [x, y]
def i3 (x y z : Integer) : List Term := ints [x, y, z]

#prep_uplc pAbs   mathAbs   i1 40
#prep_uplc pMin   mathMin   i2 40
#prep_uplc pMax   mathMax   i2 40
#prep_uplc pClamp mathClamp i3 60
#prep_uplc pExp2  mathExp2  i1 150

/-! ### anti-vacuity guards

    Each check below runs a program at exactly the budget its theorems use, on that program's
    longest measured branch path. Their job is to fail loudly if a codegen change pushes a
    program past its budget, which would otherwise turn the conditional theorems below into
    vacuous truths that still report Valid.

    The generated checks in `Generated/Targets.lean` do NOT serve this purpose: they run at a
    fixed budget of 20000, unrelated to these budgets.

    Measured worst-case step counts per path: abs 26 (negative input), min 23, max 23,
    clamp 39 (above-hi and in-range; below-lo is 28), exp2 70 (e >= 0) and 18 (e < 0). -/

example : runInts mathAbs   [(-7)]     40  = some 7    := by native_decide
example : runInts mathMin   [3, 5]     40  = some 3    := by native_decide
example : runInts mathMax   [3, 5]     40  = some 5    := by native_decide
example : runInts mathClamp [9, 1, 5]  60  = some 5    := by native_decide
example : runInts mathClamp [3, 1, 5]  60  = some 3    := by native_decide
example : runInts mathExp2  [(-1)]     150 = some 0    := by native_decide
example : runInts mathExp2  [10]       150 = some 1024 := by native_decide

/-! ### abs -/

theorem abs_total : ∀ (x : Integer), isSuccessful (pAbs.prop x) := by blaster

theorem abs_nonneg : ∀ (x r : Integer),
    (fromFrameToInt $ pAbs.prop x) = some r → r ≥ 0 := by blaster

theorem abs_magnitude : ∀ (x r : Integer),
    (fromFrameToInt $ pAbs.prop x) = some r → r = x ∨ r = -x := by blaster

/-! ### min and max -/

theorem min_lower_bound : ∀ (x y r : Integer),
    (fromFrameToInt $ pMin.prop x y) = some r → r ≤ x ∧ r ≤ y := by blaster

theorem min_is_one_of : ∀ (x y r : Integer),
    (fromFrameToInt $ pMin.prop x y) = some r → r = x ∨ r = y := by blaster

theorem max_upper_bound : ∀ (x y r : Integer),
    (fromFrameToInt $ pMax.prop x y) = some r → r ≥ x ∧ r ≥ y := by blaster

theorem min_max_sum : ∀ (x y a b : Integer),
    (fromFrameToInt $ pMin.prop x y) = some a →
    (fromFrameToInt $ pMax.prop x y) = some b →
    a + b = x + y := by blaster

/-! ### clamp -/

theorem clamp_in_range : ∀ (x lo hi r : Integer), lo ≤ hi →
    (fromFrameToInt $ pClamp.prop x lo hi) = some r → lo ≤ r ∧ r ≤ hi := by blaster

theorem clamp_identity : ∀ (x lo hi r : Integer), lo ≤ x → x ≤ hi →
    (fromFrameToInt $ pClamp.prop x lo hi) = some r → r = x := by blaster

/-! ### exp2 -/

theorem exp2_negative_is_zero : ∀ (e r : Integer), e < 0 →
    (fromFrameToInt $ pExp2.prop e) = some r → r = 0 := by blaster

/- `exp2_nonneg` is omitted. As stated in the brief:

     theorem exp2_nonneg : ∀ (e r : Integer),
         (fromFrameToInt $ pExp2.prop e) = some r → r ≥ 0 := by blaster

   it does not elaborate. Blaster fails with:

     error: Inductive datatype with instance parameters not supported: `BitVec

   Cause: on the `e ≥ 0` path, `Math.exp2` compiles to `shiftByteString` /
   `integerToByteString` / `byteStringToInteger` (see Math.scala), so the CEK trace produces a
   `ByteString`-valued term. Blaster's `translateInductiveType` cannot encode `BitVec` (its
   width is a value index, not a type parameter), and `ByteString` is built on `BitVec`, so any
   proof needing *general* reasoning over that branch fails to translate, independent of budget.

   Verified budget-independent and domain-independent: fails identically at budgets 80, 100,
   150 and 300, and with the domain narrowed to `e ≥ 0` or even `0 ≤ e ≤ 5`. A fully concrete
   `e` (e.g. `pExp2.prop 10`) *does* prove, confirming the failure is in Blaster's generic
   datatype translation, not in the underlying arithmetic. This is a Blaster tooling
   limitation, not a Scalus stdlib bug, and not a falsification of the property. -/

/-! ### negative control -/

def bogus_min_is_upper_bound : Prop := ∀ (x y r : Integer),
    (fromFrameToInt $ pMin.prop x y) = some r → r ≥ x ∧ r ≥ y
#blaster (gen-cex: 0) (solve-result: 1) [bogus_min_is_upper_bound]

end ScalusProofs.Math
