import Amm.Generated.amm_deposit_dilutes_no_token

/-!
A proof written by hand that a deposit into a pool with liquidity dilutes no LP token: every LP
token is backed by at least as much of each reserve after the deposit as before.

The statement is `AmmMath.depositDatum`'s `ensuring` clause, with its condition as a premise:

  ∀ r0 r1 lpSupply x0 x1,
    r0 > 0 ∧ r1 > 0 ∧ lpSupply > 0 ∧ x0 ≥ 0 ∧ x1 ≥ 0 →
    depositDatum returns next →
    next.r0 * lpSupply ≥ r0 * next.lpSupply ∧ next.r1 * lpSupply ≥ r1 * next.lpSupply

In the module, the variables are `x0 x1 x2` (the pool's `r0 r1 lpSupply`) and `x3 x4` (the
amounts deposited). Each test is a program, and `preparedN` is Blaster's optimization of its run
on Lean's machine: a term of integer arithmetic and `Blaster.dite'`. The clause as written in the
code is not provable at any budget: its test computes the deposit before it reads the condition,
and the first deposit, which the condition excludes, takes a square root whose loop outruns any
fixed number of steps for large enough amounts.

What the proof rests on: Lean's kernel, over the optimized runs. That `preparedN` is the run of
the program is Blaster's optimizer's to say: `rfl` does not show it (verification-overview §6.4).
Blaster's solver is not used, and the proof cites no statement that Blaster proved.
-/

namespace Amm.Proofs.Deposit

open ScalusProofs.Run
open Amm.Generated.amm_deposit_dilutes_no_token

/-! What Blaster's optimized terms are made of, as ordinary `if` and `decide`. -/

theorem dite'_pos {α : Sort u} {p : Prop} (h : p) (t : p → α) (e : ¬p → α) :
    Blaster.dite' p t e = t h := by
  unfold Blaster.dite'
  split
  · rfl
  · next hf => exact absurd h ((Blaster.decide'_false p).mp hf)

theorem dite'_neg {α : Sort u} {p : Prop} (h : ¬p) (t : p → α) (e : ¬p → α) :
    Blaster.dite' p t e = e h := by
  unfold Blaster.dite'
  split
  · next ht => exact absurd ((Blaster.decide'_true p).mp ht) h
  · rfl

theorem dite'_eq_dite {α : Sort u} (p : Prop) [Decidable p] (t : p → α) (e : ¬p → α) :
    Blaster.dite' p t e = dite p t e := by
  by_cases h : p
  · rw [dite'_pos h, dif_pos h]
  · rw [dite'_neg h, dif_neg h]

theorem decide'_eq_decide (p : Prop) [Decidable p] : Blaster.decide' p = decide p := by
  by_cases h : p
  · rw [(Blaster.decide'_true p).mpr h, decide_eq_true h]
  · rw [(Blaster.decide'_false p).mpr h, decide_eq_false h]

theorem ofNat_zero : Int.ofNat 0 = 0 := rfl
theorem mul_eq (a b : Int) : a.mul b = a * b := rfl
theorem add_eq (a b : Int) : a.add b = a + b := rfl

/-- The first test, the premise: where it does not return `false`, the pool has positive
    reserves and supply, and the amounts deposited are not negative. -/
theorem premise {x0 x1 x2 x3 x4 : Int}
    (h : fromFrameToBool (Amm_Generated_amm_deposit_dilutes_no_token_prepared0 x0 x1 x2 x3 x4)
      ≠ some false) :
    0 < x0 ∧ 0 < x1 ∧ 0 < x2 ∧ 0 ≤ x3 ∧ 0 ≤ x4 := by
  delta Amm_Generated_amm_deposit_dilutes_no_token_prepared0 at h
  simp only [dite'_eq_dite, decide'_eq_decide, dite_eq_ite, ofNat_zero] at h
  repeat' split at h
  all_goals simp_all [fromFrameToBool]
  all_goals omega

/-- A deposit into a pool with liquidity dilutes no LP token. -/
theorem deposit_dilutes_no_token : statement := by
  -- The variables are PlutusCore's `Integer`, which is `Int`: `omega` reads `Int`.
  intro (x0 : Int) (x1 : Int) (x2 : Int) (x3 : Int) (x4 : Int) hpre _
  obtain ⟨h0, h1, h2, h3, h4⟩ := premise hpre.1
  -- The pool has a supply: the deposit mints in proportion, and takes no square root. The
  -- branch of the first deposit is not unfolded, only passed over.
  have e : Amm_Generated_amm_deposit_dilutes_no_token_prepared2 x0 x1 x2 x3 x4 = _ :=
    dite'_neg (p := Int.ofNat 0 = x2) (by simp only [ofNat_zero]; omega) _ _
  rw [e]
  simp only [dite'_eq_dite, decide'_eq_decide, dite_eq_ite, ofNat_zero, mul_eq, add_eq]
  -- The amounts are not negative, so Plutus's division, which truncates, is Lean's `/`.
  rw [Int.tdiv_eq_ediv_of_nonneg (Int.mul_nonneg (Int.le_of_lt h2) h3),
    Int.tdiv_eq_ediv_of_nonneg (Int.mul_nonneg (Int.le_of_lt h2) h4)]
  -- What the deposit of each token would mint, `q0` and `q1`, is at most its share of the supply.
  have k0 : x2 * x3 / x0 * x0 ≤ x2 * x3 := Int.ediv_mul_le _ (by omega)
  have k1 : x2 * x4 / x1 * x1 ≤ x2 * x4 := Int.ediv_mul_le _ (by omega)
  generalize x2 * x3 / x0 = q0 at *
  generalize x2 * x4 / x1 = q1 at *
  rw [Int.mul_comm q0 x0] at k0
  rw [Int.mul_comm q1 x1] at k1
  have a0 : x2 * (x0 + x3) = x0 * x2 + x2 * x3 := by rw [Int.mul_add, Int.mul_comm x2 x0]
  have a1 : x2 * (x1 + x4) = x1 * x2 + x2 * x4 := by rw [Int.mul_add, Int.mul_comm x2 x1]
  -- Neither reserve is zero, so nothing is divided by zero.
  rw [if_neg (by omega), if_neg (by omega)]
  -- The deposit mints the smaller of the two, `min q0 q1`.
  by_cases hm : q1 < q0
  · have m0 : x0 * q1 ≤ x0 * q0 :=
      Int.mul_le_mul_of_nonneg_left (Int.le_of_lt hm) (Int.le_of_lt h0)
    have b0 : x0 * (x2 + q1) = x0 * x2 + x0 * q1 := Int.mul_add _ _ _
    have b1 : x1 * (x2 + q1) = x1 * x2 + x1 * q1 := Int.mul_add _ _ _
    rw [if_pos hm, if_neg (by omega)]
    simp only [fromFrameToBool, decide_eq_true_eq, Option.some.injEq]
    omega
  · have m1 : x1 * q0 ≤ x1 * q1 :=
      Int.mul_le_mul_of_nonneg_left (by omega) (Int.le_of_lt h1)
    have b0 : x0 * (x2 + q0) = x0 * x2 + x0 * q0 := Int.mul_add _ _ _
    have b1 : x1 * (x2 + q0) = x1 * x2 + x1 * q0 := Int.mul_add _ _ _
    rw [if_neg hm, if_neg (by omega)]
    simp only [fromFrameToBool, decide_eq_true_eq, Option.some.injEq]
    omega

end Amm.Proofs.Deposit
