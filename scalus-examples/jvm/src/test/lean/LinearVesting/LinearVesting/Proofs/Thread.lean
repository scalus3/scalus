import ScalusProofs.Thread

/-!
The vesting thread: in every thread of withdrawals from an output under the datum `C0`, what has
been withdrawn is at most what has vested (docs/design/verification-details/multi-transaction.md,
"Vesting").

The state is the vesting output: its datum and the lovelace it locks. A withdrawal spends it, and
leaves the rest at the script's address unless it takes everything: a withdrawal of everything
ends the thread. Whether the validator accepts a withdrawal, `accepted`, is a parameter: its run
on the withdrawal's script context.

The two hypotheses are guarantees `VestingValidator.spend` states, each of one script context,
which `VestingVerificationTest` states of the compiled validator:
- #3, `vesting.spend.unvested_stays_locked`: proved of every script context, with the walks as
  unknowns (`ProofKind.BlasterOfUnknowns`);
- #4, `vesting.spend.state_is_carried`: open; of the validator with its walks given, and the lemma
  on `Value` as a premise, it is proved (`VestingWalksProbeTest`).
The theorem is proved of every thread, of any length, from them: it rests on Lean's kernel and on
the two. That each hypothesis is the statement Scalus proved, read as a proposition of Lean, is
step 4 of the design, and is not built yet.
-/

namespace LinearVesting.Proofs.Thread

open ScalusProofs.Thread

variable {Datum Output Context : Type}

/-- The vesting thread: what a withdrawal spends, the rest it leaves where it takes less than
    everything, and whether the validator accepts it. -/
def steps (locked : Output → Int) (amount : Context → Int)
    (ownInput : Context → Output) (rest : Context → Option Output) (accepted : Context → Prop) :
    Steps Output Context where
  ownInput := ownInput
  continuing c := if amount c = locked (ownInput c) then none else rest c
  accepted := accepted

/-- In every thread from an output under the datum `C0`, each withdrawal leaves locked at least
    what has not vested of `C0` at its time: `initialAmount C0 − locked after it ≤ vested C0 t`.
    From a deposit of `initialAmount C0`, with the lovelace carried, what has been withdrawn is at
    most what has vested. -/
theorem withdrawn_at_most_vested
    (datum : Output → Datum) (locked : Output → Int) (amount validFrom : Context → Int)
    (ownInput : Context → Output) (rest : Context → Option Output) (accepted : Context → Prop)
    (initialAmount : Datum → Int) (vested : Datum → Int → Int)
    -- Guarantee #3, `vesting.spend.unvested_stays_locked`: what stays locked is at least what has
    -- not vested at the transaction's earliest time, by the transaction's own datum.
    (unvestedStaysLocked : ∀ c, accepted c →
      locked (ownInput c) - amount c ≥
        initialAmount (datum (ownInput c)) - vested (datum (ownInput c)) (validFrom c))
    -- Guarantee #4, `vesting.spend.state_is_carried`: a withdrawal of less than everything leaves
    -- the rest under the same datum, with as much less lovelace.
    (stateIsCarried : ∀ c, accepted c →
      amount c = locked (ownInput c) ∨
        ∃ o', rest c = some o' ∧ datum o' = datum (ownInput c) ∧
          locked o' = locked (ownInput c) - amount c)
    (C0 : Datum) {o : Output} {cs : List Context}
    (first : datum o = C0)
    (thread : Thread (steps locked amount ownInput rest accepted) o cs) :
    ∀ c ∈ cs, initialAmount C0 - (locked (ownInput c) - amount c) ≤ vested C0 (validFrom c) := by
  let s := steps locked amount ownInput rest accepted
  -- The invariant: the datum is the first one.
  have step : ∀ c o', datum (s.ownInput c) = C0 → s.accepted c → s.continuing c = some o' →
      datum o' = C0 := by
    intro c o' hI ha hcont
    show datum o' = C0
    have hcont' : (if amount c = locked (ownInput c) then none else rest c) = some o' := hcont
    by_cases hall : amount c = locked (ownInput c)
    · rw [if_pos hall] at hcont'; cases hcont'
    · rw [if_neg hall] at hcont'
      rcases stateIsCarried c ha with h | ⟨o'', hrest, hdatum, _⟩
      · exact absurd h hall
      · rw [hcont'] at hrest
        cases hrest
        rw [hdatum]; exact hI
  refine property s (fun o => datum o = C0) _ step ?_ first thread
  intro c hI ha
  have h3 := unvestedStaysLocked c ha
  have hI' : datum (ownInput c) = C0 := hI
  rw [hI'] at h3
  omega

/-- The same at any later time, as the schedule does not decrease
    (`vesting.schedule.does_not_decrease`). -/
theorem withdrawn_at_most_vested_later
    (datum : Output → Datum) (locked : Output → Int) (amount validFrom : Context → Int)
    (ownInput : Context → Output) (rest : Context → Option Output) (accepted : Context → Prop)
    (initialAmount : Datum → Int) (vested : Datum → Int → Int)
    (unvestedStaysLocked : ∀ c, accepted c →
      locked (ownInput c) - amount c ≥
        initialAmount (datum (ownInput c)) - vested (datum (ownInput c)) (validFrom c))
    (stateIsCarried : ∀ c, accepted c →
      amount c = locked (ownInput c) ∨
        ∃ o', rest c = some o' ∧ datum o' = datum (ownInput c) ∧
          locked o' = locked (ownInput c) - amount c)
    (C0 : Datum)
    (doesNotDecrease : ∀ t t', t ≤ t' → vested C0 t ≤ vested C0 t')
    {o : Output} {cs : List Context}
    (first : datum o = C0)
    (thread : Thread (steps locked amount ownInput rest accepted) o cs) :
    ∀ c ∈ cs, ∀ t, validFrom c ≤ t →
      initialAmount C0 - (locked (ownInput c) - amount c) ≤ vested C0 t := by
  intro c hc t ht
  have h := withdrawn_at_most_vested datum locked amount validFrom ownInput rest accepted
    initialAmount vested unvestedStaysLocked stateIsCarried C0 first thread c hc
  have := doesNotDecrease _ _ ht
  omega

end LinearVesting.Proofs.Thread
