import ScalusProofs.Thread

/-!
The vesting thread: in every thread of withdrawals from a deposit of `initialAmount C0` under the
datum `C0`, what has been withdrawn is at most what has vested
(docs/design/verification-details/multi-transaction.md, "Vesting").

The state is the vesting output: its datum and the lovelace it locks. A withdrawal spends it, and
leaves the rest at the script's address unless it takes everything: a withdrawal of everything
ends the thread. Whether the validator accepts a withdrawal, `accepted`, is a parameter: its run
on the withdrawal's script context.

The two hypotheses are guarantees `VestingValidator.spend` states, each of one script context,
which `VestingVerificationTest` states of the compiled validator:
- #3, `vesting.spend.unvested_stays_locked`: proved of every script context, with the walks as
  unknowns (`ProofKind.BlasterOfUnknowns`);
- #4, `vesting.spend.state_is_carried`: proved of every script context, with the walks as
  unknowns and the contracts of `Value.eq`, `Value.minus` and `Value.lovelace`, each proved
  (`ProofKind.BlasterOfUnknowns`).
Each hypothesis is its guarantee read with `==>:`: where the validator accepts, the clause holds.
Scalus states them in the rejecting form, `==>?:`, which says so where the clause returns; that it
returns wherever the validator accepts is tested on samples (`VestingVerificationTest`, "spend's
conditions return true on the samples the validator accepts"), not proved.

The theorems are proved of every thread, of any length, from the hypotheses: they rest on Lean's
kernel and on them. That each hypothesis is the statement Scalus proved, read as a proposition of
Lean, is step 4 of the design, and is not built yet. #3 as Scalus proved it is
`LinearVesting.Generated.vesting_spend_unvested_stays_locked.statement`, of the validator's
programs and runs of any number of steps, and its axiom `proved`.
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

section

variable (datum : Output → Datum) (locked : Output → Int) (amount validFrom : Context → Int)
  (ownInput : Context → Output) (rest : Context → Option Output) (accepted : Context → Prop)
  (initialAmount : Datum → Int) (vested : Datum → Int → Int)

/-- Guarantee #3, `vesting.spend.unvested_stays_locked`: what stays locked is at least what has
    not vested at the transaction's earliest time, by the transaction's own datum. -/
def UnvestedStaysLocked : Prop :=
  ∀ c, accepted c →
    locked (ownInput c) - amount c ≥
      initialAmount (datum (ownInput c)) - vested (datum (ownInput c)) (validFrom c)

/-- Guarantee #4, `vesting.spend.state_is_carried`: a withdrawal of less than everything leaves the
    rest under the same datum, with as much less lovelace. -/
def StateIsCarried : Prop :=
  ∀ c, accepted c →
    amount c = locked (ownInput c) ∨
      ∃ o', rest c = some o' ∧ datum o' = datum (ownInput c) ∧
        locked o' = locked (ownInput c) - amount c

/-- What has been withdrawn before a thread from `o`, where `o` locks `initialAmount C0` less it,
    and the amounts of the thread's withdrawals up to each one, are at most what has vested of
    `C0` at that one's time. By induction over the thread. -/
theorem withdrawn_since
    (unvestedStaysLocked : UnvestedStaysLocked datum locked amount validFrom ownInput accepted
      initialAmount vested)
    (stateIsCarried : StateIsCarried datum locked amount ownInput rest accepted)
    (C0 : Datum) {o : Output} {cs : List Context}
    (thread : Thread (steps locked amount ownInput rest accepted) o cs) :
    ∀ (before : Int), datum o = C0 → locked o = initialAmount C0 - before →
      ∀ earlier c later, cs = earlier ++ c :: later →
        before + (earlier.map amount).sum + amount c ≤ vested C0 (validFrom c) := by
  -- The step that ends the list: its input is `o`, under `C0`, with `before` withdrawn.
  have first : ∀ (before : Int) (o : Output) (c : Context), ownInput c = o → accepted c →
      datum o = C0 → locked o = initialAmount C0 - before →
      before + amount c ≤ vested C0 (validFrom c) := by
    intro before o c hin ha hdatum hlocked
    have h3 := unvestedStaysLocked c ha
    rw [hin, hdatum, hlocked] at h3
    omega
  induction thread with
  | done o =>
    intro before _ _ earlier c later h
    cases earlier <;> simp at h
  | last o c hin ha _ =>
    intro before hdatum hlocked earlier c' later h
    cases earlier with
    | nil =>
      simp only [List.nil_append, List.cons.injEq] at h
      obtain ⟨rfl, _⟩ := h
      simp only [List.map_nil, List.sum_nil, Int.add_zero]
      exact first before o c hin ha hdatum hlocked
    | cons x earlier =>
      simp only [List.cons_append, List.cons.injEq] at h
      obtain ⟨_, h⟩ := h
      cases earlier <;> simp at h
  | next o c cs o' hin ha hcont _ ih =>
    intro before hdatum hlocked earlier c' later h
    cases earlier with
    | nil =>
      simp only [List.nil_append, List.cons.injEq] at h
      obtain ⟨rfl, _⟩ := h
      simp only [List.map_nil, List.sum_nil, Int.add_zero]
      exact first before o c hin ha hdatum hlocked
    | cons x earlier =>
      simp only [List.cons_append, List.cons.injEq] at h
      obtain ⟨rfl, hcs⟩ := h
      -- The withdrawal `c` leaves the rest `o'`: under `C0`, with its amount less.
      have hcont' : (if amount c = locked (ownInput c) then none else rest c) = some o' := hcont
      have hall : ¬ amount c = locked (ownInput c) := by
        intro hall; rw [if_pos hall] at hcont'; cases hcont'
      rw [if_neg hall] at hcont'
      obtain ⟨o'', hrest, hdatum', hlocked'⟩ :=
        (stateIsCarried c ha).resolve_left hall
      rw [hcont'] at hrest
      cases hrest
      have hin' : ownInput c = o := hin
      rw [hin'] at hdatum' hlocked'
      have := ih (before + amount c) (by rw [hdatum', hdatum])
        (by rw [hlocked', hlocked]; omega) earlier c' later hcs
      simp only [List.map_cons, List.sum_cons]
      omega

/-- In every thread from a deposit of `initialAmount C0` under `C0`, what has been withdrawn up to
    each withdrawal, the amounts before it and its own, is at most what has vested of `C0` at its
    time. -/
theorem withdrawn_at_most_vested
    (unvestedStaysLocked : UnvestedStaysLocked datum locked amount validFrom ownInput accepted
      initialAmount vested)
    (stateIsCarried : StateIsCarried datum locked amount ownInput rest accepted)
    (C0 : Datum) {o : Output} {cs : List Context}
    (first : datum o = C0) (deposit : locked o = initialAmount C0)
    (thread : Thread (steps locked amount ownInput rest accepted) o cs) :
    ∀ earlier c later, cs = earlier ++ c :: later →
      (earlier.map amount).sum + amount c ≤ vested C0 (validFrom c) := by
  intro earlier c later h
  have := withdrawn_since datum locked amount validFrom ownInput rest accepted initialAmount vested
    unvestedStaysLocked stateIsCarried C0 thread 0 first (by rw [deposit]; omega) earlier c later h
  omega

/-- The same at any later time: the schedule of a datum whose amount is not negative does not
    decrease (`vesting.schedule.does_not_decrease`, proved of every datum and two times). -/
theorem withdrawn_at_most_vested_later
    (unvestedStaysLocked : UnvestedStaysLocked datum locked amount validFrom ownInput accepted
      initialAmount vested)
    (stateIsCarried : StateIsCarried datum locked amount ownInput rest accepted)
    (doesNotDecrease : ∀ d t t', 0 ≤ initialAmount d → t ≤ t' → vested d t ≤ vested d t')
    (C0 : Datum) (notNegative : 0 ≤ initialAmount C0) {o : Output} {cs : List Context}
    (first : datum o = C0) (deposit : locked o = initialAmount C0)
    (thread : Thread (steps locked amount ownInput rest accepted) o cs) :
    ∀ earlier c later, cs = earlier ++ c :: later → ∀ t, validFrom c ≤ t →
      (earlier.map amount).sum + amount c ≤ vested C0 t := by
  intro earlier c later h t ht
  have := withdrawn_at_most_vested datum locked amount validFrom ownInput rest accepted
    initialAmount vested unvestedStaysLocked stateIsCarried C0 first deposit thread earlier c later h
  have := doesNotDecrease C0 _ _ notNegative ht
  omega

end

end LinearVesting.Proofs.Thread
