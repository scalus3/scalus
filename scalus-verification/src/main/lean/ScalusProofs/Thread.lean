/-! A thread of a contract's outputs, and the trace lemma
    (docs/design/verification-details/multi-transaction.md, "The method").

    A statement Scalus proves is of one run of a validator, on one script context. A property of
    a contract over a sequence of its transactions is of a thread: a first output, and
    transactions each of which spends the output the one before it left. The ledger links them
    (fact 1 of the design): the next transaction's context has, as its own input, the output the
    previous one left. That link is the hypothesis of a thread, and the lemma here, proved once
    for every contract, turns statements of one transaction each into a property of every thread,
    of any length.

    `Output`, `Context`, the accessors and `accepted` are parameters: an instance takes them from
    the statements of its steps. -/

namespace ScalusProofs.Thread

/-- What the trace lemma needs of a contract's transactions, each as one script context:
    the output it spends (the ledger's link of fact 1), the output it leaves for the next one of
    the thread, if any, and whether the script accepts it. -/
structure Steps (Output Context : Type) where
  ownInput : Context → Output
  continuing : Context → Option Output
  accepted : Context → Prop

variable {Output Context : Type} (s : Steps Output Context)

/-- A thread from the output `o`: transactions each of which spends the output the one before it
    left, the first spending `o`. The last may leave none. -/
inductive Thread : Output → List Context → Prop
  | done (o : Output) : Thread o []
  | last (o : Output) (c : Context) :
      s.ownInput c = o → s.accepted c → s.continuing c = none → Thread o [c]
  | next (o : Output) (c : Context) (cs : List Context) (o' : Output) :
      s.ownInput c = o → s.accepted c → s.continuing c = some o' → Thread o' cs →
      Thread o (c :: cs)

/-- Every transaction of a thread is accepted. -/
theorem accepted_of_thread {o : Output} {cs : List Context} (h : Thread s o cs) :
    ∀ c ∈ cs, s.accepted c := by
  induction h with
  | done => intro c hc; cases hc
  | last o c _ ha _ =>
    intro c' hc'
    cases hc' with
    | head => exact ha
    | tail _ h => cases h
  | next o c cs o' _ ha _ _ ih =>
    intro c' hc'
    cases hc' with
    | head => exact ha
    | tail _ h => exact ih c' h

/-- The trace lemma: where the first output satisfies `I` and every accepted step keeps it, the
    input of every transaction of the thread satisfies `I`. -/
theorem invariant (I : Output → Prop)
    (step : ∀ c o', I (s.ownInput c) → s.accepted c → s.continuing c = some o' → I o')
    {o : Output} {cs : List Context} (hI : I o) (h : Thread s o cs) :
    ∀ c ∈ cs, I (s.ownInput c) := by
  induction h with
  | done => intro c hc; cases hc
  | last o c hin _ _ =>
    intro c' hc'
    cases hc' with
    | head => rw [hin]; exact hI
    | tail _ h => cases h
  | next o c cs o' hin ha hcont _ ih =>
    intro c' hc'
    have hc : I (s.ownInput c) := by rw [hin]; exact hI
    cases hc' with
    | head => exact hc
    | tail _ h => exact ih (step c o' hc ha hcont) c' h

/-- A property of each step, which holds where the step's input satisfies `I`, holds of every
    transaction of the thread: a guarantee of the validator, of one context, over a thread. -/
theorem property (I : Output → Prop) (P : Context → Prop)
    (step : ∀ c o', I (s.ownInput c) → s.accepted c → s.continuing c = some o' → I o')
    (holds : ∀ c, I (s.ownInput c) → s.accepted c → P c)
    {o : Output} {cs : List Context} (hI : I o) (h : Thread s o cs) :
    ∀ c ∈ cs, P c := fun c hc =>
  holds c (invariant s I step hI h c hc) (accepted_of_thread s h c hc)

end ScalusProofs.Thread

