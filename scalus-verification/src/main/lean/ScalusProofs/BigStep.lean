import PlutusCore.UPLC
import PlutusCore.UPLC.CekMachine.Lemmas
import ScalusProofs.Runs

/-! # What a term computes, and the machine's runs

    A big-step semantics of PlutusCore's CEK machine: `Big sv g v` says that `g`, a term in an
    environment, a value applied to another, or one of the other parts of a run below, computes
    the value `v`. It is the machine's own: `Big.run` proves that the machine, from where `g`
    starts over any stack, comes to `v` on that stack. So a proof of what a program returns is a
    proof of how its parts compute, one part at a time, and a loop is an induction over its list,
    with no state of the machine written by hand (as `ScalusProofs.Mappings.IsSignedBy` writes
    them).

    The machine's step decides each rule: the environment's value at an index, a builtin's
    result, a `case`'s branch. A derivation is made by applying the rules to the program's terms,
    where the values are any (`big_step` applies those that the term and the values decide). -/

namespace ScalusProofs.BigStep

open PlutusCore.Default (BuiltinSemanticsVariant)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.CekMachine
open PlutusCore.UPLC.CekValue
open PlutusCore.UPLC.Builtins
open PlutusCore.UPLC.BuiltinFunctions.Evaluate (evaluateBuiltinFunction)
open ScalusProofs.Run
open ScalusProofs.Runs

/-- A part of a run: a term in an environment; a value applied to a value, or to several in turn;
    a value forced; the `case` of a value, with the branches and their environment; and the
    arguments of a constructor from one on, with the values of those before it, last first. -/
inductive Goal where
  | eval : Environment → Term → Goal
  | apply : CekValue → CekValue → Goal
  | applyAll : CekValue → List CekValue → Goal
  | force : CekValue → Goal
  | case : Environment → List Term → CekValue → Goal
  | args : Nat → List CekValue → Environment → Term → List Term → Goal

/-- The machine's state where the part starts, over the stack `s`. -/
def Goal.start : Goal → Stack → State
  | .eval ρ t, s => .Eval s ρ t
  | .apply f a, s => .Return (.LeftApplicationToValue a :: s) f
  | .applyAll f as, s => .Return (step.folding as s) f
  | .force f, s => .Return (.ForceFrame :: s) f
  | .case ρ Ms v, s => .Return (.CaseScrutinee Ms ρ :: s) v
  | .args i acc ρ M Ms, s => .Eval (.ConstructorArgument i acc Ms ρ :: s) ρ M

/-- `Big sv g v`: the part `g` computes `v`, under the builtins' semantics `sv`. A rule for each
    way the machine's step goes on and comes back with a value. -/
inductive Big (sv : BuiltinSemanticsVariant) : Goal → CekValue → Prop where
  | var {ρ i v} : List.get?Internal ρ i = some v → Big sv (.eval ρ (.Var i)) v
  | const {ρ c} : Big sv (.eval ρ (.Const c)) (.VCon c)
  | lam {ρ M} : Big sv (.eval ρ (.Lam M)) (.VLam M ρ)
  | delay {ρ M} : Big sv (.eval ρ (.Delay M)) (.VDelay M ρ)
  | builtin {ρ b} : Big sv (.eval ρ (.Builtin b)) (.VBuiltin b [] (expectedArgs b))
  | force {ρ M f v} :
      Big sv (.eval ρ M) f → Big sv (.force f) v → Big sv (.eval ρ (.Force M)) v
  | app {ρ M N f a v} :
      Big sv (.eval ρ M) f → Big sv (.eval ρ N) a → Big sv (.apply f a) v →
        Big sv (.eval ρ (.Apply M N)) v
  | constrNil {ρ i} : Big sv (.eval ρ (.Constr i [])) (.VConstr i [])
  | constr {ρ i M Ms v} :
      Big sv (.args i [] ρ M Ms) v → Big sv (.eval ρ (.Constr i (M :: Ms))) v
  | argsLast {i acc ρ M x} :
      Big sv (.eval ρ M) x → Big sv (.args i acc ρ M []) (.VConstr i (List.reverse (x :: acc)))
  | argsNext {i acc ρ M M' Ms x v} :
      Big sv (.eval ρ M) x → Big sv (.args i (x :: acc) ρ M' Ms) v →
        Big sv (.args i acc ρ M (M' :: Ms)) v
  | case {ρ N Ms sc v} :
      Big sv (.eval ρ N) sc → Big sv (.case ρ Ms sc) v → Big sv (.eval ρ (.Case N Ms)) v
  | applyLam {M ρ a v} : Big sv (.eval (a :: ρ) M) v → Big sv (.apply (.VLam M ρ) a) v
  | applyPartial {b Vs η a} :
      Big sv (.apply (.VBuiltin b Vs (.More .ArgV η)) a) (.VBuiltin b (a :: Vs) η)
  | applyBuiltin {b Vs a v} :
      evaluateBuiltinFunction sv b (a :: Vs) = some v →
        Big sv (.apply (.VBuiltin b Vs (.One .ArgV)) a) v
  | applyNil {f} : Big sv (.applyAll f []) f
  | applyCons {f a as g v} :
      Big sv (.apply f a) g → Big sv (.applyAll g as) v → Big sv (.applyAll f (a :: as)) v
  | forceDelay {M ρ v} : Big sv (.eval ρ M) v → Big sv (.force (.VDelay M ρ)) v
  | forcePartial {b Vs η} :
      Big sv (.force (.VBuiltin b Vs (.More .ArgQ η))) (.VBuiltin b Vs η)
  | forceBuiltin {b Vs v} :
      evaluateBuiltinFunction sv b Vs = some v → Big sv (.force (.VBuiltin b Vs (.One .ArgQ))) v
  | caseConstr {ρ Ms i Vs mi f v} :
      List.get?Internal Ms i = some mi → Big sv (.eval ρ mi) f → Big sv (.applyAll f Vs) v →
        Big sv (.case ρ Ms (.VConstr i Vs)) v
  | caseInteger {ρ Ms n mi v} :
      caseBranch Ms n = some mi → Big sv (.eval ρ mi) v →
        Big sv (.case ρ Ms (.VCon (.Integer n))) v
  | caseTrue {ρ Ms mi v} :
      Ms.length = 2 → List.get?Internal Ms 1 = some mi → Big sv (.eval ρ mi) v →
        Big sv (.case ρ Ms (.VCon (.Bool true))) v
  | caseFalse {ρ Ms mi v} :
      (Ms.length == 1 || Ms.length == 2) = true → List.get?Internal Ms 0 = some mi →
        Big sv (.eval ρ mi) v → Big sv (.case ρ Ms (.VCon (.Bool false))) v
  | caseUnit {ρ Ms mi v} :
      Ms.length = 1 → Ms[0]? = some mi → Big sv (.eval ρ mi) v →
        Big sv (.case ρ Ms (.VCon .Unit)) v
  | casePair {ρ Ms p mi f v} :
      Ms.length = 1 → List.get?Internal Ms 0 = some mi → Big sv (.eval ρ mi) f →
        Big sv (.applyAll f [.VCon p.1, .VCon p.2]) v →
        Big sv (.case ρ Ms (.VCon (.Pair p))) v
  | casePairData {ρ Ms p mi f v} :
      Ms.length = 1 → List.get?Internal Ms 0 = some mi → Big sv (.eval ρ mi) f →
        Big sv (.applyAll f [.VCon (.Data p.1), .VCon (.Data p.2)]) v →
        Big sv (.case ρ Ms (.VCon (.PairData p))) v
  | caseListCons {ρ Ms c cs mi f v} :
      (Ms.length == 1 || Ms.length == 2) = true → List.get?Internal Ms 0 = some mi →
        Big sv (.eval ρ mi) f → Big sv (.applyAll f [.VCon c, .VCon (.ConstList cs)]) v →
        Big sv (.case ρ Ms (.VCon (.ConstList (c :: cs)))) v
  | caseListNil {ρ Ms mi v} :
      Ms.length = 2 → List.get?Internal Ms 1 = some mi → Big sv (.eval ρ mi) v →
        Big sv (.case ρ Ms (.VCon (.ConstList []))) v
  | caseDataCons {ρ Ms c cs mi f v} :
      (Ms.length == 1 || Ms.length == 2) = true → Ms[0]? = some mi → Big sv (.eval ρ mi) f →
        Big sv (.applyAll f [.VCon (.Data c), .VCon (.ConstDataList cs)]) v →
        Big sv (.case ρ Ms (.VCon (.ConstDataList (c :: cs)))) v
  | caseDataNil {ρ Ms mi v} :
      Ms.length = 2 → List.get?Internal Ms 1 = some mi → Big sv (.eval ρ mi) v →
        Big sv (.case ρ Ms (.VCon (.ConstDataList []))) v
  | casePairDataCons {ρ Ms c cs mi f v} :
      (Ms.length == 1 || Ms.length == 2) = true → List.get?Internal Ms 0 = some mi →
        Big sv (.eval ρ mi) f →
        Big sv (.applyAll f [.VCon (.PairData c), .VCon (.ConstPairDataList cs)]) v →
        Big sv (.case ρ Ms (.VCon (.ConstPairDataList (c :: cs)))) v
  | casePairDataNil {ρ Ms mi v} :
      Ms.length = 2 → List.get?Internal Ms 1 = some mi → Big sv (.eval ρ mi) v →
        Big sv (.case ρ Ms (.VCon (.ConstPairDataList []))) v

/-- The `i`-th part of a term: of `Apply M N`, `M` and then `N`; of a `Lam`, a `Delay` or a `Force`,
    its body; of `Case N Ms`, `N` and then the branches; of `Constr i Ts`, the arguments. -/
def part : Term → Nat → Term
  | .Apply M _, 0 => M
  | .Apply _ N, 1 => N
  | .Lam M, 0 => M
  | .Delay M, 0 => M
  | .Force M, 0 => M
  | .Case N _, 0 => N
  | .Case _ Ms, i + 1 => Ms.getD i .Error
  | .Constr _ Ts, i => Ts.getD i .Error
  | _, _ => .Error

/-- The part of `t` at the path `p`, of the indices of `part` from the root: how a proof names
    a function of a program, as the program is, and not as written again by hand. -/
def subterm (t : Term) : List Nat → Term
  | [] => t
  | i :: p => subterm (part t i) p

theorem stepN_succ (sv : BuiltinSemanticsVariant) (S : State) (n : Nat) :
    stepN sv S (n + 1) = stepN sv (step sv S) n := rfl

/-- The machine goes from `S` to `T` in some number of steps. -/
def Reaches (sv : BuiltinSemanticsVariant) (S T : State) : Prop := ∃ n, stepN sv S n = T

theorem Reaches.trans {sv S T U} (h₁ : Reaches sv S T) (h₂ : Reaches sv T U) :
    Reaches sv S U := by
  obtain ⟨n, hn⟩ := h₁
  obtain ⟨m, hm⟩ := h₂
  exact ⟨n + m, by rw [stepN_add, hn, hm]⟩

theorem Reaches.step {sv S T} (h : Reaches sv (step sv S) T) : Reaches sv S T := by
  obtain ⟨n, hn⟩ := h
  exact ⟨n + 1, by rw [stepN_succ, hn]⟩

theorem Reaches.refl (sv : BuiltinSemanticsVariant) (S : State) : Reaches sv S S := ⟨0, rfl⟩

/-- An application of a value goes on as one of a value to it: both frames apply `f` to `a`. -/
theorem step_right_application (sv : BuiltinSemanticsVariant) (s : Stack) (f a : CekValue) :
    step sv (.Return (.RightApplicationOfValue f :: s) a) =
      step sv (.Return (.LeftApplicationToValue a :: s) f) := by
  cases f with
  | VBuiltin b Vs η =>
    cases η with
    | One x => cases x <;> rfl
    | More x η => cases x <;> rfl
  | _ => rfl

/-- What `g` computes, the machine computes: from where `g` starts, over any stack `s`, it comes
    to `v` on `s`. -/
theorem Big.run {sv : BuiltinSemanticsVariant} {g : Goal} {v : CekValue} (h : Big sv g v) :
    ∀ s, Reaches sv (g.start s) (.Return s v) := by
  induction h with
  | var hv => intro s; exact Reaches.step ⟨0, by simp only [Goal.start, step, hv]; rfl⟩
  | const => intro s; exact Reaches.step (Reaches.refl _ _)
  | lam => intro s; exact Reaches.step (Reaches.refl _ _)
  | delay => intro s; exact Reaches.step (Reaches.refl _ _)
  | builtin => intro s; exact Reaches.step (Reaches.refl _ _)
  | force _ _ ihM ihF =>
    intro s
    exact Reaches.step ((ihM (.ForceFrame :: s)).trans (ihF s))
  | @app ρ M N f a v _ _ _ ihM ihN ihA =>
    intro s
    refine Reaches.step ((ihM (.LeftApplicationToTerm N ρ :: s)).trans ?_)
    refine Reaches.step ((ihN (.RightApplicationOfValue f :: s)).trans ?_)
    obtain ⟨n, hn⟩ := ihA s
    cases n with
    | zero =>
      simp only [Goal.start, stepN, State.Return.injEq] at hn
      exact absurd hn.1 (List.cons_ne_self _ _)
    | succ n =>
      refine Reaches.step ⟨n, ?_⟩
      rw [step_right_application]
      exact hn
  | constrNil => intro s; exact Reaches.step (Reaches.refl _ _)
  | constr _ ih => intro s; exact Reaches.step (ih s)
  | @argsLast i acc ρ M x _ ih =>
    intro s
    exact (ih (.ConstructorArgument i acc [] ρ :: s)).trans (Reaches.step (Reaches.refl _ _))
  | @argsNext i acc ρ M M' Ms x v _ _ ihM ihN =>
    intro s
    exact (ihM (.ConstructorArgument i acc (M' :: Ms) ρ :: s)).trans (Reaches.step (ihN s))
  | @case ρ N Ms sc v _ _ ihN ihC =>
    intro s
    exact Reaches.step ((ihN (.CaseScrutinee Ms ρ :: s)).trans (ihC s))
  | applyLam _ ih => intro s; exact Reaches.step (ih s)
  | applyPartial => intro s; exact Reaches.step (Reaches.refl _ _)
  | applyBuiltin hb =>
    intro s
    exact Reaches.step ⟨0, by simp [Goal.start, step, evalBuiltin, hb, stepN]⟩
  | applyNil => intro s; exact Reaches.refl _ _
  | @applyCons f a as g v _ _ ihA ihAs =>
    intro s
    exact (ihA (step.folding as s)).trans (ihAs s)
  | forceDelay _ ih => intro s; exact Reaches.step (ih s)
  | forcePartial => intro s; exact Reaches.step (Reaches.refl _ _)
  | forceBuiltin hb =>
    intro s
    exact Reaches.step ⟨0, by simp [Goal.start, step, evalBuiltin, hb, stepN]⟩
  | @caseConstr ρ Ms i Vs mi f v hmi _ _ ihM ihA =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hmi]
    exact (ihM (step.folding Vs s)).trans (ihA s)
  | caseInteger hmi _ ih =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hmi]
    exact ih s
  | caseTrue hl hmi _ ih =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true, beq_self_eq_true]
    exact ih s
  | caseFalse hl hmi _ ih =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true, Bool.false_eq_true, if_false]
    exact ih s
  | caseUnit hl hmi _ ih =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true, beq_self_eq_true]
    exact ih s
  | @casePair ρ Ms p mi f v hl hmi _ _ ihM ihA =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true, beq_self_eq_true]
    exact (ihM _).trans (ihA s)
  | @casePairData ρ Ms p mi f v hl hmi _ _ ihM ihA =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true, beq_self_eq_true]
    exact (ihM _).trans (ihA s)
  | @caseListCons ρ Ms c cs mi f v hl hmi _ _ ihM ihA =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true]
    exact (ihM _).trans (ihA s)
  | caseListNil hl hmi _ ih =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true, beq_self_eq_true]
    exact ih s
  | @caseDataCons ρ Ms c cs mi f v hl hmi _ _ ihM ihA =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true]
    exact (ihM _).trans (ihA s)
  | caseDataNil hl hmi _ ih =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true, beq_self_eq_true]
    exact ih s
  | @casePairDataCons ρ Ms c cs mi f v hl hmi _ _ ihM ihA =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true]
    exact (ihM _).trans (ihA s)
  | casePairDataNil hl hmi _ ih =>
    intro s
    refine Reaches.step ?_
    simp only [Goal.start, step, hl, hmi, if_true, beq_self_eq_true]
    exact ih s

/-- A program that computes the Boolean `b` on `args` returns it, within some number of steps. -/
theorem returns_of_big {ver : Version} {body : Term} {args : List Term} {b : Bool}
    (h : Big default (.eval [] (applyParams body args)) (.VCon (.Bool b))) :
    returns (.Program ver body) args b := by
  obtain ⟨n, hn⟩ := h.run []
  refine ⟨n + 1, ?_⟩
  show fromFrameToBool (runFor default (initialState (applyParams body args)) (n + 1)) = some b
  rw [runFor_eq_stepN, stepN_add]
  simp only [initialState]
  rw [show stepN default (.Eval [] [] (applyParams body args)) n = _ from hn]
  rfl

/-- A program that computes a value on `args` halts, within some number of steps. -/
theorem succeeds_of_big {ver : Version} {body : Term} {args : List Term} {v : CekValue}
    (h : Big default (.eval [] (applyParams body args)) v) :
    succeeds (.Program ver body) args := by
  obtain ⟨n, hn⟩ := h.run []
  refine ⟨n + 1, ?_⟩
  show halted (runFor default (initialState (applyParams body args)) (n + 1)) = true
  rw [runFor_eq_stepN, stepN_add]
  simp only [initialState]
  rw [show stepN default (.Eval [] [] (applyParams body args)) n = _ from hn]
  rfl

/-- A `case` of a Boolean that a hypothesis says, as of a builtin's result over values that are
    any: `big_step [Big.case_bool h]` goes on with the branch. -/
theorem Big.case_bool {sv : BuiltinSemanticsVariant} {ρ : Environment} {Ms : List Term} {b b' : Bool}
    {v : CekValue} (h : b = b') (hb : Big sv (.case ρ Ms (.VCon (.Bool b'))) v) :
    Big sv (.case ρ Ms (.VCon (.Bool b))) v := h ▸ hb

/-- One rule that the term or the values decide: the environment's value, a builtin's result and a
    `case`'s branch are computed (`rfl`). An environment's value past the values it lists is read
    from a hypothesis, `ρ.get?Internal i = some v`, so a lemma may be of any environment that has
    the values its term reads. It fails where no rule applies, as on a `case` of a value that is
    not known, which a proof splits on first. -/
macro "big_rule" : tactic => `(tactic| first
  | (apply Big.var; first | rfl | (simp only [List.get?Internal]; assumption))
  | apply Big.const | apply Big.lam | apply Big.delay | apply Big.builtin
  | apply Big.constrNil | apply Big.applyNil | apply Big.applyPartial | apply Big.forcePartial
  | (apply Big.applyBuiltin; rfl) | (apply Big.forceBuiltin; rfl)
  | apply Big.forceDelay | apply Big.applyLam
  | apply Big.app | apply Big.force | apply Big.case | apply Big.constr
  | apply Big.argsLast | apply Big.argsNext | apply Big.applyCons
  | (apply Big.caseConstr; rfl) | (apply Big.caseInteger; rfl)
  | (apply Big.caseTrue; rfl; rfl) | (apply Big.caseFalse; rfl; rfl)
  | (apply Big.caseUnit; rfl; rfl) | (apply Big.casePair; rfl; rfl)
  | (apply Big.casePairData; rfl; rfl)
  | (apply Big.caseListCons; rfl; rfl) | (apply Big.caseListNil; rfl; rfl)
  | (apply Big.caseDataCons; rfl; rfl) | (apply Big.caseDataNil; rfl; rfl)
  | (apply Big.casePairDataCons; rfl; rfl) | (apply Big.casePairDataNil; rfl; rfl))

/-- `big_step [h₁, …]` applies the lemmas `hᵢ` where one applies, and otherwise `big_rule`, as
    long as one does: a derivation of what a term computes, up to a `case` of a value that is not
    known, or a part that no rule nor lemma decides. A lemma of a loop goes here, as its
    induction's hypothesis does, so the loop is not entered again. -/
syntax "big_step" (" [" term,* "]")? : tactic

/- A loop, and not `repeat`: `repeat` nests a macro for each step, and a derivation of a few hundred
   steps reaches Lean's recursion limit, where `repeat` stops as if no rule applied. -/
open Lean Elab Tactic in
elab_rules : tactic
  | `(tactic| big_step $[ [$ts,*]]?) => do
    let lemmas := (ts.map (·.getElems)).getD #[]
    let tacs ← lemmas.mapM fun t => `(tactic| apply $t)
    let step ← `(tactic| first $[| $tacs:tactic]* | big_rule)
    let mut going := true
    while going do
      if (← getUnsolvedGoals).isEmpty then
        going := false
      else
        let saved ← saveState
        try
          evalTactic step
        catch _ =>
          saved.restore
          going := false

section Examples

/-- `(λ x. x) 1` computes `1`. -/
example : Big default (.eval [] (.Apply (.Lam (.Var 0)) (.Const (.Integer 1))))
    (.VCon (.Integer 1)) := by
  big_step

/-- `addInteger 1 2` computes `3`. -/
example : Big default
    (.eval [] (.Apply (.Apply (.Builtin .AddInteger) (.Const (.Integer 1))) (.Const (.Integer 2))))
    (.VCon (.Integer 3)) := by
  big_step

/-- `case (constr 1 [5]) [λ x. 0, λ x. x]` computes `5`. -/
example : Big default
    (.eval [] (.Case (.Constr 1 [.Const (.Integer 5)])
      [.Lam (.Const (.Integer 0)), .Lam (.Var 0)]))
    (.VCon (.Integer 5)) := by
  big_step

/-- `headList` of a list of any `Data` whose head is `x`, forced, computes `x`. -/
example (x : PlutusCore.Data.Data) (xs : List PlutusCore.Data.Data) : Big default
    (.eval [] (.Apply (.Force (.Builtin .HeadList)) (.Const (.ConstDataList (x :: xs)))))
    (.VCon (.Data x)) := by
  big_step

end Examples

end ScalusProofs.BigStep
