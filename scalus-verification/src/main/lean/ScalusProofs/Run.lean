import PlutusCore.UPLC
import Blaster

/-! A budgeted CEK run that keeps a failing program apart from an exhausted budget.

    PlutusCore's `runSteps` returns `State.Error` both when a program fails and when the step
    budget runs out, so a proof over it cannot say that a program fails. `runFor` makes the same
    steps, but a run that is still going after its budget ends in the state it reached. After
    `runFor`, `State.Error` means that the program failed within the budget.

    `#prep_uplc_run` is `#prep_uplc` over `runFor`. The `UplcBlaster` tactic proves statements with
    quantified variables with it and Blaster. A closed statement it decides with `native_decide`
    over `runProgramFor`, which is computable, so every reading below is a `Bool`. -/

namespace ScalusProofs.Run

open PlutusCore.Default (BuiltinSemanticsVariant)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.CekMachine

/-- Runs at most `n` CEK steps from `Sigma`. Unlike `runSteps`, a run that has neither halted nor
    failed after `n` steps ends in the state it reached, not in `State.Error`. -/
def runFor (semanticsVariant : BuiltinSemanticsVariant) (Sigma : State) (n : Nat) : State :=
  match n, Sigma with
  | _, State.Halt _ => Sigma
  | _, State.Error => Sigma
  | 0, _ => Sigma
  | Nat.succ n, _ => runFor semanticsVariant (step semanticsVariant Sigma) n

/-- `cekExecuteProgram` over `runFor`: applies `p` to `params` and runs at most `n` steps. -/
def runProgramFor (p : Program) (params : List Term) (n : Nat) : State :=
  match p with
  | Program.Program _ body => runFor default (initialState (applyParams body params)) n

/-- The Boolean a halted run returned. Blaster translates this projection on every goal; a
    `Prop`-valued match on `.Halt (.VCon (Const.Bool true))` fails to translate (on `Fin`)
    whenever the goal is falsifiable. -/
def fromFrameToBool (s : State) : Option Bool :=
  match s with
  | .Halt (.VCon (Const.Bool b)) => some b
  | _ => none

/-- The run failed. After `runFor`, the program itself failed within the budget. -/
def failed (s : State) : Bool :=
  match s with
  | .Error => true
  | _ => false

/-- The run halted: the program returned within the budget. Unlike `isSuccessful`, it is a
    `Bool`, so `native_decide` can evaluate it. -/
def halted (s : State) : Bool :=
  match s with
  | .Halt _ => true
  | _ => false

/-- The steps `Sigma` takes until it halts or fails, at most `n` of them, with the state it is in
    then. `made` counts the steps before `Sigma`. The count is the least budget at which
    `runFor` ends in that state. -/
def stepsFor (semanticsVariant : BuiltinSemanticsVariant) (Sigma : State) (n : Nat)
    (made : Nat := 0) : Nat × State :=
  match n, Sigma with
  | _, State.Halt _ => (made, Sigma)
  | _, State.Error => (made, Sigma)
  | 0, _ => (made, Sigma)
  | Nat.succ n, _ => stepsFor semanticsVariant (step semanticsVariant Sigma) n (made + 1)

/-- What a program does on `params` within `n` steps, for the `UplcBlaster` tactic to read: the
    steps it made, and `true` or `false` where it returned that, `returned` where it returned
    another value, `failed`, or `running` where it has done neither after `n` steps. The tactic
    finds a budget with it: on a counterexample that is spurious, this is how many steps each
    test needs. -/
def measureRun (p : Program) (params : List Term) (n : Nat) : String :=
  match p with
  | Program.Program _ body =>
    let (made, state) := stepsFor default (initialState (applyParams body params)) n
    let outcome :=
      match state with
      | .Halt (.VCon (Const.Bool true)) => "true"
      | .Halt (.VCon (Const.Bool false)) => "false"
      | .Halt _ => "returned"
      | .Error => "failed"
      | _ => "running"
    s!"{made} {outcome}"

section
open Lean Elab Command Meta Blaster.Optimize

/-- Whether `e` has a `Blaster.dite' c t f` whose branches do not take `c` and `¬ c` as they are
    written. Blaster's optimizer writes such terms, which the kernel can reject: on a case split
    over a `Bool` it gives the second branch `false = b` where `dite'` asks for `¬ true = b`
    (Lean-blaster#154). -/
def hasMismatchedDite (e : Expr) : Bool :=
  (e.find? fun e =>
    e.isAppOfArity ``Blaster.dite' 4 &&
      match e.appFn!.appFn!.appArg!, e.appFn!.appArg!, e.appArg! with
      | c, .lam _ yes _ _, .lam _ no _ _ => yes != c || no != mkNot c
      | _, _, _ => false).isSome

/-- `#prep_uplc_run name script inputs budget` defines `name` as the function of `inputs`'
    parameters that runs `script` on `inputs` for at most `budget` steps with `runFor`, optimized
    by Blaster as `#prep_uplc` does, or as written where the kernel rejects the optimized run.
    `inputs` is a function returning `List Term`, or a `List Term` for a program without
    parameters. -/
syntax (name := prepUplcRun) "#prep_uplc_run" ident ident ident num : command

@[command_elab prepUplcRun]
def prepUplcRunImp : CommandElab := fun stx => do
  let name := stx[1].getId
  let (value, optimized, plain) ← withoutModifyingEnv $ runTermElabM fun _ => do
    let some script ← Term.resolveId? stx[2] |
      throwErrorAt stx[2] m!"unknown constant '{stx[2].getId}'"
    let some inputs ← Term.resolveId? stx[3] |
      throwErrorAt stx[3] m!"unknown constant '{stx[3].getId}'"
    let some budget := stx[4].isNatLit? |
      throwErrorAt stx[4] "expected a step budget"
    let program := mkProj ``PlutusCore.UPLC.PlutusScript.PlutusScript 1 script
    let run ← Meta.lambdaTelescope (← Meta.etaExpand inputs) fun xs _ =>
      mkLambdaFVars xs
        (mkApp3 (mkConst ``runProgramFor) program (mkAppN inputs xs) (mkNatLit budget))
    let (value, _) ← Optimize.main run |>.run default
    let definition (value : Expr) : TermElabM Declaration := do
      return Declaration.defnDecl {
        name := name,
        levelParams := [],
        type := ← inferType value,
        value := value,
        hints := .abbrev,
        safety := .safe }
    return (value, ← definition value, ← definition run)
  modifyEnv (addNoncomputable · name)
  -- `addDecl` puts a definition in the environment before the kernel checks it, so the commands
  -- after it read its term even where the kernel rejects it, whose error comes later. Where the
  -- optimized run may be ill-typed, the kernel checks it here, first: where it accepts it,
  -- `addDecl` need not check it again, and where it does not, `name` is the run as written.
  if hasMismatchedDite value then
    match (← getEnv).toKernelEnv.addDecl (← getOptions) optimized with
    | .ok _ =>
      liftCoreM <| withOptions (debug.skipKernelTC.set · true) <| addDecl optimized
    | .error ex =>
      logWarning m!"Blaster's optimization of the run of {name} is ill-typed, so {name} is \
        the run as written: {ex.toMessageData (← getOptions)}"
      liftCoreM <| addDecl plain
  else
    liftCoreM <| addDecl optimized

end

end ScalusProofs.Run
