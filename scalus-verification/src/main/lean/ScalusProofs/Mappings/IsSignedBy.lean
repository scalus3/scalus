import PlutusCore.UPLC
import PlutusCore.UPLC.CekMachine.Lemmas
import CardanoLedgerApi.V3
import ScalusProofs.Runs

/-! # The compiled `isSignedBy`

    `TxInfo.isSignedBy` of Scalus's prelude, `self.signatories.contains(pubKeyHash)`, as Scalus
    compiles `(tx: TxInfo, pk: PubKeyHash) => tx.isSignedBy(pk)` with the options of
    `blaster-uplc`, returns whether the transaction is signed by the key: CardanoLedgerApi's
    `txSignedBy pk tx`, for any number of signatories (`isSignedBy_txSignedBy`).

    A lemma of the machine's run itself, `runProgramFor`, within some number of steps, proved by
    hand: no Blaster, no Z3, nothing but Lean's kernel. The program compiles to

      λ self pk. (λ sigs key. (λ go. go go) (λ go lst. BODY) sigs)
                   (unListData (headList (dropList 8 self))) (bData pk)

    where `self` is the fields of the `TxInfo`, and `BODY` is one element or the end of the list.
    `PlutusCore`'s `step` reduces on values whose parts are any, so each step of the machine is
    `rfl`: 59 steps to the loop, 17 for an element to the case on `equalsData`, and an induction
    over the list does the rest.

    `scalus.verify.uplcblaster.IsSignedByMappingTest` holds the program to the compiler: where
    Scalus compiles `isSignedBy` to another, it fails, and the lemma is to be proved again. -/

namespace ScalusProofs.Mappings.IsSignedBy

open PlutusCore.UPLC.Term
open PlutusCore.UPLC.CekMachine
open PlutusCore.UPLC.CekValue
open PlutusCore.Default
open PlutusCore.Data (Data)
open ScalusProofs.Runs

/-- The program, as Scalus compiles `isSignedBy`. -/
def program : Program :=
  -- The program, written by scalus.verify.uplcblaster.LeanPrograms:
  Program.Program (Version.Version 1 1 0)
    (.Lam (.Lam (.Apply (.Apply (.Lam (.Lam (.Apply (.Apply (.Lam (.Apply (.Var 0) (.Var 0))) (.Lam (.Lam (.Case (.Var 0) [.Lam (.Lam (.Case (.Apply (.Apply (.Builtin .EqualsData) (.Var 1)) (.Var 4)) [.Apply (.Apply (.Var 3) (.Var 3)) (.Var 0), .Const (Const.Bool true)])), .Const (Const.Bool false)])))) (.Var 1)))) (.Apply (.Builtin .UnListData) (.Apply (.Force (.Builtin .HeadList)) (.Apply (.Apply (.Force (.Builtin .DropList)) (.Const (Const.Integer 8))) (.Var 1))))) (.Apply (.Builtin .BData) (.Var 0)))))
  -- The end of the program.

def programBody : Term := match program with | .Program _ b => b

/-! ## The loop -/

/-- The loop's body: `case lst [λ h t. case (equalsData h key) [go go t, True], False]`, in the
    environment `[lst, go, key, …]`. -/
def body : Term :=
  .Case (.Var 0)
    [ .Lam (.Lam (.Case (.Apply (.Apply (.Builtin .EqualsData) (.Var 1)) (.Var 4))
        [ .Apply (.Apply (.Var 3) (.Var 3)) (.Var 0), .Const (.Bool true) ]))
    , .Const (.Bool false) ]

/-- The loop itself, `λ go lst. BODY`, as a closure over `ρ`, whose first value is the key. -/
def goV (ρ : List CekValue) : CekValue := .VLam (.Lam body) ρ

/-- The machine at the loop's body, on the list `l`, with the key `k` and the rest of the
    closure's environment `ρ`. -/
def atBody (s : Stack) (l : List Data) (k : Data) (ρ : List CekValue) : State :=
  .Eval s ([.VCon (.ConstDataList l), goV (.VCon (.Data k) :: ρ), .VCon (.Data k)] ++ ρ) body

/-- The end of the list: false, in 4 steps. -/
theorem at_nil (s : Stack) (k : Data) (ρ : List CekValue) :
    stepN default (atBody s [] k ρ) 4 = .Return s (.VCon (.Bool false)) := by
  rfl

/-- The recursive call, `go go t`. -/
def recur : Term := .Apply (.Apply (.Var 3) (.Var 3)) (.Var 0)

/-- The environment of an element's branch, `[t, h, lst, go, key, …]`. -/
def branchEnv (x : Data) (xs : List Data) (k : Data) (ρ : List CekValue) : List CekValue :=
  [.VCon (.ConstDataList xs), .VCon (.Data x), .VCon (.ConstDataList (x :: xs)),
    goV (.VCon (.Data k) :: ρ), .VCon (.Data k)] ++ ρ

/-- The machine where `equalsData x k` returns to the case on it. -/
def atTest (s : Stack) (x : Data) (xs : List Data) (k : Data) (ρ : List CekValue) : State :=
  .Return (.CaseScrutinee [recur, .Const (.Bool true)] (branchEnv x xs k ρ) :: s)
    (.VCon (.Bool (x == k)))

/-- An element takes the machine, in 17 steps, to the case on `equalsData x k`. -/
theorem to_test (s : Stack) (x : Data) (xs : List Data) (k : Data) (ρ : List CekValue) :
    stepN default (atBody s (x :: xs) k ρ) 17 = atTest s x xs k ρ := by
  rfl

/-- An element that is the key: true, in 19 steps. -/
theorem at_cons_key (s : Stack) (x : Data) (xs : List Data) (k : Data) (ρ : List CekValue)
    (h : (x == k) = true) :
    stepN default (atBody s (x :: xs) k ρ) 19 = .Return s (.VCon (.Bool true)) := by
  rw [show (19 : Nat) = 17 + 2 from rfl, stepN_add, to_test]
  simp only [atTest, h]
  rfl

/-- An element that is not the key: the loop on the rest, in 28 steps. -/
theorem at_cons_other (s : Stack) (x : Data) (xs : List Data) (k : Data) (ρ : List CekValue)
    (h : (x == k) = false) :
    stepN default (atBody s (x :: xs) k ρ) 28 = atBody s xs k ρ := by
  rw [show (28 : Nat) = 17 + 11 from rfl, stepN_add, to_test]
  simp only [atTest, h]
  rfl

/-- The loop on `l` returns whether an element of `l` is the key: by induction over `l`. -/
theorem loop_returns (s : Stack) (k : Data) (ρ : List CekValue) (l : List Data) :
    ∃ n, stepN default (atBody s l k ρ) n = .Return s (.VCon (.Bool (l.any (· == k)))) := by
  induction l with
  | nil => exact ⟨4, at_nil s k ρ⟩
  | cons x xs ih =>
    cases h : (x == k) with
    | true => exact ⟨19, by rw [at_cons_key s x xs k ρ h]; simp [List.any, h]⟩
    | false =>
      obtain ⟨n, hn⟩ := ih
      exact ⟨28 + n, by rw [stepN_add, at_cons_other s x xs k ρ h, hn]; simp [List.any, h]⟩

/-! ## The program, of `Data` -/

/-- Fields of a `TxInfo`'s `Data`, the ninth its signatories. -/
def fields (f0 f1 f2 f3 f4 f5 f6 f7 : Data) (sigs rest : List Data) : List Data :=
  f0 :: f1 :: f2 :: f3 :: f4 :: f5 :: f6 :: f7 :: Data.List sigs :: rest

/-- The machine at the start of the program, on the fields and the key. -/
def start (f0 f1 f2 f3 f4 f5 f6 f7 : Data) (sigs rest : List Data)
    (pk : PlutusCore.ByteString.ByteString) : State :=
  initialState (applyParams programBody
    [.Const (.ConstDataList (fields f0 f1 f2 f3 f4 f5 f6 f7 sigs rest)), .Const (.ByteString pk)])

/-- The environment of the loop's closure after the key: the signatories, `pk` and the fields. -/
def closure (f0 f1 f2 f3 f4 f5 f6 f7 : Data) (sigs rest : List Data)
    (pk : PlutusCore.ByteString.ByteString) : List CekValue :=
  [.VCon (.ConstDataList sigs), .VCon (.ByteString pk),
    .VCon (.ConstDataList (fields f0 f1 f2 f3 f4 f5 f6 f7 sigs rest))]

/-- The program reads the signatories, field 8, and `bData pk`, and comes to its loop in 59
    steps. -/
theorem entry (f0 f1 f2 f3 f4 f5 f6 f7 : Data) (sigs rest : List Data)
    (pk : PlutusCore.ByteString.ByteString) :
    stepN default (start f0 f1 f2 f3 f4 f5 f6 f7 sigs rest pk) 59 =
      atBody [] sigs (Data.B pk) (closure f0 f1 f2 f3 f4 f5 f6 f7 sigs rest pk) := by
  rfl

/-- The program, on the fields of a transaction and a key, returns whether a signatory is
    `B pk`, for any number of signatories. -/
theorem isSignedBy_returns (f0 f1 f2 f3 f4 f5 f6 f7 : Data) (sigs rest : List Data)
    (pk : PlutusCore.ByteString.ByteString) :
    returns program
      [.Const (.ConstDataList (fields f0 f1 f2 f3 f4 f5 f6 f7 sigs rest)), .Const (.ByteString pk)]
      (sigs.any (· == Data.B pk)) := by
  obtain ⟨m, hm⟩ := loop_returns [] (Data.B pk) (closure f0 f1 f2 f3 f4 f5 f6 f7 sigs rest pk) sigs
  refine ⟨59 + m + 1, ?_⟩
  show ScalusProofs.Run.fromFrameToBool
    (ScalusProofs.Run.runFor default (start f0 f1 f2 f3 f4 f5 f6 f7 sigs rest pk) (59 + m + 1)) = _
  rw [runFor_eq_stepN, stepN_add, stepN_add, entry, hm]
  rfl

/-! ## The program, of CardanoLedgerApi's `TxInfo` -/

open CardanoLedgerApi.IsData.Class (IsData)
open CardanoLedgerApi.V3.Contexts (TxInfo txSignedBy)
open CardanoLedgerApi.V1.Contexts (txInfoSignatoriesToListData)

/-- The fields of a `TxInfo`'s `Data`, as CardanoLedgerApi encodes it: `Constr 0` of them. -/
def txFields (tx : TxInfo) : List Data :=
  match IsData.toData tx with
  | .Constr _ fs => fs
  | _ => []

theorem toData_txInfo (tx : TxInfo) : IsData.toData tx = Data.Constr 0 (txFields tx) := rfl

/-- Field 8 is the signatories, each `B` of its bytes. -/
theorem txFields_eq (tx : TxInfo) :
    ∃ f0 f1 f2 f3 f4 f5 f6 f7 rest,
      txFields tx = fields f0 f1 f2 f3 f4 f5 f6 f7
        (txInfoSignatoriesToListData tx.txInfoSignatories) rest :=
  ⟨_, _, _, _, _, _, _, _, _, rfl⟩

/-- CardanoLedgerApi's `txSignedBy` is whether the `Data` of a signatory is `B pk`. -/
theorem txSignedBy_eq (pk : CardanoLedgerApi.V2.PubKeyHash) (tx : TxInfo) :
    txSignedBy pk tx = (txInfoSignatoriesToListData tx.txInfoSignatories).any (· == Data.B pk) := by
  unfold txSignedBy txInfoSignatoriesToListData
  generalize tx.txInfoSignatories = l
  induction l with
  | nil => rfl
  | cons x xs ih =>
    -- Both sides are `(x == pk) || …` of the rest by definition: `Recursor.any` and
    -- `Recursor.map` are local recursive functions, and `Data.B x == Data.B pk` is `x == pk`.
    exact congrArg (fun rest => (x == pk) || rest) ih

/-- The mapping: the compiled `isSignedBy`, on a transaction's fields and a key, returns
    CardanoLedgerApi's `txSignedBy pk tx`, for any number of signatories. -/
theorem isSignedBy_txSignedBy (tx : TxInfo) (pk : CardanoLedgerApi.V2.PubKeyHash) :
    returns program [.Const (.ConstDataList (txFields tx)), .Const (.ByteString pk)]
      (txSignedBy pk tx) := by
  obtain ⟨f0, f1, f2, f3, f4, f5, f6, f7, rest, h⟩ := txFields_eq tx
  rw [h, txSignedBy_eq]
  exact isSignedBy_returns f0 f1 f2 f3 f4 f5 f6 f7 _ rest pk

end ScalusProofs.Mappings.IsSignedBy
