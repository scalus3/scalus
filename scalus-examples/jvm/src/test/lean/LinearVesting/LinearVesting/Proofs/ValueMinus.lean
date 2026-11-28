import LinearVesting.Generated.value_minus_stated_contract_holds
import ScalusProofs.BigStep

/-!
# The contract of `Value.minus`

`Value.minus(a, b)` ends in `.ensuring(_.getLovelace === a.getLovelace - b.getLovelace)`. Its
statement, `value.minus.stated_contract_holds` of `VestingVerificationTest`, is of every two values
of the type, whose keys ascend, and of runs of any number of steps: the program returns, and the
lovelace of what it returns is the difference. `minus_contract` proves it, by hand, on Lean's own
axioms: no run of a budget follows `minus`, which walks both values to their ends.

The proof is of the statement's programs as Scalus compiled them, `Leaf1` (it returns) and `Leaf3`
(the postcondition) of the statement's module; `Leaf2`, the success that the postcondition assumes,
is not needed. In three parts:

- What the programs compute, with `ScalusProofs.BigStep`: a lemma for each loop of `minus`, by
  induction over its list (the conversions between pairs and tuples, `List.map`, the merge of the
  tokens and that of the policies), then `binaryOpTokens`, `minus` itself (`minus_body`) and
  `getLovelace` (`gl_spec`), whose search for the empty key ends at a map's first entry. A loop's
  lemma is of any environment that has the values its body reads; a function of the program is
  named by its path in the program's term (`subterm`), and not written again by hand.
- Models of what they compute: `mergeTok`, `mergeVal`, `minusModel`, `glModel`.
- The arithmetic of the models (`valGl_merge`, `tokGl_merge`): at the head of a merge is the
  difference of the heads' amounts, as the ada policy and token name are the empty bytes, the least
  keys; where a merge drops an entry that comes to nothing, the keys after it are not empty, as the
  keys of a value ascend (the type's predicate, `scalusAscendingBytes`).

Where Scalus compiles `minus` or `getLovelace` to other programs, the statement's module changes,
and this proof is to be made again: the paths and the models are of these programs.
-/

namespace LinearVesting.Proofs.ValueMinus

open PlutusCore.UPLC.Term
open PlutusCore.UPLC.CekValue
open PlutusCore.UPLC.CekMachine
open PlutusCore.UPLC.Builtins
open PlutusCore.Data (Data)
open ScalusProofs.BigStep

/-! ## The programs -/

/-- The program of the postcondition, `Leaf3`: `minus` is its argument, at `[1]`. -/
def leaf3Body : Term :=
  match LinearVesting_Generated_value_minus_stated_contract_holds_leaf3.script with
  | .Program _ b => b

/-- `SortedMap.toList`'s conversion of a map's pairs to tuples, `Constr 0 [k, v]`. -/
def body858 : Term :=
  subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,1,1,0,0]

def ml858V (ρc : Environment) : CekValue := .VLam (.Lam body858) ρc

/-- A pair as the tuple the program makes of it. -/
def tuple (p : Data × Data) : Data := .Constr 0 [p.1, p.2]

theorem pairs_to_tuples (ρc : Environment)
    (hmc : ρc.get?Internal 9 = some (.VBuiltin .MkCons [] (.More .ArgV (.One .ArgV)))) :
    ∀ ps : List (Data × Data),
      Big default (.eval (.VCon (.ConstPairDataList ps) :: ml858V ρc :: ρc) body858)
        (.VCon (.ConstDataList (ps.map tuple))) := by
  intro ps
  induction ps with
  | nil => big_step
  | cons p ps ih => big_step [ih]

/-- The same loop, in `binaryOpTokens`. -/
def body858b : Term :=
  subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0]

def ml858bV (ρc : Environment) : CekValue := .VLam (.Lam body858b) ρc

theorem pairs_to_tuples_b (ρc : Environment)
    (hmc : ρc.get?Internal 8 = some (.VBuiltin .MkCons [] (.More .ArgV (.One .ArgV)))) :
    ∀ ps : List (Data × Data),
      Big default (.eval (.VCon (.ConstPairDataList ps) :: ml858bV ρc :: ρc) body858b)
        (.VCon (.ConstDataList (ps.map tuple))) := by
  intro ps
  induction ps with
  | nil => big_step
  | cons p ps ih => big_step [ih]

def body740 : Term :=
  subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0]

def ml740V (ρc : Environment) : CekValue := .VLam (.Lam body740) ρc

/-- The pair a tuple of two is. -/
def pairOf : Data → Data × Data
  | .Constr _ (k :: v :: _) => (k, v)
  | _ => (.I 0, .I 0)

/-- A tuple of two: `Constr 0 [k, v]`. -/
def isTuple (x : Data) : Prop := ∃ k v, x = .Constr 0 [k, v]

theorem tuples_to_pairs (ρc : Environment)
    (htl : ρc.get?Internal 5 = some (.VBuiltin .TailList [] (.One .ArgV)))
    (hsp : ρc.get?Internal 6 = some (.VBuiltin .SndPair [] (.One .ArgV)))
    (hmc : ρc.get?Internal 8 = some (.VBuiltin .MkCons [] (.More .ArgV (.One .ArgV))))
    (hhl : ρc.get?Internal 9 = some (.VBuiltin .HeadList [] (.One .ArgV))) :
    ∀ l : List Data, (∀ x ∈ l, isTuple x) →
      Big default (.eval (.VCon (.ConstDataList l) :: ml740V ρc :: ρc) body740)
        (.VCon (.ConstPairDataList (l.map pairOf))) := by
  intro l
  induction l with
  | nil => intro _; big_step
  | cons x l ih =>
    intro hl
    obtain ⟨k, v, rfl⟩ := hl x (by simp)
    have ih' := ih (fun y hy => hl y (by simp [hy]))
    big_step [ih']

def mcV : CekValue := .VBuiltin .MkCons [] (.More .ArgV (.One .ArgV))

/-- `List.map`'s loop, `foldRight` of `mkCons (mapper x)`. -/
def frBody : Term := subterm leaf3Body [1,1,0,0,0,0,0,1,0,0,0,1,0,0]

def frV (ρf : Environment) : CekValue := .VLam (.Lam frBody) ρf

theorem map_loop (ρf : Environment) (mapper : CekValue) (P : Data → Prop) (g : Data → Data)
    (hm : ρf.get?Internal 0 = some mapper)
    (hmc : ρf.get?Internal 5 = some mcV)
    (hg : ∀ x, P x → Big default (.apply mapper (.VCon (.Data x))) (.VCon (.Data (g x)))) :
    ∀ l : List Data, (∀ x ∈ l, P x) →
      Big default (.eval (.VCon (.ConstDataList l) :: frV ρf :: ρf) frBody)
        (.VCon (.Data (.List (l.map g)))) := by
  intro l
  induction l with
  | nil => intro _; big_step
  | cons x l ih =>
    intro hl
    have hx := hg x (hl x (by simp))
    have ih' := ih (fun y hy => hl y (by simp [hy]))
    big_step [hx, ih']

open PlutusCore.ByteString (ByteString) in
/-- The constructor `Ord` of byte strings returns: `Less`, `Equal` and `Greater` are 0, 1 and 2. -/
def cmpTag (x y : ByteString) : Nat :=
  if PlutusCore.ByteString.lessThanByteString x y then 0
  else if PlutusCore.ByteString.equalsByteString x y then 1 else 2

/-- `Ord[ByteString]`'s `compare`, after its first argument. -/
def ordBody : Term := subterm leaf3Body [1,1,0,0,0,0,0,0,0,1,0,0]

open PlutusCore.ByteString (ByteString) in
theorem ord_cmp (ρo : Environment) (x y : ByteString) :
    Big default (.eval (.VCon (.ByteString y) :: .VCon (.ByteString x) :: ρo) ordBody)
      (.VConstr (cmpTag x y) []) := by
  big_step
  cases h1 : PlutusCore.ByteString.lessThanByteString x y
  · big_step
    cases h2 : PlutusCore.ByteString.equalsByteString x y
    · simp only [cmpTag, h1, h2, Bool.false_eq_true, ↓reduceIte]; big_step
    · simp only [cmpTag, h1, h2, Bool.false_eq_true, ↓reduceIte]; big_step
  · simp only [cmpTag, h1, ↓reduceIte]; big_step

/-! The values the program's preamble binds: forced builtins, `List.map`, `compare`. -/

def hlV : CekValue := .VBuiltin .HeadList [] (.One .ArgV)
def nlV : CekValue := .VBuiltin .NullList [] (.One .ArgV)
def spV : CekValue := .VBuiltin .SndPair [] (.One .ArgV)
def tlV : CekValue := .VBuiltin .TailList [] (.One .ArgV)
def subV : CekValue := .VBuiltin .SubtractInteger [] (.More .ArgV (.One .ArgV))
def env0 : Environment := [tlV, spV, nlV, mcV, hlV]
def mapV : CekValue := .VLam (subterm leaf3Body [1,1,0,0,0,0,0,1,0]) env0
def ordV : CekValue := .VLam (.Lam ordBody) (mapV :: env0)

/-! The tokens' merge, `binaryOpTokens`'s `go`, of tuples `Constr 0 [B k, I n]`. -/

def keyOf : Data → Data
  | .Constr _ (k :: _) => k
  | _ => .I 0

def keyBytes : Data → PlutusCore.ByteString.ByteString
  | .Constr _ (.B k :: _) => k
  | _ => default

def amtOf : Data → Int
  | .Constr _ (_ :: .I n :: _) => n
  | _ => 0

def pos (x : Data) : Data := .Constr 0 [keyOf x, .I (amtOf x - 0)]
def neg (x : Data) : Data := .Constr 0 [keyOf x, .I (0 - amtOf x)]

def mergeTokWith (x : Data) (t : List Data) (rest : List Data → List Data) : List Data → List Data
  | [] => (x :: t).map pos
  | y :: u =>
    match cmpTag (keyBytes x) (keyBytes y) with
    | 0 => pos x :: rest (y :: u)
    | 1 =>
      if PlutusCore.Integer.equalsInteger (PlutusCore.Integer.subtractInteger (amtOf x) (amtOf y)) 0
      then rest u
      else .Constr 0 [keyOf x, .I (amtOf x - amtOf y)] :: rest u
    | _ => neg y :: mergeTokWith x t rest u

def mergeTok : List Data → List Data → List Data
  | [], r => r.map neg
  | x :: t, r => mergeTokWith x t (mergeTok t) r

def isTok (x : Data) : Prop := ∃ k n, x = .Constr 0 [.B k, .I n]

def body631 : Term :=
  subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0]

def go631V (ρ : Environment) : CekValue := .VLam (.Lam (.Lam body631)) ρ

/-- The mapper of the empty left: `(tn, op(0, v))`. -/
def negBody : Term :=
  subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,2,1,1,0]

/-- The mapper of the empty right: `(tn, op(v, 0))`. -/
def posBody : Term :=
  subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,2,1,1,0]

open PlutusCore.ByteString (ByteString) in
/-- `ord_cmp` where the comparison's outcome is known. -/
theorem ord_cmp_eq (ρo : Environment) (x y : ByteString) (n : Nat) (h : cmpTag x y = n) :
    Big default (.eval (.VCon (.ByteString y) :: .VCon (.ByteString x) :: ρo) ordBody)
      (.VConstr n []) := h ▸ ord_cmp ρo x y

theorem cmpTag_cases (x y : PlutusCore.ByteString.ByteString) :
    cmpTag x y = 0 ∨ cmpTag x y = 1 ∨ cmpTag x y = 2 := by
  unfold cmpTag
  split
  · exact Or.inl rfl
  · split
    · exact Or.inr (Or.inl rfl)
    · exact Or.inr (Or.inr rfl)

theorem merge_tok (ρ : Environment)
    (hop : ρ.get?Internal 1 = some subV)
    (hord : ρ.get?Internal 4 = some ordV)
    (hmap : ρ.get?Internal 5 = some mapV)
    (htl : ρ.get?Internal 6 = some tlV)
    (hsp : ρ.get?Internal 7 = some spV)
    (hmc : ρ.get?Internal 9 = some mcV)
    (hhl : ρ.get?Internal 10 = some hlV) :
    ∀ l r : List Data, (∀ x ∈ l, isTok x) → (∀ y ∈ r, isTok y) →
      Big default (.eval (.VCon (.ConstDataList r) :: .VCon (.ConstDataList l) :: go631V ρ :: ρ)
        body631) (.VCon (.ConstDataList (mergeTok l r))) := by
  intro l
  induction l with
  | nil =>
    intro r _ hr
    let mapper := CekValue.VLam negBody
      (.VCon (.ConstDataList r) :: .VCon (.ConstDataList []) :: go631V ρ :: ρ)
    have hg : ∀ x, isTok x → Big default (.apply mapper (.VCon (.Data x))) (.VCon (.Data (neg x))) := by
      rintro x ⟨k, n, rfl⟩
      big_step
    have hm := map_loop (mapper :: .VCon (.ConstDataList r) :: env0) mapper isTok neg rfl rfl hg r hr
    big_step [hm]
  | cons x t ih =>
    intro r hl
    have ht : ∀ z ∈ t, isTok z := fun z hz => hl z (by simp [hz])
    obtain ⟨k1, n1, rfl⟩ := hl x (by simp)
    induction r with
    | nil =>
      intro _
      let mapper := CekValue.VLam posBody
        (.VCon (.ConstDataList t) :: .VCon (.Data (.Constr 0 [.B k1, .I n1])) ::
          .VCon (.ConstDataList []) ::
          .VCon (.ConstDataList (.Constr 0 [.B k1, .I n1] :: t)) :: go631V ρ :: ρ)
      have hg : ∀ x, isTok x → Big default (.apply mapper (.VCon (.Data x))) (.VCon (.Data (pos x))) := by
        rintro x ⟨k, n, rfl⟩
        big_step
      have hm := map_loop (mapper :: .VCon (.ConstDataList (.Constr 0 [.B k1, .I n1] :: t)) :: env0)
        mapper isTok pos rfl rfl hg _ hl
      big_step [hm]
    | cons y u ihr =>
      intro hr
      have hu : ∀ z ∈ u, isTok z := fun z hz => hr z (by simp [hz])
      obtain ⟨k2, n2, rfl⟩ := hr y (by simp)
      have ihu := ihr hu
      have iht := ih u ht hu
      have ihy := ih _ ht hr
      rcases cmpTag_cases k1 k2 with hc | hc | hc
      · simp only [mergeTok, mergeTokWith, keyBytes, hc]
        big_step [ord_cmp_eq _ _ _ _ hc, ihy]
      · simp only [mergeTok, mergeTokWith, keyBytes, amtOf, hc]
        cases hz : PlutusCore.Integer.equalsInteger (PlutusCore.Integer.subtractInteger n1 n2) 0
        · simp only [Bool.false_eq_true, ↓reduceIte]
          big_step [ord_cmp_eq _ _ _ _ hc, Big.case_bool hz, iht]
        · simp only [↓reduceIte]
          big_step [ord_cmp_eq _ _ _ _ hc, Big.case_bool hz, iht]
      · simp only [mergeTok, mergeTokWith, keyBytes, hc]
        big_step [ord_cmp_eq _ _ _ _ hc, ihu]

/-! `binaryOpTokens` of two maps of tokens, with `subtractInteger`. -/

/-- `binaryOpTokens`, after its first parameter: `λ b op. …`. -/
def botV : CekValue := .VLam (subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,1,0]) (ordV :: mapV :: env0)

/-- A pair of a map of tokens: `(B tn, I n)`. -/
def isTokPair (p : Data × Data) : Prop := ∃ k n, p = (.B k, .I n)

theorem tuple_tok {X : List (Data × Data)} (hX : ∀ p ∈ X, isTokPair p) :
    ∀ x ∈ X.map tuple, isTok x := by
  intro x hx
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hx
  obtain ⟨k, n, rfl⟩ := hX p hp
  exact ⟨k, n, rfl⟩

theorem bot_spec (X Y : List (Data × Data)) (hX : ∀ p ∈ X, isTokPair p)
    (hY : ∀ p ∈ Y, isTokPair p) :
    Big default (.applyAll botV [.VCon (.Data (.Map X)), .VCon (.Data (.Map Y)), subV])
      (.VCon (.ConstDataList (mergeTok (X.map tuple) (Y.map tuple)))) := by
  let envB : Environment :=
    subV :: .VCon (.Data (.Map Y)) :: .VCon (.Data (.Map X)) :: ordV :: mapV :: env0
  have h1 := pairs_to_tuples_b envB rfl X
  have h2 := pairs_to_tuples_b envB rfl Y
  have hm := merge_tok (.VLam body858b (ml858bV envB :: envB) :: envB) rfl rfl rfl rfl rfl rfl rfl
    (X.map tuple) (Y.map tuple) (tuple_tok hX) (tuple_tok hY)
  big_step [h1, h2, hm]

/-! The policies' merge, `binaryOpValues`'s `go`, of tuples `Constr 0 [B cs, Map tokens]`. -/

/-- The tokens of a policy's tuple. -/
def innerOf : Data → List (Data × Data)
  | .Constr _ (_ :: .Map ps :: _) => ps
  | _ => []

/-- A policy only the left has: its tokens less nothing. -/
def lessV (x : Data) : Data := .Constr 0 [keyOf x, .List (mergeTok ((innerOf x).map tuple) [])]

/-- A policy only the right has: nothing less its tokens. -/
def greaterV (y : Data) : Data := .Constr 0 [keyOf y, .List (mergeTok [] ((innerOf y).map tuple))]

def mergeValWith (x : Data) (t : List Data) (rest : List Data → List Data) : List Data → List Data
  | [] => (x :: t).map lessV
  | y :: u =>
    match cmpTag (keyBytes x) (keyBytes y) with
    | 0 => lessV x :: rest (y :: u)
    | 1 =>
      if PlutusCore.List.nullList (mergeTok ((innerOf x).map tuple) ((innerOf y).map tuple))
      then rest u
      else .Constr 0 [keyOf x, .List (mergeTok ((innerOf x).map tuple) ((innerOf y).map tuple))] ::
        rest u
    | _ => greaterV y :: mergeValWith x t rest u

def mergeVal : List Data → List Data → List Data
  | [], r => r.map greaterV
  | x :: t, r => mergeValWith x t (mergeVal t) r

/-- A policy's tuple of a value: `Constr 0 [B cs, Map tokens]`. -/
def isVal (x : Data) : Prop := ∃ k ps, x = .Constr 0 [.B k, .Map ps] ∧ ∀ p ∈ ps, isTokPair p

def body750 : Term :=
  subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0]

def go750V (ρ : Environment) : CekValue := .VLam (.Lam (.Lam body750)) ρ

/-- The mapper of the empty left: a policy only the right has. -/
def greaterBody : Term :=
  subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,2,1,1,0]

/-- The mapper of the empty right: a policy only the left has. -/
def lessBody : Term :=
  subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,1,0,0,2,1,1,0]

theorem merge_val (ρ : Environment)
    (hbot : ρ.get?Internal 4 = some botV)
    (hord : ρ.get?Internal 5 = some ordV)
    (hmap : ρ.get?Internal 6 = some mapV)
    (htl : ρ.get?Internal 7 = some tlV)
    (hsp : ρ.get?Internal 8 = some spV)
    (hnl : ρ.get?Internal 9 = some nlV)
    (hmc : ρ.get?Internal 10 = some mcV)
    (hhl : ρ.get?Internal 11 = some hlV) :
    ∀ l r : List Data, (∀ x ∈ l, isVal x) → (∀ y ∈ r, isVal y) →
      Big default (.eval (.VCon (.ConstDataList r) :: .VCon (.ConstDataList l) :: go750V ρ :: ρ)
        body750) (.VCon (.ConstDataList (mergeVal l r))) := by
  intro l
  induction l with
  | nil =>
    intro r _ hr
    let mapper := CekValue.VLam greaterBody
      (.VCon (.ConstDataList r) :: .VCon (.ConstDataList []) :: go750V ρ :: ρ)
    have hg : ∀ y, isVal y → Big default (.apply mapper (.VCon (.Data y))) (.VCon (.Data (greaterV y))) := by
      rintro y ⟨k, ps, rfl, hps⟩
      have hb := bot_spec [] ps (by simp) hps
      big_step [hb]
    have hm := map_loop (mapper :: .VCon (.ConstDataList r) :: env0) mapper isVal greaterV rfl rfl hg r hr
    big_step [hm]
  | cons x t ih =>
    intro r hl
    have ht : ∀ z ∈ t, isVal z := fun z hz => hl z (by simp [hz])
    obtain ⟨k1, ps1, rfl, hps1⟩ := hl x (by simp)
    induction r with
    | nil =>
      intro _
      let mapper := CekValue.VLam lessBody
        (.VCon (.ConstDataList t) :: .VCon (.Data (.Constr 0 [.B k1, .Map ps1])) ::
          .VCon (.ConstDataList []) ::
          .VCon (.ConstDataList (.Constr 0 [.B k1, .Map ps1] :: t)) :: go750V ρ :: ρ)
      have hg : ∀ x, isVal x → Big default (.apply mapper (.VCon (.Data x))) (.VCon (.Data (lessV x))) := by
        rintro x ⟨k, ps, rfl, hps⟩
        have hb := bot_spec ps [] hps (by simp)
        big_step [hb]
      have hm := map_loop (mapper :: .VCon (.ConstDataList (.Constr 0 [.B k1, .Map ps1] :: t)) :: env0)
        mapper isVal lessV rfl rfl hg _ hl
      big_step [hm]
    | cons y u ihr =>
      intro hr
      have hu : ∀ z ∈ u, isVal z := fun z hz => hr z (by simp [hz])
      obtain ⟨k2, ps2, rfl, hps2⟩ := hr y (by simp)
      have ihu := ihr hu
      have iht := ih u ht hu
      have ihy := ih _ ht hr
      have hb1 := bot_spec ps1 [] hps1 (by simp)
      have hb2 := bot_spec [] ps2 (by simp) hps2
      have hb12 := bot_spec ps1 ps2 hps1 hps2
      rcases cmpTag_cases k1 k2 with hc | hc | hc
      · simp only [mergeVal, mergeValWith, keyBytes, hc]
        big_step [ord_cmp_eq _ _ _ _ hc, hb1, ihy]
      · simp only [mergeVal, mergeValWith, keyBytes, innerOf, hc]
        cases hz : PlutusCore.List.nullList (mergeTok (ps1.map tuple) (ps2.map tuple))
        · simp only [Bool.false_eq_true, ↓reduceIte]
          big_step [ord_cmp_eq _ _ _ _ hc, hb12, Big.case_bool hz, iht]
        · simp only [↓reduceIte]
          big_step [ord_cmp_eq _ _ _ _ hc, hb12, Big.case_bool hz, iht]
      · simp only [mergeVal, mergeValWith, keyBytes, hc]
        big_step [ord_cmp_eq _ _ _ _ hc, hb2, ihu]

/-! `Value.minus` of two values. -/

/-- The tokens of a result's policy, as tuples. -/
def listOf : Data → List Data
  | .Constr _ (_ :: .List l :: _) => l
  | _ => []

/-- A policy of the result, its tokens as a map again. -/
def outConv (x : Data) : Data := .Constr 0 [keyOf x, .Map ((listOf x).map pairOf)]

/-- A policy of the merge: `Constr 0 [cs, List tokens]`, its tokens tuples. -/
def isMerged (x : Data) : Prop := ∃ k l, x = .Constr 0 [k, .List l] ∧ ∀ e ∈ l, isTuple e

theorem mergeTokWith_tuples (x : Data) (t : List Data) (rest : List Data → List Data)
    (hrest : ∀ r, ∀ e ∈ rest r, isTuple e) (hx : isTuple (pos x)) (ht : ∀ e ∈ t, isTuple (pos e)) :
    ∀ r, ∀ e ∈ mergeTokWith x t rest r, isTuple e := by
  intro r
  induction r with
  | nil =>
    intro e he
    simp only [mergeTokWith, List.map_cons, List.mem_cons, List.mem_map] at he
    rcases he with rfl | ⟨z, hz, rfl⟩
    · exact hx
    · exact ht z hz
  | cons y u ih =>
    intro e he
    simp only [mergeTokWith] at he
    split at he
    · rcases List.mem_cons.mp he with rfl | he
      · exact hx
      · exact hrest _ e he
    · split at he
      · exact hrest _ e he
      · rcases List.mem_cons.mp he with rfl | he
        · exact ⟨_, _, rfl⟩
        · exact hrest _ e he
    · rcases List.mem_cons.mp he with rfl | he
      · exact ⟨_, _, rfl⟩
      · exact ih e he

theorem mergeTok_tuples : ∀ l r, ∀ e ∈ mergeTok l r, isTuple e := by
  intro l
  induction l with
  | nil =>
    intro r e he
    obtain ⟨z, _, rfl⟩ := List.mem_map.mp he
    exact ⟨_, _, rfl⟩
  | cons x t ih =>
    exact mergeTokWith_tuples x t (mergeTok t) ih ⟨_, _, rfl⟩ (fun _ _ => ⟨_, _, rfl⟩)

theorem mergeValWith_merged (x : Data) (t : List Data) (rest : List Data → List Data)
    (hrest : ∀ r, ∀ e ∈ rest r, isMerged e) :
    ∀ r, ∀ e ∈ mergeValWith x t rest r, isMerged e := by
  have hless : ∀ z, isMerged (lessV z) := fun z => ⟨_, _, rfl, mergeTok_tuples _ _⟩
  intro r
  induction r with
  | nil =>
    intro e he
    obtain ⟨z, _, rfl⟩ := List.mem_map.mp he
    exact hless z
  | cons y u ih =>
    intro e he
    simp only [mergeValWith] at he
    split at he
    · rcases List.mem_cons.mp he with rfl | he
      · exact hless x
      · exact hrest _ e he
    · split at he
      · exact hrest _ e he
      · rcases List.mem_cons.mp he with rfl | he
        · exact ⟨_, _, rfl, mergeTok_tuples _ _⟩
        · exact hrest _ e he
    · rcases List.mem_cons.mp he with rfl | he
      · exact ⟨_, _, rfl, mergeTok_tuples _ _⟩
      · exact ih e he

theorem mergeVal_merged : ∀ l r, ∀ e ∈ mergeVal l r, isMerged e := by
  intro l
  induction l with
  | nil =>
    intro r e he
    obtain ⟨z, _, rfl⟩ := List.mem_map.mp he
    exact ⟨_, _, rfl, mergeTok_tuples _ _⟩
  | cons x t ih => exact mergeValWith_merged x t (mergeVal t) ih

/-- `Value.minus`, after its first parameter: `λ b. …`. -/
def minusV : CekValue :=
  .VLam (subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,0,0,0]) (botV :: ordV :: mapV :: env0)

/-- The mapper of the result: a policy's tokens as a map again. -/
def outBody : Term := subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,1,0]

/-- A pair of a value: `(B cs, Map tokens)`. -/
def isValPair (p : Data × Data) : Prop :=
  ∃ k ps, p = (.B k, .Map ps) ∧ ∀ q ∈ ps, isTokPair q

/-- What `Value.minus` returns, of the pairs of two values. -/
def minusModel (A B : List (Data × Data)) : Data :=
  .Map (((mergeVal (A.map tuple) (B.map tuple)).map outConv).map pairOf)

theorem tuple_val {A : List (Data × Data)} (hA : ∀ p ∈ A, isValPair p) :
    ∀ x ∈ A.map tuple, isVal x := by
  intro x hx
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hx
  obtain ⟨k, ps, rfl, hps⟩ := hA p hp
  exact ⟨k, ps, rfl, hps⟩

/-- `Value.minus`'s body, after both parameters. -/
def minusBody : Term := subterm leaf3Body [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0]

theorem minus_body (A B : List (Data × Data)) (hA : ∀ p ∈ A, isValPair p)
    (hB : ∀ p ∈ B, isValPair p) :
    Big default (.eval (.VCon (.Data (.Map B)) :: .VCon (.Data (.Map A)) :: botV :: ordV :: mapV ::
      env0) minusBody) (.VCon (.Data (minusModel A B))) := by
  let env1 : Environment :=
    .VCon (.Data (.Map B)) :: .VCon (.Data (.Map A)) :: botV :: ordV :: mapV :: env0
  let env2 : Environment := .VLam body740 (ml740V env1 :: env1) :: env1
  let env3 : Environment := .VLam body858 (ml858V env2 :: env2) :: env2
  let merged := mergeVal (A.map tuple) (B.map tuple)
  have hA' := pairs_to_tuples env2 rfl A
  have hB' := pairs_to_tuples env2 rfl B
  have hmv := merge_val env3 rfl rfl rfl rfl rfl rfl rfl rfl (A.map tuple) (B.map tuple)
    (tuple_val hA) (tuple_val hB)
  let mapper := CekValue.VLam outBody env2
  have hg : ∀ x, isMerged x →
      Big default (.apply mapper (.VCon (.Data x))) (.VCon (.Data (outConv x))) := by
    rintro x ⟨k, l, rfl, hl⟩
    have h := tuples_to_pairs env1 rfl rfl rfl rfl l hl
    big_step [h]
  have hm := map_loop (mapper :: .VCon (.ConstDataList merged) :: env0) mapper isMerged outConv
    rfl rfl hg merged (mergeVal_merged _ _)
  have ht := tuples_to_pairs env1 rfl rfl rfl rfl (merged.map outConv)
    (fun x hx => by obtain ⟨z, _, rfl⟩ := List.mem_map.mp hx; exact ⟨_, _, rfl⟩)
  big_step [hA', hB', hmv, hm, ht]

theorem tok_tuple_pos {x : Data} (hx : isTok x) : isTok (pos x) := by
  obtain ⟨k, n, rfl⟩ := hx
  exact ⟨k, _, rfl⟩

theorem tok_tuple_neg {x : Data} (hx : isTok x) : isTok (neg x) := by
  obtain ⟨k, n, rfl⟩ := hx
  exact ⟨k, _, rfl⟩

theorem mergeTokWith_tok (x : Data) (t : List Data) (rest : List Data → List Data)
    (hx : isTok x) (ht : ∀ e ∈ t, isTok e) (hrest : ∀ r, (∀ y ∈ r, isTok y) → ∀ e ∈ rest r, isTok e) :
    ∀ r, (∀ y ∈ r, isTok y) → ∀ e ∈ mergeTokWith x t rest r, isTok e := by
  obtain ⟨k1, n1, rfl⟩ := hx
  intro r
  induction r with
  | nil =>
    intro _ e he
    simp only [mergeTokWith, List.map_cons, List.mem_cons, List.mem_map] at he
    rcases he with rfl | ⟨z, hz, rfl⟩
    · exact ⟨k1, _, rfl⟩
    · exact tok_tuple_pos (ht z hz)
  | cons y u ih =>
    intro hr e he
    have hu : ∀ z ∈ u, isTok z := fun z hz => hr z (by simp [hz])
    obtain ⟨k2, n2, rfl⟩ := hr y (by simp)
    simp only [mergeTokWith] at he
    split at he
    · rcases List.mem_cons.mp he with rfl | he
      · exact ⟨k1, _, rfl⟩
      · exact hrest _ hr e he
    · split at he
      · exact hrest _ hu e he
      · rcases List.mem_cons.mp he with rfl | he
        · exact ⟨k1, _, rfl⟩
        · exact hrest _ hu e he
    · rcases List.mem_cons.mp he with rfl | he
      · exact ⟨k2, _, rfl⟩
      · exact ih hu e he

theorem mergeTok_tok : ∀ l r, (∀ x ∈ l, isTok x) → (∀ y ∈ r, isTok y) →
    ∀ e ∈ mergeTok l r, isTok e := by
  intro l
  induction l with
  | nil =>
    intro r _ hr e he
    obtain ⟨z, hz, rfl⟩ := List.mem_map.mp he
    exact tok_tuple_neg (hr z hz)
  | cons x t ih =>
    intro r hl hr
    exact mergeTokWith_tok x t (mergeTok t) (hl x (by simp)) (fun z hz => hl z (by simp [hz]))
      (fun r hr' => ih r (fun z hz => hl z (by simp [hz])) hr') r hr

/-- A policy of the merge of values: `Constr 0 [B cs, List tokens]`, its tokens' tuples. -/
def isMergedVal (x : Data) : Prop :=
  ∃ k l, x = .Constr 0 [.B k, .List l] ∧ ∀ e ∈ l, isTok e

theorem val_tups {x : Data} (hx : isVal x) : ∀ e ∈ (innerOf x).map tuple, isTok e := by
  obtain ⟨k, ps, rfl, hps⟩ := hx
  exact tuple_tok hps

theorem mergeValWith_val (x : Data) (t : List Data) (rest : List Data → List Data)
    (hx : isVal x) (ht : ∀ e ∈ t, isVal e)
    (hrest : ∀ r, (∀ y ∈ r, isVal y) → ∀ e ∈ rest r, isMergedVal e) :
    ∀ r, (∀ y ∈ r, isVal y) → ∀ e ∈ mergeValWith x t rest r, isMergedVal e := by
  have hless : ∀ z, isVal z → isMergedVal (lessV z) := by
    intro z hz
    obtain ⟨k, ps, rfl, hps⟩ := hz
    exact ⟨k, _, rfl, mergeTok_tok _ _ (tuple_tok hps) (by simp)⟩
  have hgreater : ∀ z, isVal z → isMergedVal (greaterV z) := by
    intro z hz
    obtain ⟨k, ps, rfl, hps⟩ := hz
    exact ⟨k, _, rfl, mergeTok_tok _ _ (by simp) (tuple_tok hps)⟩
  intro r
  induction r with
  | nil =>
    intro _ e he
    simp only [mergeValWith, List.map_cons, List.mem_cons, List.mem_map] at he
    rcases he with rfl | ⟨z, hz, rfl⟩
    · exact hless x hx
    · exact hless z (ht z hz)
  | cons y u ih =>
    intro hr e he
    have hu : ∀ z ∈ u, isVal z := fun z hz => hr z (by simp [hz])
    have hy := hr y (by simp)
    simp only [mergeValWith] at he
    split at he
    · rcases List.mem_cons.mp he with rfl | he
      · exact hless x hx
      · exact hrest _ hr e he
    · split at he
      · exact hrest _ hu e he
      · rcases List.mem_cons.mp he with rfl | he
        · have hxt := val_tups hx
          obtain ⟨k1, ps1, rfl, _⟩ := hx
          exact ⟨k1, _, rfl, mergeTok_tok _ _ hxt (val_tups hy)⟩
        · exact hrest _ hu e he
    · rcases List.mem_cons.mp he with rfl | he
      · exact hgreater y hy
      · exact ih hu e he

theorem mergeVal_val : ∀ l r, (∀ x ∈ l, isVal x) → (∀ y ∈ r, isVal y) →
    ∀ e ∈ mergeVal l r, isMergedVal e := by
  intro l
  induction l with
  | nil =>
    intro r _ hr e he
    obtain ⟨z, hz, rfl⟩ := List.mem_map.mp he
    obtain ⟨k, ps, rfl, hps⟩ := hr z hz
    exact ⟨k, _, rfl, mergeTok_tok _ _ (by simp) (tuple_tok hps)⟩
  | cons x t ih =>
    intro r hl hr
    exact mergeValWith_val x t (mergeVal t) (hl x (by simp)) (fun z hz => hl z (by simp [hz]))
      (fun r hr' => ih r (fun z hz => hl z (by simp [hz])) hr') r hr

/-- The pairs of what `Value.minus` returns. -/
def minusPairs (A B : List (Data × Data)) : List (Data × Data) :=
  ((mergeVal (A.map tuple) (B.map tuple)).map outConv).map pairOf

theorem minusPairs_val {A B : List (Data × Data)} (hA : ∀ p ∈ A, isValPair p)
    (hB : ∀ p ∈ B, isValPair p) : ∀ p ∈ minusPairs A B, isValPair p := by
  intro p hp
  simp only [minusPairs, List.map_map, List.mem_map, Function.comp] at hp
  obtain ⟨x, hx, rfl⟩ := hp
  obtain ⟨k, l, rfl, hl⟩ := mergeVal_val _ _ (tuple_val hA) (tuple_val hB) x hx
  refine ⟨k, l.map pairOf, rfl, ?_⟩
  intro q hq
  obtain ⟨e, he, rfl⟩ := List.mem_map.mp hq
  obtain ⟨t, n, rfl⟩ := hl e he
  exact ⟨t, n, rfl⟩

/-! `getLovelace`, of the statement's programs. -/

open PlutusCore.ByteString (ByteString)

/-- The ada policy and token name: the empty bytes. -/
def emptyBS : ByteString := ⟨""⟩

theorem empty_lt (k : ByteString) (h : k ≠ emptyBS) :
    PlutusCore.ByteString.lessThanByteString emptyBS k = true := by
  obtain ⟨s⟩ := k
  have hs : s ≠ "" := fun hs => h (by rw [hs]; rfl)
  simp only [PlutusCore.ByteString.lessThanByteString, decide_eq_true_eq]
  show "".data < s.data
  cases hd : s.data with
  | nil => exact absurd (String.ext hd) hs
  | cons c cs => exact List.nil_lt_cons c cs

def fpV : CekValue := .VBuiltin .FstPair [] (.One .ArgV)

/-- `getLovelace`, after its parameter: `λ v. …`. -/
def glBody : Term := subterm leaf3Body [0,1,0,0,0,0,0,0,0,0,0,0,0,1,0]

/-- `SortedMap.get`, after its first parameter: `λ key evidence. …`. -/
def getBody : Term := subterm leaf3Body [0,1,0,0,0,0,0,0,0,0,0,1,0]

/-- The amount of the empty token name, of the first token of a map. -/
def innerGl : List (Data × Data) → Int
  | (.B t, .I n) :: _ => if t = emptyBS then n else 0
  | _ => 0

/-- What `getLovelace` returns of a value's pairs: of its first policy, where it is ada. -/
def glModel : List (Data × Data) → Int
  | (.B k, .Map ts) :: _ => if k = emptyBS then innerGl ts else 0
  | _ => 0

theorem gl_spec (envG ρg : Environment)
    (hget : envG.get?Internal 0 = some (.VLam getBody ρg))
    (hmcg : ρg.get?Internal 5 = some mcV)
    (hsp : envG.get?Internal 5 = some spV)
    (hhl : envG.get?Internal 7 = some hlV)
    (hfp : envG.get?Internal 8 = some fpV) :
    ∀ ps, (∀ p ∈ ps, isValPair p) →
      Big default (.apply (.VLam glBody envG) (.VCon (.Data (.Map ps))))
        (.VCon (.Integer (glModel ps))) := by
  intro ps hps
  cases ps with
  | nil => big_step
  | cons p rest =>
    obtain ⟨k, ts, rfl, hts⟩ := hps p (by simp)
    by_cases hk : k = emptyBS
    · subst hk
      cases ts with
      | nil =>
        have : glModel ((.B emptyBS, .Map []) :: rest) = 0 := by simp [glModel, innerGl]
        rw [this]
        big_step
      | cons q qs =>
        obtain ⟨t, n, rfl⟩ := hts q (by simp)
        by_cases ht : t = emptyBS
        · subst ht
          have : glModel ((.B emptyBS, .Map ((.B emptyBS, .I n) :: qs)) :: rest) = n := by
            simp [glModel, innerGl]
          rw [this]
          big_step
        · have : glModel ((.B emptyBS, .Map ((.B t, .I n) :: qs)) :: rest) = 0 := by
            simp [glModel, innerGl, ht]
          rw [this]
          big_step [Big.case_bool (empty_lt t ht)]
    · have : glModel ((.B k, .Map ts) :: rest) = 0 := by simp [glModel, hk]
      rw [this]
      big_step [Big.case_bool (empty_lt k hk)]

/-! The statement's programs, on the `Data` of two values. -/

def leaf1Body : Term :=
  match LinearVesting_Generated_value_minus_stated_contract_holds_leaf1.script with
  | .Program _ b => b

/-- The environment of `getLovelace`'s `get`, in the program of the postcondition. -/
def envR3 (A B : List (Data × Data)) : Environment :=
  [.VCon (.Data (minusModel A B)), .VCon (.Data (.Map B)), .VCon (.Data (.Map A)), minusV, spV, mcV,
    hlV, fpV]

def envG3 (A B : List (Data × Data)) : Environment := .VLam getBody (envR3 A B) :: envR3 A B

theorem leaf3_big (A B : List (Data × Data)) (hA : ∀ p ∈ A, isValPair p)
    (hB : ∀ p ∈ B, isValPair p) :
    Big default (.eval [] (applyParams leaf3Body [.Const (.Data (.Map A)), .Const (.Data (.Map B))]))
      (.VCon (.Bool (PlutusCore.Integer.equalsInteger (glModel (minusPairs A B))
        (PlutusCore.Integer.subtractInteger (glModel A) (glModel B))))) := by
  have hm := minus_body A B hA hB
  have g1 := gl_spec (envG3 A B) (envR3 A B) rfl rfl rfl rfl rfl (minusPairs A B)
    (minusPairs_val hA hB)
  have g2 := gl_spec (envG3 A B) (envR3 A B) rfl rfl rfl rfl rfl A hA
  have g3 := gl_spec (envG3 A B) (envR3 A B) rfl rfl rfl rfl rfl B hB
  big_step [hm, g1, g2, g3]

theorem leaf1_big (A B : List (Data × Data)) (hA : ∀ p ∈ A, isValPair p)
    (hB : ∀ p ∈ B, isValPair p) :
    Big default (.eval [] (applyParams leaf1Body [.Const (.Data (.Map A)), .Const (.Data (.Map B))]))
      (.VCon (.Bool true)) := by
  have hm := minus_body A B hA hB
  big_step [hm]

/-! The arithmetic: the lovelace of the difference is the difference of the lovelace. -/

theorem not_lt_empty (x : ByteString) : PlutusCore.ByteString.lessThanByteString x emptyBS = false := by
  simp only [PlutusCore.ByteString.lessThanByteString, decide_eq_false_iff_not]
  show ¬ x.data.data < ([] : List Char)
  exact List.not_lt_nil _

theorem eq_of_equals {x y : ByteString} (h : PlutusCore.ByteString.equalsByteString x y = true) :
    x = y := by
  simpa [PlutusCore.ByteString.equalsByteString] using h

theorem cmp_less {x y : ByteString} (h : cmpTag x y = 0) : y ≠ emptyBS := by
  rintro rfl
  simp only [cmpTag, not_lt_empty, Bool.false_eq_true, ↓reduceIte] at h
  split at h <;> omega

theorem cmp_equal {x y : ByteString} (h : cmpTag x y = 1) : x = y := by
  unfold cmpTag at h
  split at h
  · exact absurd h (by decide)
  · split at h
    · exact eq_of_equals (by assumption)
    · exact absurd h (by decide)

theorem cmp_greater {x y : ByteString} (h : cmpTag x y = 2) : x ≠ emptyBS := by
  rintro rfl
  unfold cmpTag at h
  by_cases hy : y = emptyBS
  · subst hy
    simp [not_lt_empty, PlutusCore.ByteString.equalsByteString] at h
  · simp [empty_lt y hy] at h

theorem sub_def (a b : Int) : PlutusCore.Integer.subtractInteger a b = a - b := rfl

theorem equals_def (a b : Int) : PlutusCore.Integer.equalsInteger a b = (a == b) := rfl

/-- The amount of the empty token name, of the first tuple of tokens. -/
def tokGl : List Data → Int
  | x :: _ => if keyBytes x = emptyBS then amtOf x else 0
  | [] => 0

/-- No key of the list is the empty bytes. -/
def noEmpty (l : List Data) : Prop := ∀ e ∈ l, keyBytes e ≠ emptyBS

theorem tokGl_noEmpty {l : List Data} (h : noEmpty l) : tokGl l = 0 := by
  cases l with
  | nil => rfl
  | cons x t => simp [tokGl, h x (by simp)]

theorem mergeTokWith_noEmpty (x : Data) (t : List Data) (rest : List Data → List Data)
    (hx : isTok x ∧ keyBytes x ≠ emptyBS) (ht : ∀ e ∈ t, isTok e ∧ keyBytes e ≠ emptyBS)
    (hrest : ∀ r, (∀ y ∈ r, isTok y ∧ keyBytes y ≠ emptyBS) → noEmpty (rest r)) :
    ∀ r, (∀ y ∈ r, isTok y ∧ keyBytes y ≠ emptyBS) → noEmpty (mergeTokWith x t rest r) := by
  obtain ⟨⟨k1, n1, rfl⟩, hk1⟩ := hx
  intro r
  induction r with
  | nil =>
    intro _ e he
    simp only [mergeTokWith, List.map_cons, List.mem_cons, List.mem_map] at he
    rcases he with rfl | ⟨z, hz, rfl⟩
    · exact hk1
    · obtain ⟨⟨k, n, rfl⟩, hk⟩ := ht z hz
      exact hk
  | cons y u ih =>
    intro hr e he
    have hu : ∀ z ∈ u, isTok z ∧ keyBytes z ≠ emptyBS := fun z hz => hr z (by simp [hz])
    obtain ⟨⟨k2, n2, rfl⟩, hk2⟩ := hr y (by simp)
    simp only [mergeTokWith] at he
    split at he
    · rcases List.mem_cons.mp he with rfl | he
      · exact hk1
      · exact hrest _ hr e he
    · split at he
      · exact hrest _ hu e he
      · rcases List.mem_cons.mp he with rfl | he
        · exact hk1
        · exact hrest _ hu e he
    · rcases List.mem_cons.mp he with rfl | he
      · exact hk2
      · exact ih hu e he

theorem mergeTok_noEmpty : ∀ l r, (∀ x ∈ l, isTok x ∧ keyBytes x ≠ emptyBS) →
    (∀ y ∈ r, isTok y ∧ keyBytes y ≠ emptyBS) → noEmpty (mergeTok l r) := by
  intro l
  induction l with
  | nil =>
    intro r _ hr e he
    obtain ⟨z, hz, rfl⟩ := List.mem_map.mp he
    obtain ⟨⟨k, n, rfl⟩, hk⟩ := hr z hz
    exact hk
  | cons x t ih =>
    intro r hl hr
    exact mergeTokWith_noEmpty x t (mergeTok t) (hl x (by simp)) (fun z hz => hl z (by simp [hz]))
      (fun r hr' => ih r (fun z hz => hl z (by simp [hz])) hr') r hr

/-- The tail's keys are no empty bytes: as of a list whose keys ascend. -/
def tailNoEmpty (l : List Data) : Prop := noEmpty l.tail

theorem tokGl_merge (l r : List Data) (hl : ∀ e ∈ l, isTok e) (hr : ∀ e ∈ r, isTok e)
    (hlt : tailNoEmpty l) (hrt : tailNoEmpty r) :
    tokGl (mergeTok l r) = tokGl l - tokGl r := by
  cases l with
  | nil =>
    cases r with
    | nil => rfl
    | cons y u =>
      obtain ⟨k2, n2, rfl⟩ := hr y (by simp)
      simp only [mergeTok, List.map_cons, tokGl, neg, keyOf, keyBytes, amtOf]
      split <;> omega
  | cons x t =>
    obtain ⟨k1, n1, rfl⟩ := hl x (by simp)
    cases r with
    | nil =>
      simp only [mergeTok, mergeTokWith, List.map_cons, tokGl, pos, keyOf, keyBytes, amtOf]
      split <;> omega
    | cons y u =>
      obtain ⟨k2, n2, rfl⟩ := hr y (by simp)
      have ht : ∀ e ∈ t, isTok e ∧ keyBytes e ≠ emptyBS :=
        fun e he => ⟨hl e (by simp [he]), hlt e he⟩
      have hu : ∀ e ∈ u, isTok e ∧ keyBytes e ≠ emptyBS :=
        fun e he => ⟨hr e (by simp [he]), hrt e he⟩
      rcases cmpTag_cases k1 k2 with hc | hc | hc
      · have h2 := cmp_less hc
        simp only [mergeTok, mergeTokWith, keyBytes, hc, tokGl, pos, keyOf, amtOf, h2,
          if_false]
        split <;> omega
      · have h12 := cmp_equal hc
        subst h12
        simp only [mergeTok, mergeTokWith, keyBytes, amtOf, hc, sub_def, equals_def]
        by_cases hz : n1 - n2 = 0
        · simp only [hz, beq_self_eq_true, ↓reduceIte]
          rw [tokGl_noEmpty (mergeTok_noEmpty t u ht hu)]
          simp only [tokGl, keyBytes, amtOf]
          by_cases hk : k1 = emptyBS <;> simp only [hk, ↓reduceIte]
          · exact hz.symm
          · omega
        · have : (n1 - n2 == 0) = false := by simpa using hz
          simp only [this, Bool.false_eq_true, ↓reduceIte, tokGl, keyBytes, keyOf, amtOf]
          split <;> omega
      · have h1 := cmp_greater hc
        simp only [mergeTok, mergeTokWith, keyBytes, hc, tokGl, neg, keyOf, amtOf, h1,
          if_false]
        split <;> omega

/-- The lovelace of the merge of two values: of its first policy, its tokens a list of tuples. -/
def valGl : List Data → Int
  | x :: _ => if keyBytes x = emptyBS then tokGl (listOf x) else 0
  | [] => 0

/-- The lovelace of a value's policies as tuples. -/
def glTup : List Data → Int
  | x :: _ => if keyBytes x = emptyBS then tokGl ((innerOf x).map tuple) else 0
  | [] => 0

/-- A policy's tokens have no empty name past the first: as of a map whose keys ascend. -/
def innerTailNoEmpty (x : Data) : Prop := tailNoEmpty ((innerOf x).map tuple)

theorem mergeValWith_noEmpty (x : Data) (t : List Data) (rest : List Data → List Data)
    (hx : isVal x ∧ keyBytes x ≠ emptyBS) (ht : ∀ e ∈ t, isVal e ∧ keyBytes e ≠ emptyBS)
    (hrest : ∀ r, (∀ y ∈ r, isVal y ∧ keyBytes y ≠ emptyBS) → noEmpty (rest r)) :
    ∀ r, (∀ y ∈ r, isVal y ∧ keyBytes y ≠ emptyBS) → noEmpty (mergeValWith x t rest r) := by
  obtain ⟨⟨k1, ps1, rfl, _⟩, hk1⟩ := hx
  intro r
  induction r with
  | nil =>
    intro _ e he
    simp only [mergeValWith, List.map_cons, List.mem_cons, List.mem_map] at he
    rcases he with rfl | ⟨z, hz, rfl⟩
    · exact hk1
    · obtain ⟨⟨k, ps, rfl, _⟩, hk⟩ := ht z hz
      exact hk
  | cons y u ih =>
    intro hr e he
    have hu : ∀ z ∈ u, isVal z ∧ keyBytes z ≠ emptyBS := fun z hz => hr z (by simp [hz])
    obtain ⟨⟨k2, ps2, rfl, _⟩, hk2⟩ := hr y (by simp)
    simp only [mergeValWith] at he
    split at he
    · rcases List.mem_cons.mp he with rfl | he
      · exact hk1
      · exact hrest _ hr e he
    · split at he
      · exact hrest _ hu e he
      · rcases List.mem_cons.mp he with rfl | he
        · exact hk1
        · exact hrest _ hu e he
    · rcases List.mem_cons.mp he with rfl | he
      · exact hk2
      · exact ih hu e he

theorem mergeVal_noEmpty : ∀ l r, (∀ x ∈ l, isVal x ∧ keyBytes x ≠ emptyBS) →
    (∀ y ∈ r, isVal y ∧ keyBytes y ≠ emptyBS) → noEmpty (mergeVal l r) := by
  intro l
  induction l with
  | nil =>
    intro r _ hr e he
    obtain ⟨z, hz, rfl⟩ := List.mem_map.mp he
    obtain ⟨⟨k, ps, rfl, _⟩, hk⟩ := hr z hz
    exact hk
  | cons x t ih =>
    intro r hl hr
    exact mergeValWith_noEmpty x t (mergeVal t) (hl x (by simp)) (fun z hz => hl z (by simp [hz]))
      (fun r hr' => ih r (fun z hz => hl z (by simp [hz])) hr') r hr

theorem valGl_noEmpty {l : List Data} (h : noEmpty l) : valGl l = 0 := by
  cases l with
  | nil => rfl
  | cons x t => simp [valGl, h x (by simp)]

theorem valGl_merge (L R : List Data) (hL : ∀ x ∈ L, isVal x) (hR : ∀ x ∈ R, isVal x)
    (hLt : tailNoEmpty L) (hRt : tailNoEmpty R)
    (hLi : ∀ x ∈ L, innerTailNoEmpty x) (hRi : ∀ x ∈ R, innerTailNoEmpty x) :
    valGl (mergeVal L R) = glTup L - glTup R := by
  cases L with
  | nil =>
    cases R with
    | nil => simp [mergeVal, valGl, glTup]
    | cons y u =>
      have hyi := hRi y (by simp)
      obtain ⟨k2, ps2, rfl, hps2⟩ := hR y (by simp)
      have hm := tokGl_merge [] (ps2.map tuple) (by simp) (tuple_tok hps2) (by simp [tailNoEmpty, noEmpty]) hyi
      simp only [mergeVal, List.map_cons, valGl, glTup, greaterV, keyOf, keyBytes, listOf, innerOf,
        hm]
      by_cases hk : k2 = emptyBS <;> simp [hk, tokGl]
  | cons x t =>
    have hxi := hLi x (by simp)
    obtain ⟨k1, ps1, rfl, hps1⟩ := hL x (by simp)
    cases R with
    | nil =>
      have hm := tokGl_merge (ps1.map tuple) [] (tuple_tok hps1) (by simp) hxi (by simp [tailNoEmpty, noEmpty])
      simp only [mergeVal, mergeValWith, List.map_cons, valGl, glTup, lessV, keyOf, keyBytes, listOf,
        innerOf, hm]
      by_cases hk : k1 = emptyBS <;> simp [hk, tokGl]
    | cons y u =>
      have hyi := hRi y (by simp)
      obtain ⟨k2, ps2, rfl, hps2⟩ := hR y (by simp)
      have ht : ∀ e ∈ t, isVal e ∧ keyBytes e ≠ emptyBS :=
        fun e he => ⟨hL e (by simp [he]), hLt e he⟩
      have hu : ∀ e ∈ u, isVal e ∧ keyBytes e ≠ emptyBS :=
        fun e he => ⟨hR e (by simp [he]), hRt e he⟩
      have hm1 := tokGl_merge (ps1.map tuple) [] (tuple_tok hps1) (by simp) hxi
        (by simp [tailNoEmpty, noEmpty])
      have hm2 := tokGl_merge [] (ps2.map tuple) (by simp) (tuple_tok hps2)
        (by simp [tailNoEmpty, noEmpty]) hyi
      have hm12 := tokGl_merge (ps1.map tuple) (ps2.map tuple) (tuple_tok hps1) (tuple_tok hps2) hxi
        hyi
      rcases cmpTag_cases k1 k2 with hc | hc | hc
      · have h2 := cmp_less hc
        simp only [mergeVal, mergeValWith, keyBytes, hc, valGl, glTup, lessV, keyOf, listOf, innerOf,
          hm1, h2, if_false]
        by_cases hk : k1 = emptyBS <;> simp [hk, tokGl]
      · have h12 := cmp_equal hc
        subst h12
        simp only [mergeVal, mergeValWith, keyBytes, hc, innerOf]
        by_cases hz : PlutusCore.List.nullList (mergeTok (ps1.map tuple) (ps2.map tuple)) = true
        · simp only [hz, ↓reduceIte]
          rw [valGl_noEmpty (mergeVal_noEmpty t u ht hu)]
          have he : mergeTok (ps1.map tuple) (ps2.map tuple) = [] := by
            simpa [PlutusCore.List.nullList] using hz
          rw [he] at hm12
          simp only [glTup, keyBytes, innerOf]
          by_cases hk : k1 = emptyBS <;> simp only [hk, ↓reduceIte]
          · simp only [tokGl] at hm12
            exact hm12
          · rfl
        · simp only [hz, Bool.false_eq_true, ↓reduceIte, valGl, glTup, keyBytes, keyOf, listOf,
            innerOf, hm12]
          by_cases hk : k1 = emptyBS <;> simp only [hk, ↓reduceIte]
          show (0 : Int) = 0 - 0
          rfl
      · have h1 := cmp_greater hc
        simp only [mergeVal, mergeValWith, keyBytes, hc, valGl, glTup, greaterV, keyOf, listOf,
          innerOf, hm2, h1, if_false]
        by_cases hk : k2 = emptyBS <;> simp [hk, tokGl]

/-! From the models to `getLovelace`'s, and from the type's predicates to the models'. -/

theorem innerGl_tuples (l : List Data) (hl : ∀ e ∈ l, isTok e) : innerGl (l.map pairOf) = tokGl l := by
  cases l with
  | nil => rfl
  | cons e t =>
    obtain ⟨k, n, rfl⟩ := hl e (by simp)
    simp [innerGl, tokGl, pairOf, keyBytes, amtOf]

theorem innerGl_pairs (ps : List (Data × Data)) (h : ∀ p ∈ ps, isTokPair p) :
    innerGl ps = tokGl (ps.map tuple) := by
  cases ps with
  | nil => rfl
  | cons p t =>
    obtain ⟨k, n, rfl⟩ := h p (by simp)
    simp [innerGl, tokGl, tuple, keyBytes, amtOf]

theorem glModel_minus (A B : List (Data × Data)) (hA : ∀ p ∈ A, isValPair p)
    (hB : ∀ p ∈ B, isValPair p) :
    glModel (minusPairs A B) = valGl (mergeVal (A.map tuple) (B.map tuple)) := by
  have hv := mergeVal_val _ _ (tuple_val hA) (tuple_val hB)
  unfold minusPairs
  cases hm : mergeVal (A.map tuple) (B.map tuple) with
  | nil => rfl
  | cons x rest =>
    rw [hm] at hv
    obtain ⟨k, l, rfl, hl⟩ := hv x (by simp)
    simp only [List.map_cons, outConv, keyOf, listOf, pairOf, glModel, valGl, keyBytes,
      innerGl_tuples l hl]

theorem glModel_tup (A : List (Data × Data)) (hA : ∀ p ∈ A, isValPair p) :
    glModel A = glTup (A.map tuple) := by
  cases A with
  | nil => rfl
  | cons p t =>
    obtain ⟨k, ps, rfl, hps⟩ := hA p (by simp)
    simp only [List.map_cons, glModel, glTup, tuple, keyBytes, innerOf, innerGl_pairs ps hps]

open LinearVesting.Generated.value_minus_stated_contract_holds

theorem ascFrom_noEmpty : ∀ (prev : ByteString) (ps : List (Data × Data)),
    scalusAscendingBytesFrom prev ps = true → noEmpty (ps.map tuple) := by
  intro prev ps
  induction ps generalizing prev with
  | nil => intro _ e he; simp at he
  | cons p t ih =>
    obtain ⟨k, v⟩ := p
    intro h e he
    cases k with
    | B b =>
      simp only [scalusAscendingBytesFrom, Bool.and_eq_true, decide_eq_true_eq] at h
      rcases List.mem_cons.mp he with rfl | he
      · rintro (hb : b = emptyBS)
        subst hb
        have := not_lt_empty prev
        simp only [PlutusCore.ByteString.lessThanByteString, decide_eq_false_iff_not] at this
        exact this h.1
      · exact ih b h.2 e he
    | _ => simp [scalusAscendingBytesFrom] at h

theorem asc_tail (ps : List (Data × Data)) (h : scalusAscendingBytes ps = true) :
    tailNoEmpty (ps.map tuple) := by
  cases ps with
  | nil => intro e he; simp at he
  | cons p t =>
    obtain ⟨k, v⟩ := p
    cases k with
    | B a => exact ascFrom_noEmpty a t (by simpa [scalusAscendingBytes] using h)
    | _ => simp [scalusAscendingBytes] at h

theorem tokPairs_of (ps : List (Data × Data)) (h : scalusAllPairs_40dd2b2fc5848d71 ps = true) :
    ∀ p ∈ ps, isTokPair p := by
  induction ps with
  | nil => intro p hp; simp at hp
  | cons q t ih =>
    obtain ⟨k, v⟩ := q
    simp only [scalusAllPairs_40dd2b2fc5848d71, Bool.and_eq_true] at h
    intro p hp
    rcases List.mem_cons.mp hp with rfl | hp
    · cases k <;> cases v <;> simp_all [scalusIsB, scalusIsI] <;> exact ⟨_, _, rfl⟩
    · exact ih h.2 p hp

/-- What the type's predicate gives of a pair of a value: its policy, its tokens' pairs, and that
    its tokens' names ascend. -/
def goodPair (p : Data × Data) : Prop :=
  ∃ k ps, p = (.B k, .Map ps) ∧ (∀ q ∈ ps, isTokPair q) ∧ scalusAscendingBytes ps = true

theorem goodPairs_of (ps : List (Data × Data)) (h : scalusAllPairs_0e0b932a4ef47b29 ps = true) :
    ∀ p ∈ ps, goodPair p := by
  induction ps with
  | nil => intro p hp; simp at hp
  | cons q t ih =>
    obtain ⟨k, v⟩ := q
    simp only [scalusAllPairs_0e0b932a4ef47b29, Bool.and_eq_true] at h
    intro p hp
    rcases List.mem_cons.mp hp with rfl | hp
    · obtain ⟨⟨hk, hv⟩, _⟩ := h
      cases k with
      | B b =>
        cases v with
        | Map ps =>
          have hv' : scalusAllPairs_40dd2b2fc5848d71 ps = true ∧ scalusAscendingBytes ps = true := by
            simpa [Bool.and_eq_true] using hv
          exact ⟨b, ps, rfl, tokPairs_of ps hv'.1, hv'.2⟩
        | _ => simp at hv
      | _ => simp [scalusIsB] at hk
    · exact ih h.2 p hp

theorem valPairs_of {ps : List (Data × Data)} (h : ∀ p ∈ ps, goodPair p) : ∀ p ∈ ps, isValPair p := by
  intro p hp
  obtain ⟨k, qs, rfl, hqs, _⟩ := h p hp
  exact ⟨k, qs, rfl, hqs⟩

theorem innerTail_of {ps : List (Data × Data)} (h : ∀ p ∈ ps, goodPair p) :
    ∀ x ∈ ps.map tuple, innerTailNoEmpty x := by
  intro x hx
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hx
  obtain ⟨k, qs, rfl, _, hasc⟩ := h p hp
  exact asc_tail qs hasc

/-- `Value.minus`'s contract, as Scalus states it of every two values of the type: the program
    returns, and the lovelace of what it returns is the difference of the lovelace. -/
theorem minus_contract : statement := by
  intro x0 h0 x1 h1 _
  simp only [Bool.and_eq_true] at h0 h1
  have g0 := goodPairs_of x0 h0.1
  have g1 := goodPairs_of x1 h1.1
  have hA := valPairs_of g0
  have hB := valPairs_of g1
  refine ⟨fun _ => returns_of_big (leaf1_big x0 x1 hA hB), fun _ => ?_⟩
  have h := leaf3_big x0 x1 hA hB
  have hgl : glModel (minusPairs x0 x1) = glModel x0 - glModel x1 := by
    rw [glModel_minus x0 x1 hA hB, glModel_tup x0 hA, glModel_tup x1 hB]
    exact valGl_merge _ _ (tuple_val hA) (tuple_val hB) (asc_tail x0 h0.2) (asc_tail x1 h1.2)
      (innerTail_of g0) (innerTail_of g1)
  have hb : PlutusCore.Integer.equalsInteger (glModel (minusPairs x0 x1))
      (PlutusCore.Integer.subtractInteger (glModel x0) (glModel x1)) = true := by
    rw [hgl, equals_def, sub_def]
    exact beq_self_eq_true _
  rw [hb] at h
  exact returns_of_big h

end LinearVesting.Proofs.ValueMinus
