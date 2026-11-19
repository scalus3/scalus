import Amm.Generated.amm_redeem_stated_contract_holds

/-!
Proofs written by hand of statements about a redemption of the AMM's liquidity.

A statement is a module that Scalus writes from the Scala sources, under `Amm/Generated`:
`statement : Prop`, over the programs of the statement's tests as they are compiled now, and the
axiom `proved : statement`, the result of the proof Scalus made of it. A theorem here may use that
result as a lemma, and then rests on it: `#print axioms` names the axiom.
-/

namespace Amm.Proofs.Redeem

open Amm.Generated

/-- The redemption's contract, as Blaster proved it: a redemption of at most the supply leaves no
    reserve negative, and every LP token left is backed by as much of each reserve as before. -/
theorem redeem_contract : amm_redeem_stated_contract_holds.statement :=
  amm_redeem_stated_contract_holds.proved

end Amm.Proofs.Redeem
