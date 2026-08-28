package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{AssocMap, List as PList, Option as POption}
import scalus.cardano.onchain.plutus.v3
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.ByteString.given
import scalus.uplc.builtin.Data.toData
import scalus.uplc.PlutusV3
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.Result
import scalus.testing.kit.EvalTestKit

/** Defect 2: redeemer keys are positional, ordered by (constructor, AsIx), so no content-based
  * `Ord` can track them. `redeemers` is therefore an `AssocMap` with a linear `Eq` lookup, as
  * `PlutusTx.AssocMap` and Aiken's `Pairs` are.
  *
  * The golden corpus cannot cover this: its generator emits only `Spending` purposes, so all its
  * redeemer entries share a constructor and no ordering difference can show. Hence these tests.
  */
class RedeemersAssocMapTest extends AnyFunSuite with EvalTestKit {

    private val ref = v3.TxOutRef(
      v3.TxId(
        ByteString.fromHex("abababababababababababababababababababababababababababababababab")
      ),
      0
    )
    private val spending = v3.ScriptPurpose.Spending(ref)
    private val minting = v3.ScriptPurpose.Minting(
      ByteString.fromHex("cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd")
    )

    /** Ledger order for a spend+mint transaction: ConwaySpending before ConwayMinting
      * (Conway/Scripts.hs:202-209). Scalus's Ord[ScriptPurpose] disagrees, ranking Minting first,
      * which is exactly why a SortedMap lookup used to miss.
      */
    private def deliveredOrder: AssocMap[v3.ScriptPurpose, Data] =
        AssocMap.unsafeFromList(
          PList.Cons(
            (spending, BigInt(1).toData),
            PList.Cons((minting, BigInt(2).toData), PList.Nil)
          )
        )

    test("off-chain: both redeemers are found in ledger order") {
        assert(deliveredOrder.get(spending) == POption.Some(BigInt(1).toData))
        assert(
          deliveredOrder.get(minting) == POption.Some(BigInt(2).toData),
          "Minting is present but was not found - the spend+mint case is still broken"
        )
    }

    test("on-chain: the lookup finds Minting in a ledger-ordered map") {
        val compiled = PlutusV3.compile { (d: Data) =>
            val m = d.to[AssocMap[v3.ScriptPurpose, Data]]
            m.get(
              v3.ScriptPurpose.Minting(
                ByteString.fromHex("cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd")
              )
            ).isDefined
        }
        val applied = compiled.program.term $ deliveredOrder.toData.asTerm
        applied.evaluateDebug match
            case Result.Success(term, _, _, _) =>
                assert(
                  term.toString.contains("true"),
                  s"on-chain lookup of a present Minting redeemer returned: $term"
                )
            case Result.Failure(e, _, _, _) => fail(s"evaluation failed: $e")
    }

    test("lookup is not order-dependent: the reversed order also works") {
        val reversed = AssocMap.unsafeFromList(
          PList.Cons(
            (minting, BigInt(2).toData),
            PList.Cons((spending, BigInt(1).toData), PList.Nil)
          )
        )
        assert(reversed.get(spending) == POption.Some(BigInt(1).toData))
        assert(reversed.get(minting) == POption.Some(BigInt(2).toData))
    }
}
