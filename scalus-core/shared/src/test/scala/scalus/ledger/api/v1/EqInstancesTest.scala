package scalus.cardano.onchain.plutus.v1

import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.kit.EvalTestKit
import scalus.uplc.builtin.ByteString.hex

/** Regression: the `Eq[DCert]` and `Eq[ScriptPurpose]` instances used to compare every field to
  * itself (the inner pattern binder shadowed the outer one), so any two values with the same
  * constructor compared equal off-chain. On-chain the lowering replaces `Eq` with structural
  * `equalsData`, which is why the bug never showed there; these tests cover both.
  */
class EqInstancesTest extends AnyFunSuite with EvalTestKit {

    private val credA =
        StakingCredential.StakingHash(Credential.PubKeyCredential(PubKeyHash(hex"01")))
    private val credB =
        StakingCredential.StakingHash(Credential.PubKeyCredential(PubKeyHash(hex"02")))
    private val pkhA = PubKeyHash(hex"aa")
    private val pkhB = PubKeyHash(hex"bb")

    test("Eq[DCert] distinguishes values that differ in one field") {
        assert(DCert.DelegRegKey(credA) === DCert.DelegRegKey(credA))
        assert(!(DCert.DelegRegKey(credA) === DCert.DelegRegKey(credB)))
        assert(!(DCert.DelegDeRegKey(credA) === DCert.DelegDeRegKey(credB)))
        assert(!(DCert.DelegDelegate(credA, pkhA) === DCert.DelegDelegate(credA, pkhB)))
        assert(!(DCert.DelegDelegate(credA, pkhA) === DCert.DelegDelegate(credB, pkhA)))
        assert(!(DCert.PoolRegister(pkhA, pkhA) === DCert.PoolRegister(pkhA, pkhB)))
        assert(!(DCert.PoolRetire(pkhA, 1) === DCert.PoolRetire(pkhA, 2)))
        assert(!(DCert.DelegRegKey(credA) === DCert.DelegDeRegKey(credA)))
        assert(DCert.Genesis === DCert.Genesis)
        assert(!(DCert.Genesis === DCert.Mir))

        assertEval(
          !(DCert.DelegRegKey(
            StakingCredential.StakingHash(Credential.PubKeyCredential(PubKeyHash(hex"01")))
          ) === DCert.DelegRegKey(
            StakingCredential.StakingHash(Credential.PubKeyCredential(PubKeyHash(hex"02")))
          ))
        )
        assertEval(
          !(DCert.PoolRetire(PubKeyHash(hex"aa"), 1) === DCert.PoolRetire(PubKeyHash(hex"aa"), 2))
        )
    }

    test("Eq[ScriptPurpose] distinguishes values that differ in one field") {
        val refA = TxOutRef(TxId(hex"00"), 0)
        val refB = TxOutRef(TxId(hex"00"), 1)
        assert(ScriptPurpose.Minting(hex"aa") === ScriptPurpose.Minting(hex"aa"))
        assert(!(ScriptPurpose.Minting(hex"aa") === ScriptPurpose.Minting(hex"bb")))
        assert(!(ScriptPurpose.Spending(refA) === ScriptPurpose.Spending(refB)))
        assert(!(ScriptPurpose.Rewarding(credA) === ScriptPurpose.Rewarding(credB)))
        assert(
          !(ScriptPurpose.Certifying(DCert.DelegRegKey(credA)) ===
              ScriptPurpose.Certifying(DCert.DelegRegKey(credB)))
        )
        assert(!(ScriptPurpose.Minting(hex"aa") === ScriptPurpose.Rewarding(credA)))

        assertEval(!(ScriptPurpose.Minting(hex"aa") === ScriptPurpose.Minting(hex"bb")))
        assertEval(
          !(ScriptPurpose.Spending(TxOutRef(TxId(hex"00"), 0)) ===
              ScriptPurpose.Spending(TxOutRef(TxId(hex"00"), 1)))
        )
    }
}
