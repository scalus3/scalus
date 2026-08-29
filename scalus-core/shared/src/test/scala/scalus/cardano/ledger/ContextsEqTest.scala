package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.===
import scalus.cardano.onchain.plutus.v1
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.given

/** Regression tests for the shadowed-binder bug in the hand-written `Eq` instances.
  *
  * `Eq[DCert]` and `Eq[v1.ScriptPurpose]` bound the inner pattern to the same name as the outer
  * one, so every field comparison was a self-comparison and both returned `true` for any two values
  * sharing a constructor.
  *
  * This is invisible on-chain - `===` there lowers to structural comparison and never calls the
  * instance body, which was confirmed by compiling one expression and reading both paths - so it
  * has to be asserted on the JVM, where the body does run.
  */
class ContextsEqTest extends AnyFunSuite {

    private val aa = ByteString.fromHex("aa" * 28)
    private val bb = ByteString.fromHex("bb" * 28)

    private def keyCred(h: ByteString) =
        v1.StakingCredential.StakingHash(v1.Credential.PubKeyCredential(v1.PubKeyHash(h)))

    test("Eq[DCert] distinguishes different values of the same constructor") {
        assert(!(v1.DCert.DelegRegKey(keyCred(aa)) === v1.DCert.DelegRegKey(keyCred(bb))))
        assert(v1.DCert.DelegRegKey(keyCred(aa)) === v1.DCert.DelegRegKey(keyCred(aa)))
        assert(
          !(v1.DCert.PoolRetire(v1.PubKeyHash(aa), 1) === v1.DCert.PoolRetire(v1.PubKeyHash(aa), 2))
        )
        assert(
          !(v1.DCert.PoolRegister(v1.PubKeyHash(aa), v1.PubKeyHash(aa))
              === v1.DCert.PoolRegister(v1.PubKeyHash(aa), v1.PubKeyHash(bb)))
        )
    }

    test("Eq[DCert] still distinguishes different constructors") {
        assert(!(v1.DCert.DelegRegKey(keyCred(aa)) === v1.DCert.DelegDeRegKey(keyCred(aa))))
        assert(v1.DCert.Genesis === v1.DCert.Genesis)
        assert(!(v1.DCert.Genesis === v1.DCert.Mir))
    }

    test("Eq[v1.ScriptPurpose] distinguishes different values of the same constructor") {
        assert(!(v1.ScriptPurpose.Minting(aa) === v1.ScriptPurpose.Minting(bb)))
        assert(v1.ScriptPurpose.Minting(aa) === v1.ScriptPurpose.Minting(aa))
        assert(
          !(v1.ScriptPurpose.Rewarding(keyCred(aa)) === v1.ScriptPurpose.Rewarding(keyCred(bb)))
        )
        val ref1 = v1.TxOutRef(v1.TxId(ByteString.fromHex("ab" * 32)), 0)
        val ref2 = v1.TxOutRef(v1.TxId(ByteString.fromHex("ab" * 32)), 1)
        assert(!(v1.ScriptPurpose.Spending(ref1) === v1.ScriptPurpose.Spending(ref2)))
    }

    test("Eq[v1.ScriptPurpose] still distinguishes different constructors") {
        assert(!(v1.ScriptPurpose.Minting(aa) === v1.ScriptPurpose.Rewarding(keyCred(aa))))
    }
}
