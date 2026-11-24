package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import TransactionWitnessSet.given

class NativeScriptsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    /** The UTxO that the transactions of this suite spend, locked by `script`. */
    private def lockedBy(script: Timelock): Utxos =
        Map(scriptUtxo(input(0), script.scriptHash))

    /** A transaction with `body` that provides `script` and a signature of each of `signers` over
      * its id.
      */
    private def withScript(body: TransactionBody, script: Timelock, signers: Party*) = {
        val unsigned = Transaction(
          body,
          TransactionWitnessSet(nativeScripts = TaggedSortedMap(Script.Native(script)))
        )
        signers.foldLeft(unsigned)((tx, party) => party.signer.sign(tx))
    }

    private val spendBody: TransactionBody = emptyBody.copy(inputs = TaggedSortedSet(input(0)))

    /** A transaction that spends the UTxO of `script` and is signed by `signers`. */
    private def spending(script: Timelock, signers: Party*): Transaction =
        withScript(spendBody, script, signers*)

    private def assertNativeScriptsFailure(result: NativeScriptsValidator.Result): Unit = {
        assert(result.isLeft)
        assert(
          result.left.exists(_.isInstanceOf[TransactionException.NativeScriptsException])
        )
    }

    test("NativeScriptsValidator success with no native scripts") {
        val utxo = Map(input(0) -> Output(keyAddress(1), Value(Coin(1000000L))))
        val transaction = Transaction(spendBody)

        val result = validate(NativeScriptsValidator, utxo, transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator success with valid Signature native script") {
        val nativeScript = Timelock.Signature(Alice.addrKeyHash)

        val result =
            validate(NativeScriptsValidator, lockedBy(nativeScript), spending(nativeScript, Alice))
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with invalid Signature native script") {
        val nativeScript = Timelock.Signature(Alice.addrKeyHash)

        val result =
            validate(NativeScriptsValidator, lockedBy(nativeScript), spending(nativeScript))
        assertNativeScriptsFailure(result)
    }

    test("NativeScriptsValidator success with AllOf native script") {
        val nativeScript = Timelock.AllOf(
          IndexedSeq(Timelock.Signature(Alice.addrKeyHash), Timelock.Signature(Bob.addrKeyHash))
        )

        val result = validate(
          NativeScriptsValidator,
          lockedBy(nativeScript),
          spending(nativeScript, Alice, Bob)
        )
        assert(result.isRight)
    }

    test(
      "NativeScriptsValidator failure with AllOf native script missing one signature"
    ) {
        val nativeScript = Timelock.AllOf(
          IndexedSeq(Timelock.Signature(Alice.addrKeyHash), Timelock.Signature(Bob.addrKeyHash))
        )

        val result =
            validate(NativeScriptsValidator, lockedBy(nativeScript), spending(nativeScript, Alice))
        assertNativeScriptsFailure(result)
    }

    test("NativeScriptsValidator success with AnyOf native script") {
        val nativeScript = Timelock.AnyOf(
          IndexedSeq(Timelock.Signature(Alice.addrKeyHash), Timelock.Signature(Bob.addrKeyHash))
        )

        val result =
            validate(NativeScriptsValidator, lockedBy(nativeScript), spending(nativeScript, Alice))
        assert(result.isRight)
    }

    test(
      "NativeScriptsValidator failure with AnyOf native script no matching signatures"
    ) {
        val nativeScript = Timelock.AnyOf(
          IndexedSeq(Timelock.Signature(Alice.addrKeyHash), Timelock.Signature(Bob.addrKeyHash))
        )

        val result =
            validate(NativeScriptsValidator, lockedBy(nativeScript), spending(nativeScript))
        assertNativeScriptsFailure(result)
    }

    test("NativeScriptsValidator success with MOf native script") {
        val nativeScript = Timelock.MOf(
          2,
          IndexedSeq(
            Timelock.Signature(Alice.addrKeyHash),
            Timelock.Signature(Bob.addrKeyHash),
            Timelock.Signature(Charles.addrKeyHash)
          )
        )

        val result = validate(
          NativeScriptsValidator,
          lockedBy(nativeScript),
          spending(nativeScript, Alice, Bob)
        )
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with MOf native script insufficient signatures") {
        val nativeScript = Timelock.MOf(
          2,
          IndexedSeq(
            Timelock.Signature(Alice.addrKeyHash),
            Timelock.Signature(Bob.addrKeyHash),
            Timelock.Signature(Charles.addrKeyHash)
          )
        )

        val result =
            validate(NativeScriptsValidator, lockedBy(nativeScript), spending(nativeScript, Alice))
        assertNativeScriptsFailure(result)
    }

    test("NativeScriptsValidator success with TimeStart script within validity interval") {
        val nativeScript = Timelock.TimeStart(100L)
        val transaction =
            withScript(spendBody.copy(validityStartSlot = Some(150L)), nativeScript)

        val result = validate(NativeScriptsValidator, lockedBy(nativeScript), transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with TimeStart script outside validity interval") {
        val nativeScript = Timelock.TimeStart(100L)
        val transaction =
            withScript(spendBody.copy(validityStartSlot = Some(50L)), nativeScript)

        val result = validate(NativeScriptsValidator, lockedBy(nativeScript), transaction)
        assertNativeScriptsFailure(result)
    }

    test("NativeScriptsValidator success with TimeExpire script within validity interval") {
        val nativeScript = Timelock.TimeExpire(200L)
        val transaction = withScript(spendBody.copy(ttl = Some(150L)), nativeScript)

        val result = validate(NativeScriptsValidator, lockedBy(nativeScript), transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with TimeExpire script outside validity interval") {
        val nativeScript = Timelock.TimeExpire(100L)
        val transaction = withScript(spendBody.copy(ttl = Some(150L)), nativeScript)

        val result = validate(NativeScriptsValidator, lockedBy(nativeScript), transaction)
        assertNativeScriptsFailure(result)
    }

    test("NativeScriptsValidator success with empty AllOf script") {
        val nativeScript = Timelock.AllOf(IndexedSeq.empty)

        val result =
            validate(NativeScriptsValidator, lockedBy(nativeScript), spending(nativeScript))
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with empty AnyOf script") {
        val nativeScript = Timelock.AnyOf(IndexedSeq.empty)

        val result =
            validate(NativeScriptsValidator, lockedBy(nativeScript), spending(nativeScript))
        assertNativeScriptsFailure(result)
    }

    test("NativeScriptsValidator success with MOf script where m=0") {
        val nativeScript = Timelock.MOf(0, IndexedSeq(Timelock.Signature(Alice.addrKeyHash)))

        val result =
            validate(NativeScriptsValidator, lockedBy(nativeScript), spending(nativeScript))
        assert(result.isRight)
    }
}
