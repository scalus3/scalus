package scalus.examples.vesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v3.{ScriptContext, ScriptInfo, TxId, TxInfo, TxOutRef}
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.{FunctionDef, Representation}
import scalus.verify.uplcblaster.LeanProofs

/** What [[VestingVerificationTest]] rests on, checked without Lean: the example's Lean workspace,
  * the specification's erasure, and the contexts the statements build.
  */
class VestingVerificationSupportTest extends AnyFunSuite {
    import VestingVerificationTest.*

    test("the example's Lean workspace has the Lean and the packages of Scalus's library") {
        // The two workspaces share the clones of the packages and what is built of them. A
        // manifest that names another revision would check it out for both, and another Lean
        // would build them again, each time the other workspace is used.
        val library = LeanProofs.librarySources
        assert(LeanProofs.pinned(library).nonEmpty)
        assert(LeanProofs.pinned(workspace) == LeanProofs.pinned(library))
        assert(LeanProofs.toolchain(workspace) == LeanProofs.toolchain(library))
    }

    test("the specification is not part of the script") {
        // The clause is checked where the code runs as Scala,
        val halfway = VestingValidator.linearVesting(
          config(BigInt(0), BigInt(10), BigInt(100)),
          BigInt(5)
        )
        assert(halfway == BigInt(50))
        // and kept in the SIR, with the function that checks it and that function's error.
        val sir = FunctionDef(VestingValidator.linearVesting)(Representation.Sir).toString
        assert(sir.contains("Spec$.ensuring"))
        assert(sir.contains("a postcondition does not hold"))
        // It is gone from the program. Compiled with error traces, a program has the message of
        // every error it can raise, as the validator has its own, and none for a clause. The
        // tactic's options leave messages out, so its programs would not show one.
        given Options = Options.debug
        val schedule = PlutusV3.compile(VestingValidator.linearVesting).program.show
        val script = PlutusV3.compile(VestingValidator.validate).program.show
        assert(script.contains(VestingValidator.DatumNotFound))
        assert(!schedule.contains("postcondition"))
        assert(!script.contains("postcondition"))
    }

    test("the contexts built from Data are the ledger's encoding") {
        // The statements of `VestingVerificationTest` say that the validator fails, which a malformed context would make
        // true for the wrong reason.
        val ref = TxOutRef(TxId(hex"00"), 0)
        val datum = config(BigInt(1), BigInt(2), BigInt(3)).toData
        val redeemer = Action(BigInt(4)).toData
        def expected(datum: prelude.Option[Data]) =
            ScriptContext(
              TxInfo.placeholder,
              redeemer,
              ScriptInfo.SpendingScript(ref, datum)
            ).toData
        def built(datum: Data) =
            scriptContext(TxInfo.placeholder.toData, redeemer, spendingInfo(ref.toData, datum))
        assert(built(noDatum) == expected(prelude.Option.None))
        assert(built(someDatum(datum)) == expected(prelude.Option.Some(datum)))
    }
}
