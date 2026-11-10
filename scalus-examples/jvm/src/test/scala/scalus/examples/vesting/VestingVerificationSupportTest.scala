package scalus.examples.vesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.verify.{FunctionDef, Representation}
import scalus.verify.lean.{LeanTypes, LeanWorkspace}
import scalus.verify.uplcblaster.LeanProofs

/** What [[VestingVerificationTest]] rests on, checked without Lean: the example's Lean workspace,
  * and the specification's erasure.
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

    test("the workspace has the Lean types of the contract's Scala types") {
        // What a proof written in Lean names the datum, the redeemer and the script context by:
        // `LinearVesting/Generated/Types.lean`, which `lake build` there builds. It is written
        // from the Scala types, so it is written again here where they have changed, and the
        // test fails once: the file is to be committed with the change.
        val types = LeanTypes.of(
          List(
            LeanTypes.typeOf[Config],
            LeanTypes.typeOf[Action],
            LeanTypes.typeOf[ScriptContext]
          )
        )
        val (module, file) = LeanWorkspace.generated(workspace, "Types")
        assert(module == "LinearVesting.Generated.Types")
        assert(types.codec(LeanTypes.typeOf[Config]).map(_.leanType).contains("Config"))
        assert(types.untyped.isEmpty, types.untyped)
        val written = LeanWorkspace.write(file, types.module(module))
        assert(!written, s"$file was not the types of the Scala sources: it is written again")
    }
}
