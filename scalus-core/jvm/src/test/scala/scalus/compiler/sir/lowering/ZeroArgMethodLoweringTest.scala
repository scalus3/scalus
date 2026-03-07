package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Tag
import scalus.*
import scalus.compiler.Options
import scalus.uplc.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.eval.PlutusVM

/** Parameterless (zero-arg) method defined in a @Compile object.
  *
  * Regression test: calling a zero-arg method like `foo()` inside a @Compile object
  * used to produce `foo((), ())` in the SIR — applying the ByteString result as a
  * function with a Unit argument, causing:
  *   LoweringException: Expected function type, but have: ByteString
  *
  * See: CompactMerklePatriciaForestry.nullHashes() failure.
  */
@Compile
object ZeroArgMethodTestDefs {
    def zeroArgMethod(): ByteString =
        blake2b_256(ByteString.empty)

    def callsZeroArg(x: ByteString): ByteString =
        appendByteString(zeroArgMethod(), x)
}

class ZeroArgMethodLoweringTest extends AnyFunSuite {
    private given Options = Options.release

    // Known bug: zero-arg methods in @Compile objects produce incorrect SIR.
    // The lowering sees `nullHashes((), ())` instead of `nullHashes()`.
    // Change `ignore` to `test` once the bug is fixed.
    ignore("zero-arg method in @Compile object should lower without error") {
        // This should not throw LoweringException
        val compiled = PlutusV3.compile(ZeroArgMethodTestDefs.callsZeroArg)
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val result = compiled(ByteString.empty).program.term.evaluate
        // zeroArgMethod() returns blake2b_256(empty) which is a 32-byte hash
        // callsZeroArg appends that hash to the input
        assert(result.isInstanceOf[Term.Const])
    }
}
