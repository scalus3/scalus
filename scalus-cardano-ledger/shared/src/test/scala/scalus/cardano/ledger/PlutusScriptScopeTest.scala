package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.rules.ValidatorRulesTestKit
import scalus.uplc.Term.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.{Constant, DeBruijnedProgram, NamedDeBruijn, Program, ProgramFlatCodec, Term}

/** Scripts must be closed before they run, exactly as the Cardano ledger checks it: Plutus
  * `mkTermToEvaluate` runs `UntypedPlutusCore.Check.Scope.checkScope`, which rejects an
  * out-of-scope index even where evaluation never reaches it, but does not look inside `Constr` or
  * `Case`.
  */
class PlutusScriptScopeTest extends AnyFunSuite, ValidatorRulesTestKit {

    private val unit = Const(Constant.Unit)

    /** An index pointing past every enclosing binder. */
    private val outOfScope = Var(NamedDeBruijn("free", 3))

    private def mint(script: Term): Seq[Redeemer] = {
        // Encode the term as is: converting it to De Bruijn form would renumber the indices.
        val flat = ProgramFlatCodec.unsafeEncodeFlat(Program.plutusV3(script))
        val plutusScript = Script.PlutusV3(
          ByteString.unsafeFromArray(DeBruijnedProgram.fromFlatEncoded(flat).cborEncoded)
        )
        val (tx, utxos) = mintTx(plutusScript)
        PlutusScriptEvaluator(CardanoInfo.mainnet, EvaluatorMode.EvaluateAndComputeCost)
            .evalPlutusScripts(tx, utxos)
    }

    test("a closed script evaluates") {
        assert(mint(LamAbs("ctx", unit)).size == 1)
    }

    test("an out-of-scope index fails the script even where evaluation never reaches it") {
        // \ctx -> (\_ -> ()) (delay #3): the delayed variable is never forced.
        intercept[PlutusScriptEvaluationException] {
            mint(LamAbs("ctx", Apply(LamAbs("_", unit), Delay(outOfScope))))
        }
    }

    test("an out-of-scope index in a case branch never taken does not fail the script") {
        // \ctx -> case (constr 0) [(), #3]: the ledger's scope check does not look inside Case.
        assert(
          mint(LamAbs("ctx", Case(Constr(Word64.Zero, Nil), List(unit, outOfScope)))).size == 1
        )
    }
}
