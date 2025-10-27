import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.ledger.api.v3.TxId
import scalus.sir.simpleLowering.SirToUplc110Lowering
import scalus.uplc.{DeBruijn, Term}

object DebugTest extends App {
    val sir = compile {
        TxId(hex"DEADBEEF") match
            case _ => BigInt(1)
    }

    println("=== SIR ===")
    println(sir.show)
    println()

    val lowered = SirToUplc110Lowering(sir, generateErrorTraces = false).lower()
    println("=== Lowered (Named) ===")
    println(lowered.pretty.render(100))
    println()

    val deBruijn = DeBruijn.deBruijnTerm(lowered)
    println("=== DeBruijn ===")
    println(deBruijn.pretty.render(100))
    println()

    val expected = Term.LamAbs("scrutinee", Term.Const(scalus.uplc.Constant.Integer(1))) $
        Term.Const(scalus.uplc.Constant.ByteString(hex"DEADBEEF"))
    println("=== Expected (Named) ===")
    println(expected.pretty.render(100))
    println()

    val expectedDB = DeBruijn.deBruijnTerm(expected)
    println("=== Expected DeBruijn ===")
    println(expectedDB.pretty.render(100))
    println()

    println("=== Alpha Equal? ===")
    println(Term.alphaEq(deBruijn, expectedDB))
}
