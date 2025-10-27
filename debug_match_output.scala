import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.ledger.api.v3.TxId
import scalus.sir.simpleLowering.SirToUplc110Lowering
import scalus.uplc.{DeBruijn, Term}

object DebugMatchOutput extends App {
    val ae = AnnotationsDecl.empty

    println("=== Test 1: lower Match ===")
    val sir1 = compile {
        (prelude.List.Nil: prelude.List[BigInt]) match
            case prelude.List.Nil         => BigInt(1)
            case prelude.List.Cons(h, tl) => BigInt(2)
    }
    val compiled1 = SirToUplc110Lowering(sir1, generateErrorTraces = false).lower()
    println(s"Compiled: ${compiled1.pretty.render(100)}")
    println(s"DeBruijn: ${DeBruijn.deBruijnTerm(compiled1).pretty.render(100)}")

    println("\n=== Test 2: lower newtype Match ===")
    val sir2 = compile {
        TxId(hex"DEADBEEF") match
            case TxId(id) => BigInt(1)
    }
    val compiled2 = SirToUplc110Lowering(sir2, generateErrorTraces = false).lower()
    println(s"Compiled: ${compiled2.pretty.render(100)}")
    println(s"DeBruijn: ${DeBruijn.deBruijnTerm(compiled2).pretty.render(100)}")
}
