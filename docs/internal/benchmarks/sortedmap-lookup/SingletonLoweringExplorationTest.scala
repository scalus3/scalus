package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.compiler.compile
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}

private object SingletonLoweringPrograms {
    import scalus.cardano.onchain.plutus.prelude.given

    val preludeSingletonSir = compile { (tn: ByteString) =>
        SortedMap.singleton(tn, BigInt(1)).toData
    }

    val rawSingletonSir = compile { (tn: ByteString) =>
        mapData(mkCons(mkPairData(bData(tn), iData(BigInt(1))), mkNilPairData()))
    }
}

class SingletonLoweringExplorationTest extends AnyFunSuite {
    import SingletonLoweringPrograms.*
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private def run(t: Term): (Term, scalus.cardano.ledger.ExUnits) = t.evaluateDebug match
        case Result.Success(r, b, _, _) => (r, b)
        case f                          => fail(s"evaluation failed: $f")

    test("what does SortedMap.singleton(tn, 1).toData lower to?") {
        val tn = ByteString.fromString("BEACON")
        val preludeUplc = preludeSingletonSir.toUplcOptimized()
        val rawUplc = rawSingletonSir.toUplcOptimized()
        info("=== prelude SortedMap.singleton.toData (optimized) ===")
        info(preludeUplc.show)
        info("=== raw builtins (optimized) ===")
        info(rawUplc.show)
        val (pr, pb) = run(preludeUplc $ tn.asTerm)
        val (rr, rb) = run(rawUplc $ tn.asTerm)
        assert(pr == rr, s"results differ: $pr vs $rr")
        info(f"prelude cpu=${pb.steps}%9d mem=${pb.memory}%6d")
        info(f"raw     cpu=${rb.steps}%9d mem=${rb.memory}%6d")
    }
}
