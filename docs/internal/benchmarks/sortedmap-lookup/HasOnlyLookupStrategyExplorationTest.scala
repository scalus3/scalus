package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.onchain.plutus
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.builtin.{ByteString, Data}

/** Scratch exploration: for the exact single-token check on a Value, which lookup strategy is
  * cheaper, and does the sorted-map early exit (Ord[ByteString]) ever win?
  *
  *   - A: `tokens(cs) === SortedMap.singleton(tn, 1)` - SortedMap.get with Ord[ByteString]
  *     (lessThanByteString + equalsByteString per miss, early exit on Less), then structural ===.
  *   - B: `hasOnly(cs, tn, 1)` - plain equalsByteString scan (1 builtin per miss, no early exit),
  *     then one equalsData.
  *   - C: best-case early-exit scan - lessThanByteString-first (1 builtin per skipped entry, early
  *     exit once key sorts after target), then one equalsData. Isolates "does sortedness pay" from
  *     the A-vs-B API differences.
  */
class HasOnlyLookupStrategyExplorationTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices

    private val beacon = ByteString.fromString("BEACON")

    private def policy(i: Int): ByteString = ByteString.fromHex(f"$i%02x" * 28)

    /** n policies with ascending 28-byte ids, each holding exactly (BEACON, 1). */
    private def valueData(n: Int): Data =
        Data.Map(
          plutus.prelude.List.from(
            (1 to n).map { i =>
                (
                  Data.B(policy(i)),
                  Data.Map(plutus.prelude.List.from(Seq((Data.B(beacon), Data.I(BigInt(1))))))
                )
            }
          )
        )

    private def run(uplc: Term, d: Data, cs: ByteString): (Boolean, scalus.cardano.ledger.ExUnits) =
        (uplc $ d.asTerm $ cs.asTerm $ beacon.asTerm).evaluateDebug match
            case Result.Success(Term.Const(scalus.uplc.Constant.Bool(b), _), budget, _, _) =>
                (b, budget)
            case f => fail(s"evaluation failed: $f")

    test("lookup strategy comparison across Value sizes and target positions") {
        import HasOnlyLookupStrategyPrograms.*
        val strategies = scala.List(
          "A tokens===" -> tokensEqSir.toUplcOptimized(),
          "B hasOnly" -> hasOnlySir.toUplcOptimized(),
          "C earlyExit" -> earlyExitScanSir.toUplcOptimized()
        )
        val absentEarly = ByteString.fromHex("00" * 28)
        val absentLate = ByteString.fromHex("ff" * 28)
        for n <- scala.List(1, 3, 5, 10, 20, 50) do {
            val d = valueData(n)
            val positions = scala.List(
              "first" -> policy(1),
              "middle" -> policy((n + 1) / 2),
              "last" -> policy(n),
              "absent<" -> absentEarly,
              "absent>" -> absentLate
            ).distinctBy(_._2)
            for (posName, cs) <- positions do {
                val results = strategies.map { (name, uplc) =>
                    val (r, b) = run(uplc, d, cs)
                    (name, r, b)
                }
                val expected = results.head._2
                assert(results.forall(_._2 == expected), s"n=$n $posName: strategies disagree")
                val line = results
                    .map { (name, _, b) =>
                        f"$name cpu=${b.steps}%9d mem=${b.memory}%6d fee=${b.fee(prices).value}%5d"
                    }
                    .mkString(" | ")
                info(f"n=$n%2d ${posName}%-7s found=$expected%-5s $line")
            }
        }
    }
}

private object HasOnlyLookupStrategyPrograms {
    import scalus.cardano.onchain.plutus.prelude.{===, SortedMap}
    import scalus.cardano.onchain.plutus.v1.Value
    import scalus.compiler.compile
    import scalus.uplc.builtin.Builtins.*
    import scalus.uplc.builtin.Data.{fromData, toData}
    import scalus.uplc.builtin.{BuiltinList, BuiltinPair, ByteString, Data}

    val tokensEqSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        fromData[Value](d).tokens(cs) === SortedMap.singleton(tn, BigInt(1))
    }

    val hasOnlySir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        fromData[Value](d).hasOnly(cs, tn, BigInt(1))
    }

    val earlyExitScanSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        val expectedTokens = SortedMap.singleton(tn, BigInt(1)).toData
        def go(pairs: BuiltinList[BuiltinPair[Data, Data]]): Boolean =
            if pairs.isEmpty then false
            else
                val key = unBData(pairs.head.fst)
                if lessThanByteString(key, cs) then go(pairs.tail)
                else if equalsByteString(key, cs) then equalsData(pairs.head.snd, expectedTokens)
                else false
        go(unMapData(d))
    }
}
