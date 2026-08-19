package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.onchain.plutus
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.cardano.onchain.plutus.v1.Value
import scalus.compiler.compile
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.{ByteString, Data}

/** Scratch exploration: cheapest way to check `tx.mint` has EXACTLY {BEACON: 1} under a policy
  * (no BEACON1, no other tokens of that policy).
  */
private object MintExactCheckPrograms {
    import scalus.cardano.onchain.plutus.prelude.{given, *}
    // A: current approach - slice via getOrFail, structural SortedMap ===
    val currentSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        fromData[Value](d).toSortedMap.getOrFail(cs) === SortedMap.singleton(tn, BigInt(1))
    }

    // B: slice via tokens(), single equalsData against the expected inner map
    val sliceEqualsDataSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        equalsData(fromData[Value](d).tokens(cs).toData, SortedMap.singleton(tn, BigInt(1)).toData)
    }

    // C: STRICT full-mint equality via one equalsData (forbids any other simultaneous mint)
    val fullEqualsDataSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        equalsData(d, Value(cs, tn, BigInt(1)).toData)
    }

    // D: lookupCoin only - INSUFFICIENT (accepts extra BEACON1), for cost reference
    val lookupOnlySir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        fromData[Value](d).quantityOf(cs, tn) === BigInt(1)
    }

    // E: STRICT full equality via CIP-153 double valueContains, for cost reference
    val doubleContainsSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        val m = unValueData(d)
        val e = unValueData(Value(cs, tn, BigInt(1)).toData)
        valueContains(m, e) && valueContains(e, m)
    }

    // F: composable, hand-rolled - scan outer Data map for policy, one equalsData on the inner map
    val handRolledSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        val expectedInner: Data =
            mapData(mkCons(mkPairData(bData(tn), iData(BigInt(1))), mkNilPairData()))
        val policyKey: Data = bData(cs)
        def go(l: scalus.uplc.builtin.BuiltinList[
              scalus.uplc.builtin.BuiltinPair[Data, Data]
            ]): Boolean =
            if l.isEmpty then false
            else
                val h = l.head
                if equalsData(h.fst, policyKey) then equalsData(h.snd, expectedInner)
                else go(l.tail)
        go(unMapData(d))
    }

    // F2: like F, but compares policy keys with equalsByteString instead of equalsData
    val handRolledBsSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        val expectedInner: Data =
            mapData(mkCons(mkPairData(bData(tn), iData(BigInt(1))), mkNilPairData()))
        def go(l: scalus.uplc.builtin.BuiltinList[
              scalus.uplc.builtin.BuiltinPair[Data, Data]
            ]): Boolean =
            if l.isEmpty then false
            else
                val h = l.head
                if equalsByteString(unBData(h.fst), cs) then equalsData(h.snd, expectedInner)
                else go(l.tail)
        go(unMapData(d))
    }

    // C2: STRICT full equality, expected Data built with raw builtins (no Value.apply)
    val fullEqualsRawSir = compile { (d: Data, cs: ByteString, tn: ByteString) =>
        val expected: Data = mapData(
          mkCons(
            mkPairData(
              bData(cs),
              mapData(mkCons(mkPairData(bData(tn), iData(BigInt(1))), mkNilPairData()))
            ),
            mkNilPairData()
          )
        )
        equalsData(d, expected)
    }
}

class MintExactCheckExplorationTest extends AnyFunSuite {
    import MintExactCheckPrograms.*
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices

    private val ourPolicy = ByteString.fromHex("aa" * 28)
    private val beacon = ByteString.fromString("BEACON")
    private val policyBefore = ByteString.fromHex("11" * 28)
    private val policyAfter = ByteString.fromHex("ff" * 28)

    private def mintData(entries: (ByteString, Seq[(String, BigInt)])*): Data =
        Data.Map(
          plutus.prelude.List.from(
            entries.map { (p, toks) =>
                (
                  Data.B(p),
                  Data.Map(
                    plutus.prelude.List.from(
                      toks.map((t, a) => (Data.B(ByteString.fromString(t)), Data.I(a)))
                    )
                  )
                )
            }
          )
        )

    private val soloGood = mintData(ourPolicy -> Seq("BEACON" -> BigInt(1)))
    private val multiGood = mintData(
      policyBefore -> Seq("tokenX" -> BigInt(5)),
      ourPolicy -> Seq("BEACON" -> BigInt(1)),
      policyAfter -> Seq("tokenY" -> BigInt(2))
    )
    private val badExtraToken = mintData(
      policyBefore -> Seq("tokenX" -> BigInt(5)),
      ourPolicy -> Seq("BEACON" -> BigInt(1), "BEACON1" -> BigInt(1))
    )
    private val badAmount = mintData(ourPolicy -> Seq("BEACON" -> BigInt(2)))

    private def run(uplc: Term, d: Data): (Boolean, ExUnitsBudget) = {
        val applied = uplc $ d.asTerm $ ourPolicy.asTerm $ beacon.asTerm
        applied.evaluateDebug match
            case Result.Success(Term.Const(c, _), budget, _, _) =>
                (c.asInstanceOf[scalus.uplc.Constant.Bool].value, ExUnitsBudget(budget))
            case f => fail(s"evaluation failed: $f")
    }

    private case class ExUnitsBudget(b: scalus.cardano.ledger.ExUnits) {
        def cpu: Long = b.steps
        def mem: Long = b.memory
        def fee: BigInt = b.fee(prices).value
    }

    test("exploration: exact single-NFT mint check strategies") {
        val candidates = scala.List(
          "A getOrFail+===" -> currentSir,
          "B slice equalsData" -> sliceEqualsDataSir,
          "C full equalsData" -> fullEqualsDataSir,
          "D lookupCoin only" -> lookupOnlySir,
          "E dbl valueContains" -> doubleContainsSir,
          "F hand-rolled scan" -> handRolledSir,
          "F2 hand-rolled bs" -> handRolledBsSir,
          "C2 full eq raw data" -> fullEqualsRawSir
        )
        for (name, sir) <- candidates do {
            val uplc = sir.toUplcOptimized()
            val (rSolo, bSolo) = run(uplc, soloGood)
            val (rMulti, bMulti) = run(uplc, multiGood)
            val (rExtra, bExtra) = run(uplc, badExtraToken)
            val (rAmt, _) = run(uplc, badAmount)
            info(
              f"$name%-20s solo cpu=${bSolo.cpu}%9d mem=${bSolo.mem}%6d fee=${bSolo.fee}%6d | multi cpu=${bMulti.cpu}%9d mem=${bMulti.mem}%6d fee=${bMulti.fee}%6d | results solo=$rSolo multi=$rMulti extraTok=$rExtra badAmt=$rAmt"
            )
            assert(rSolo, s"$name must accept solo good mint")
            assert(!rAmt, s"$name must reject amount 2")
        }
    }
}
