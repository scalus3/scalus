package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.builtin.Data

/** Scratch: why did the Eq-based SortedMap.get cost MORE cpu than the Ord one on a hit? */
class GetLoweringDiffExplorationTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("diff get lowering: Eq-based public get vs local Ord clone vs local Eq clone") {
        import GetLoweringDiffPrograms.*
        val d = Data.Map(
          scalus.cardano.onchain.plutus.prelude.List.from(
            Seq((Data.I(1), Data.I(1)))
          )
        )
        for (name, sir) <- scala.List(
              "public get" -> publicGetSir,
              "ord clone" -> ordCloneSir,
              "eq clone" -> eqCloneSir
            )
        do {
            val uplc = sir.toUplcOptimized()
            (uplc $ d.asTerm).evaluateDebug match
                case Result.Success(r, budget, _, _) =>
                    info(f"$name%-11s cpu=${budget.steps}%9d mem=${budget.memory}%6d result=$r")
                case f => fail(s"$name failed: $f")
            info(s"=== $name UPLC ===")
            info(uplc.show)
        }
    }

    test("ByteString keys: new Eq get vs old Ord clone across positions") {
        import GetLoweringDiffPrograms.*
        import scalus.uplc.builtin.ByteString
        val prices =
            scalus.cardano.ledger.CardanoInfo.mainnet.protocolParams.executionUnitPrices
        def bs(i: Int) = ByteString.fromHex(f"$i%02x" * 28)
        val d5 = Data.Map(
          scalus.cardano.onchain.plutus.prelude.List.from(
            (1 to 5).map(i => (Data.B(bs(i)), Data.I(BigInt(i))))
          )
        )
        val newGet = bsGetSir.toUplcOptimized()
        val oldGet = bsOrdCloneSir.toUplcOptimized()
        for (pos, key) <- scala.List(
              "first" -> bs(1),
              "last" -> bs(5),
              "absent<" -> ByteString.fromHex("00" * 28),
              "absent>" -> ByteString.fromHex("ff" * 28)
            )
        do {
            def run(uplc: Term): (Term, scalus.cardano.ledger.ExUnits) =
                (uplc $ d5.asTerm $ key.asTerm).evaluateDebug match
                    case Result.Success(r, b, _, _) => (r, b)
                    case f                          => fail(s"$pos failed: $f")
            val (rn, bn) = run(newGet)
            val (ro, bo) = run(oldGet)
            assert(rn == ro, s"$pos: results differ")
            info(
              f"$pos%-7s new cpu=${bn.steps}%9d mem=${bn.memory}%6d fee=${bn.fee(prices).value}%5d | old cpu=${bo.steps}%9d mem=${bo.memory}%6d fee=${bo.fee(prices).value}%5d"
            )
        }
    }
}

private object GetLoweringDiffPrograms {
    import scalus.cardano.onchain.plutus.prelude.{===, <=>, Eq, Option, Ord, Order, PairList, SortedMap}
    import scalus.cardano.onchain.plutus.prelude.PairList.*
    import scalus.compiler.compile
    import scalus.uplc.builtin.Data
    import scalus.uplc.builtin.Data.fromData

    val publicGetSir = compile { (d: Data) =>
        fromData[SortedMap[BigInt, BigInt]](d).get(BigInt(1))
    }

    val ordCloneSir = compile { (d: Data) =>
        val m = fromData[SortedMap[BigInt, BigInt]](d)
        val key = BigInt(1)
        def go(lst: PairList[BigInt, BigInt]): Option[BigInt] = lst match
            case PairNil => Option.None
            case PairCons(pair, tail) =>
                pair match
                    case (k, v) =>
                        key <=> k match
                            case Order.Less    => Option.None
                            case Order.Greater => go(tail)
                            case Order.Equal   => Option.Some(v)
        go(m.toPairList)
    }

    val bsGetSir = compile { (d: Data, key: scalus.uplc.builtin.ByteString) =>
        fromData[SortedMap[scalus.uplc.builtin.ByteString, BigInt]](d).get(key)
    }

    val bsOrdCloneSir = compile { (d: Data, key: scalus.uplc.builtin.ByteString) =>
        val m = fromData[SortedMap[scalus.uplc.builtin.ByteString, BigInt]](d)
        def go(lst: PairList[scalus.uplc.builtin.ByteString, BigInt]): Option[BigInt] = lst match
            case PairNil => Option.None
            case PairCons(pair, tail) =>
                pair match
                    case (k, v) =>
                        key <=> k match
                            case Order.Less    => Option.None
                            case Order.Greater => go(tail)
                            case Order.Equal   => Option.Some(v)
        go(m.toPairList)
    }

    val eqCloneSir = compile { (d: Data) =>
        val m = fromData[SortedMap[BigInt, BigInt]](d)
        val key = BigInt(1)
        def go(lst: PairList[BigInt, BigInt]): Option[BigInt] = lst match
            case PairNil => Option.None
            case PairCons(pair, tail) =>
                pair match
                    case (k, v) =>
                        if key === k then Option.Some(v) else go(tail)
        go(m.toPairList)
    }
}
