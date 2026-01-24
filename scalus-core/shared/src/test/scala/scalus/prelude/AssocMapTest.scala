package scalus.prelude

import scalus.cardano.onchain.plutus.prelude.{`???`, AssocMap, List, Option, Rational, SortedMap}

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.onchain.plutus.prelude.List.*
import scalus.cardano.onchain.plutus.prelude.These.*
import scalus.uplc.test.ArbitraryInstances

@Compile
private object AssocMapTest {
    val m1 = AssocMap.unsafeFromList(
      Cons((BigInt(1), BigInt(2)), Cons((BigInt(0), BigInt(0)), List.Nil))
    )
    val m2 = AssocMap.unsafeFromList(
      Cons((BigInt(1), BigInt(3)), Cons((BigInt(3), BigInt(4)), List.Nil))
    )
    def equalsAssets(
        a: AssocMap[BigInt, BigInt],
        b: AssocMap[BigInt, BigInt]
    ): Boolean = {
        val combined = AssocMap.union(a, b).toList
        // all values are equal, absent values are 0
        combined.foldLeft(true) { case (acc, pair) =>
            pair._2 match
                case These(v1, v2) => acc && v1 == v2
                case This(v1)      => acc && v1 == BigInt(0)
                case That(v2)      => acc && v2 == BigInt(0)
        }
    }

}

class AssocMapTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {

    given compiler.Options = compiler.Options(
      targetLoweringBackend = compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("empty") {
        assert(AssocMap.empty.toList == List.Nil)
    }

    test("union") {
        import AssocMapTest.*
        val m3 = AssocMap.union(m1, m2)
        val compiled = compiler.compile {
            val a = BigInt(132)
            AssocMap.union(m1, m2)
        }
        // println(compiled.pretty.render(100))
        val term = compiled.toUplc()
        // println(VM.evaluateTerm(term).pretty.render(100))
        assert(
          m3.toList == List(
            (BigInt(1), These(2, 3)),
            (BigInt(0), This(BigInt(0))),
            (BigInt(3), That(BigInt(4)))
          )
        )
        {
            val compiled = compiler.compile {
                equalsAssets(m1, m2)
            }
            // println(compiled.pretty.render(100))
            val term = compiled.toUplc()
            // println(VM.evaluateTerm(term).pretty.render(100))
        }
        {
            val compiled = compiler.compile {
                equalsAssets(m1, m1)
            }
            val term = compiled.toUplc()
            // println(VM.evaluateTerm(term).pretty.render(100))
        }
    }

//    test("fromList(lst).toList == lst.unique(using AssocMap.eq)") {
//        check { (lst: List[(BigInt, Boolean)]) =>
//            AssocMap.fromList(lst).toList == lst.unique(using AssocMap.pairEq)
//        }
//    }

    test("insert") {
        check { (map: AssocMap[BigInt, BigInt], k: BigInt, v: BigInt) =>
            val m1 = map.insert(k, v)
            val lst1 = m1.toList.asScala
            lst1.contains((k, v))
        }
    }

    test("get") {
        check { (map: AssocMap[BigInt, BigInt], k: BigInt, v: BigInt) =>
            val m1 = map.insert(k, v)
            m1.get(k) == scalus.cardano.onchain.plutus.prelude.Option.Some(v)
        }
    }

    test("delete") {
        check { (map: AssocMap[BigInt, BigInt], k: BigInt, v: BigInt) =>
            val m1 = map.insert(k, v)
            m1.get(k) == scalus.cardano.onchain.plutus.prelude.Option.Some(v)
            val m2 = m1.delete(k)
            m2.get(k) == scalus.cardano.onchain.plutus.prelude.Option.None
        }
    }

}
