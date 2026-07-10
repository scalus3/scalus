package scalus.uplc
package test

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data

/** Guards against heavy-tailed generators producing pathologically large values.
  *
  * An unbounded `arbData`/`arbConstantByType` once generated a term whose single constant printed
  * to ~100MB, OOM-ing the CI test JVM (property "CCE is idempotent on arbitrary terms"). These
  * tests sample the generators with fixed seeds and assert hard node-count caps, so a reintroduced
  * size explosion fails deterministically instead of OOM-ing a random build.
  */
class ArbitraryInstancesBoundsTest extends AnyFunSuite with ArbitraryInstances {

    private val SampleSize = 100
    private val Samples = 5000

    private def dataNodeCount(d: Data): Long = d match
        case Data.I(_) | Data.B(_) => 1
        case Data.Constr(_, args)  => 1 + args.toScalaList.map(dataNodeCount).sum
        case Data.List(values)     => 1 + values.toScalaList.map(dataNodeCount).sum
        case Data.Map(values) =>
            1 + values.toScalaList.map((k, v) => dataNodeCount(k) + dataNodeCount(v)).sum

    private def constantNodeCount(c: Constant): Long = c match
        case Constant.Data(d)         => 1 + dataNodeCount(d)
        case Constant.List(_, elems)  => 1 + elems.map(constantNodeCount).sum
        case Constant.Array(_, elems) => 1 + elems.map(constantNodeCount).sum
        case Constant.Pair(a, b)      => 1 + constantNodeCount(a) + constantNodeCount(b)
        case _                        => 1

    private def sample[A](gen: Gen[A])(check: A => Unit): Unit = {
        val params = Gen.Parameters.default.withSize(SampleSize)
        var seed = Seed(42L)
        var generated = 0
        while generated < Samples do
            gen(params, seed).foreach { a =>
                check(a)
                generated += 1
            }
            seed = seed.slide
    }

    test("arbitrary Data node count is bounded") {
        sample(summon[org.scalacheck.Arbitrary[Data]].arbitrary) { d =>
            val n = dataNodeCount(d)
            assert(n <= 1000, s"Data with $n nodes exceeds bound")
        }
    }

    test("arbitrary Constant node count is bounded") {
        sample(arbitraryConstant.arbitrary) { c =>
            val n = constantNodeCount(c)
            assert(n <= 5000, s"Constant with $n nodes exceeds bound")
        }
    }
}
