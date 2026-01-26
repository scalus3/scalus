package scalus.prelude

import scalus.cardano.onchain.plutus.prelude.Option

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.ByteString.hex
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.compiler.sir.SIR
import scalus.testing.kit.EvalTestKit

import scala.language.implicitConversions

class OptionTest extends AnyFunSuite with EvalTestKit {

    test("eq") {

        assert(Option.empty[String] == None)
        assert(Option.Some(BigInt(1)) == Option.Some(BigInt(1)))
        assert(Option.Some(BigInt(1)) !== Option.Some(BigInt(2)))

        assertEval(Option.Some(true) !== None)
        assertEval(Option.Some(true) === Option.Some(true))
        assertEval(Option.Some(true) !== Option.Some(false))

        assert(scala.None.asScalus == Option.empty[String])
        assert(Option.empty[String].asScala.isEmpty)
        assert(scala.Some(BigInt(1)).asScalus == Option.Some(BigInt(1)))
        assert(Option.Some(BigInt(1)).asScala.contains(BigInt(1)))

    }

    test("ord") {
        check { (pair: (Int, Int)) =>
            import scalus.cardano.onchain.plutus.prelude.Ord.*

            val (left, right) = pair
            val leftOpt = scalus.cardano.onchain.plutus.prelude.Option(BigInt(left))
            val rightOpt = scalus.cardano.onchain.plutus.prelude.Option(BigInt(right))
            leftOpt > Option.empty &&
            rightOpt > Option.empty &&
            (if left > right then {
                 leftOpt > rightOpt
             } else if left < right then {
                 leftOpt < rightOpt
             } else leftOpt equiv rightOpt)
        }

//        assertEval(
//          Some(BigInt(-5)).gt(Option.empty[BigInt])
//        )
//        assertEval(
//          Some(BigInt(-5)).gt(Some(BigInt(-10)))
//        )
    }

    test("ToData <-> FromData") {
        check { (opt: Option[BigInt]) =>
            val data = opt.toData
            val fromDataOpt = fromData[Option[BigInt]](data)

            opt == fromDataOpt
        }
    }

    test("flatten") {
        assertEvalEq(None.flatten, None)
        assertEvalEq(Some(Some("")).flatten, Some(""))
        assertEvalEq(Some(Option.empty[String]).flatten, None)

        check { (o: Option[Option[String]]) =>
            o.flatten == o.asScala.flatMap(_.asScala).asScalus
        }

        check { (o: scala.Option[scala.Option[String]]) =>
            o.flatten.asScalus == o.asScalus.map(_.asScalus).flatten
        }
    }

    test("empties") {
        check { (asScala: scala.Option[Boolean]) =>
            val asScalus = asScala.asScalus
            asScalus.isEmpty == asScala.isEmpty && asScalus.isDefined == asScala.isDefined && asScala.nonEmpty == asScalus.nonEmpty
        }
        assertEval(None.isEmpty)
        assertEval(!None.isDefined)
        assertEval(!None.nonEmpty)
        assertEval(Some("").nonEmpty)
        assertEval(Some("").isDefined)
        assertEval(!Some("").isEmpty)

        check { (asScalus: Option[Boolean]) =>
            val asScala = asScalus.asScala
            asScalus.isEmpty == asScala.isEmpty && asScalus.isDefined == asScala.isDefined && asScala.nonEmpty == asScala.nonEmpty
        }
    }

    test("throwing accessors") {
        assertEvalFails[NoSuchElementException](None.get)
        assertEvalFails[NoSuchElementException](None.getOrFail(""))
        assertEvalFails[NoSuchElementException](None.orFail(""))

        assertEvalEq(Some("").get, "")
        assertEvalEq(Some(BigInt(5)).getOrFail(""), BigInt(5))
        assertEvalEq(Some(hex"deadbeef").orFail(""), ())
    }

    test("getOrElse") {
        check { (o: scala.Option[String], default: String) =>
            val asScalus = o.asScalus
            val asScala = asScalus.asScala
            asScala.getOrElse(default) == asScalus.getOrElse(default)
        }

        check { (o: Option[String], default: String) =>
            val asScala = o.asScala
            val asScalus = asScala.asScalus
            asScala.getOrElse(default) == asScalus.getOrElse(default)
        }

        assertEvalEq(Some("foo").getOrElse("bar"), "foo")
        assertEvalEq(None.getOrElse("bar"), "bar")
    }

    test("orElse") {
        check { (l: scala.Option[String], r: scala.Option[String]) =>
            val (asScalusL, asScalusR) = (l.asScalus, r.asScalus)
            l.orElse(r).asScalus == asScalusL.orElse(asScalusR)
        }

        check { (l: Option[String], r: Option[String]) =>
            val (asScalaL, asScalaR) = (l.asScala, r.asScala)
            asScalaL.orElse(asScalaR).asScalus == l.orElse(r)
        }

        assertEvalEq(Some("foo").orElse(Some("bar")), Some("foo"))
        assertEvalEq(Some("foo").orElse(None), Some("foo"))
        assertEvalEq(None.orElse(Some("bar")), Some("bar"))
        assertEvalEq(None.orElse(None), None)
    }

    test("map") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).map(x => x / BigInt(0)))
        assertEvalEq(Option.empty[BigInt].map(x => x / BigInt(0)), None)

        assertEvalEq(Some(BigInt(42)).map(_ / 2), Some(BigInt(21)))
        assertEval(Option.empty[BigInt].map(_ / 2).isEmpty)

        check { (o: scala.Option[BigInt]) =>
            o.map(_ * 2).asScalus == o.asScalus.map(_ * 2)
        }

        check { (o: Option[BigInt]) =>
            o.map(_ * 2).asScala == o.asScala.map(_ * 2)
        }
    }

    test("flatMap") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).flatMap(x => Some(x / BigInt(0))))
        assertEvalEq(Option.empty[BigInt].flatMap(x => Some(x / BigInt(0))), None)

        assertEvalEq(Some(BigInt(5)).flatMap(x => Some(x * BigInt(5))), Some(BigInt(25)))
        assertEvalEq(Some(BigInt(42)).flatMap(_ => None), None)
        assertEvalEq(Option.empty[BigInt].flatMap(x => Some(x * BigInt(5))), None)
        assertEvalEq(None.flatMap(_ => None), None)

        check { (o: scala.Option[BigInt]) =>
            o.flatMap(x => scala.Some(x * 2)).asScalus == o.asScalus.flatMap(x => Some(x * 2))
        }

        check { (o: Option[BigInt]) =>
            o.flatMap(x => Some(x * 2)).asScala == o.asScala.flatMap(x => scala.Some(x * 2))
        }
    }

    test("filter + filterNot") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).filter(x => (x / 0) > BigInt(10)))
        assertEvalFails[ArithmeticException](Some(BigInt(5)).filterNot(x => (x / 0) > BigInt(10)))
        assertEvalEq(Option.empty[BigInt].filter(x => (x / 0) > BigInt(10)), None)
        assertEvalEq(Option.empty[BigInt].filterNot(x => (x / 0) > BigInt(10)), None)

        assertEval(None.filter(_ => true).isEmpty)
        assertEval(None.filter(_ => false).isEmpty)
        assertEvalEq(Some(BigInt(5)).filter(_ > 4), Some(BigInt(5)))
        assertEvalEq(Some(BigInt(5)).filter(_ % 2 == BigInt(0)), None)

        assertEval(None.filterNot(_ => true).isEmpty)
        assertEval(None.filterNot(_ => false).isEmpty)
        assertEvalEq(Some(BigInt(5)).filterNot(_ > 4), None)
        assertEvalEq(Some(BigInt(5)).filterNot(_ % 2 == BigInt(0)), Some(BigInt(5)))

        check { (o: scala.Option[BigInt]) =>
            o.filter(_ % 2 == 1).asScalus == o.asScalus.filter(_ % 2 == 1)
        }

        check { (o: Option[BigInt]) =>
            o.filter(_ % 2 == 1).asScala == o.asScala.filter(_ % 2 == 1)
        }

        check { (o: scala.Option[BigInt]) =>
            o.filterNot(_ % 2 == 1).asScalus == o.asScalus.filterNot(_ % 2 == 1)
        }

        check { (o: Option[BigInt]) =>
            o.filterNot(_ % 2 == 1).asScala == o.asScala.filterNot(_ % 2 == 1)
        }
    }

    test("contains") {
        assertEval(!None.contains(BigInt(5)))
        assertEval(Some(BigInt(5)).contains(BigInt(5)))
        assertEval(!Some(BigInt(5)).contains(BigInt(0)))

        // TODO: UPLC error
        // assertEval(!Some(BigInt(5)).contains(BigInt(5))(using (_, _) => false))

        check { (o: scala.Option[BigInt], value: BigInt) =>
            o.contains(value) == o.asScalus.contains(value)
        }

        check { (o: Option[BigInt], value: BigInt) =>
            o.contains(value) == o.asScala.contains(value)
        }
    }

    test("exists") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).exists(x => (x / 0) > BigInt(2)))
        assertEval(!Option.empty[BigInt].exists(x => (x / 0) > BigInt(2)))

        assertEval(!None.exists(_ => true))
        assertEval(Some(BigInt(5)).exists(_ > BigInt(0)))
        assertEval(!Some(BigInt(5)).exists(_ < BigInt(0)))

        check { (o: scala.Option[BigInt]) =>
            o.exists(_ > 0) == o.asScalus.exists(_ > 0)
        }

        check { (o: Option[BigInt]) =>
            o.exists(_ > 0) == o.asScala.exists(_ > 0)
        }
    }

    test("forall") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).forall(x => (x / 0) > BigInt(2)))
        assertEval(Option.empty[BigInt].forall(x => (x / 0) > BigInt(2)))

        assertEval(None.forall(_ => true))
        assertEval(Some(BigInt(5)).forall(_ > BigInt(0)))
        assertEval(!Some(BigInt(5)).forall(_ < BigInt(0)))

        check { (o: scala.Option[BigInt]) =>
            o.forall(_ > 0) == o.asScalus.forall(_ > 0)
        }

        check { (o: Option[BigInt]) =>
            o.forall(_ > 0) == o.asScala.forall(_ > 0)
        }
    }

}
