package scalus.cardano.ledger.value

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import spire.implicits.{ArrayEq as _, ArrayOrder as _, MapEq as _, MapGroup as _, SeqEq as _, SeqOrder as _}
import spire.laws.arb.{*, given}
import spire.laws.{OrderLaws, PredicateFromMonoid, VectorSpaceLaws}
import spire.math.{Rational, SafeLong}

class CoinLaws extends AnyFunSuite with FunSuiteDiscipline with Checkers {
    import Gen.Arb.given
    // FAILS
    checkAll("Coin.Coin ordered", OrderLaws[Coin.Coin].order)

    checkAll("Coin.Unbounded cModule", VectorSpaceLaws[Coin.Unbounded, SafeLong].cModule)

    checkAll("Coin.Unbounded ordered", OrderLaws[Coin.Unbounded].order)

    checkAll("Coin.Fractional vector space", VectorSpaceLaws[Coin.Fractional, Rational].vectorSpace)

    checkAll("Coin.Fractional ordered", OrderLaws[Coin.Fractional].order)
}
