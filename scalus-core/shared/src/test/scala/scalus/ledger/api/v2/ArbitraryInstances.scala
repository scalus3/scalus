package scalus.cardano.onchain.plutus.v2

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.uplc.builtin.Data
import scalus.cardano.ledger.DataHash
import scalus.cardano.onchain.plutus.v1
import scalus.testing.ArbitraryDerivation.autoDerived

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends v1.ArbitraryInstances {
    import scalus.cardano.ledger.ArbitraryInstances.given
    given Arbitrary[OutputDatum] = Arbitrary {
        Gen.oneOf(
          Gen.const(OutputDatum.NoOutputDatum),
          arbitrary[DataHash].map(OutputDatum.OutputDatumHash.apply),
          arbitrary[Data].map(OutputDatum.OutputDatum.apply)
        )
    }
    given Arbitrary[TxOut] = autoDerived
}
