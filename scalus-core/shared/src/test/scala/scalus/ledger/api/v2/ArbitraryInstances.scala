package scalus.cardano.onchain.plutus.v2

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.uplc.builtin.Data
import scalus.cardano.ledger.DataHash
import scalus.cardano.onchain.plutus.prelude.Option
import scalus.cardano.onchain.plutus.v1
import scalus.cardano.onchain.plutus.v1.{Address, ScriptHash}

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

    /** The generator behind `Arbitrary[TxOut]`: the value always holds lovelace ≥ 1
      * (`[gen.value.output-lovelace]`). The other fields use the same instances as `autoDerived`.
      */
    def genTxOut: Gen[TxOut] =
        for
            address <- arbitrary[Address]
            value <- genOutputValue
            datum <- arbitrary[OutputDatum]
            referenceScript <- arbitrary[Option[ScriptHash]]
        yield TxOut(address, value, datum, referenceScript)

    given Arbitrary[TxOut] = Arbitrary(genTxOut)
}
