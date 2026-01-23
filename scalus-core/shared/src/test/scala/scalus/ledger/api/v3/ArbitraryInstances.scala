package scalus.cardano.onchain.plutus.v3

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.*
import scalus.cardano.onchain.plutus.v1.{Interval, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.prelude.{List, SortedMap}

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends v2.ArbitraryInstances {
    given Arbitrary[TxId] = Arbitrary(genByteStringOfN(32).map(TxId.apply))
    given Arbitrary[TxOutRef] = Arbitrary {
        for
            txId <- arbitrary[TxId]
            index <- Gen.frequency(
              (10, Gen.choose(0, 3)),
              (3, Gen.choose(4, 10)),
              (1, Gen.choose(3, 1000))
            )
        yield TxOutRef(txId, index)
    }

    given Arbitrary[TxInInfo] = Arbitrary {
        for
            outRef <- arbitrary[TxOutRef]
            resolved <- arbitrary[TxOut]
        yield TxInInfo(outRef, resolved)
    }

    given Arbitrary[TxInfo] = Arbitrary {
        for
            inputs <- arbitrary[prelude.List[TxInInfo]]
            fee <- Gen.choose(0L, 1000000_000000L)
            signatories <- arbitrary[prelude.List[PubKeyHash]]
            mint <- arbitrary[Value]
            outputs <- arbitrary[prelude.List[TxOut]]
            interval <- arbitrary[Interval]
            id <- arbitrary[TxId]
        yield TxInfo(
          inputs = inputs,
          referenceInputs = List.Nil,
          outputs = outputs,
          fee = fee,
          mint = mint,
          certificates = List.Nil,
          withdrawals = SortedMap.empty,
          validRange = interval,
          signatories = signatories,
          redeemers = SortedMap.empty,
          data = SortedMap.empty,
          id = id,
        )
    }

    given Arbitrary[ScriptPurpose] = Arbitrary {
        Gen.oneOf(
          arbitrary[PolicyId].map(ScriptPurpose.Minting.apply),
          arbitrary[TxOutRef].map(ScriptPurpose.Spending.apply),
        )
    }

}
