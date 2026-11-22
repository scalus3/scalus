package scalus.cardano.onchain.plutus.v3

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.*
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.v1.{Credential, Interval, PubKeyHash}
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.prelude.{AssocMap, List, Option, SortedMap}

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

    /** A lovelace amount for a deposit, a refund, a fee or a withdrawal: the values of `genCoin`.
      */
    private def genLovelaceAmount: Gen[Lovelace] =
        scalus.cardano.ledger.ArbitraryInstances.genCoin.map(c => BigInt(c.value))

    private def genDRep: Gen[DRep] =
        Gen.oneOf(
          arbitrary[Credential].map(DRep.DRep(_)),
          Gen.const(DRep.AlwaysAbstain),
          Gen.const(DRep.AlwaysNoConfidence)
        )

    private def genDelegatee: Gen[Delegatee] =
        Gen.oneOf(
          arbitrary[PubKeyHash].map(Delegatee.Stake(_)),
          genDRep.map(Delegatee.Vote(_)),
          Gen.zip(arbitrary[PubKeyHash], genDRep).map(Delegatee.StakeVote(_, _))
        )

    /** A certificate of each `TxCert` case, with the amounts of `genLovelaceAmount`. */
    def genTxCert: Gen[TxCert] = {
        val credential = arbitrary[Credential]
        val pkh = arbitrary[PubKeyHash]
        val optionalAmount = Gen.frequency(
          1 -> Gen.const(Option.None),
          1 -> genLovelaceAmount.map(Option.Some(_))
        )
        Gen.oneOf(
          Gen.zip(credential, optionalAmount).map(TxCert.RegStaking(_, _)),
          Gen.zip(credential, optionalAmount).map(TxCert.UnRegStaking(_, _)),
          Gen.zip(credential, genDelegatee).map(TxCert.DelegStaking(_, _)),
          Gen.zip(credential, genDelegatee, genLovelaceAmount).map(TxCert.RegDeleg(_, _, _)),
          Gen.zip(credential, genLovelaceAmount).map(TxCert.RegDRep(_, _)),
          credential.map(TxCert.UpdateDRep(_)),
          Gen.zip(credential, genLovelaceAmount).map(TxCert.UnRegDRep(_, _)),
          Gen.zip(pkh, pkh).map(TxCert.PoolRegister(_, _)),
          Gen.zip(pkh, Gen.choose(0L, 10_000L)).map((p, e) => TxCert.PoolRetire(p, BigInt(e))),
          Gen.zip(credential, credential).map(TxCert.AuthHotCommittee(_, _)),
          credential.map(TxCert.ResignColdCommittee(_))
        )
    }

    /** `[gen.size.default-caps]`: 1 to 4 items in 90 % of samples, 5 to 16 in 10 %. */
    private def genCapped[A](item: Gen[A]): Gen[List[A]] =
        Gen.frequency(9 -> Gen.choose(1, 4), 1 -> Gen.choose(5, 16))
            .flatMap(Gen.listOfN(_, item))
            .map(List.from)

    /** An optional collection (`[gen.cover.optional-empty]`, `[gen.cover.no-gen-option]`): empty in
      * 50 % of samples, else 1 to 4 items.
      */
    private def genOptional[A](item: Gen[A]): Gen[scala.List[A]] =
        Gen.frequency(1 -> Gen.const(Nil), 1 -> Gen.choose(1, 4).flatMap(Gen.listOfN(_, item)))

    private def genOptionalAmount: Gen[Option[Lovelace]] =
        Gen.frequency(1 -> Gen.const(Option.None), 1 -> genLovelaceAmount.map(Option.Some(_)))

    /** The generator behind `Arbitrary[TxInfo]`. Inputs, outputs and signatories are capped by
      * construction (`[gen.size.caps]`); reference inputs, certificates, withdrawals, redeemers,
      * datums and the two treasury amounts are empty in half of the samples. Votes and proposal
      * procedures stay empty: no generator of the governance types exists yet.
      */
    def genTxInfo: Gen[TxInfo] =
        for
            inputs <- genCapped(arbitrary[TxInInfo])
            referenceInputs <- genOptional(arbitrary[TxInInfo])
            outputs <- genCapped(arbitrary[TxOut])
            fee <- Gen.choose(0L, 1000000_000000L)
            mint <- genMintValue // [gen.value.txinfo-mint]
            certificates <- genOptional(genTxCert)
            withdrawals <- genOptional(Gen.zip(arbitrary[Credential], genLovelaceAmount))
            interval <- arbitrary[Interval]
            signatories <- genCapped(arbitrary[PubKeyHash])
            redeemers <- genOptional(Gen.zip(arbitrary[ScriptPurpose], arbitrary[Data]))
            data <- genOptional(Gen.zip(genByteStringOfN(32), arbitrary[Data]))
            id <- arbitrary[TxId]
            treasury <- genOptionalAmount
            donation <- genOptionalAmount
        yield TxInfo(
          inputs = inputs,
          referenceInputs = List.from(referenceInputs),
          outputs = outputs,
          fee = fee,
          mint = mint,
          certificates = List.from(certificates),
          withdrawals = SortedMap.fromList(List.from(withdrawals)),
          validRange = interval,
          signatories = signatories,
          redeemers = AssocMap.fromList(List.from(redeemers)),
          data = SortedMap.fromList(List.from(data)),
          id = id,
          currentTreasuryAmount = treasury,
          treasuryDonation = donation
        )

    given Arbitrary[TxInfo] = Arbitrary(genTxInfo)

    given Arbitrary[ScriptPurpose] = Arbitrary {
        Gen.oneOf(
          arbitrary[PolicyId].map(ScriptPurpose.Minting.apply),
          arbitrary[TxOutRef].map(ScriptPurpose.Spending.apply),
        )
    }

}
