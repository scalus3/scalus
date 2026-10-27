package scalus.cardano.ledger
package rules

import scalus.testing.kit.KeyPairGenerator
import org.scalacheck.Arbitrary
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart, StakeAddress}
import scalus.cardano.ledger.*
import scalus.uplc.builtin.{ByteString, Data}

import scala.collection.immutable

trait ValidatorRulesTestKit extends ArbitraryInstances {
    extension (tx: Transaction)
        def withNetwork(network: Network): Transaction = tx.copy(
          body = KeepRaw(
            tx.body.value.copy(
              networkId = Some(network.networkId),
              outputs = tx.body.value.outputs
                  .map(x =>
                      Sized(
                        Output(
                          Arbitrary.arbitrary[ShelleyAddress].sample.get.copy(network = network),
                          x.value.value
                        )
                      )
                  )
                  .appended(
                    Sized(
                      Output(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get.copy(network = network),
                        Arbitrary.arbitrary[Value].sample.get
                      )
                    )
                  ),
              withdrawals = tx.body.value.withdrawals
                  .map(w =>
                      w.copy(withdrawals =
                          w.withdrawals.map((k, v) =>
                              (k.copy(address = k.address.copy(network = network)), v)
                          )
                      )
                  )
                  .orElse(
                    Some(
                      Withdrawals(
                        immutable.SortedMap(
                          (
                            RewardAccount(
                              Arbitrary.arbitrary[StakeAddress].sample.get.copy(network = network)
                            ),
                            Arbitrary.arbitrary[Coin].sample.get
                          )
                        )
                      )
                    )
                  )
            )
          )
        )

    end extension

    protected def randomTransactionWithIsValidField: Transaction =
        Arbitrary.arbitrary[Transaction].sample.get.copy(isValid = true)

    protected def generateKeyPair(): (ByteString, ByteString) =
        KeyPairGenerator.generateKeyPair()

    /** A transaction that mints one token with `plutusScript`, and the UTxO it spends. */
    protected def mintTx(plutusScript: PlutusScript): (Transaction, Utxos) = {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxos = Map(
          input -> TransactionOutput(
            // A key address: a script address would need a spending script in the transaction.
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment = ShelleyPaymentPart.Key(AddrKeyHash.fromHex("a" * 56))),
            Value(Coin(1_000_000L))
          )
        )
        val tx = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = Some(
              Mint(
                MultiAsset(
                  immutable.SortedMap(
                    plutusScript.scriptHash -> immutable.SortedMap(AssetName.empty -> 1)
                  )
                )
              )
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Mint, 0, Data.unit, ExUnits.zero))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        (tx, utxos)
    }
}
