package scalus.cardano.ledger
package rules

import scalus.testing.kit.KeyPairGenerator
import org.scalacheck.Arbitrary
import scalus.cardano.address.{Network, ShelleyAddress, StakeAddress}
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

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
}
