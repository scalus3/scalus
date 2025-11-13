package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.*
import scalus.cardano.ledger.*

class StakeDistributionTest extends AnyFunSuite with Matchers:

    test("validate stake aggregation with simple mock data") {
        val cred = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
        val addr = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(AddrKeyHash.fromHex("1" * 56)),
          ShelleyDelegationPart.Key(StakeKeyHash.fromHex("a" * 56))
        )

        // Multiple UTxOs for same credential
        val utxos: Utxos = Map(
          TransactionInput(TransactionHash.fromHex("1" * 64), 0) ->
              TransactionOutput.Shelley(addr, Value(Coin(1000000000), MultiAsset.empty), None),
          TransactionInput(TransactionHash.fromHex("2" * 64), 0) ->
              TransactionOutput.Shelley(addr, Value(Coin(2000000000L), MultiAsset.empty), None)
        )

        val snapshot = StakeDistribution.computeSnapshot(utxos, Map.empty, Map.empty)

        // Should aggregate stake for the same credential
        snapshot.stakeCredentials.size shouldBe 1
        snapshot.stakeCredentials(cred).value shouldBe 3000000000L
    }

    test("handle enterprise addresses without delegation") {
        val addr = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(AddrKeyHash.fromHex("1" * 56)),
          ShelleyDelegationPart.Null // Enterprise address
        )

        val utxos: Utxos = Map(
          TransactionInput(TransactionHash.fromHex("1" * 64), 0) ->
              TransactionOutput.Shelley(addr, Value(Coin(1000000000), MultiAsset.empty), None)
        )

        val snapshot = StakeDistribution.computeSnapshot(utxos, Map.empty, Map.empty)

        // Enterprise addresses don't contribute to stake distribution
        snapshot.activeStake.value shouldBe 0L
        snapshot.stakeCredentials shouldBe empty
    }

    test("validate DRep voting stake with simple mock data") {
        val cred1 = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
        val cred2 = Credential.KeyHash(AddrKeyHash.fromHex("b" * 56))

        val stakeByCredential = Map(
          cred1 -> Coin(1000000000),
          cred2 -> Coin(2000000000L)
        )

        val drep1 = DRep.KeyHash(AddrKeyHash.fromHex("d" * 56))
        val drep2 = DRep.AlwaysAbstain

        val drepDelegations = Map(
          cred1 -> drep1,
          cred2 -> drep2
        )

        val drepStakes = StakeDistribution.computeDRepVotingStake(
          stakeByCredential,
          drepDelegations
        )

        println(s"\nDRep voting stakes:")
        println(s"  DRep stakes: ${drepStakes}")

        drepStakes.size shouldBe 2
        drepStakes(drep1).value shouldBe 1000000000L
        drepStakes(drep2).value shouldBe 2000000000L
    }
