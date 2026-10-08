package scalus.bloxbean

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap

/** [[StakeStateResolver]] from cached Blockfrost and Koios responses, so no request leaves the
  * test.
  */
class StakeStateResolverTest extends AnyFunSuite {

    private val slotConfig = SlotConfig.mainnet
    private val slot = slotConfig.firstSlotOfEpoch(543) + 1000
    private val time = slotConfig.slotToTime(slot) / 1000

    private val stakeKeyHash = StakeKeyHash.fromHex("11" * 28)
    private val stakeAddress = StakeAddress(Network.Mainnet, StakePayload.Stake(stakeKeyHash))
    private val credential = stakeAddress.credential
    private val bech32 = stakeAddress.toBech32.get

    private val pool = PoolKeyHash.fromHex("22" * 28)
    private val drep = AddrKeyHash.fromHex("33" * 28)
    private val target = AddrKeyHash.fromHex("44" * 28)

    private def drepId(hash: AddrKeyHash) = Bech32.encode("drep", 0x22.toByte +: hash.bytes)

    private val tx = Transaction(
      TransactionBody(
        inputs = TaggedSortedSet.empty[TransactionInput],
        outputs = IndexedSeq.empty[Sized[TransactionOutput]],
        fee = Coin.zero,
        certificates = TaggedOrderedStrictSet.from(
          Seq(Certificate.VoteDelegCert(credential, DRep.KeyHash(target)))
        ),
        withdrawals = Some(Withdrawals(SortedMap(RewardAccount(stakeAddress) -> Coin.zero)))
      )
    )

    private def certs(txHash: String, certType: String, info: String) =
        s"""[{"tx_hash":"$txHash","certificates":[{"type":"$certType","index":0,"info":{"stake_address":"$bech32",$info}}]}]"""

    private def drepUpdates(actions: (String, Long)*) = actions
        .map((action, t) => s"""{"block_time":$t,"action":"$action","deposit":"500000000"}""")
        .mkString("[", ",", "]")

    /** A cache where the account registered with a 2 ADA deposit, then delegated to `pool` and to
      * the DRep `drep`, before `slot`, and delegated its votes again after it. It has 5 in rewards
      * spendable by then and withdrew 3 before `slot`. `target` registered and deregistered before
      * `slot`.
      */
    private def cache(drepActions: (String, Long)*): Path = {
        val dir = Files.createTempDirectory("stake-state")
        def write(name: String, json: String) = Files.writeString(dir.resolve(name), json)
        // 4 and 1 spendable by epoch 543, and 7 spendable only from epoch 544
        write(
          s"koios-account-rewards-key-${"11" * 28}-1.json",
          """[{"spendable_epoch":541,"amount":"4","type":"member"},
            |{"spendable_epoch":543,"amount":"1","type":"treasury"},
            |{"spendable_epoch":544,"amount":"7","type":"member"}]""".stripMargin
        )
        write(
          s"blockfrost-account-registrations-key-${"11" * 28}-1.json",
          s"""[{"action":"registered","deposit":"2000000","tx_slot":${slot - 300}}]"""
        )
        // a withdrawal of 3 before the tx, and one of 1 in its slot
        write(
          s"blockfrost-account-withdrawals-key-${"11" * 28}-1.json",
          s"""[{"amount":"3","tx_slot":${slot - 50}},{"amount":"1","tx_slot":$slot}]"""
        )
        write(
          s"blockfrost-account-delegations-key-${"11" * 28}-1.json",
          s"""[{"pool_id":"${Bech32.encode("pool", pool.bytes)}","tx_slot":${slot - 200}}]"""
        )
        write(
          s"koios-account-updates-key-${"11" * 28}.json",
          s"""[{"stake_address":"$bech32","updates":[
             |{"tx_hash":"${"cc" * 32}","action_type":"delegation_drep","absolute_slot":${slot - 100}},
             |{"tx_hash":"${"dd" * 32}","action_type":"delegation_drep","absolute_slot":${slot + 10}}
             |]}]""".stripMargin
        )
        write(
          s"koios-tx-certs-${"cc" * 32}.json",
          certs("cc" * 32, "vote_delegation", s""""drep_id":"${drepId(drep)}"""")
        )
        write(
          s"koios-pool-updates-${"22" * 28}.json",
          s"""[{"tx_hash":"${"ee" * 32}","block_time":${time - 1000},"update_type":"registration",
             |"vrf_key_hash":"${"55" * 32}","margin":0.049,"fixed_cost":"340000000",
             |"pledge":"1000000000","reward_addr":"$bech32","owners":["$bech32"],
             |"relays":[{"dns":"relay.example","port":3001}],"retiring_epoch":null}]""".stripMargin
        )
        write(s"koios-drep-updates-key-${"33" * 28}.json", drepUpdates(drepActions*))
        write(
          s"koios-drep-updates-key-${"44" * 28}.json",
          drepUpdates("registered" -> (time - 900), "deregistered" -> (time - 50))
        )
        write("koios-totals-543.json", """[{"epoch_no":543,"treasury":"1690501545334536"}]""")
        dir
    }

    private def resolve(dir: Path) = StakeStateResolver("", dir).resolveForTx(tx, slot)

    test("an account carries its balance, deposit, pool and DRep as of the tx") {
        val state = resolve(cache("registered" -> (time - 1000)))
        assert(
          state.dstate.accounts == Map(
            credential -> ConwayAccountState(
              Coin(2),
              Coin(2_000_000),
              Some(pool),
              Some(DRep.KeyHash(drep))
            )
          )
        )
        assert(state.pstate.stakePools.keySet == Set(pool))
        assert(state.pstate.stakePools(pool).margin == UnitInterval(49, 1000))
        // `target` deregistered before the tx, so the tx's delegation to it must fail
        assert(state.vstate.dreps.keySet == Set(Credential.KeyHash(drep)))
    }

    test("a DRep deregistration after the delegation clears it") {
        val state = resolve(cache("registered" -> (time - 1000), "deregistered" -> (time - 50)))
        assert(state.dstate.accounts(credential).dRepDelegation.isEmpty)
        assert(state.vstate.dreps.isEmpty)
    }

    test("an account registered after the tx is not there") {
        val state = StakeStateResolver("", cache("registered" -> (time - 1000)))
            .resolveForTx(tx, slot - 300)
        assert(state.dstate.accounts.isEmpty)
    }

    test("the treasury of an epoch comes from Koios totals") {
        val dir = cache()
        assert(StakeStateResolver("", dir).treasuryAt(543) == Coin(1690501545334536L))
    }
}
