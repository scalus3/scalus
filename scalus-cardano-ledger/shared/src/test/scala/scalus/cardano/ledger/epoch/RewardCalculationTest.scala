package scalus.cardano.ledger.epoch

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.internal.{EpochEnv, EpochTransition}

import scala.collection.immutable.AbstractMap

/** The reward calculation of ledger-rules spec section 13.2, `EpochTransition.rewardUpdate`. */
class RewardCalculationTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private val maxSupply = BigInt(45_000_000_000_000_000L)

    /** Mainnet pparams with the rho, tau, a0 and nOpt of the spec example of section 13.4. */
    private val params: ProtocolParams = CardanoInfo.mainnet.protocolParams.copy(
      monetaryExpansion = UnitInterval(3, 1000),
      treasuryCut = UnitInterval(1, 5),
      poolPledgeInfluence = NonNegativeInterval(3, 10),
      stakePoolTargetNum = 500
    )

    private def env(
        performance: PoolPerformance,
        params: ProtocolParams = params,
        epochLength: Long = 432_000L
    ): EpochEnv = EpochEnv(params, epochLength, performance, UnitInterval(1, 20))

    private def poolId(i: Int): PoolKeyHash = PoolKeyHash.fromHex(f"$i%02x" * 28)
    private def keyHash(i: Int): AddrKeyHash = AddrKeyHash.fromHex(f"$i%02x" * 28)
    private def credential(i: Int): Credential = Credential.KeyHash(keyHash(i))
    private def scriptCredential(i: Int): Credential =
        Credential.ScriptHash(ScriptHash.fromHex(f"$i%02x" * 28))

    private def pool(
        stake: Long,
        rewardAccount: Credential,
        pledge: Long = 0,
        cost: Long = 170_000_000L,
        margin: UnitInterval = UnitInterval(1, 100),
        owners: Set[AddrKeyHash] = Set.empty,
        ownersStake: Long = 0
    ): PoolSnapshot =
        PoolSnapshot(
          stake = Coin(stake),
          selfDelegatedOwners = owners,
          selfDelegatedOwnersStake = Coin(ownersStake),
          vrfKeyHash = VrfKeyHash.fromHex("00" * 32),
          pledge = Coin(pledge),
          cost = Coin(cost),
          margin = margin,
          numDelegators = 1,
          rewardAccount = rewardAccount
        )

    /** A snapshot of the members of each pool, by credential and stake. */
    private def snapshot(
        pools: Map[PoolKeyHash, PoolSnapshot],
        members: (Credential, Long, PoolKeyHash)*
    ): StakeSnapshot =
        StakeSnapshot(
          members.map((c, s, _) => c -> Coin(s)).toMap,
          members.map((c, _, p) => c -> p).toMap,
          pools
        )

    /** The single-pool setup of the spec example of section 13.4: the member is `credential(1)`,
      * the pool's reward account `credential(2)`.
      */
    private val examplePool = pool(stake = 10_000_000_000_000L, rewardAccount = credential(2))
    private val exampleGo =
        snapshot(Map(poolId(1) -> examplePool), (credential(1), 10_000_000_000_000L, poolId(1)))
    private val exampleReserves = Coin(35_000_000_000_000_000L)

    /** The reward update of `go` with no fee, at the example reserves; no blocks unless given. */
    private def ideal(
        go: StakeSnapshot,
        e: EpochEnv = env(PoolPerformance.Ideal),
        blocks: Map[PoolKeyHash, Long] = Map.empty
    ): RewardUpdate =
        EpochTransition.rewardUpdate(go, Coin.zero, blocks, exampleReserves, e)

    test("the normative example of spec section 13.4") {
        // spec [RW-2], [RW-4], [RW-13], [DM-11a]
        val update =
            ideal(exampleGo)
        val deltaR1 = 105_000_000_000_000L
        val deltaR2 = 83_935_384_615_386L
        assert(update.deltaT == Coin(21_000_000_000_000L))
        assert(update.deltaR == Coin(deltaR2 - deltaR1))
        assert(update.deltaF == Coin.zero)
        assert(
          update.rewards == Map(
            credential(1) -> Coin(63_800_930_768L),
            credential(2) -> Coin(814_453_846L)
          )
        )
    }

    test("Ideal does not read the blocks made") {
        // spec [RW-15]: "MUST NOT be read"; a map that throws on any read shows an eager sum
        object unreadable extends AbstractMap[PoolKeyHash, Long] {
            private def read = throw IllegalStateException("blocksMadePrev was read")
            def get(key: PoolKeyHash): Option[Long] = read
            def iterator: Iterator[(PoolKeyHash, Long)] = read
            def removed(key: PoolKeyHash): Map[PoolKeyHash, Long] = read
            def updated[V >: Long](key: PoolKeyHash, value: V): Map[PoolKeyHash, V] = read
        }
        assert(ideal(exampleGo, blocks = unreadable).rewards.nonEmpty)
    }

    test(
      "Ideal pays no pool with zero stake, and a pool reward below the cost goes to the leader"
    ) {
        // spec [RW-15]: only pools with stake earn; S/Rewards.hs:110-127, 140-157 for poolR <= cost
        val cheap = pool(stake = 10_000_000_000L, rewardAccount = credential(3))
        val empty = pool(stake = 0, rewardAccount = credential(4))
        val go = snapshot(
          Map(poolId(1) -> cheap, poolId(2) -> empty),
          (credential(1), 10_000_000_000L, poolId(1))
        )
        val update = ideal(go)
        // maxP = floor(R/(1+a0) · sigma) = floor(84e12 / 1.3 · 1e10 / 1e16) = 64 615 384
        assert(update.rewards == Map(credential(3) -> Coin(64_615_384L)))
    }

    test("a pool whose owners do not meet the pledge earns nothing, and its leader entry is 0") {
        // S/Rewards.hs:359-362: maxP = 0 when pledge > ownerStake; spec [RW-8a]
        val unpledged = examplePool.copy(pledge = Coin(1))
        val go = exampleGo.copy(pools = Map(poolId(1) -> unpledged))
        val update = ideal(go)
        assert(update.rewards == Map(credential(2) -> Coin.zero))
        // deltaR2 = R: nothing is paid, so deltaR = R - deltaR1 = -deltaT
        assert(update.deltaR == Coin(-21_000_000_000_000L))
    }

    test("a self-delegated owner gets no member reward; a script with the same hash does") {
        // spec [RW-5], [RW-18]: notPoolOwner (S/Rewards.hs:285-290) excludes key hashes only
        val owners = pool(
          stake = 30_000_000_000_000L,
          rewardAccount = credential(9),
          pledge = 10_000_000_000_000L,
          owners = Set(keyHash(1)),
          ownersStake = 10_000_000_000_000L
        )
        val go = snapshot(
          Map(poolId(1) -> owners),
          (credential(1), 10_000_000_000_000L, poolId(1)),
          (scriptCredential(1), 10_000_000_000_000L, poolId(1)),
          (credential(2), 10_000_000_000_000L, poolId(1))
        )
        val update = ideal(go)
        assert(!update.rewards.contains(credential(1)))
        assert(update.rewards(scriptCredential(1)) == update.rewards(credential(2)))
        assert(update.rewards(credential(2)) > Coin.zero)
        assert(update.rewards(credential(9)) > Coin.zero)
    }

    test("a member whose share floors to 0 gets no entry, even when the pool earns") {
        // spec [RW-8]: calcStakePoolMemberReward gives 0 at margin 1 (S/Rewards.hs:140-157), and
        // rewardOnePoolMember drops it (:310-313); the leader takes all of poolR > cost
        val update = ideal(
          exampleGo.copy(pools = Map(poolId(1) -> examplePool.copy(margin = UnitInterval(1, 1))))
        )
        assert(update.rewards.keySet == Set(credential(2)))
        assert(update.rewards(credential(2)) > examplePool.cost)
    }

    test("the leader and member rewards of one credential are summed") {
        // spec [RW-7], [RW-20]: hardforkAllegraAggregatedRewards
        val own = examplePool.copy(rewardAccount = credential(1))
        val update = ideal(exampleGo.copy(pools = Map(poolId(1) -> own)))
        assert(update.rewards == Map(credential(1) -> Coin(63_800_930_768L + 814_453_846L)))
    }

    test("Measured pays only the pools with blocks; a block of a pool with no stake gives 0") {
        // spec [RW-3], [RW-15], [RW-8a]: mkApparentPerformance is 0 at sigmaA = 0
        // (S/Rewards.hs:92-93), and collectLRs keeps the leader reward of 0
        // (S/LedgerState/PulsingReward.hs:177-182)
        val go = snapshot(
          Map(
            poolId(1) -> examplePool,
            poolId(2) -> pool(stake = 0, rewardAccount = credential(4)),
            poolId(3) -> pool(stake = 5_000_000_000_000L, rewardAccount = credential(5))
          ),
          (credential(1), 10_000_000_000_000L, poolId(1)),
          (credential(3), 5_000_000_000_000L, poolId(3))
        )
        val update = EpochTransition.rewardUpdate(
          go,
          Coin.zero,
          Map(poolId(1) -> 10L, poolId(2) -> 10L),
          exampleReserves,
          env(PoolPerformance.Measured)
        )
        assert(update.rewards(credential(4)) == Coin.zero)
        assert(update.rewards(credential(2)) > Coin.zero)
        assert(!update.rewards.contains(credential(5)))
        assert(!update.rewards.contains(credential(3)))
    }

    test("Measured with expectedBlocks = 0: no block gives eta 0, one block gives eta 1") {
        // spec [RW-14]: floor(1/20 · 10) = 0, so eta = min(1, blocks / max(1, 0))
        def deltaR1(blocks: Map[PoolKeyHash, Long]): Coin = {
            val u = EpochTransition.rewardUpdate(
              StakeSnapshot.empty,
              Coin.zero,
              blocks,
              exampleReserves,
              env(PoolPerformance.Measured, epochLength = 10L)
            )
            // with no pool, rewards are empty: deltaR = R - deltaR1 = -deltaT1, deltaT1 = tau·deltaR1
            assert(u.rewards.isEmpty)
            Coin(u.deltaT.value * 5)
        }
        assert(deltaR1(Map.empty) == Coin.zero)
        // floor(3/1000 · 35e15) = 105e12, from a block of a pool that is not in go
        assert(deltaR1(Map(poolId(9) -> 1L)) == Coin(105_000_000_000_000L))
    }

    test("a total stake of 0 or below pays no pool and does not throw") {
        // spec [RW-16], D83: the ledger divides by zero (S/Rewards.hs:320, 390)
        for reserves <- Seq(Coin(45_000_000_000_000_000L), Coin(45_000_000_000_000_001L)) do
            val update = EpochTransition.rewardUpdate(
              exampleGo,
              Coin(1000),
              Map.empty,
              reserves,
              env(PoolPerformance.Ideal)
            )
            assert(update.rewards == Map(credential(2) -> Coin.zero))
            assert(update.deltaF == Coin(-1000))
    }

    test("an nOpt of 0 counts as 1") {
        // spec [RW-17], D83: maxPool takes 1 (K/State/SnapShots.hs:148-151)
        def run(nOpt: Long) =
            ideal(exampleGo, env(PoolPerformance.Ideal, params.copy(stakePoolTargetNum = nOpt)))
        assert(run(0) == run(1))
        assert(run(1) != run(2000))
    }

    // ---------------------------------------------------------------------------------------
    // spec section 16, phase 4 acceptance 2: a property test against a port of the reference
    // `rewardOld` and `rewardOnePool` (eras/shelley/test-suite/src/Test/Cardano/Ledger/Shelley/
    // Rewards.hs:368-520) with d = 0, at PV >= 7: rewards summed per credential, no prefilter.

    /** An exact rational over BigInt, separate from the production one. */
    private final case class Q(n: BigInt, d: BigInt) {
        def +(o: Q): Q = Q(n * o.d + o.n * d, d * o.d)
        def -(o: Q): Q = Q(n * o.d - o.n * d, d * o.d)
        def *(o: Q): Q = Q(n * o.n, d * o.d)
        def /(o: Q): Q = if o.n < 0 then Q(-n * o.d, d * -o.n) else Q(n * o.d, d * o.n)
        def <=(o: Q): Boolean = n * o.d <= o.n * d
        def min(o: Q): Q = if this <= o then this else o
        def floor: BigInt = {
            val (q, r) = n /% d
            if r < 0 then q - 1 else q
        }
    }
    private object Q {
        def apply(n: BigInt): Q = Q(n, 1)
        def of(i: UnitInterval): Q = Q(i.numerator, i.denominator)
        def of(i: NonNegativeInterval): Q = Q(i.numerator, i.denominator)
        // %?: 0 when the denominator is 0
        def safe(n: BigInt, d: BigInt): Q = if d == 0 then Q(0) else Q(n, d)
    }

    /** `rewardOld` of the reference, PV >= 7 and d = 0: per pool with a block, `rewardOnePool`. */
    private def referenceRewards(
        pp: ProtocolParams,
        blocks: Map[PoolKeyHash, Long],
        r: BigInt,
        go: StakeSnapshot,
        totalStake: BigInt
    ): Map[Credential, BigInt] = {
        val totalBlocks = BigInt(blocks.values.sum)
        val activeStake = go.activeStake.values.map(c => BigInt(c.value)).sum
        val perPool = go.pools.toSeq.flatMap { (poolId, ps) =>
            val members = go.activeStake.filter((c, _) => go.delegations.get(c).contains(poolId))
            val pstake = members.values.map(c => BigInt(c.value)).sum
            val sigma = Q.safe(pstake, totalStake)
            val sigmaA = Q.safe(pstake, activeStake)
            blocks
                .get(poolId)
                .map { n =>
                    val ostake = BigInt(ps.selfDelegatedOwnersStake.value)
                    val pledge = BigInt(ps.pledge.value)
                    val pr = Q(pledge, totalStake)
                    val maxP = if pledge <= ostake then maxPool(pp, r, sigma, pr) else BigInt(0)
                    val appPerf =
                        if sigmaA.n == 0 then Q(0)
                        else Q(n, totalBlocks.max(1)) / sigmaA
                    val poolR = (appPerf * Q(maxP)).floor
                    val cost = BigInt(ps.cost.value)
                    val m = Q.of(ps.margin)
                    def memberRew(t: Q): BigInt =
                        if poolR <= cost then 0
                        else (Q(poolR - cost) * (Q(1) - m) * t / sigma).floor
                    def leaderRew(s: Q): BigInt =
                        if poolR <= cost then poolR
                        else cost + (Q(poolR - cost) * (m + (Q(1) - m) * s / sigma)).floor
                    def notPoolOwner(c: Credential): Boolean = c match
                        case Credential.KeyHash(hk)   => !ps.selfDelegatedOwners.contains(hk)
                        case Credential.ScriptHash(_) => true
                    val mRewards = members.toSeq.collect {
                        case (c, coin) if notPoolOwner(c) =>
                            c -> memberRew(Q(coin.value, totalStake))
                    }
                    mRewards :+ (ps.rewardAccount -> leaderRew(Q(ostake, totalStake)))
                }
                .toSeq
                .flatten
        }
        perPool.groupMapReduce(_._1)(_._2)(_ + _).filter(_._2 != 0)
    }

    /** `maxPool` of the reference (S/Rewards.hs:330-344), nOpt 0 taken as 1. */
    private def maxPool(pp: ProtocolParams, r: BigInt, sigma: Q, pR: Q): BigInt = {
        val z0 = Q(1, BigInt(pp.stakePoolTargetNum.max(1)))
        val a0 = Q.of(pp.poolPledgeInfluence)
        val sigma1 = sigma.min(z0)
        val p1 = pR.min(z0)
        val factor4 = (z0 - sigma1) / z0
        val factor3 = (sigma1 - p1 * factor4) / z0
        val factor2 = sigma1 + p1 * a0 * factor3
        val factor1 = Q(r) / (Q(1) + a0)
        (factor1 * factor2).floor
    }

    private def interval(maxDen: Int): Gen[UnitInterval] =
        for
            d <- Gen.choose(1, maxDen)
            n <- Gen.choose(0, d)
        yield UnitInterval(n, d)

    private case class Inputs(
        go: StakeSnapshot,
        fee: Coin,
        blocks: Map[PoolKeyHash, Long],
        reserves: Coin,
        pp: ProtocolParams,
        epochLength: Long
    )

    private val genInputs: Gen[Inputs] = for
        nPools <- Gen.choose(1, 5)
        nCreds <- Gen.choose(0, 12)
        creds <- Gen.listOfN(
          nCreds,
          for
              i <- Gen.choose(1, 40)
              script <- Gen.oneOf(false, true)
              stake <- Gen.choose(1L, 200_000_000_000_000L)
              p <- Gen.choose(1, nPools)
          yield (if script then scriptCredential(i) else credential(i), stake, p)
        )
        members = creds.groupMapReduce(_._1)(c => (c._2, c._3))((a, _) => a)
        pools <- Gen.sequence[List[(PoolKeyHash, PoolSnapshot)], (PoolKeyHash, PoolSnapshot)](
          (1 to nPools).map { p =>
              val mine = members.filter(_._2._2 == p)
              val keyMembers = mine.keys.collect { case Credential.KeyHash(h) => h }.toSeq
              for
                  owners <- Gen.someOf(keyMembers)
                  ownersStake = owners
                      .map(h => mine(Credential.KeyHash(h))._1)
                      .sum
                  pledge <- Gen.oneOf(
                    Gen.const(0L),
                    Gen.choose(0L, ownersStake),
                    Gen.choose(ownersStake, ownersStake + 1_000_000L)
                  )
                  cost <- Gen.choose(0L, 500_000_000L)
                  margin <- interval(100)
                  account <- Gen.choose(1, 45).map(credential)
              yield poolId(p) -> pool(
                stake = mine.values.map(_._1).sum,
                rewardAccount = account,
                pledge = pledge,
                cost = cost,
                margin = margin,
                owners = owners.toSet,
                ownersStake = ownersStake
              )
          }
        )
        blocks <- Gen.mapOf(
          for
              p <- Gen.choose(1, nPools + 2)
              n <- Gen.choose(1L, 300L)
          yield poolId(p) -> n
        )
        activeSum = members.values.map(_._1).sum
        extra <- Gen.choose(1L, 2_000_000_000_000_000L)
        rho <- interval(1000)
        tau <- interval(1000)
        a0n <- Gen.choose(0L, 2000L)
        a0d <- Gen.choose(1L, 1000L)
        nOpt <- Gen.choose(1L, 600L)
        fee <- Gen.choose(0L, 100_000_000_000L)
        epochLength <- Gen.oneOf(10L, 3_600L, 432_000L)
    yield Inputs(
      StakeSnapshot(
        members.view.mapValues(v => Coin(v._1)).toMap,
        members.view.mapValues(v => poolId(v._2)).toMap,
        pools.toMap
      ),
      Coin(fee),
      blocks,
      Coin(maxSupply.toLong - activeSum - extra),
      params.copy(
        monetaryExpansion = rho,
        treasuryCut = tau,
        poolPledgeInfluence = NonNegativeInterval(a0n, a0d),
        stakePoolTargetNum = nOpt
      ),
      epochLength
    )

    test("Measured agrees with the reference rewardOnePool with d = 0, and conserves the pots") {
        forAll(genInputs, minSuccessful(300)) { in =>
            val update = EpochTransition.rewardUpdate(
              in.go,
              in.fee,
              in.blocks,
              in.reserves,
              env(PoolPerformance.Measured, in.pp, in.epochLength)
            )
            // startStep, S/LedgerState/PulsingReward.hs:115-141, with d = 0 and [RW-14]
            val expected = (Q(1, 20) * Q(in.epochLength)).floor
            val eta = Q(1).min(Q(BigInt(in.blocks.values.sum), expected.max(1)))
            val deltaR1 = (eta * Q.of(in.pp.monetaryExpansion) * Q(in.reserves.value)).floor
            val rPot = in.fee.value + deltaR1
            val deltaT1 = (Q.of(in.pp.treasuryCut) * Q(rPot)).floor
            val r = rPot - deltaT1
            val rewards =
                referenceRewards(in.pp, in.blocks, r, in.go, maxSupply - in.reserves.value)
            assert(
              update.rewards.filter(_._2 != Coin.zero).view.mapValues(_.value).toMap ==
                  rewards.view.mapValues(_.toLong).toMap
            )
            assert(update.deltaT.value == deltaT1)
            assert(update.deltaR.value == r - rewards.values.sum - deltaR1)
            assert(update.deltaF == Coin(-in.fee.value))
            // spec [RW-9]
            val paid = update.rewards.values.map(c => BigInt(c.value)).sum
            assert(
              BigInt(update.deltaT.value) + update.deltaR.value + paid + update.deltaF.value == 0
            )
            // spec [RW-8a]: every pool with a block has a leader entry, also of 0
            in.go.pools.foreach { (id, ps) =>
                if in.blocks.contains(id) then assert(update.rewards.contains(ps.rewardAccount))
            }
            // spec [RW-8]: a 0 entry is a leader entry, never a member entry
            val leaderAccounts =
                in.go.pools.collect { case (id, ps) if in.blocks.contains(id) => ps.rewardAccount }
            update.rewards.foreach { (cred, coin) =>
                if coin == Coin.zero then assert(leaderAccounts.toSet.contains(cred), cred)
            }
        }
    }
}
