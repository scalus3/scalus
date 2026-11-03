package scalus.testing.conformance

import io.bullet.borer.*
import io.bullet.borer.Dom.{ArrayElem, Element, MapElem, TaggedElem}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.{AccountState, PoolSnapshot, RewardUpdate, SnapShots, StakeSnapshot}
import scalus.testing.conformance.CardanoLedgerVectors.{cacheDirectory, extractOnce}
import scalus.testing.conformance.LedgerState.given

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The Dugite NEWEPOCH dumps, spec [CV-9] to [CV-11]: each case is one epoch boundary, the
  * `NewEpochState` before it (`st`), the boundary epoch (`sig`) and the `NewEpochState` after it
  * (`st_out`). The provenance is in `newepoch-vectors.README.md` beside the archive.
  */
object NewEpochVectors {

    /** The fields of a ledger `NewEpochState` that the runner reads, spec [CV-10], [CV-14].
      *
      * @param blocksPrev
      *   the blocks each pool made in the epoch before, `nesBprev`
      * @param blocksCur
      *   the blocks each pool made in this epoch, `nesBcur`
      */
    final case class NewEpochState(
        lastEpoch: EpochNo,
        blocksPrev: Map[PoolKeyHash, Long],
        blocksCur: Map[PoolKeyHash, Long],
        accountState: AccountState,
        ledger: LedgerState,
        snapshots: SnapShots
    )

    /** One case of the corpus.
      *
      * @param enacted
      *   the ids of the actions the DRep pulser of `before` enacts at the boundary, spec [CV-11]
      * @param expired
      *   the ids of the actions the DRep pulser of `before` expires at the boundary, spec [CV-11]
      * @param rewardUpdate
      *   the reward update of `before`, spec [CV-11], [CV-14]
      * @param params
      *   the current pparams of `before`, gov state field 3, with only the fields the reward
      *   calculation reads taken from the dump, spec [CV-14]
      * @param prevParams
      *   the previous pparams of `before`, gov state field 4, likewise, spec [CV-14], [CV-16]
      */
    final case class Case(
        name: String,
        boundary: EpochNo,
        before: NewEpochState,
        after: NewEpochState,
        enacted: Vector[GovActionId],
        expired: Set[GovActionId],
        rewardUpdate: RewardUpdate,
        params: ProtocolParams,
        prevParams: ProtocolParams
    )

    /** The extracted `newepoch/NEWEPOCH` directory. Fails when the archive is not on the class
      * path: a skip would pass with no case.
      */
    lazy val casesPath: Path = {
        val resource = getClass.getClassLoader.getResource("newepoch-vectors.tar.gz")
        require(resource != null, "newepoch-vectors.tar.gz is not on the test class path")
        val archive = Paths.get(resource.toURI)
        extractOnce(archive, cacheDirectory(archive)).resolve("newepoch/NEWEPOCH")
    }

    /** The case directory names, `test_NNNN`, in numeric order. */
    def caseNames(): List[String] =
        Using
            .resource(Files.list(casesPath)) { paths =>
                paths
                    .iterator()
                    .asScala
                    .map(_.getFileName.toString)
                    .filter(_.startsWith("test_"))
                    .toList
            }
            .sortBy(_.stripPrefix("test_").toInt)

    def load(name: String): Case = {
        val dir = casesPath.resolve(name)
        def bytes(file: String) = Files.readAllBytes(dir.resolve(s"conformance_dump_$file.cbor"))
        val st = bytes("st")
        val read = readGovStateAndRewardUpdate(st)
        Case(
          name = name,
          boundary = Cbor.decode(bytes("sig")).to[Long].value,
          before = readNewEpochState(st),
          after = readNewEpochState(bytes("st_out")),
          enacted = read.enacted,
          expired = read.expired,
          rewardUpdate = read.rewardUpdate,
          params = read.params,
          prevParams = read.prevParams
        )
    }

    /** `[lastEpoch, blocksPrev, blocksCur, [[treasury, reserves], [certState, utxoState],
      * snapshots, nonMyopic], rewardUpdate, poolDistr, null]`, shelley
      * LedgerState/Types.hs:134-142, 401-411, spec [CV-10].
      */
    def readNewEpochState(cbor: Array[Byte]): NewEpochState = {
        given OriginalCborByteArray = OriginalCborByteArray(cbor)
        given Decoder[NewEpochState] = r => {
            r.readArrayHeader(7)
            val lastEpoch = r.readLong()
            val blocksPrev = r.read[Map[PoolKeyHash, Long]]()
            val blocksCur = r.read[Map[PoolKeyHash, Long]]()
            r.readArrayHeader(4)
            r.readArrayHeader(2) // ChainAccountState, K/State/ChainAccount.hs:46-48
            val accountState = AccountState(treasury = r.read[Coin](), reserves = r.read[Coin]())
            val ledger = r.read[LedgerState]()
            val snapshots = r.read[SnapShots]()
            r.read[Element]() // non-myopic
            r.read[Element]() // reward update
            r.read[Element]() // pool distribution
            r.read[Element]() // null
            NewEpochState(lastEpoch, blocksPrev, blocksCur, accountState, ledger, snapshots)
        }
        Cbor.decode(cbor).to[NewEpochState].value
    }

    /** What the runner reads from the gov state and the reward update of `st`. */
    private final case class GovStateAndRewardUpdate(
        enacted: Vector[GovActionId],
        expired: Set[GovActionId],
        rewardUpdate: RewardUpdate,
        params: ProtocolParams,
        prevParams: ProtocolParams
    )

    /** The `RatifyState` of the DRep pulser, the current and previous pparams and the reward
      * update, spec [CV-11], [CV-14].
      *
      * The gov state is `[proposals, committee, constitution, curPParams, prevPParams,
      * futurePParams, pulser]` (conway Governance.hs:361-371). The pulser is `[snapshot,
      * ratifyState]` with `ratifyState = [enactState, enacted, expired, delayed]` (conway
      * Governance/Internal.hs:672-680); the dump always encodes it finished (`DRComplete`, conway
      * Governance/DRepPulser.hs:463-467). The reward update is `[[1, [deltaT, -deltaR, rs, -deltaF,
      * nonMyopic]]]`, a `Complete` update in a `StrictMaybe`: the ledger encodes `invert deltaR`
      * and `invert deltaF` (shelley RewardUpdate.hs:125-131, re-inverted at :140-142, 348), and
      * `rs` maps a credential to a set of `[type, pool, amount]` (core Rewards.hs:89-91).
      */
    private def readGovStateAndRewardUpdate(st: Array[Byte]): GovStateAndRewardUpdate = {
        val nes = elems(Cbor.decode(st).to[Element].value)
        val govState = elems(elems(elems(elems(nes(3))(1))(1))(3))
        val ratifyState = elems(elems(govState(6))(1))
        val enacted = elems(ratifyState(1)).map(e => as[GovActionState](e).id)
        val expired = untagged(ratifyState(2)) match
            case a: ArrayElem => a.elems.map(as[GovActionId]).toSet
            case other        => sys.error(s"expected the expired set, got $other")
        val rewardUpdate = elems(nes(4))
        require(rewardUpdate.size == 1, s"expected a reward update, got $rewardUpdate")
        val complete = elems(rewardUpdate(0))
        require(as[Int](complete(0)) == 1, s"expected a Complete reward update, got $complete")
        val update = elems(complete(1))
        val rewards: Map[Credential, Coin] = update(2) match
            case rs: MapElem =>
                rs.toMap.map { (cred, set) =>
                    val amounts = elems(untagged(set)).map(reward => as[Long](elems(reward)(2)))
                    as[Credential](cred) -> Coin(amounts.sum)
                }
            case other => sys.error(s"expected the reward map, got $other")
        GovStateAndRewardUpdate(
          enacted = enacted,
          expired = expired,
          rewardUpdate = RewardUpdate(
            deltaT = Coin(as[Long](update(0))),
            deltaR = Coin(-as[Long](update(1))),
            rewards = rewards,
            deltaF = Coin(-as[Long](update(3)))
          ),
          params = rewardParams(govState(3)),
          prevParams = rewardParams(govState(4))
        )
    }

    /** The mainnet pparams with the fields the reward calculation reads taken from a Conway pparams
      * array: `nOpt`, `a0`, `rho`, `tau` are elements 8 to 11 (conway PParams.hs:860-873).
      */
    private def rewardParams(pparams: Element): ProtocolParams = {
        val pp = elems(pparams)
        CardanoInfo.mainnet.protocolParams.copy(
          stakePoolTargetNum = as[Long](pp(8)),
          poolPledgeInfluence = as[NonNegativeInterval](pp(9)),
          monetaryExpansion = as[UnitInterval](pp(10)),
          treasuryCut = as[UnitInterval](pp(11))
        )
    }

    /** `[mark, set, go, fee]` (core State/SnapShots.hs:355-361). */
    private given Decoder[SnapShots] = r => {
        r.readArrayHeader(4)
        SnapShots(
          r.read[StakeSnapshot](),
          r.read[StakeSnapshot](),
          r.read[StakeSnapshot](),
          r.read[Coin]()
        )
    }

    /** `[activeStake, pools]` with `activeStake` a map from credential to `[stake, pool]` (core
      * State/SnapShots.hs:290-296, State/Stake.hs:116-117). Checks the stake ratio of each pool
      * against the stake and the total active stake of the snapshot, so that equal snapshots have
      * equal ratios.
      */
    private given Decoder[StakeSnapshot] = r => {
        r.readArrayHeader(2)
        val active = r.read[Map[Credential, (Coin, PoolKeyHash)]]()
        val rated = r.read[Map[PoolKeyHash, RatedPool]]()
        val snapshot = StakeSnapshot(
          active.view.mapValues(_._1).toMap,
          active.view.mapValues(_._2).toMap,
          rated.view.mapValues(_.pool).toMap
        )
        val total = BigInt(snapshot.totalActiveStake.value)
        rated.foreach { (id, p) =>
            require(
              p.ratioNumerator * total == BigInt(p.pool.stake.value) * p.ratioDenominator,
              s"pool $id: stake ratio ${p.ratioNumerator}/${p.ratioDenominator} is not " +
                  s"${p.pool.stake.value}/$total"
            )
        }
        snapshot
    }

    /** A pool of a snapshot with its `spssStakeRatio`. */
    private final case class RatedPool(
        pool: PoolSnapshot,
        ratioNumerator: BigInt,
        ratioDenominator: BigInt
    )

    /** The 10-element `StakePoolSnapShot` (core State/SnapShots.hs:239-253). */
    private given Decoder[RatedPool] = r => {
        r.readArrayHeader(10)
        val stake = r.read[Coin]()
        if r.hasTag then r.readTag() // a Rational, tag 30 or none
        r.readArrayHeader(2)
        val numerator = r.read[BigInt]()
        val denominator = r.read[BigInt]()
        val owners = LedgerState.readTaggedSet[AddrKeyHash](r)
        RatedPool(
          PoolSnapshot(
            stake = stake,
            selfDelegatedOwners = owners,
            selfDelegatedOwnersStake = r.read[Coin](),
            vrfKeyHash = r.read[VrfKeyHash](),
            pledge = r.read[Coin](),
            cost = r.read[Coin](),
            margin = r.read[UnitInterval](),
            numDelegators = r.readInt(),
            rewardAccount = r.read[Credential]()
          ),
          numerator,
          denominator
        )
    }

    private def elems(e: Element): Vector[Element] = e match
        case a: ArrayElem => a.elems
        case other        => sys.error(s"expected an array, got $other")

    private def untagged(e: Element): Element = e match
        case TaggedElem(Tag.Other(258), value) => value
        case other                             => other

    private def as[A: Decoder](e: Element): A =
        Cbor.decode(Cbor.encode(e).toByteArray).to[A].value
}
