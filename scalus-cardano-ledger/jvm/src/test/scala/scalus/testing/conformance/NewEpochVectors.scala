package scalus.testing.conformance

import io.bullet.borer.*
import io.bullet.borer.Dom.{ArrayElem, Element, MapElem, TaggedElem}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.AccountState
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

    /** The fields of a ledger `NewEpochState` that the runner reads, spec [CV-10]. */
    final case class NewEpochState(
        lastEpoch: EpochNo,
        accountState: AccountState,
        ledger: LedgerState
    )

    /** One case of the corpus.
      *
      * @param enacted
      *   the ids of the actions the DRep pulser of `before` enacts at the boundary, spec [CV-11]
      * @param expired
      *   the ids of the actions the DRep pulser of `before` expires at the boundary, spec [CV-11]
      * @param deltaT
      *   the treasury change of the reward update of `before`, spec [CV-11]
      */
    final case class Case(
        name: String,
        boundary: EpochNo,
        before: NewEpochState,
        after: NewEpochState,
        enacted: Vector[GovActionId],
        expired: Set[GovActionId],
        deltaT: Coin
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
        val (enacted, expired, deltaT) = readPulserAndRewardUpdate(st)
        Case(
          name = name,
          boundary = Cbor.decode(bytes("sig")).to[Long].value,
          before = readNewEpochState(st),
          after = readNewEpochState(bytes("st_out")),
          enacted = enacted,
          expired = expired,
          deltaT = deltaT
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
            r.read[Element]() // blocks made in the previous epoch
            r.read[Element]() // blocks made in this epoch
            r.readArrayHeader(4)
            r.readArrayHeader(2) // ChainAccountState, K/State/ChainAccount.hs:46-48
            val accountState = AccountState(treasury = r.read[Coin](), reserves = r.read[Coin]())
            val ledger = r.read[LedgerState]()
            r.read[Element]() // snapshots
            r.read[Element]() // non-myopic
            r.read[Element]() // reward update
            r.read[Element]() // pool distribution
            r.read[Element]() // null
            NewEpochState(lastEpoch, accountState, ledger)
        }
        Cbor.decode(cbor).to[NewEpochState].value
    }

    /** The `RatifyState` of the DRep pulser and `deltaT` of the reward update, spec [CV-11].
      *
      * The pulser is the last field of the gov state, `[snapshot, ratifyState]` with `ratifyState
      * = [enactState, enacted, expired, delayed]` (conway Governance/Internal.hs:672-680); the dump
      * always encodes it finished (`DRComplete`, conway Governance/DRepPulser.hs:463-467). The
      * reward update is `[[1, [deltaT, deltaR, rs, deltaF, nonMyopic]]]`, a `Complete` update in a
      * `StrictMaybe` (shelley RewardUpdate.hs:125-131, 348).
      */
    private def readPulserAndRewardUpdate(
        st: Array[Byte]
    ): (Vector[GovActionId], Set[GovActionId], Coin) = {
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
        update(2) match
            case rs: MapElem => require(rs.size == 0, s"expected no rewards, got $rs")
            case other       => sys.error(s"expected the reward map, got $other")
        (enacted, expired, as[Coin](update(0)))
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
