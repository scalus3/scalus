package scalus.testing.conformance

import io.bullet.borer.*
import io.bullet.borer.Dom.{ArrayElem, ByteArrayElem, Element, MapElem}
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.cardano.ledger.*

import java.nio.file.{Files, Path}
import scala.util.Using

// Decoders only: nothing encodes a vector state, and the gov state has no encoder (spec D36)
case class LedgerState(certs: LedgerState.CertState, utxos: UTxOState) derives Decoder
object LedgerState {

    /** A CBOR set, with or without tag 258. */
    private[conformance] def readTaggedSet[A: Decoder](r: Reader): Set[A] =
        if r.dataItem() == DataItem.Tag then
            val tag = r.readTag()
            if tag.code != 258 then r.validationFailure(s"Expected tag 258 for Set, got $tag")
        r.read[Set[A]]()

    def fromCbor(cbor: Array[Byte]): LedgerState = {
        given OriginalCborByteArray = OriginalCborByteArray(cbor)
        Cbor.decode(cbor).to[LedgerState].value
    }

    /** Create UtxoEnv for conformance testing with small deposit values.
      *
      * Cardano ledger conformance tests use small deposit values (e.g., 2 lovelace) to simplify
      * testing. This creates an environment with those test-appropriate values.
      *
      * Note: For actual conformance tests, prefer extracting protocol params from the test vectors
      * using ConwayProtocolParams to get the exact values used in each test case.
      */
    def conformanceTestEnv(slot: SlotNo = 0): rules.UtxoEnv =
        val baseParams: ProtocolParams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )
        // Override deposit values to match conformance test expectations
        val params = baseParams.copy(
          stakeAddressDeposit = 2, // keyDeposit = Coin 2 in conformance tests
          stakePoolDeposit = 3, // poolDeposit = Coin 3 in some tests
          dRepDeposit = 1000000 // Keep reasonable DRep deposit
        )
        rules.UtxoEnv(
          slot,
          params,
          scalus.cardano.ledger.CertState.empty,
          scalus.cardano.address.Network.Testnet,
          scalus.cardano.ledger.Coin.zero
        )

    extension (ledgerState: LedgerState)
        def ruleState = rules.State(
          utxos = ledgerState.utxos.utxo,
          deposited = ledgerState.utxos.deposited,
          fees = ledgerState.utxos.fees,
          govState = ledgerState.utxos.govState,
          stakeDistribution = ledgerState.utxos.stakeDistribution,
          donation = ledgerState.utxos.donation,
          certState = ledgerState.certs.toCertState
        )

    /** Conway CertState from cardano-ledger test vectors.
      *
      * ConwayCertState is encoded as: [vstate, pstate, dstate] (3 elements)
      *   - vstate: VState (voting state with DReps)
      *   - pstate: PState (pool state)
      *   - dstate: DState (delegation state with accounts/deposits)
      */
    case class CertState(
        vstate: VState,
        pstate: PState,
        dstate: DState
    )

    object CertState {
        val empty: CertState =
            CertState(VState.empty, PState.empty, DState.empty)

        given Decoder[CertState] with
            def read(r: Reader): CertState =
                r.readArrayHeader(3)
                val vstate = r.read[VState]()
                val pstate = r.read[PState]()
                val dstate = r.read[DState]()
                CertState(vstate, pstate, dstate)
    }

    extension (certState: CertState)
        def toCertState: scalus.cardano.ledger.CertState =
            scalus.cardano.ledger.CertState(
              vstate = certState.vstate.toVotingState,
              pstate = PoolsState(
                stakePools = certState.pstate.stakePools,
                futureStakePoolParams = certState.pstate.futureStakePoolParams,
                retiring = certState.pstate.retiring,
                deposits = certState.pstate.deposits,
                vrfKeyHashes = certState.pstate.vrfKeyHashes
              ),
              dstate = certState.dstate.toDelegationState
            )

    /** Conway PState from cardano-ledger test vectors.
      *
      * PState is a 4-element array in one of two layouts:
      *   - older dumps (the JVM vectors): [psStakePoolParams: Map PoolKeyHash PoolParams,
      *     psFutureStakePoolParams: Map PoolKeyHash PoolParams, psRetiring: Map PoolKeyHash
      *     EpochNo, psDeposits: Map PoolKeyHash Coin]
      *   - current cardano-ledger (the IT vectors, `CertState.hs` `EncCBOR (PState era)`):
      *     [psVRFKeyHashes: Map VrfKeyHash Word64, psStakePools: Map PoolKeyHash StakePoolState,
      *     psFutureStakePoolParams: Map PoolKeyHash StakePoolParams, psRetiring: Map PoolKeyHash
      *     EpochNo]; a pool deposit is the `spsDeposit` field of its StakePoolState.
      *
      * PoolParams and StakePoolParams are a 9-element array: [operator, vrfKeyHash, pledge, cost,
      * margin, rewardAccount, poolOwners, relays, poolMetadata]. StakePoolState is a 10-element
      * array: [vrfKeyHash, pledge, cost, margin, accountId, poolOwners, relays, poolMetadata,
      * deposit, delegators].
      */
    case class PState(
        stakePools: Map[PoolKeyHash, Certificate.PoolRegistration],
        futureStakePoolParams: Map[PoolKeyHash, Certificate.PoolRegistration],
        retiring: Map[PoolKeyHash, EpochNo],
        deposits: Map[PoolKeyHash, Coin],
        vrfKeyHashes: Map[VrfKeyHash, Long]
    )

    object PState {
        val empty: PState = PState(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

        /** Decode a 9-element PoolParams or StakePoolParams; skip trailing fields of later
          * versions.
          */
        private def readPoolParams(r: Reader): Certificate.PoolRegistration =
            val size = r.readArrayHeader()
            val registration: Certificate.PoolRegistration = Certificate.PoolRegistration(
              operator = r.read[AddrKeyHash](),
              vrfKeyHash = r.read[VrfKeyHash](),
              pledge = r.read[Coin](),
              cost = r.read[Coin](),
              margin = r.read[UnitInterval](),
              rewardAccount = r.read[RewardAccount](),
              poolOwners = readTaggedSet[AddrKeyHash](r),
              relays = r.read[IndexedSeq[Relay]](),
              poolMetadata = readStrictMaybePoolMetadata(r)
            )
            for _ <- 9 until size.toInt do r.read[Element]()
            registration

        /** Decode a StakePoolState of a registered pool into its registration and deposit. The
          * delegators field has no counterpart in [[PoolsState]] and is skipped.
          */
        private def readStakePoolState(
            r: Reader,
            operator: PoolKeyHash
        ): (Certificate.PoolRegistration, Coin) =
            r.readArrayHeader(10)
            val registration: Certificate.PoolRegistration = Certificate.PoolRegistration(
              operator = AddrKeyHash.fromByteString(operator),
              vrfKeyHash = r.read[VrfKeyHash](),
              pledge = r.read[Coin](),
              cost = r.read[Coin](),
              margin = r.read[UnitInterval](),
              rewardAccount = readAccountId(r),
              poolOwners = readTaggedSet[AddrKeyHash](r),
              relays = r.read[IndexedSeq[Relay]](),
              poolMetadata = readStrictMaybePoolMetadata(r)
            )
            val deposit = r.read[Coin]()
            r.read[Element]() // delegators
            (registration, deposit)

        /** Read StrictMaybe PoolMetadata: null, [] (SNothing), or [x] / value (SJust). */
        private def readStrictMaybePoolMetadata(r: Reader): Option[PoolMetadata] =
            if r.tryReadNull() then None
            else if r.dataItem() == DataItem.ArrayHeader then
                val len = r.readArrayHeader()
                if len == 0 then None // [] = SNothing
                else if len == 1 then Some(r.read[PoolMetadata]()) // [x] = SJust
                else
                    // Inline PoolMetadata = [url, hash]
                    val url = r.readString()
                    val hash = r.read[MetadataHash]()
                    // Skip remaining if any
                    for _ <- 2 until len.toInt do r.read[Element]()
                    Some(PoolMetadata(url, hash))
            else Some(r.read[PoolMetadata]())

        /** Read an AccountId (a staking Credential [tag, hash]) as a Testnet reward account. */
        private def readAccountId(r: Reader): RewardAccount =
            val payload = r.read[Credential]() match
                case Credential.KeyHash(hash) =>
                    scalus.cardano.address.StakePayload.Stake(StakeKeyHash.fromByteString(hash))
                case Credential.ScriptHash(hash) =>
                    scalus.cardano.address.StakePayload.Script(hash)
            RewardAccount(
              scalus.cardano.address.StakeAddress(scalus.cardano.address.Network.Testnet, payload)
            )

        given Decoder[PState] with
            def read(r: Reader): PState =
                r.readArrayHeader(4)
                val maps = Vector.fill(4)(r.read[Element]() match
                    case m: MapElem => m
                    case other      => r.validationFailure(s"Expected a map in PState, got $other"))
                if isCurrentLayout(maps) then
                    val stakePools = decodeMap(maps(1))(
                      r => r.read[PoolKeyHash](),
                      (r, pool) => readStakePoolState(r, pool)
                    )
                    PState(
                      stakePools = stakePools.view.mapValues(_._1).toMap,
                      futureStakePoolParams = decodeMap(maps(2))(
                        r => r.read[PoolKeyHash](),
                        (r, _) => readPoolParams(r)
                      ),
                      retiring =
                          decodeMap(maps(3))(r => r.read[PoolKeyHash](), (r, _) => r.readLong()),
                      deposits = stakePools.view.mapValues(_._2).toMap,
                      // spec [CV-4]
                      vrfKeyHashes =
                          decodeMap(maps(0))(r => r.read[VrfKeyHash](), (r, _) => r.readLong())
                    )
                else
                    PState(
                      stakePools = decodeMap(maps(0))(
                        r => r.read[PoolKeyHash](),
                        (r, _) => readPoolParams(r)
                      ),
                      futureStakePoolParams = decodeMap(maps(1))(
                        r => r.read[PoolKeyHash](),
                        (r, _) => readPoolParams(r)
                      ),
                      retiring =
                          decodeMap(maps(2))(r => r.read[PoolKeyHash](), (r, _) => r.readLong()),
                      deposits =
                          decodeMap(maps(3))(r => r.read[PoolKeyHash](), (r, _) => r.read[Coin]()),
                      // the old layout has no VRF map, spec [CV-4]
                      vrfKeyHashes = Map.empty
                    )

        /** Tell the layouts apart by the first non-empty map among the first three: its values are
          * VRF counts or PoolParams (slot 0), StakePoolState starting with a 32-byte VRF hash or
          * PoolParams starting with a 28-byte operator (slot 1), StakePoolParams or EpochNo (slot
          * 2). With all three empty, both layouts decode to the same pools.
          */
        private def isCurrentLayout(maps: Vector[MapElem]): Boolean =
            def firstValue(slot: Int): Option[Element] = maps(slot).members.nextOption().map(_._2)
            firstValue(0)
                .map(!_.isInstanceOf[ArrayElem])
                .orElse(firstValue(1).map {
                    case pool: ArrayElem =>
                        pool.elems.headOption.exists {
                            case bytes: ByteArrayElem => bytes.bytes.length == 32
                            case _                    => false
                        }
                    case _ => false
                })
                .orElse(firstValue(2).map(_.isInstanceOf[ArrayElem]))
                .getOrElse(false)

        private def decodeMap[K, V](m: MapElem)(
            readKey: Reader => K,
            readValue: (Reader, K) => V
        ): Map[K, V] =
            m.members.map { (k, v) =>
                val key = reread(k)(readKey)
                key -> reread(v)(readValue(_, key))
            }.toMap

        private def reread[A](element: Element)(read: Reader => A): A =
            given Decoder[A] = r => read(r)
            Cbor.decode(Cbor.encode(element).toByteArray).to[A].value
    }

    /** Conway VState from cardano-ledger.
      *
      * VState is encoded as: [dreps, committeeState, numDormantEpochs] (3 elements)
      *   - dreps: Map Credential DRepState
      *   - committeeState: CommitteeState (a map)
      *   - numDormantEpochs: EpochNo
      */
    case class VState(
        dreps: Map[Credential, ConwayDRepState],
        committeeState: Map[Credential, CommitteeAuthorization],
        numDormantEpochs: Long
    )

    object VState {
        val empty: VState = VState(Map.empty, Map.empty, 0)

        given Decoder[VState] with
            def read(r: Reader): VState =
                r.readArrayHeader(3)
                val dreps = r.read[Map[Credential, ConwayDRepState]]()
                // spec [CV-6]: CommitteeState is a newtype over the map
                val committeeState = r.read[Map[Credential, CommitteeAuthorization]]()
                val numDormantEpochs = r.readLong()
                VState(dreps, committeeState, numDormantEpochs)
    }

    extension (vstate: VState)
        def toVotingState: VotingState =
            VotingState(
              dreps = vstate.dreps.map { case (cred, drepState) =>
                  cred -> DRepState(
                    expiry = drepState.expiry,
                    anchor = drepState.anchor,
                    deposit = drepState.deposit,
                    delegates = drepState.delegates
                  )
              },
              // spec [CV-4]
              numDormantEpochs = vstate.numDormantEpochs,
              // spec [CV-6]
              committeeState = vstate.committeeState
            )

    /** Conway DRepState from cardano-ledger.
      *
      * DRepState is encoded as: [expiry, anchor, deposit, delegates] (4 elements)
      */
    case class ConwayDRepState(
        expiry: Long,
        anchor: Option[Anchor],
        deposit: Coin,
        delegates: Set[Credential]
    )

    object ConwayDRepState {
        given Decoder[ConwayDRepState] with
            def read(r: Reader): ConwayDRepState =
                r.readArrayHeader(4)
                val expiry = r.readLong()
                val anchor = r.read[Option[Anchor]]()
                val deposit = r.read[Coin]()
                val delegates = readTaggedSet[Credential](r)
                ConwayDRepState(expiry, anchor, deposit, delegates)
    }

    /** Conway DState from cardano-ledger.
      *
      * DState is encoded as: [dsAccounts, dsFutureGenDelegs, dsGenDelegs, dsIRewards] (4 elements)
      *
      * In test vectors, dsAccounts uses old UMap format: [[umElems: Map], [umPtrs: Map]] The
      * umElems map contains UMElem entries with rewards, deposits, and delegations.
      *
      * UMElem encoding: [RDPair, stakePool, dRep, ptrs] where: - RDPair = [reward, deposit] (both
      * CompactCoin) - stakePool = null | KeyHash - dRep = null | DRep - ptrs = Set (always empty
      * set 0x80 for simplicity)
      */
    case class DState(
        accounts: Map[Credential, ConwayAccountState],
        futureGenDelegs: Element,
        genDelegs: Element,
        iRewards: Element
    )

    object DState {
        // Create empty DOM elements for the empty state
        private val emptyMapElem: Element = Cbor.decode(Array[Byte](0xa0.toByte)).to[Element].value
        private val emptyArrayElem: Element = Cbor
            .decode(Array[Byte](0x84.toByte, 0xa0.toByte, 0xa0.toByte, 0xa0.toByte, 0xa0.toByte))
            .to[Element]
            .value

        val empty: DState = DState(
          Map.empty,
          emptyMapElem,
          emptyMapElem,
          emptyArrayElem
        )

        given Decoder[DState] with
            def read(r: Reader): DState =
                r.readArrayHeader(4)
                // dsAccounts - supports two formats:
                // New format (cardano-ledger 1.x+): Map Credential AccountState
                // Old format (UMap): [umElems: Map, umPtrs: Map]
                val accounts = parseAccounts(r)
                val futureGenDelegs = r.read[Element]()
                val genDelegs = r.read[Element]()
                val iRewards = r.read[Element]()
                DState(accounts, futureGenDelegs, genDelegs, iRewards)

        private def parseAccounts(r: Reader): Map[Credential, ConwayAccountState] =
            // Check if next item is a Map (new format) or Array (old UMap format)
            val dataItem = r.dataItem()
            if dataItem == DataItem.MapHeader || dataItem == DataItem.MapStart then
                // New format: direct Map Credential AccountState
                parseDirectMap(r)
            else
                // Old format: UMap as [umElems: Map, umPtrs: Map]
                parseUMapAccounts(r)

        /** Parse new direct Map format: Map Credential AccountState */
        private def parseDirectMap(r: Reader): Map[Credential, ConwayAccountState] =
            val mapSize = r.readMapHeader()
            (0 until mapSize.toInt).map { _ =>
                val cred = r.read[Credential]()
                val accountState = r.read[ConwayAccountState]()
                cred -> accountState
            }.toMap

        /** Parse old UMap format: [umElems: Map, umPtrs: Map] */
        private def parseUMapAccounts(r: Reader): Map[Credential, ConwayAccountState] =
            r.readArrayHeader(2)
            // umElems: Map Credential UMElem
            val umElemsSize = r.readMapHeader()
            val accounts = (0 until umElemsSize.toInt).map { _ =>
                val cred = r.read[Credential]()
                val accountState = parseUMElem(r)
                cred -> accountState
            }.toMap
            // umPtrs: Map Ptr Credential - skip
            r.read[Element]()
            accounts

        /** Parse UMElem from cardano-ledger UMap format
          *
          * UMElem is encoded as: [StrictMaybe RDPair, Set Ptr, StrictMaybe KeyHash, StrictMaybe
          * DRep]
          *
          * StrictMaybe encoding:
          *   - SNothing = [] (array length 0)
          *   - SJust x = [x] (array length 1)
          *
          * RDPair = [reward, deposit] (CompactCoin each)
          */
        private def parseUMElem(r: Reader): ConwayAccountState =
            r.readArrayHeader(4)
            // StrictMaybe RDPair - encoded as [] or [RDPair]
            val rdPairArrayLen = r.readArrayHeader()
            val (balance, deposit) =
                if rdPairArrayLen == 0 then (Coin.zero, Coin.zero)
                else
                    // RDPair = [reward, deposit]
                    r.readArrayHeader(2)
                    val reward = r.read[Coin]()
                    val dep = r.read[Coin]()
                    (reward, dep)
            // Set Ptr - skip
            r.read[Element]()
            // StrictMaybe KeyHash - encoded as [] or [KeyHash]
            val stakePoolArrayLen = r.readArrayHeader()
            val stakePoolDelegation =
                if stakePoolArrayLen == 0 then None else Some(r.read[PoolKeyHash]())
            // StrictMaybe DRep - encoded as [] or [DRep]
            val dRepArrayLen = r.readArrayHeader()
            val dRepDelegation = if dRepArrayLen == 0 then None else Some(r.read[DRep]())
            ConwayAccountState(balance, deposit, stakePoolDelegation, dRepDelegation)
    }

    extension (dstate: DState)
        def toDelegationState: DelegationState = DelegationState(dstate.accounts)

    given Decoder[TransactionInput] with
        def read(r: Reader): TransactionInput =
            if r.hasByteArray then MempackParser.parseTransactionInput(r.readByteArray())
            else r.read[TransactionInput]()

    given Decoder[TransactionOutput] with
        def read(r: Reader): TransactionOutput =
            if r.hasByteArray then MempackParser.parseOutput(r.readByteArray())
            else r.read[TransactionOutput]()

    /** A `StrictMaybe` in the ledger state: `[]` or `[x]` (spec [CV-6]). */
    private def readStrictMaybe[A: Decoder](r: Reader): Option[A] =
        r.readArrayHeader() match
            case 0 => None
            case 1 => Some(r.read[A]())
            case n => r.validationFailure(s"Expected a StrictMaybe of 0 or 1 elements, got $n")

    /** `[0, credential]` or `[1, [] | [anchor]]`, K/State/CertState.hs:315-325. */
    given Decoder[CommitteeAuthorization] with
        def read(r: Reader): CommitteeAuthorization =
            r.readArrayHeader(2)
            r.readInt() match
                case 0 => CommitteeAuthorization.HotCredential(r.read[Credential]())
                case 1 => CommitteeAuthorization.Resigned(readStrictMaybe[Anchor](r))
                case t => r.validationFailure(s"Unknown CommitteeAuthorization tag $t")

    /** `[members, threshold]`, C/Governance/Procedures.hs:575-580. */
    given Decoder[Committee] with
        def read(r: Reader): Committee =
            r.readArrayHeader(2)
            Committee(r.read[Map[Credential, EpochNo]](), r.read[UnitInterval]())

    /** The seven fields in order, C/Governance/Procedures.hs:317-327. */
    given Decoder[GovActionState] with
        def read(r: Reader): GovActionState =
            r.readArrayHeader(7)
            GovActionState(
              id = r.read[GovActionId](),
              committeeVotes = r.read[Map[Credential, Vote]](),
              drepVotes = r.read[Map[Credential, Vote]](),
              spoVotes = r.read[Map[PoolKeyHash, Vote]](),
              procedure = r.read[ProposalProcedure](),
              proposedIn = r.readLong(),
              expiresAfter = r.readLong()
            )

    /** `GovRelation StrictMaybe`, four `[] | [id]`, C/Governance/Procedures.hs:721-729. */
    given Decoder[GovRoots] with
        def read(r: Reader): GovRoots =
            r.readArrayHeader(4)
            GovRoots(
              pparamUpdate = readStrictMaybe[GovActionId](r),
              hardFork = readStrictMaybe[GovActionId](r),
              committee = readStrictMaybe[GovActionId](r),
              constitution = readStrictMaybe[GovActionId](r)
            )

    /** The `ConwayGovState` record (C/Governance.hs:361-371), spec [CV-6]: the proposals as
      * `[roots, [GovActionState…]]` (C/Governance/Proposals.hs:385-388), the committee and the
      * constitution. The pparams, future pparams and DRep pulser that follow are skipped; the
      * header length is read, not assumed, so a dump with another tail still decodes.
      */
    given Decoder[GovState] with
        def read(r: Reader): GovState =
            val size = r.readArrayHeader()
            r.readArrayHeader(2)
            val roots = r.read[GovRoots]()
            val proposals = r.read[Vector[GovActionState]]()
            val committee = readStrictMaybe[Committee](r)
            val constitution = r.read[Constitution]()
            // read[Element], not skipElement: on the vectors skipElement stopped after the header
            // of the tag-258 set in the DRep pulser and left its content unread
            for _ <- 3L until size do r.read[Element]()
            GovState(proposals, roots, committee, constitution)

    given Decoder[UTxOState] = Decoder.derived

}

// Conway PParams (case class + Decoder + toProtocolParams + fromCbor) lives in
// scalus.cardano.ledger.ConwayProtocolParams — needed by production callers (e.g. the
// LocalStateQuery N2C path in scalus-node) for `GetCurrentPParams`. The conformance-test
// fixture helpers below depend on it but stay in test code because they index a directory
// of test vectors keyed by hash.

/** Test-fixture helpers for loading [[ConwayProtocolParams]] from the on-disk `pparams-by-hash`
  * directory used by cardano-ledger conformance test vectors.
  */
object ConwayProtocolParamsFixtures:

    /** Load protocol params from pparams-by-hash directory using hash found in ledger state */
    def loadFromHash(pparamsDir: Path, hash: String): Option[ConwayProtocolParams] =
        val pparamsFile = pparamsDir.resolve(hash)
        if Files.exists(pparamsFile) then
            val cbor = Files.readAllBytes(pparamsFile)
            Some(ConwayProtocolParams.fromCbor(cbor))
        else None

    /** Extract pparams hash from ledger state CBOR hex string.
      *
      * The hash appears after "5820" (CBOR prefix for 32-byte bytestring) in the GovState section.
      * Since multiple pparams hashes may appear in the state (prevPParams and curPParams), we find
      * all matching hashes and return the one with the highest protocol version, as the current
      * pparams should have the highest version.
      */
    def extractPparamsHash(oldLedgerStateHex: String, pparamsDir: Path): Option[String] =
        import scala.jdk.CollectionConverters.*
        if !Files.exists(pparamsDir) then return None

        // closed: a leaked stream per case ran the sbt server out of file descriptors
        val matchingHashes = Using.resource(Files.list(pparamsDir)) { paths =>
            paths
                .iterator()
                .asScala
                .map(_.getFileName.toString)
                .filter(hash => oldLedgerStateHex.contains(hash))
                .toList
        }

        if matchingHashes.isEmpty then return None
        if matchingHashes.size == 1 then return Some(matchingHashes.head)

        val hashesWithVersion = matchingHashes.flatMap { hash =>
            loadFromHash(pparamsDir, hash).map(pp => (hash, pp.protocolVersion.major))
        }

        hashesWithVersion
            .sortBy(-_._2)
            .headOption
            .map(_._1)
