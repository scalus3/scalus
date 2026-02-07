package scalus.testing.conformance

import io.bullet.borer.*
import io.bullet.borer.Dom.Element
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.cardano.ledger.*

import java.nio.file.{Files, Path}

case class LedgerState(certs: LedgerState.CertState, utxos: UTxOState) derives Codec
object LedgerState {

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
          scalus.cardano.address.Network.Testnet
        )

    extension (ledgerState: LedgerState)
        def ruleState = rules.State(
          utxos = ledgerState.utxos.utxo,
          deposited = ledgerState.utxos.deposited,
          fees = ledgerState.utxos.fees,
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

        given Encoder[CertState] with
            def write(w: Writer, value: CertState): Writer =
                w.writeArrayHeader(3)
                w.write(value.vstate)
                w.write(value.pstate)
                w.write(value.dstate)
    }

    extension (certState: CertState)
        def toCertState: scalus.cardano.ledger.CertState =
            scalus.cardano.ledger.CertState(
              vstate = certState.vstate.toVotingState,
              pstate = PoolsState(
                stakePools = certState.pstate.stakePools,
                futureStakePoolParams = certState.pstate.futureStakePoolParams,
                retiring = certState.pstate.retiring,
                deposits = certState.pstate.deposits
              ),
              dstate = certState.dstate.toDelegationState
            )

    /** Conway PState from cardano-ledger test vectors.
      *
      * PState is encoded as: [psStakePoolParams, psFutureStakePoolParams, psRetiring, psDeposits]
      * (4 elements)
      *   - psStakePoolParams: Map PoolKeyHash PoolParams
      *   - psFutureStakePoolParams: Map PoolKeyHash PoolParams
      *   - psRetiring: Map PoolKeyHash EpochNo
      *   - psDeposits: Map PoolKeyHash Coin
      *
      * PoolParams is encoded as a 9-element array (without certificate tag prefix): [operator,
      * vrfKeyHash, pledge, cost, margin, rewardAccount, poolOwners, relays, poolMetadata]
      */
    case class PState(
        stakePools: Map[PoolKeyHash, Certificate.PoolRegistration],
        futureStakePoolParams: Map[PoolKeyHash, Certificate.PoolRegistration],
        retiring: Map[PoolKeyHash, EpochNo],
        deposits: Map[PoolKeyHash, Coin]
    )

    object PState {
        val empty: PState = PState(Map.empty, Map.empty, Map.empty, Map.empty)

        /** Decode PoolParams from CBOR array (internal ledger state encoding).
          *
          * Two formats exist:
          *   - Old format (9 elements, matching transaction CDDL): [operator, vrfKeyHash, pledge,
          *     cost, margin, rewardAccount, poolOwners, relays, poolMetadata]
          *   - New format (8+ elements, operator is the map key): [vrfKeyHash, pledge, cost,
          *     margin, rewardAccount, poolOwners, relays, poolMetadata, ...]
          *
          * We distinguish them by checking whether the first byte array is 28 bytes (operator/ppId)
          * or 32 bytes (vrfKeyHash).
          */
        private def readPoolParams(r: Reader, operator: PoolKeyHash): Certificate.PoolRegistration =
            val size = r.readArrayHeader()
            // Peek at the first bytes element to determine the format.
            // ByteString-Header carries the length; 28 = AddrKeyHash, 32 = VrfKeyHash.
            val firstBytes = r.readBytes[Array[Byte]]()
            val (actualOperator, vrfKeyHash) =
                if firstBytes.length == 28 then
                    // Old format: first element is operator (ppId), second is vrfKeyHash
                    val vrfKH = r.read[VrfKeyHash]()
                    val opHash = PoolKeyHash.fromArray(firstBytes)
                    (AddrKeyHash.fromByteString(opHash), vrfKH)
                else
                    // New format: first element is vrfKeyHash, operator from map key
                    (AddrKeyHash.fromByteString(operator), VrfKeyHash.fromArray(firstBytes))
            val isOldFormat = firstBytes.length == 28
            val pledge = r.read[Coin]()
            val cost = r.read[Coin]()
            val margin = r.read[UnitInterval]()
            val rewardAccount = readRewardAccount(r, isOldFormat)
            val poolOwners = readTaggedSet[AddrKeyHash](r)
            val relays = r.read[IndexedSeq[Relay]]()
            // StrictMaybe encoding: null for SNothing, [] for SNothing, [x] for SJust x
            val poolMetadata = readStrictMaybePoolMetadata(r)
            // Skip any extra trailing fields (old format: 9 consumed, new format: 8 consumed)
            val consumed = if firstBytes.length == 28 then 9 else 8
            for _ <- consumed until size.toInt do r.read[Element]()
            Certificate.PoolRegistration(
              operator = actualOperator,
              vrfKeyHash = vrfKeyHash,
              pledge = pledge,
              cost = cost,
              margin = margin,
              rewardAccount = rewardAccount,
              poolOwners = poolOwners,
              relays = relays,
              poolMetadata = poolMetadata
            )

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

        /** Read RewardAccount from ledger state encoding.
          *
          * Two formats:
          *   - Old format (isOldFormat=true): raw bytes (same as transaction CDDL)
          *   - New internal format (isOldFormat=false): Credential [tag, hash] array, reconstructed
          *     with Testnet network
          */
        private def readRewardAccount(r: Reader, isOldFormat: Boolean): RewardAccount =
            if isOldFormat then
                // Old format: raw bytes encoding matching transaction CDDL
                r.read[RewardAccount]()
            else
                // New internal format: Credential [tag, hash]
                val cred = r.read[Credential]()
                val payload = cred match
                    case Credential.KeyHash(hash) =>
                        scalus.cardano.address.StakePayload.Stake(
                          StakeKeyHash.fromByteString(hash)
                        )
                    case Credential.ScriptHash(hash) =>
                        scalus.cardano.address.StakePayload.Script(hash)
                RewardAccount(
                  scalus.cardano.address.StakeAddress(
                    scalus.cardano.address.Network.Testnet,
                    payload
                  )
                )

        private def readTaggedSet[A](r: Reader)(using decoder: Decoder[A]): Set[A] =
            if r.dataItem() == DataItem.Tag then
                val tag = r.readTag()
                if tag.code != 258 then r.validationFailure(s"Expected tag 258 for Set, got $tag")
            r.read[Set[A]]()

        given Decoder[PState] with
            def read(r: Reader): PState =
                r.readArrayHeader(4)
                // psStakePoolParams: Map PoolKeyHash PoolParams
                val stakePoolsSize = r.readMapHeader()
                val stakePools = (0 until stakePoolsSize.toInt).map { _ =>
                    val poolId = r.read[PoolKeyHash]()
                    val params = readPoolParams(r, poolId)
                    poolId -> params
                }.toMap
                // psFutureStakePoolParams: Map PoolKeyHash PoolParams
                val futureSize = r.readMapHeader()
                val futureStakePoolParams = (0 until futureSize.toInt).map { _ =>
                    val poolId = r.read[PoolKeyHash]()
                    val params = readPoolParams(r, poolId)
                    poolId -> params
                }.toMap
                // psRetiring: Map PoolKeyHash EpochNo
                val retiringSize = r.readMapHeader()
                val retiring = (0 until retiringSize.toInt).map { _ =>
                    val poolId = r.read[PoolKeyHash]()
                    val epoch = r.readLong()
                    poolId -> epoch
                }.toMap
                // psDeposits: Map PoolKeyHash Coin
                val depositsSize = r.readMapHeader()
                val deposits = (0 until depositsSize.toInt).map { _ =>
                    val poolId = r.read[PoolKeyHash]()
                    val coin = r.read[Coin]()
                    poolId -> coin
                }.toMap
                PState(stakePools, futureStakePoolParams, retiring, deposits)

        given Encoder[PState] with
            def write(w: Writer, value: PState): Writer =
                w.writeArrayHeader(4)
                // psStakePoolParams
                w.writeMapHeader(value.stakePools.size)
                for (poolId, params) <- value.stakePools do
                    w.write(poolId)
                    writePoolParams(w, params)
                // psFutureStakePoolParams
                w.writeMapHeader(value.futureStakePoolParams.size)
                for (poolId, params) <- value.futureStakePoolParams do
                    w.write(poolId)
                    writePoolParams(w, params)
                // psRetiring
                w.writeMapHeader(value.retiring.size)
                for (poolId, epoch) <- value.retiring do
                    w.write(poolId)
                    w.writeLong(epoch)
                // psDeposits
                w.writeMapHeader(value.deposits.size)
                for (poolId, coin) <- value.deposits do
                    w.write(poolId)
                    w.write(coin)
                w

        private def writePoolParams(w: Writer, p: Certificate.PoolRegistration): Unit =
            w.writeArrayHeader(9)
            w.write(p.operator)
            w.write(p.vrfKeyHash)
            w.write(p.pledge)
            w.write(p.cost)
            w.write(p.margin)
            w.write(p.rewardAccount)
            w.write(p.poolOwners)
            w.write(p.relays)
            w.write(p.poolMetadata)
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
        numDormantEpochs: Long
    )

    object VState {
        val empty: VState = VState(Map.empty, 0)

        given Decoder[VState] with
            def read(r: Reader): VState =
                r.readArrayHeader(3)
                val dreps = r.read[Map[Credential, ConwayDRepState]]()
                r.read[Element]() // Skip committeeState (a map)
                val numDormantEpochs = r.readLong()
                VState(dreps, numDormantEpochs)

        given Encoder[VState] with
            def write(w: Writer, value: VState): Writer =
                w.writeArrayHeader(3)
                w.write(value.dreps)
                w.writeMapHeader(0) // Empty committeeState
                w.writeLong(value.numDormantEpochs)
    }

    extension (vstate: VState)
        def toVotingState: VotingState =
            VotingState(
              dreps = vstate.dreps.map { case (cred, drepState) =>
                  cred -> DRepState(
                    expiry = drepState.expiry,
                    anchor = drepState.anchor,
                    deposit = drepState.deposit,
                    delegates = Set.empty // Skip delegates for now
                  )
              }
            )

    /** Conway DRepState from cardano-ledger.
      *
      * DRepState is encoded as: [expiry, anchor, deposit, delegates] (4 elements)
      */
    case class ConwayDRepState(
        expiry: Long,
        anchor: Option[Anchor],
        deposit: Coin,
        delegates: Array[Element] // Skip parsing delegates for now
    )

    object ConwayDRepState {
        given Decoder[ConwayDRepState] with
            def read(r: Reader): ConwayDRepState =
                r.readArrayHeader(4)
                val expiry = r.readLong()
                val anchor = r.read[Option[Anchor]]()
                val deposit = r.read[Coin]()
                // delegates is encoded as Tag(258, array) - a CBOR set
                if r.dataItem() == DataItem.Tag then
                    val tag = r.readTag()
                    if tag.code != 258 then
                        r.validationFailure(s"Expected tag 258 for Set, got $tag")
                val delegates = r.read[Array[Element]]()
                ConwayDRepState(expiry, anchor, deposit, delegates)

        given Encoder[ConwayDRepState] with
            def write(w: Writer, value: ConwayDRepState): Writer =
                w.writeArrayHeader(4)
                w.writeLong(value.expiry)
                w.write(value.anchor)
                w.write(value.deposit)
                w.write(value.delegates)
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

        given Encoder[DState] with
            def write(w: Writer, value: DState): Writer =
                w.writeArrayHeader(4)
                // Write accounts in UMap format
                w.writeArrayHeader(2)
                w.writeMapHeader(value.accounts.size)
                value.accounts.foreach { case (cred, acc) =>
                    w.write(cred)
                    // Write UMElem
                    w.writeArrayHeader(4)
                    w.writeArrayHeader(2)
                    w.write(acc.balance)
                    w.write(acc.deposit)
                    acc.stakePoolDelegation match
                        case None    => w.writeNull()
                        case Some(v) => w.write(v)
                    acc.dRepDelegation match
                        case None    => w.writeNull()
                        case Some(v) => w.write(v)
                    w.writeArrayHeader(0) // empty ptrs set
                }
                w.writeMapHeader(0) // empty umPtrs
                w.write(value.futureGenDelegs)
                w.write(value.genDelegs)
                w.write(value.iRewards)
    }

    extension (dstate: DState)
        def toDelegationState: DelegationState =
            val deposits = dstate.accounts.collect {
                case (cred, accountState) if accountState.deposit.value > 0 =>
                    cred -> accountState.deposit
            }

            val rewards = dstate.accounts.map { case (cred, accountState) =>
                cred -> accountState.balance
            }
            val stakePools = dstate.accounts.collect {
                case (cred, accountState) if accountState.stakePoolDelegation.isDefined =>
                    cred -> accountState.stakePoolDelegation.get
            }
            val dreps = dstate.accounts.collect {
                case (cred, accountState) if accountState.dRepDelegation.isDefined =>
                    cred -> accountState.dRepDelegation.get
            }
            DelegationState(rewards, deposits, stakePools, dreps)

    /** Conway AccountState from cardano-ledger.
      *
      * ConwayAccountState is encoded as: [balance, deposit, stakePoolDelegation, dRepDelegation] (4
      * elements)
      *   - balance: CompactForm Coin
      *   - deposit: CompactForm Coin
      *   - stakePoolDelegation: StrictMaybe (KeyHash 'StakePool) (null or value)
      *   - dRepDelegation: StrictMaybe DRep (null or value)
      */
    case class ConwayAccountState(
        balance: Coin,
        deposit: Coin,
        stakePoolDelegation: Option[PoolKeyHash],
        dRepDelegation: Option[DRep]
    )

    object ConwayAccountState {
        given Decoder[ConwayAccountState] with
            def read(r: Reader): ConwayAccountState =
                r.readArrayHeader(4)
                val balance = r.read[Coin]()
                val deposit = r.read[Coin]()
                // StrictMaybe is encoded as null (for SNothing) or the value (for SJust)
                val stakePoolDelegation =
                    if r.tryReadNull() then None else Some(r.read[PoolKeyHash]())
                val dRepDelegation = if r.tryReadNull() then None else Some(r.read[DRep]())
                ConwayAccountState(balance, deposit, stakePoolDelegation, dRepDelegation)

        given Encoder[ConwayAccountState] with
            def write(w: Writer, value: ConwayAccountState): Writer =
                w.writeArrayHeader(4)
                w.write(value.balance)
                w.write(value.deposit)
                value.stakePoolDelegation match
                    case None    => w.writeNull()
                    case Some(v) => w.write(v)
                value.dRepDelegation match
                    case None    => w.writeNull()
                    case Some(v) => w.write(v)
    }

    given Decoder[TransactionInput] with
        def read(r: Reader): TransactionInput =
            if r.hasByteArray then MempackParser.parseTransactionInput(r.readByteArray())
            else r.read[TransactionInput]()

    given Decoder[TransactionOutput] with
        def read(r: Reader): TransactionOutput =
            if r.hasByteArray then MempackParser.parseOutput(r.readByteArray())
            else r.read[TransactionOutput]()

    given Codec[UTxOState] = Codec.derived

}

/** Conway protocol parameters from cardano-ledger test vectors.
  *
  * Conway PParams is encoded as an array of 31 elements in this order (from eraPParams in
  * Conway/PParams.hs):
  *   - 0: minFeeA (Coin)
  *   - 1: minFeeB (Coin)
  *   - 2: maxBBSize (Word32)
  *   - 3: maxTxSize (Word32)
  *   - 4: maxBHSize (Word16)
  *   - 5: keyDeposit (Coin)
  *   - 6: poolDeposit (CompactForm Coin)
  *   - 7: eMax (EpochInterval)
  *   - 8: nOpt (Word16)
  *   - 9: a0 (NonNegativeInterval)
  *   - 10: rho (UnitInterval)
  *   - 11: tau (UnitInterval)
  *   - 12: protocolVersion (ProtVer)
  *   - 13: minPoolCost (Coin)
  *   - 14: coinsPerUTxOByte (CoinPerByte)
  *   - 15: costModels (CostModels)
  *   - 16: prices (Prices - [memPrice, stepPrice])
  *   - 17: maxTxExUnits (ExUnits)
  *   - 18: maxBlockExUnits (ExUnits)
  *   - 19: maxValSize (Word32)
  *   - 20: collateralPercentage (Word16)
  *   - 21: maxCollateralInputs (Word16)
  *   - 22: poolVotingThresholds
  *   - 23: dRepVotingThresholds
  *   - 24: committeeMinSize (Word16)
  *   - 25: committeeMaxTermLength (EpochInterval)
  *   - 26: govActionLifetime (EpochInterval)
  *   - 27: govActionDeposit (Coin)
  *   - 28: dRepDeposit (CompactForm Coin)
  *   - 29: dRepActivity (EpochInterval)
  *   - 30: minFeeRefScriptCostPerByte (NonNegativeInterval)
  */
case class ConwayProtocolParams(
    minFeeA: Long,
    minFeeB: Long,
    maxBBSize: Long,
    maxTxSize: Long,
    maxBHSize: Long,
    keyDeposit: Long, // stakeAddressDeposit
    poolDeposit: Long,
    eMax: Long,
    nOpt: Long,
    a0Numerator: Long, // pool pledge influence numerator
    a0Denominator: Long,
    rhoNumerator: Long, // monetary expansion numerator
    rhoDenominator: Long,
    tauNumerator: Long, // treasury cut numerator
    tauDenominator: Long,
    protocolVersionMajor: Long,
    protocolVersionMinor: Long,
    minPoolCost: Long,
    coinsPerUTxOByte: Long,
    costModelsRaw: Element, // CostModels as DOM element
    pricesMemNumerator: Long,
    pricesMemDenominator: Long,
    pricesStepNumerator: Long,
    pricesStepDenominator: Long,
    maxTxExMem: Long,
    maxTxExSteps: Long,
    maxBlockExMem: Long,
    maxBlockExSteps: Long,
    maxValSize: Long,
    collateralPercentage: Long,
    maxCollateralInputs: Long,
    poolVotingThresholdsRaw: Element,
    dRepVotingThresholdsRaw: Element,
    committeeMinSize: Long,
    committeeMaxTermLength: Long,
    govActionLifetime: Long,
    govActionDeposit: Long,
    dRepDeposit: Long,
    dRepActivity: Long,
    minFeeRefScriptCostPerByteNum: Long,
    minFeeRefScriptCostPerByteDen: Long
):
    /** Parse cost models from raw DOM element. Returns None if parsing fails, allowing fallback to
      * default cost models.
      */
    private def parseCostModels: Option[CostModels] =
        try
            costModelsRaw match
                case mapElem: Dom.MapElem =>
                    val models = mapElem.toMap.flatMap { case (key, value) =>
                        val langIdOpt = key match
                            case Dom.IntElem(v)  => Some(v)
                            case Dom.LongElem(v) => Some(v.toInt)
                            case _               => None
                        val costsOpt = value match
                            case arr: Dom.ArrayElem =>
                                Some(
                                  arr.elems.map {
                                      case Dom.IntElem(v)  => v.toLong
                                      case Dom.LongElem(v) => v
                                      case _               => 0L
                                  }.toIndexedSeq
                                )
                            case _ => None
                        for langId <- langIdOpt; costs <- costsOpt yield langId -> costs
                    }
                    Some(CostModels(models))
                case _ => None
        catch case _: Exception => None

    /** Convert to Scalus ProtocolParams */
    def toProtocolParams: ProtocolParams =
        // Use default params as fallback for fields not in CBOR
        val baseParams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )
        // Parse cost models from the test vectors (important for script data hash validation)
        // Fall back to blockfrost params if parsing fails
        val costModels = parseCostModels.getOrElse(baseParams.costModels)
        baseParams.copy(
          txFeePerByte = minFeeA,
          txFeeFixed = minFeeB,
          maxBlockBodySize = maxBBSize,
          maxTxSize = maxTxSize,
          maxBlockHeaderSize = maxBHSize,
          stakeAddressDeposit = keyDeposit,
          stakePoolDeposit = poolDeposit,
          poolRetireMaxEpoch = eMax,
          stakePoolTargetNum = nOpt,
          poolPledgeInfluence =
              if a0Denominator == 0 then 0.0 else a0Numerator.toDouble / a0Denominator,
          monetaryExpansion =
              if rhoDenominator == 0 then 0.0 else rhoNumerator.toDouble / rhoDenominator,
          treasuryCut = if tauDenominator == 0 then 0.0 else tauNumerator.toDouble / tauDenominator,
          protocolVersion = ProtocolVersion(protocolVersionMajor.toInt, protocolVersionMinor.toInt),
          minPoolCost = minPoolCost,
          utxoCostPerByte = coinsPerUTxOByte,
          costModels = costModels,
          executionUnitPrices = ExUnitPrices(
            priceMemory = NonNegativeInterval(
              if pricesMemDenominator == 0 then 0.0
              else pricesMemNumerator.toDouble / pricesMemDenominator
            ),
            priceSteps = NonNegativeInterval(
              if pricesStepDenominator == 0 then 0.0
              else pricesStepNumerator.toDouble / pricesStepDenominator
            )
          ),
          maxTxExecutionUnits = ExUnits(maxTxExMem, maxTxExSteps),
          maxBlockExecutionUnits = ExUnits(maxBlockExMem, maxBlockExSteps),
          maxValueSize = maxValSize,
          collateralPercentage = collateralPercentage,
          maxCollateralInputs = maxCollateralInputs,
          committeeMinSize = committeeMinSize,
          committeeMaxTermLength = committeeMaxTermLength,
          govActionLifetime = govActionLifetime,
          govActionDeposit = govActionDeposit,
          dRepDeposit = dRepDeposit,
          dRepActivity = dRepActivity,
          minFeeRefScriptCostPerByte =
              if minFeeRefScriptCostPerByteDen == 0 then 0L
              else minFeeRefScriptCostPerByteNum / minFeeRefScriptCostPerByteDen
        )

object ConwayProtocolParams:
    def fromCbor(cbor: Array[Byte]): ConwayProtocolParams =
        Cbor.decode(cbor).to[ConwayProtocolParams].value

    /** Load protocol params from pparams-by-hash directory using hash found in ledger state */
    def loadFromHash(pparamsDir: Path, hash: String): Option[ConwayProtocolParams] =
        val pparamsFile = pparamsDir.resolve(hash)
        if Files.exists(pparamsFile) then
            val cbor = Files.readAllBytes(pparamsFile)
            Some(fromCbor(cbor))
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

        // Find all pparams hashes that appear in the ledger state
        val matchingHashes = Files
            .list(pparamsDir)
            .iterator()
            .asScala
            .map(_.getFileName.toString)
            .filter(hash => oldLedgerStateHex.contains(hash))
            .toList

        if matchingHashes.isEmpty then return None
        if matchingHashes.size == 1 then return Some(matchingHashes.head)

        // If multiple matches, load each and pick the one with highest protocol version
        val hashesWithVersion = matchingHashes.flatMap { hash =>
            loadFromHash(pparamsDir, hash).map(pp => (hash, pp.protocolVersionMajor))
        }

        hashesWithVersion
            .sortBy(-_._2) // Sort by protocol version descending
            .headOption
            .map(_._1)

    given Decoder[ConwayProtocolParams] with
        def read(r: Reader): ConwayProtocolParams =
            val size = r.readArrayHeader()
            // Can be 31 (without minFeeRefScriptCostPerByte) or 31+ with it
            require(size >= 31, s"Expected at least 31 elements, got $size")

            // Read each field in order
            val minFeeA = r.readLong()
            val minFeeB = r.readLong()
            val maxBBSize = r.readLong()
            val maxTxSize = r.readLong()
            val maxBHSize = r.readLong()
            val keyDeposit = r.readLong()
            val poolDeposit = r.readLong()
            val eMax = r.readLong()
            val nOpt = r.readLong()

            // a0 = NonNegativeInterval = Tagged(30, [num, denom])
            val a0Tag = r.read[Dom.Element]()
            val (a0Num, a0Denom) = parseTaggedRational(a0Tag)

            // rho = UnitInterval = Tagged(30, [num, denom])
            val rhoTag = r.read[Dom.Element]()
            val (rhoNum, rhoDenom) = parseTaggedRational(rhoTag)

            // tau = UnitInterval = Tagged(30, [num, denom])
            val tauTag = r.read[Dom.Element]()
            val (tauNum, tauDenom) = parseTaggedRational(tauTag)

            // protocolVersion = [major, minor]
            r.readArrayHeader(2)
            val pvMajor = r.readLong()
            val pvMinor = r.readLong()

            val minPoolCost = r.readLong()
            val coinsPerUTxOByte = r.readLong()

            // costModels - keep as DOM element
            val costModels = r.read[Dom.Element]()

            // prices = [memPrice, stepPrice] where each is Tagged(30, [num, denom])
            r.readArrayHeader(2)
            val memPriceTag = r.read[Dom.Element]()
            val (memPriceNum, memPriceDenom) = parseTaggedRational(memPriceTag)
            val stepPriceTag = r.read[Dom.Element]()
            val (stepPriceNum, stepPriceDenom) = parseTaggedRational(stepPriceTag)

            // maxTxExUnits = [mem, steps]
            r.readArrayHeader(2)
            val maxTxExMem = r.readLong()
            val maxTxExSteps = r.readLong()

            // maxBlockExUnits = [mem, steps]
            r.readArrayHeader(2)
            val maxBlockExMem = r.readLong()
            val maxBlockExSteps = r.readLong()

            val maxValSize = r.readLong()
            val collateralPercentage = r.readLong()
            val maxCollateralInputs = r.readLong()

            // poolVotingThresholds - keep as DOM
            val poolVotingThresholds = r.read[Dom.Element]()

            // dRepVotingThresholds - keep as DOM
            val dRepVotingThresholds = r.read[Dom.Element]()

            val committeeMinSize = r.readLong()
            val committeeMaxTermLength = r.readLong()
            val govActionLifetime = r.readLong()
            val govActionDeposit = r.readLong()
            val dRepDeposit = r.readLong()
            val dRepActivity = r.readLong()

            // minFeeRefScriptCostPerByte (if present)
            val (refScriptNum, refScriptDenom) =
                if size > 30 then
                    val tag = r.read[Dom.Element]()
                    parseTaggedRational(tag)
                else (0L, 1L)

            ConwayProtocolParams(
              minFeeA = minFeeA,
              minFeeB = minFeeB,
              maxBBSize = maxBBSize,
              maxTxSize = maxTxSize,
              maxBHSize = maxBHSize,
              keyDeposit = keyDeposit,
              poolDeposit = poolDeposit,
              eMax = eMax,
              nOpt = nOpt,
              a0Numerator = a0Num,
              a0Denominator = a0Denom,
              rhoNumerator = rhoNum,
              rhoDenominator = rhoDenom,
              tauNumerator = tauNum,
              tauDenominator = tauDenom,
              protocolVersionMajor = pvMajor,
              protocolVersionMinor = pvMinor,
              minPoolCost = minPoolCost,
              coinsPerUTxOByte = coinsPerUTxOByte,
              costModelsRaw = costModels,
              pricesMemNumerator = memPriceNum,
              pricesMemDenominator = memPriceDenom,
              pricesStepNumerator = stepPriceNum,
              pricesStepDenominator = stepPriceDenom,
              maxTxExMem = maxTxExMem,
              maxTxExSteps = maxTxExSteps,
              maxBlockExMem = maxBlockExMem,
              maxBlockExSteps = maxBlockExSteps,
              maxValSize = maxValSize,
              collateralPercentage = collateralPercentage,
              maxCollateralInputs = maxCollateralInputs,
              poolVotingThresholdsRaw = poolVotingThresholds,
              dRepVotingThresholdsRaw = dRepVotingThresholds,
              committeeMinSize = committeeMinSize,
              committeeMaxTermLength = committeeMaxTermLength,
              govActionLifetime = govActionLifetime,
              govActionDeposit = govActionDeposit,
              dRepDeposit = dRepDeposit,
              dRepActivity = dRepActivity,
              minFeeRefScriptCostPerByteNum = refScriptNum,
              minFeeRefScriptCostPerByteDen = refScriptDenom
            )

    private def parseTaggedRational(elem: Dom.Element): (Long, Long) =
        elem match
            case tagged: Dom.TaggedElem if tagged.tag.code == 30 =>
                tagged.value match
                    case arr: Dom.ArrayElem if arr.elems.size == 2 =>
                        val num = extractLong(arr.elems(0))
                        val denom = extractLong(arr.elems(1))
                        (num, denom)
                    case other =>
                        throw new RuntimeException(
                          s"Expected array of 2 elements in Tagged(30), got $other"
                        )
            case other =>
                throw new RuntimeException(s"Expected Tagged(30, [num, denom]), got $other")

    private def extractLong(elem: Dom.Element): Long =
        elem match
            case Dom.IntElem(v)  => v.toLong
            case Dom.LongElem(v) => v
            case other => throw new RuntimeException(s"Expected int in rational, got $other")
