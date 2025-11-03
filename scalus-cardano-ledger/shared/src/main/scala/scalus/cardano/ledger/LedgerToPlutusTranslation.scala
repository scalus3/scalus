package scalus.cardano.ledger

import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.ledger.api
import scalus.ledger.api.*
import scalus.ledger.api.v1.{DCert, ScriptPurpose, StakingCredential}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.GovernanceActionId
import scalus.prelude.{asScalus, List, SortedMap}
import scalus.uplc.eval.*
import scalus.{builtin, ledger, prelude}

import scala.collection.{immutable, mutable}
import scala.math.BigInt

/** Advanced interoperability layer for scalus.cardano.ledger domain model.
  *
  * This object provides conversion functions and utilities for working with native Scalus types
  * instead of external library dependencies. It handles:
  *
  *   - Data conversion between Plutus Data and Scalus Data
  *   - Transaction information construction for all Plutus versions
  *   - Script context building and script purpose resolution
  *   - Cost model translation and machine parameter setup
  *   - Address, credential, and certificate type conversions
  *   - Multi-asset value handling and ordering
  *   - Governance-related type conversions (Conway era)
  *
  * All functions maintain compatibility with Cardano's CDDL specifications and preserve the
  * deterministic ordering required for script validation.
  */
object LedgerToPlutusTranslation {

    /** Creates MachineParams from CostModels and PlutusLedgerLanguage.
      *
      * This function configures the Plutus virtual machine with the appropriate cost models and
      * semantic variants based on the protocol version and Plutus language version. This is crucial
      * for accurate script execution cost calculation and validation.
      *
      * @param costModels
      *   Cost models for different Plutus versions
      * @param language
      *   Plutus language version (V1, V2, or V3)
      * @param protocolVersion
      *   Major protocol version for semantic variant selection
      * @return
      *   Configured MachineParams for script execution
      */
    @deprecated("Use MachineParams.fromCostModels instead", "scalus 0.12.1")
    def translateMachineParamsFromCostModels(
        costModels: CostModels,
        language: Language,
        protocolVersion: MajorProtocolVersion
    ): MachineParams = {
        MachineParams.fromCostModels(costModels, language, protocolVersion)
    }

    /** Convert scalus.cardano.ledger.Credential to scalus.ledger.api.v1.Credential.
      *
      * This function bridges the gap between the domain model representation and the Plutus script
      * context representation of credentials.
      */
    def getCredential(cred: Credential): v1.Credential = {
        cred match
            case Credential.KeyHash(hash) =>
                v1.Credential.PubKeyCredential(v1.PubKeyHash(hash))
            case Credential.ScriptHash(hash) =>
                v1.Credential.ScriptCredential(hash)
    }

    /** Convert scalus.cardano.ledger.Credential to v1.StakingCredential.
      *
      * Staking credentials are used for delegation and reward operations. This conversion wraps the
      * credential in the appropriate staking context.
      */
    def getStakingCredential(cred: Credential): v1.StakingCredential = {
        v1.StakingCredential.StakingHash(getCredential(cred))
    }

    /** Convert scalus.cardano.address.Address to scalus.ledger.api.v1.Address.
      *
      * This function converts between the comprehensive address representation used in the domain
      * model and the simplified address representation used in Plutus script contexts.
      */
    def getAddress(address: Address): v1.Address = {
        address match
            case shelleyAddr: ShelleyAddress =>
                val paymentCred = shelleyAddr.payment match
                    case ShelleyPaymentPart.Key(hash) =>
                        v1.Credential.PubKeyCredential(v1.PubKeyHash(hash))
                    case ShelleyPaymentPart.Script(hash) =>
                        v1.Credential.ScriptCredential(hash)

                val stakingCred = shelleyAddr.delegation match
                    case ShelleyDelegationPart.Key(hash) =>
                        prelude.Option.Some(
                          v1.StakingCredential.StakingHash(
                            v1.Credential.PubKeyCredential(v1.PubKeyHash(hash))
                          )
                        )
                    case ShelleyDelegationPart.Script(hash) =>
                        prelude.Option.Some(
                          v1.StakingCredential.StakingHash(
                            v1.Credential.ScriptCredential(hash)
                          )
                        )
                    case ShelleyDelegationPart.Pointer(pointer) =>
                        // For pointer addresses, we don't include staking credential in script context
                        prelude.Option.None
                    case ShelleyDelegationPart.Null =>
                        prelude.Option.None

                v1.Address(paymentCred, stakingCred)

            case _: ByronAddress =>
                throw new IllegalArgumentException(
                  "Byron addresses not supported in script contexts"
                )

            case _: StakeAddress =>
                throw new IllegalArgumentException(
                  "Stake addresses not supported as payment addresses"
                )
    }

    /** Create TxInInfo for Plutus V1 from transaction input and UTxO set.
      *
      * TxInInfo combines the transaction input reference with the resolved output being spent,
      * providing complete information for script validation.
      */
    def getTxInInfoV1(
        input: TransactionInput,
        utxos: Map[TransactionInput, TransactionOutput]
    ): v1.TxInInfo = {
        val output =
            utxos.getOrElse(input, throw new IllegalStateException(s"Input UTxO not found: $input"))

        val txOutRef = v1.TxOutRef(v1.TxId(input.transactionId), input.index)

        val txOut = output match
            case TransactionOutput.Shelley(address, value, datumHash) =>
                val addr = getAddress(address)
                val val1 = getValue(value)
                val optDatumHash = datumHash
                    .map(hash => prelude.Option.Some(hash))
                    .getOrElse(prelude.Option.None)
                v1.TxOut(addr, val1, optDatumHash)

            case TransactionOutput.Babbage(address, value, datumOption, _) =>
                val addr = getAddress(address)
                val val1 = getValue(value)
                val optDatumHash = datumOption match
                    case Some(DatumOption.Hash(hash)) => prelude.Option.Some(hash)
                    case _                            => prelude.Option.None
                v1.TxOut(addr, val1, optDatumHash)

        v1.TxInInfo(txOutRef, txOut)
    }

    /** Create TxInInfo for Plutus V2 from transaction input and UTxO set.
      *
      * V2 TxInInfo includes additional information like inline datums and reference scripts that
      * were introduced in the Babbage era.
      */
    def getTxInInfoV2(
        input: TransactionInput,
        utxos: Map[TransactionInput, TransactionOutput]
    ): v2.TxInInfo = {
        val output =
            utxos.getOrElse(input, throw new IllegalStateException(s"Input UTxO not found: $input"))

        val txOutRef = v1.TxOutRef(
          v1.TxId(input.transactionId),
          input.index
        )

        val txOut = output match
            case TransactionOutput.Shelley(address, value, datumHash) =>
                val addr = getAddress(address)
                val val2 = getValue(value)
                val outputDatum = datumHash match
                    case Some(hash) => OutputDatum.OutputDatumHash(hash)
                    case None       => OutputDatum.NoOutputDatum
                v2.TxOut(addr, val2, outputDatum, prelude.Option.None)

            case TransactionOutput.Babbage(address, value, datumOption, scriptRef) =>
                val addr = getAddress(address)
                val val2 = getValue(value)
                val outputDatum = datumOption match
                    case Some(DatumOption.Hash(hash))   => OutputDatum.OutputDatumHash(hash)
                    case Some(DatumOption.Inline(data)) => OutputDatum.OutputDatum(data)
                    case None                           => OutputDatum.NoOutputDatum
                val refScript = scriptRef
                    .map(script => prelude.Option.Some(script.script.scriptHash))
                    .getOrElse(prelude.Option.None)
                v2.TxOut(addr, val2, outputDatum, refScript)

        v2.TxInInfo(txOutRef, txOut)
    }

    /** Convert scalus.cardano.ledger.Value to scalus.ledger.api.v1.Value.
      *
      * This function converts the domain model value representation to the Plutus script context
      * value format, which uses nested association maps for multi-asset representation.
      */
    def getValue(value: Value): v1.Value = {
        // Add ADA entry if not empty
        val adaValue = v1.Value.lovelace(BigInt(value.coin.value))

        val assetsValue = v1.Value.fromList(
          prelude.List.from(
            value.assets.assets.view.map { case (policyId, assets) =>
                val assetList = prelude.List.from(assets.view.map { (assetName, amount) =>
                    assetName.bytes -> BigInt(amount)
                })
                policyId -> assetList
            }
          )
        )

        adaValue + assetsValue
    }

    /** Convert multi-asset values for minting context.
      *
      * Minting contexts require special handling to ensure ADA is always included in the value map,
      * even when no ADA is being minted.
      */
    def getMintValueV1V2(mint: Option[Mint]): v1.Value = {
        // Always include ADA entry with zero value for minting
        val assets = mint.getOrElse(MultiAsset.empty)
        val adaEntry = Seq(
          ByteString.empty -> prelude.List.single(ByteString.empty, BigInt(0))
        )
        val allEntries = adaEntry ++ assets.assets.view.map { case (policyId, assets) =>
            val assetMap = prelude.List.from(assets.view.map: (assetName, amount) =>
                assetName.bytes -> BigInt(amount))
            policyId -> assetMap
        }
        v1.Value.unsafeFromList(prelude.List.from(allEntries))
    }

    /** Convert multi-asset values for minting context.
      *
      * In Plutus V3, minting value can not contain zero ADA entry, so we handle it differently.
      */
    def getMintValueV3(mint: Option[Mint]): v1.Value = {
        val assets = mint.getOrElse(MultiAsset.empty)
        val allEntries = assets.assets.view.map { case (policyId, assets) =>
            val assetMap = prelude.List.from(assets.view.map: (assetName, amount) =>
                assetName.bytes -> BigInt(amount))
            policyId -> assetMap
        }
        v1.Value.unsafeFromList(prelude.List.from(allEntries))
    }

    /** Create TxOut for Plutus V1 script contexts.
      */
    def getTxOutV1(output: Sized[TransactionOutput]): v1.TxOut = {
        output.value match
            case TransactionOutput.Shelley(address, value, datumHash) =>
                val addr = getAddress(address)
                val val1 = getValue(value)
                val optDatumHash = datumHash
                    .map(hash => prelude.Option.Some(hash))
                    .getOrElse(prelude.Option.None)
                v1.TxOut(addr, val1, optDatumHash)

            case TransactionOutput.Babbage(address, value, datumOption, _) =>
                val addr = getAddress(address)
                val val1 = getValue(value)
                val optDatumHash = datumOption match
                    case Some(DatumOption.Hash(hash)) => prelude.Option.Some(hash)
                    case _                            => prelude.Option.None
                v1.TxOut(addr, val1, optDatumHash)
    }

    /** Create TxOut for Plutus V2 script contexts.
      */
    def getTxOutV2(output: Sized[TransactionOutput]): v2.TxOut = {
        output.value match
            case TransactionOutput.Shelley(address, value, datumHash) =>
                val addr = getAddress(address)
                val val2 = getValue(value)
                val outputDatum = datumHash match
                    case Some(hash) => OutputDatum.OutputDatumHash(hash)
                    case None       => OutputDatum.NoOutputDatum
                v2.TxOut(addr, val2, outputDatum, prelude.Option.None)

            case TransactionOutput.Babbage(address, value, datumOption, scriptRef) =>
                val addr = getAddress(address)
                val val2 = getValue(value)
                val outputDatum = datumOption match
                    case Some(DatumOption.Hash(hash))   => OutputDatum.OutputDatumHash(hash)
                    case Some(DatumOption.Inline(data)) => OutputDatum.OutputDatum(data)
                    case None                           => OutputDatum.NoOutputDatum
                val refScript = scriptRef
                    .map(script => prelude.Option.Some(script.script.scriptHash))
                    .getOrElse(prelude.Option.None)
                v2.TxOut(addr, val2, outputDatum, refScript)
    }

    /** Create validity interval for transaction from slot configuration.
      *
      * This function converts slot-based validity ranges to POSIX time intervals as required by
      * Plutus script contexts. The interval bounds depend on the protocol version for backward
      * compatibility.
      */
    def getInterval(
        validityStartSlot: Option[Long],
        ttl: Option[Long],
        slotConfig: SlotConfig,
        protocolVersion: MajorProtocolVersion
    ): v1.Interval = {
        (validityStartSlot.getOrElse(0L), ttl.getOrElse(0L)) match
            case (0, 0) => v1.Interval.always
            case (0, validTo) =>
                val closure = if protocolVersion.version > 8 then false else true
                val upper = v1.IntervalBound(
                  v1.IntervalBoundType.Finite(BigInt(slotConfig.slotToTime(validTo))),
                  closure
                )
                v1.Interval(v1.IntervalBound.negInf, upper)
            case (validFrom, 0) =>
                v1.Interval(
                  v1.IntervalBound.finiteInclusive(BigInt(slotConfig.slotToTime(validFrom))),
                  v1.IntervalBound.posInf
                )
            case (validFrom, validTo) =>
                val lower =
                    v1.IntervalBound.finiteInclusive(BigInt(slotConfig.slotToTime(validFrom)))
                val upper = v1.IntervalBound(
                  v1.IntervalBoundType.Finite(BigInt(slotConfig.slotToTime(validTo))),
                  false
                )
                v1.Interval(lower, upper)
    }

    /** Process withdrawals for script context construction.
      *
      * Withdrawals are sorted by staking credential for deterministic ordering as required by
      * Cardano's validation rules.
      */
    def getWithdrawals(
        withdrawals: Option[Withdrawals]
    ): prelude.List[(v1.StakingCredential, BigInt)] = {
        prelude.List.from(getOrderedWithdrawals(withdrawals))
    }

    /** Process withdrawals for script context construction.
      *
      * Withdrawals are sorted by staking credential for deterministic ordering as required by
      * Cardano's validation rules.
      */
    private def getOrderedWithdrawals(
        withdrawals: Option[Withdrawals]
    ): collection.SortedMap[v1.StakingCredential.StakingHash, BigInt] = {
        given Ordering[v1.StakingCredential.StakingHash] = Ordering.by { cred =>
            cred.cred match
                case v1.Credential.PubKeyCredential(pkh)  => pkh.hash
                case v1.Credential.ScriptCredential(hash) => hash
        }

        val wdwls = mutable.TreeMap.empty[v1.StakingCredential.StakingHash, BigInt]
        for w <- withdrawals do
            for (rewardAccount, coin) <- w.withdrawals do
                rewardAccount.address.payload match
                    case StakePayload.Stake(hash) =>
                        val cred = v1.Credential.PubKeyCredential(v1.PubKeyHash(hash))
                        wdwls.put(
                          v1.StakingCredential.StakingHash(cred),
                          BigInt(coin.value)
                        )
                    case StakePayload.Script(hash) =>
                        val cred = v1.Credential.ScriptCredential(hash)
                        wdwls.put(
                          v1.StakingCredential.StakingHash(cred),
                          BigInt(coin.value)
                        )
        wdwls
    }

    /** Convert Certificate to DCert for Plutus V1/V2 script contexts.
      *
      * V1/V2 script contexts use a simplified certificate representation that only includes
      * pre-Conway era certificates. Conway-era governance certificates are not supported and will
      * cause a translation error, which is correct behavior according to the Cardano Ledger
      * specification.
      *
      * This function follows the same logic as `transTxCertV1V2` and `transTxCertCommon` in the
      * Cardano Ledger Haskell implementation.
      */
    def getDCert(cert: Certificate): v1.DCert = {
        cert match
            // Pre-Conway era certificates that are supported in V1/V2
            case Certificate.RegCert(credential, _) =>
                v1.DCert.DelegRegKey(getStakingCredential(credential))
            case Certificate.UnregCert(credential, _) =>
                v1.DCert.DelegDeRegKey(getStakingCredential(credential))
            case Certificate.StakeDelegation(credential, poolKeyHash) =>
                v1.DCert.DelegDelegate(
                  getStakingCredential(credential),
                  v1.PubKeyHash(poolKeyHash)
                )
            case Certificate.PoolRegistration(poolKeyHash, vrfKeyHash, _, _, _, _, _, _, _) =>
                v1.DCert.PoolRegister(
                  v1.PubKeyHash(poolKeyHash),
                  v1.PubKeyHash(vrfKeyHash)
                )
            case Certificate.PoolRetirement(poolKeyHash, epochNo) =>
                v1.DCert.PoolRetire(
                  v1.PubKeyHash(poolKeyHash),
                  BigInt(epochNo)
                )

            // Conway-era governance certificates are not supported in Plutus V1/V2
            // According to Cardano Ledger specification, these should cause translation errors
            case _: Certificate.VoteDelegCert =>
                throw new IllegalArgumentException(
                  s"VoteDelegCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
            case _: Certificate.StakeVoteDelegCert =>
                throw new IllegalArgumentException(
                  s"StakeVoteDelegCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
            case _: Certificate.StakeRegDelegCert =>
                throw new IllegalArgumentException(
                  s"StakeRegDelegCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
            case _: Certificate.VoteRegDelegCert =>
                throw new IllegalArgumentException(
                  s"VoteRegDelegCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
            case _: Certificate.StakeVoteRegDelegCert =>
                throw new IllegalArgumentException(
                  s"StakeVoteRegDelegCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
            case _: Certificate.AuthCommitteeHotCert =>
                throw new IllegalArgumentException(
                  s"AuthCommitteeHotCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
            case _: Certificate.ResignCommitteeColdCert =>
                throw new IllegalArgumentException(
                  s"ResignCommitteeColdCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
            case _: Certificate.RegDRepCert =>
                throw new IllegalArgumentException(
                  s"RegDRepCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
            case _: Certificate.UnregDRepCert =>
                throw new IllegalArgumentException(
                  s"UnregDRepCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
            case _: Certificate.UpdateDRepCert =>
                throw new IllegalArgumentException(
                  s"UpdateDRepCert not supported in Plutus V1/V2 contexts. Use Plutus V3."
                )
    }

    /** Convert Certificate to TxCert for Plutus V3 script contexts.
      *
      * V3 script contexts include full support for Conway era governance features and updated
      * certificate types.
      */
    def getTxCertV3(cert: Certificate): v3.TxCert = {
        cert match
            case Certificate.AuthCommitteeHotCert(coldCredential, hotCredential) =>
                v3.TxCert.AuthHotCommittee(
                  getCredential(coldCredential),
                  getCredential(hotCredential)
                )
            case Certificate.PoolRegistration(poolKeyHash, vrfKeyHash, _, _, _, _, _, _, _) =>
                v3.TxCert.PoolRegister(
                  v1.PubKeyHash(poolKeyHash),
                  v1.PubKeyHash(vrfKeyHash)
                )
            case Certificate.PoolRetirement(poolKeyHash, epochNo) =>
                v3.TxCert.PoolRetire(
                  v1.PubKeyHash(poolKeyHash),
                  BigInt(epochNo)
                )
            case Certificate.RegCert(credential, coin) =>
                v3.TxCert.RegStaking(
                  getCredential(credential),
                  coin.map(c => BigInt(c.value)).asScalus
                )
            case Certificate.RegDRepCert(credential, coin, _) =>
                v3.TxCert.RegDRep(getCredential(credential), BigInt(coin.value))
            case Certificate.ResignCommitteeColdCert(coldCredential, _) =>
                v3.TxCert.ResignColdCommittee(getCredential(coldCredential))
            case Certificate.StakeDelegation(credential, poolKeyHash) =>
                v3.TxCert.DelegStaking(
                  getCredential(credential),
                  v3.Delegatee.Stake(v1.PubKeyHash(poolKeyHash))
                )
            case Certificate.StakeRegDelegCert(credential, poolKeyHash, coin) =>
                v3.TxCert.RegDeleg(
                  getCredential(credential),
                  v3.Delegatee.Stake(v1.PubKeyHash(poolKeyHash)),
                  BigInt(coin.value)
                )
            case Certificate.StakeVoteDelegCert(credential, poolKeyHash, drep) =>
                v3.TxCert.DelegStaking(
                  getCredential(credential),
                  v3.Delegatee.StakeVote(
                    v1.PubKeyHash(poolKeyHash),
                    getDRep(drep)
                  )
                )
            case Certificate.StakeVoteRegDelegCert(credential, poolKeyHash, drep, coin) =>
                v3.TxCert.RegDeleg(
                  getCredential(credential),
                  v3.Delegatee.StakeVote(
                    v1.PubKeyHash(poolKeyHash),
                    getDRep(drep)
                  ),
                  BigInt(coin.value)
                )
            case Certificate.UnregCert(credential, coin) =>
                v3.TxCert.UnRegStaking(
                  getCredential(credential),
                  coin.map(c => BigInt(c.value)).asScalus
                )
            case Certificate.UnregDRepCert(credential, coin) =>
                v3.TxCert.UnRegDRep(getCredential(credential), BigInt(coin.value))
            case Certificate.UpdateDRepCert(credential, _) =>
                v3.TxCert.UpdateDRep(getCredential(credential))
            case Certificate.VoteDelegCert(credential, drep) =>
                v3.TxCert.DelegStaking(
                  getCredential(credential),
                  v3.Delegatee.Vote(getDRep(drep))
                )
            case Certificate.VoteRegDelegCert(credential, drep, coin) =>
                v3.TxCert.RegDeleg(
                  getCredential(credential),
                  v3.Delegatee.Vote(getDRep(drep)),
                  BigInt(coin.value)
                )
    }

    /** Convert DRep to Plutus V3 DRep representation.
      */
    private def getDRep(drep: DRep): v3.DRep = {
        drep match
            case DRep.KeyHash(keyHash) =>
                v3.DRep.DRep(v1.Credential.PubKeyCredential(v1.PubKeyHash(keyHash)))
            case DRep.ScriptHash(scriptHash) =>
                v3.DRep.DRep(v1.Credential.ScriptCredential(scriptHash))
            case DRep.AlwaysAbstain =>
                v3.DRep.AlwaysAbstain
            case DRep.AlwaysNoConfidence =>
                v3.DRep.AlwaysNoConfidence
    }

    /** Build TxInfo for Plutus V1 script contexts.
      *
      * This function constructs the complete transaction information structure required by V1
      * Plutus scripts, including all inputs, outputs, certificates, and other transaction data in
      * the correct format and order.
      */
    def getTxInfoV1(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: MajorProtocolVersion
    ): v1.TxInfo = {
        val body = tx.body.value

        v1.TxInfo(
          inputs = prelude.List.from(body.inputs.toSet.view.map(getTxInInfoV1(_, utxos))),
          outputs = prelude.List.from(body.outputs.view.map(getTxOutV1)),
          fee = v1.Value.lovelace(body.fee.value),
          mint = getMintValueV1V2(body.mint),
          dcert = prelude.List.from(body.certificates.toSeq.view.map(getDCert)),
          withdrawals = getWithdrawals(body.withdrawals),
          validRange = getInterval(body.validityStartSlot, body.ttl, slotConfig, protocolVersion),
          signatories = prelude.List.from(
            body.requiredSigners.toSet.view
                .map(hash => v1.PubKeyHash(hash))
          ),
          data = prelude.List.from(datums.to(immutable.SortedMap)),
          id = v1.TxId(tx.id)
        )
    }

    /** Build TxInfo for Plutus V2 script contexts.
      *
      * V2 TxInfo includes additional fields like reference inputs and an associative map structure
      * for redeemers and data.
      */
    def getTxInfoV2(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: MajorProtocolVersion
    ): v2.TxInfo = {
        val body = tx.body.value
        val redeemers =
            tx.witnessSet.redeemers.map(_.value.toIndexedSeq).getOrElse(IndexedSeq.empty)

        v2.TxInfo(
          inputs = prelude.List.from(body.inputs.toSet.view.map(getTxInInfoV2(_, utxos))),
          referenceInputs =
              prelude.List.from(body.referenceInputs.toSet.view.map(getTxInInfoV2(_, utxos))),
          outputs = prelude.List.from(body.outputs.view.map(getTxOutV2)),
          fee = v1.Value.lovelace(body.fee.value),
          mint = getMintValueV1V2(body.mint),
          dcert = prelude.List.from(body.certificates.toSeq.view.map(getDCert)),
          withdrawals = SortedMap.fromList(getWithdrawals(body.withdrawals)),
          validRange = getInterval(body.validityStartSlot, body.ttl, slotConfig, protocolVersion),
          signatories = prelude.List.from(
            body.requiredSigners.toSet.view
                .map(hash => v1.PubKeyHash(hash))
          ),
          redeemers = SortedMap.unsafeFromList(prelude.List.from(redeemers.sorted.map { redeemer =>
              val purpose = getScriptPurposeV2(tx, redeemer)
              purpose -> redeemer.data
          })),
          data = SortedMap.fromList(prelude.List.from(datums)),
          id = v1.TxId(tx.id)
        )
    }

    /** Build TxInInfo for Plutus V3 script contexts.
      */
    def getTxInInfoV3(
        input: TransactionInput,
        utxos: Map[TransactionInput, TransactionOutput]
    ): v3.TxInInfo = {
        val txInInfoV2 = getTxInInfoV2(input, utxos)
        v3.TxInInfo(
          outRef = v3.TxOutRef(v3.TxId(txInInfoV2.outRef.id.hash), txInInfoV2.outRef.idx),
          resolved = txInInfoV2.resolved
        )
    }

    /** Build TxInfo for Plutus V3 script contexts.
      *
      * V3 TxInfo includes full Conway governance support with votes, proposal procedures, and
      * treasury operations.
      */
    def getTxInfoV3(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: MajorProtocolVersion
    ): v3.TxInfo = {
        val body = tx.body.value
        val redeemers =
            tx.witnessSet.redeemers.map(_.value.toIndexedSeq).getOrElse(IndexedSeq.empty)

        // Process withdrawals for V3 format
        val withdrawals = getWithdrawals(body.withdrawals).map {
            case (v1.StakingCredential.StakingHash(cred), coin) => cred -> coin
            case w => throw new IllegalStateException(s"Invalid withdrawal: $w")
        }

        v3.TxInfo(
          inputs = prelude.List.from(body.inputs.toSet.view.map(getTxInInfoV3(_, utxos))),
          referenceInputs =
              prelude.List.from(body.referenceInputs.toSet.view.map(getTxInInfoV3(_, utxos))),
          outputs = prelude.List.from(body.outputs.view.map(getTxOutV2)),
          fee = body.fee.value,
          mint = getMintValueV3(body.mint),
          certificates = prelude.List.from(body.certificates.toSeq.view.map(getTxCertV3)),
          withdrawals = SortedMap.fromList(withdrawals),
          validRange = getInterval(body.validityStartSlot, body.ttl, slotConfig, protocolVersion),
          signatories = prelude.List.from(
            body.requiredSigners.toSet.view
                .map(hash => v1.PubKeyHash(hash))
          ),
          redeemers = SortedMap.unsafeFromList(prelude.List.from(redeemers.sorted.map { redeemer =>
              val purpose = getScriptPurposeV3(tx, redeemer)
              purpose -> redeemer.data
          })),
          data = SortedMap.fromList(prelude.List.from(datums)),
          id = v3.TxId(tx.id),
          votes = getVotingProcedures(body.votingProcedures),
          proposalProcedures = prelude.List.from(
            body.proposalProcedures.toSeq.view.map(getProposalProcedureV3)
          ),
          currentTreasuryAmount = body.currentTreasuryValue
              .map(coin => prelude.Option.Some(BigInt(coin.value)))
              .getOrElse(prelude.Option.None),
          treasuryDonation = body.donation
              .map(coin => prelude.Option.Some(BigInt(coin.value)))
              .getOrElse(prelude.Option.None)
        )
    }

    /** Get script purpose for Plutus V1/V2 contexts from redeemer.
      */
    def getScriptPurposeV1(tx: Transaction, redeemer: Redeemer): v1.ScriptPurpose = {
        val body = tx.body.value
        val index = redeemer.index

        redeemer.tag match
            case RedeemerTag.Spend =>
                val inputs = body.inputs.toSeq
                if inputs.isDefinedAt(index) then
                    val input = inputs(index)
                    v1.ScriptPurpose.Spending(
                      v1.TxOutRef(
                        v1.TxId(input.transactionId),
                        input.index
                      )
                    )
                else throw new IllegalStateException(s"Input not found: $index")

            case RedeemerTag.Mint =>
                val policyIds =
                    body.mint
                        .map(_.assets.keys.toArray)
                        .getOrElse(Array.empty[ByteString])
                if policyIds.isDefinedAt(index) then v1.ScriptPurpose.Minting(policyIds(index))
                else throw new IllegalStateException(s"Policy ID not found: $index")

            case RedeemerTag.Cert =>
                val certs = body.certificates.toSeq
                if certs.isDefinedAt(index) then v1.ScriptPurpose.Certifying(getDCert(certs(index)))
                else throw new IllegalStateException(s"Certificate not found: $index")

            case RedeemerTag.Reward =>
                val withdrawals = getOrderedWithdrawals(body.withdrawals).toIndexedSeq
                if withdrawals.isDefinedAt(index) then
                    v1.ScriptPurpose.Rewarding(withdrawals(index)._1)
                else throw new IllegalStateException(s"Withdrawal not found: $index")

            case _ =>
                throw new IllegalStateException(
                  s"Unsupported redeemer tag: ${redeemer.tag} in PlutusV1/V2"
                )
    }

    def getScriptPurposeV2(tx: Transaction, redeemer: Redeemer): v1.ScriptPurpose = {
        getScriptPurposeV1(tx, redeemer)
    }

    /** Get script purpose for Plutus V3 contexts from redeemer.
      *
      * V3 script purposes include governance-related purposes like proposing and voting that
      * weren't available in earlier versions.
      */
    def getScriptPurposeV3(tx: Transaction, redeemer: Redeemer): v3.ScriptPurpose = {
        val body = tx.body.value
        val index = redeemer.index

        redeemer.tag match
            case RedeemerTag.Spend =>
                val inputs = body.inputs.toSeq
                if inputs.isDefinedAt(index) then
                    val input = inputs(index)
                    v3.ScriptPurpose.Spending(
                      v3.TxOutRef(
                        v3.TxId(input.transactionId),
                        input.index
                      )
                    )
                else throw new IllegalStateException(s"Input not found: $index")

            case RedeemerTag.Mint =>
                val policyIds =
                    body.mint
                        .map(_.assets.keys.toArray[ByteString])
                        .getOrElse(Array.empty[ByteString])
                if policyIds.isDefinedAt(index) then v3.ScriptPurpose.Minting(policyIds(index))
                else throw new IllegalStateException(s"Policy ID not found: $index")

            case RedeemerTag.Cert =>
                val certs = body.certificates.toSeq
                if certs.isDefinedAt(index) then
                    v3.ScriptPurpose.Certifying(index, getTxCertV3(certs(index)))
                else throw new IllegalStateException(s"Certificate not found: $index")

            case RedeemerTag.Reward =>
                val withdrawals = getOrderedWithdrawals(body.withdrawals).toIndexedSeq
                if withdrawals.isDefinedAt(index) then
                    val (v1.StakingCredential.StakingHash(cred), _) = withdrawals(index)
                    v3.ScriptPurpose.Rewarding(cred)
                else throw new IllegalStateException(s"Withdrawal not found: $index")

            case RedeemerTag.Proposing =>
                val proposals = body.proposalProcedures.toSeq
                if proposals.isDefinedAt(index) then
                    v3.ScriptPurpose.Proposing(index, getProposalProcedureV3(proposals(index)))
                else throw new IllegalStateException(s"Proposal not found: $index")

            case RedeemerTag.Voting =>
                val voting = body.votingProcedures
                    .map(_.procedures.toSeq.sortBy(_._1.toString))
                    .getOrElse(Seq.empty)
                if voting.isDefinedAt(index) then
                    v3.ScriptPurpose.Voting(getVoterV3(voting(index)._1))
                else throw new IllegalStateException(s"Voter not found: $index")
    }

    /** Get script info for Plutus V3 contexts.
      *
      * ScriptInfo provides context-specific information about the script being executed, including
      * relevant data like datums for spending scripts.
      */
    def getScriptInfoV3(tx: Transaction, redeemer: Redeemer, datum: Option[Data]): v3.ScriptInfo = {
        getScriptPurposeV3(tx, redeemer) match
            case v3.ScriptPurpose.Spending(ref) =>
                v3.ScriptInfo.SpendingScript(ref, datum.asScalus)
            case v3.ScriptPurpose.Minting(policyId) =>
                v3.ScriptInfo.MintingScript(policyId)
            case v3.ScriptPurpose.Certifying(index, cert) =>
                v3.ScriptInfo.CertifyingScript(index, cert)
            case v3.ScriptPurpose.Rewarding(cred) =>
                v3.ScriptInfo.RewardingScript(cred)
            case v3.ScriptPurpose.Proposing(index, proposal) =>
                v3.ScriptInfo.ProposingScript(index, proposal)
            case v3.ScriptPurpose.Voting(voter) =>
                v3.ScriptInfo.VotingScript(voter)
    }

    /** Convert proposal procedure for V3 script contexts.
      */
    def getProposalProcedureV3(proposal: ProposalProcedure): v3.ProposalProcedure = {
        v3.ProposalProcedure(
          proposal.deposit.value,
          getAddress(proposal.rewardAccount.address).credential,
          getGovernanceActionV3(proposal.govAction)
        )
    }

    /** Convert governance action for V3 script contexts.
      */
    def getGovernanceActionV3(action: GovAction): v3.GovernanceAction = {
        action match
            case GovAction.ParameterChange(previousActionId, changedParameters, policy) =>
                v3.GovernanceAction.ParameterChange(
                  id = previousActionId
                      .map(id => prelude.Option.Some(getGovActionId(id)))
                      .getOrElse(prelude.Option.None),
                  parameters = changedParameters.toData,
                  constitutionScript = policy
                      .map(p => prelude.Option.Some(p))
                      .getOrElse(prelude.Option.None)
                )
            case GovAction.HardForkInitiation(previousActionId, protocolVersion) =>
                v3.GovernanceAction.HardForkInitiation(
                  id = previousActionId
                      .map(id => prelude.Option.Some(getGovActionId(id)))
                      .getOrElse(prelude.Option.None),
                  protocolVersion = getProtocolVersion(protocolVersion)
                )
            case GovAction.TreasuryWithdrawals(withdrawals, policy) =>
                val wdwls = withdrawals.map { case (account, coin) =>
                    getAddress(account.address).credential -> BigInt(coin.value)
                }.toSeq
                v3.GovernanceAction.TreasuryWithdrawals(
                  withdrawals = SortedMap.fromList(prelude.List.from(wdwls)),
                  constitutionScript = policy
                      .map(p => prelude.Option.Some(p))
                      .getOrElse(prelude.Option.None)
                )
            case GovAction.NoConfidence(previousActionId) =>
                v3.GovernanceAction.NoConfidence(
                  id = previousActionId
                      .map(id => prelude.Option.Some(getGovActionId(id)))
                      .getOrElse(prelude.Option.None)
                )
            case GovAction.UpdateCommittee(
                  previousActionId,
                  membersToRemove,
                  membersToAdd,
                  newQuorum
                ) =>
                v3.GovernanceAction.UpdateCommittee(
                  id = previousActionId
                      .map(id => prelude.Option.Some(getGovActionId(id)))
                      .getOrElse(prelude.Option.None),
                  removedMembers = prelude.List.from(membersToRemove.toSeq.map(getCredential)),
                  addedMembers =
                      SortedMap.fromList(prelude.List.from(membersToAdd.map { case (cred, epoch) =>
                          getCredential(cred) -> BigInt(epoch)
                      }.toSeq)),
                  newQuorum =
                      prelude.Rational(BigInt(newQuorum.numerator), BigInt(newQuorum.denominator))
                )
            case GovAction.NewConstitution(previousActionId, constitution) =>
                v3.GovernanceAction.NewConstitution(
                  id = previousActionId.map(getGovActionId).asScalus,
                  constitution = constitution.scriptHash.asScalus
                )
            case GovAction.InfoAction => v3.GovernanceAction.InfoAction
    }

    /** Convert protocol version for V3 script contexts.
      */
    def getProtocolVersion(version: ProtocolVersion): v3.ProtocolVersion = {
        v3.ProtocolVersion(version.major, version.minor)
    }

    /** Convert governance action ID for V3 script contexts.
      */
    def getGovActionId(id: GovActionId): v3.GovernanceActionId = {
        v3.GovernanceActionId(
          v3.TxId(id.transactionId),
          id.govActionIndex
        )
    }

    /** Convert voter for V3 script contexts.
      */
    def getVoterV3(voter: Voter): v3.Voter = {
        voter match
            case Voter.ConstitutionalCommitteeHotKey(keyHash) =>
                v3.Voter.CommitteeVoter(v1.Credential.PubKeyCredential(v1.PubKeyHash(keyHash)))
            case Voter.ConstitutionalCommitteeHotScript(scriptHash) =>
                v3.Voter.CommitteeVoter(v1.Credential.ScriptCredential(scriptHash))
            case Voter.DRepKey(keyHash) =>
                v3.Voter.DRepVoter(
                  v1.Credential.PubKeyCredential(v1.PubKeyHash(keyHash))
                )
            case Voter.DRepScript(scriptHash) =>
                v3.Voter.DRepVoter(v1.Credential.ScriptCredential(scriptHash))
            case Voter.StakingPoolKey(keyHash) =>
                v3.Voter.StakePoolVoter(v1.PubKeyHash(keyHash))
    }

    /** Convert voting procedures for V3 script contexts.
      */
    def getVotingProcedures(
        votingProcs: Option[VotingProcedures]
    ): SortedMap[v3.Voter, SortedMap[GovernanceActionId, v3.Vote]] = {
        votingProcs match
            case None => SortedMap.empty
            case Some(vp) =>
                SortedMap.unsafeFromList(
                  prelude.List.from(
                    vp.procedures.toArray.sortBy(_._1.toString).map { case (voter, procedures) =>
                        getVoterV3(voter) -> SortedMap.unsafeFromList(
                          prelude.List.from(
                            procedures.toSeq.sortBy(_._1.toString).map {
                                case (govActionId, procedure) =>
                                    getGovActionId(govActionId) -> getVoteV3(procedure)
                            }
                          )
                        )
                    }
                  )
                )
    }

    /** Convert vote for V3 script contexts.
      */
    def getVoteV3(procedure: VotingProcedure): v3.Vote = {
        procedure.vote match
            case Vote.Yes     => v3.Vote.Yes
            case Vote.No      => v3.Vote.No
            case Vote.Abstain => v3.Vote.Abstain
    }

    /** Create a complete V2 script context from transaction and redeemer.
      */
    def getScriptContextV2(
        redeemer: Redeemer,
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: MajorProtocolVersion
    ): v2.ScriptContext = {
        val purpose = getScriptPurposeV2(tx, redeemer)
        val datums = tx.witnessSet.plutusData.value.toMap.view.mapValues(_.value).toSeq
        val txInfo = getTxInfoV2(tx, datums, utxos, slotConfig, protocolVersion)
        v2.ScriptContext(txInfo, purpose)
    }

    /** Create a complete V3 script context from transaction, redeemer, and datum.
      */
    def getScriptContextV3(
        redeemer: Redeemer,
        datum: Option[Data],
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: MajorProtocolVersion
    ): v3.ScriptContext = {
        val scriptInfo = getScriptInfoV3(tx, redeemer, datum)
        val datums = tx.witnessSet.plutusData.value.toMap.view.mapValues(_.value).toSeq
        val txInfo = getTxInfoV3(tx, datums, utxos, slotConfig, protocolVersion)
        v3.ScriptContext(txInfo, redeemer.data, scriptInfo)
    }
}
