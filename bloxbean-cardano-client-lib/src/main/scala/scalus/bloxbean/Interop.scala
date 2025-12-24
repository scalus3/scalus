package scalus.bloxbean

import com.bloxbean.cardano.client.address.{Address, AddressType, Credential, CredentialType}
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.spec.{Rational, UnitInterval}
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.spec.cert.*
import com.bloxbean.cardano.client.transaction.spec.governance.*
import com.bloxbean.cardano.client.transaction.spec.governance.actions.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import io.bullet.borer.Cbor
import scalus.builtin.Builtins.*
import scalus.builtin.Data.{toData, ToData}
import scalus.builtin.{BuiltinPair, ByteString, Data, ToData}
import scalus.cardano.ledger.{CostModels, Language, MajorProtocolVersion, Script, SlotConfig}
import scalus.ledger.api
import scalus.ledger.api.v1.{DCert, ScriptPurpose, StakingCredential}
import scalus.ledger.api.v3.GovernanceActionId
import scalus.ledger.api.{v1, v2, v3}
import scalus.prelude.List.toScalaList
import scalus.prelude.{asScalus, List as PList, SortedMap}
import scalus.uplc.eval.*
import scalus.{builtin, ledger, prelude}

import java.math.BigInteger
import java.util
import scala.collection.{immutable, mutable}
import scala.jdk.CollectionConverters.*
import scala.math.BigInt

given Ordering[TransactionInput] with
    def compare(x: TransactionInput, y: TransactionInput): Int =
        x.getTransactionId.compareTo(y.getTransactionId) match
            case 0 => x.getIndex.compareTo(y.getIndex)
            case c => c

given Ordering[v1.StakingCredential.StakingHash] = Ordering.by { cred =>
    cred.cred match
        case v1.Credential.PubKeyCredential(pkh)  => pkh.hash
        case v1.Credential.ScriptCredential(hash) => hash
}

given Ordering[Redeemer] with
    def compare(x: Redeemer, y: Redeemer): Int =
        x.getTag.value.compareTo(y.getTag.value) match
            case 0 => x.getIndex.compareTo(y.getIndex)
            case c => c

/** Interoperability between Cardano Client Lib and Scalus */
object Interop {
    given Ordering[Voter] with {
        def compare(x: Voter, y: Voter): Int =
            x.getType.compareTo(y.getType) match
                case 0 =>
                    util.Arrays.compareUnsigned(
                      x.getCredential.getBytes,
                      y.getCredential.getBytes
                    )
                case c => c
    }

    given Ordering[GovActionId] with {
        def compare(x: GovActionId, y: GovActionId): Int =
            x.getTransactionId.compareTo(y.getTransactionId) match
                case 0 => x.getGovActionIndex.compareTo(y.getGovActionIndex)
                case c => c
    }

    @deprecated("use ToData.given_ToData_BigInteger", "0.14.1")
    given ToData[BigInteger] = ToData.given_ToData_BigInteger
    @deprecated("use ToData.given_ToData_Integer", "0.14.1")
    given ToData[Integer] = ToData.given_ToData_Integer
    @deprecated("use ToData.given_ToData_jsLong", "0.14.1")
    given ToData[java.lang.Long] = ToData.given_ToData_jsLong

    given ToData[ProtocolParamUpdate] =
        given ToData[Rational] = (x: Rational) =>
            listData(
              builtin.BuiltinList(
                x.getNumerator.toData(using ToData.given_ToData_BigInteger),
                x.getDenominator.toData(using ToData.given_ToData_BigInteger)
              )
            )
        given ToData[UnitInterval] = (x: UnitInterval) =>
            listData(
              builtin.BuiltinList(
                x.getNumerator.toData(using ToData.given_ToData_BigInteger),
                x.getDenominator.toData(using ToData.given_ToData_BigInteger)
              )
            )
        given ToData[ExUnitPrices] = (x: ExUnitPrices) =>
            listData(builtin.BuiltinList(x.getMemPrice.toData, x.getStepPrice.toData))
        given ToData[ExUnits] = (x: ExUnits) =>
            listData(
              builtin.BuiltinList(
                x.getMem.toData(using ToData.given_ToData_BigInteger),
                x.getSteps.toData(using ToData.given_ToData_BigInteger)
              )
            )

        (x: ProtocolParamUpdate) => {
            val params = mutable.ArrayBuffer.empty[BuiltinPair[Data, Data]]
            def add[A: ToData](idx: Int, value: A): Unit =
                if value != null then params.append(BuiltinPair(iData(idx), value.toData))

            add(0, x.getMinFeeA)(using ToData.given_ToData_BigInteger)
            add(1, x.getMinFeeB)(using ToData.given_ToData_BigInteger)
            add(2, x.getMaxBlockSize)(using ToData.given_ToData_Integer)
            add(3, x.getMaxTxSize)(using ToData.given_ToData_Integer)
            add(4, x.getMaxBlockHeaderSize)(using ToData.given_ToData_Integer)
            add(5, x.getKeyDeposit)(using ToData.given_ToData_BigInteger)
            add(6, x.getPoolDeposit)(using ToData.given_ToData_BigInteger)
            add(7, x.getMaxEpoch)(using ToData.given_ToData_Integer)
            add(8, x.getNOpt)(using ToData.given_ToData_Integer)
            add(9, x.getPoolPledgeInfluence)
            add(10, x.getExpansionRate)
            add(11, x.getTreasuryGrowthRate)
            add(16, x.getMinPoolCost)(using ToData.given_ToData_BigInteger)
            add(17, x.getAdaPerUtxoByte)(using ToData.given_ToData_BigInteger)
            // FIXME: implement 18 x.getCostModels
            add(19, x.getExecutionCosts)
            add(20, x.getMaxTxExUnits)
            add(21, x.getMaxBlockExUnits)
            add(22, x.getMaxValSize)(using ToData.given_ToData_jsLong)
            add(23, x.getCollateralPercent)(using ToData.given_ToData_Integer)
            add(24, x.getMaxCollateralInputs)(using ToData.given_ToData_Integer)
            // FIXME: add missing fields when they are implemented in the client lib
            mapData(builtin.BuiltinList.from(params))
        }

    /// Helper for null check
    extension [A](inline a: A) inline infix def ??(b: => A): A = if a != null then a else b

    private[bloxbean] def getScriptFromScriptRefBytes(scriptRefBytes: Array[Byte]): Script = {
        Cbor.decode(scriptRefBytes).to[Script].value
    }

    /** Converts Cardano Client Lib's [[com.bloxbean.cardano.client.plutus.spec.PlutusData]] to
      * Scalus' [[scalus.builtin.Data]]
      */
    extension (datum: PlutusData)
        def toScalusData: Data = {
            datum match
                case c: ConstrPlutusData =>
                    val args = c.getData.getPlutusDataList.asScala.map(_.toScalusData).toList
                    Data.Constr(c.getAlternative, PList.from(args))
                case m: MapPlutusData =>
                    val entries = m.getMap.entrySet().iterator().asScala
                    val values = entries.map { e =>
                        (e.getKey.toScalusData, e.getValue.toScalusData)
                    }.toList
                    Data.Map(PList.from(values))
                case l: ListPlutusData =>
                    val values = l.getPlutusDataList.asScala.map(_.toScalusData).toList
                    Data.List(PList.from(values))
                case i: BigIntPlutusData =>
                    Data.I(i.getValue)
                case b: BytesPlutusData =>
                    Data.B(ByteString.fromArray(b.getValue))
        }

    /** Converts Scalus' [[scalus.builtin.Data]] to Cardano Client Lib's
      * [[com.bloxbean.cardano.client.plutus.spec.PlutusData]]
      */
    def toPlutusData(data: Data): PlutusData = {
        data match
            case Data.Constr(tag, args) =>
                val convertedArgs = ListPlutusData
                    .builder()
                    .plutusDataList(args.toScalaList.map(toPlutusData).asJava)
                    .build()
                ConstrPlutusData
                    .builder()
                    .alternative(tag.toLong)
                    .data(convertedArgs)
                    .build()
            case Data.Map(items) =>
                MapPlutusData
                    .builder()
                    .map(
                      items.toScalaList
                          .map { case (k, v) =>
                              (toPlutusData(k), toPlutusData(v))
                          }
                          .toMap
                          .asJava
                    )
                    .build()
            case Data.List(items) =>
                ListPlutusData
                    .builder()
                    .plutusDataList(items.toScalaList.map(toPlutusData).asJava)
                    .build()
            case Data.I(i) =>
                BigIntPlutusData.of(i.bigInteger)
            case Data.B(b) =>
                BytesPlutusData.of(b.bytes)
    }

    /** Converts Bloxbean's [[com.bloxbean.cardano.client.plutus.spec.CostMdls]] to Scalus
      * [[scalus.cardano.ledger.CostModels]]
      */
    def getCostModels(costMdls: CostMdls): CostModels = {
        val models = Map.newBuilder[Int, IndexedSeq[Long]]

        // Iterate through all Bloxbean Language enum values
        for bloxbeanLang <- com.bloxbean.cardano.client.plutus.spec.Language.values() do
            try {
                val costs = costMdls.get(bloxbeanLang)
                if costs != null then
                    // Use the language key from Bloxbean as the language ID
                    models += (bloxbeanLang.getKey -> immutable.ArraySeq.unsafeWrapArray(
                      costs.getCosts
                    ))
            } catch {
                case _: Exception => // Language not present, skip
            }

        CostModels(models.result())
    }

    /** Creates [[scalus.uplc.eval.MachineParams]] from a
      * [[com.bloxbean.cardano.client.plutus.spec.CostMdls]] and a
      * [[scalus.cardano.ledger.Language]]
      */
    def translateMachineParamsFromCostMdls(
        costMdls: CostMdls,
        plutus: Language,
        protocolVersion: MajorProtocolVersion
    ): MachineParams = {
        val costModels = getCostModels(costMdls)
        MachineParams.fromCostModels(costModels, plutus, protocolVersion)
    }

    def getCredential(cred: Credential): v1.Credential = {
        cred.getType match
            case CredentialType.Key =>
                v1.Credential.PubKeyCredential(v1.PubKeyHash(ByteString.fromArray(cred.getBytes)))
            case CredentialType.Script =>
                v1.Credential.ScriptCredential(ByteString.fromArray(cred.getBytes))
    }

    def getCredential(cred: StakeCredential): v1.Credential = {
        cred.getType match
            case StakeCredType.ADDR_KEYHASH =>
                v1.Credential.PubKeyCredential(v1.PubKeyHash(ByteString.fromArray(cred.getHash)))
            case StakeCredType.SCRIPTHASH =>
                v1.Credential.ScriptCredential(ByteString.fromArray(cred.getHash))
    }

    def getStakingCredential(cred: StakeCredential): v1.StakingCredential = {
        cred.getType match
            case StakeCredType.ADDR_KEYHASH =>
                v1.StakingCredential.StakingHash(
                  v1.Credential.PubKeyCredential(v1.PubKeyHash(ByteString.fromArray(cred.getHash)))
                )
            case StakeCredType.SCRIPTHASH =>
                v1.StakingCredential.StakingHash(
                  v1.Credential.ScriptCredential(ByteString.fromArray(cred.getHash))
                )
    }

    def getStakingCredential(cred: Credential): v1.StakingCredential = {
        v1.StakingCredential.StakingHash(getCredential(cred))
    }

    def getAddress(address: Address): v1.Address = {
        val cred = address.getPaymentCredential.map(getCredential).get
        val staking = address.getDelegationCredential
            .map(cred => prelude.Option.Some(getStakingCredential(cred)))
            .orElse(prelude.Option.None)
        v1.Address(cred, staking)
    }

    def getTxInInfoV1(
        input: TransactionInput,
        utxos: Map[TransactionInput, TransactionOutput]
    ): v1.TxInInfo = {
        val out = utxos.getOrElse(input, throw new IllegalStateException("Input Not Found"))
        val addr = Address(out.getAddress)
        val optionDatumHash =
            if out.getDatumHash != null then
                prelude.Option.Some(ByteString.fromArray(out.getDatumHash))
            else prelude.Option.None
        v1.TxInInfo(
          v1.TxOutRef(
            v1.TxId(ByteString.fromHex(input.getTransactionId)),
            input.getIndex
          ),
          v1.TxOut(
            getAddress(addr),
            getValue(out.getValue),
            optionDatumHash
          )
        )
    }

    def getTxInInfoV2(
        input: TransactionInput,
        utxos: Map[TransactionInput, TransactionOutput]
    ): v2.TxInInfo = {
        val out = utxos.getOrElse(input, throw new IllegalStateException("Input Not Found"))
        val addr = Address(out.getAddress)
        v2.TxInInfo(
          v1.TxOutRef(
            v1.TxId(ByteString.fromHex(input.getTransactionId)),
            input.getIndex
          ),
          v2.TxOut(
            getAddress(addr),
            getValue(out.getValue),
            getOutputDatum(out),
            if out.getScriptRef != null
            then prelude.Option.Some(getScriptFromScriptRefBytes(out.getScriptRef).scriptHash)
            else prelude.Option.None
          )
        )
    }

    def getValue(value: Value): v1.Value = {
        val ma = getValue(value.getMultiAssets)
        if value.getCoin != null then
            val lovelace = v1.Value.lovelace(value.getCoin)
            lovelace + ma
        else ma
    }

    def getValue(value: util.List[MultiAsset]): v1.Value = {
        // get sorted multi assets
        val multi = mutable.TreeMap.empty[ByteString, mutable.TreeMap[ByteString, BigInt]]
        for m <- value.asScala do
            val assets = mutable.TreeMap.empty[ByteString, BigInt]
            for asset <- m.getAssets.asScala do
                assets.put(ByteString.fromArray(asset.getNameAsBytes), asset.getValue)
            multi.put(ByteString.fromHex(m.getPolicyId), assets)

        v1.Value.fromList(
          prelude.List.from(
            for (policyId, assets) <- multi.iterator
            yield policyId -> prelude.List.from(assets)
          )
        )
    }

    private val ADA_ZERO = ByteString.empty -> PList(ByteString.empty -> BigInt(0))

    def getMintValue(value: util.List[MultiAsset]): v1.Value = {
        // Always include ADA entry with zero value for minting
        val x = getValue(value).toSortedMap.toList.map(x => x._1 -> x._2.toList)
        v1.Value.unsafeFromList(x.prepended(ADA_ZERO))
    }

    def getTxOutV1(out: TransactionOutput): v1.TxOut = {
        val addr = Address(out.getAddress)
        val optionDatumHash =
            if out.getDatumHash != null then
                prelude.Option.Some(ByteString.fromArray(out.getDatumHash))
            else prelude.Option.None
        v1.TxOut(
          getAddress(addr),
          getValue(out.getValue),
          optionDatumHash
        )
    }

    def getTxOutV2(out: TransactionOutput): v2.TxOut = {
        val addr = Address(out.getAddress)
        v2.TxOut(
          getAddress(addr),
          getValue(out.getValue),
          getOutputDatum(out),
          if out.getScriptRef != null then
              prelude.Option.Some(getScriptFromScriptRefBytes(out.getScriptRef).scriptHash)
          else prelude.Option.None
        )
    }

    def getOutputDatum(out: TransactionOutput): v2.OutputDatum = {
        if out.getDatumHash != null then
            v2.OutputDatum.OutputDatumHash(ByteString.fromArray(out.getDatumHash))
        else if out.getInlineDatum != null then
            v2.OutputDatum.OutputDatum(toScalusData(out.getInlineDatum))
        else v2.OutputDatum.NoOutputDatum
    }

    // https://github.com/IntersectMBO/cardano-ledger/blob/28ab3884cac8edbb7270fd4b8628a16429d2ec9e/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/TxInfo.hs#L186
    def getInterval(tx: Transaction, slotConfig: SlotConfig, protocolVersion: Int): v1.Interval = {
        val validFrom = tx.getBody.getValidityStartInterval
        (validFrom, tx.getBody.getTtl) match
            case (0, 0) => v1.Interval.always
            case (0, validTo) =>
                val closure =
                    if protocolVersion > 8 then false
                    else true // don't ask me why, I know it's stupid
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
                  false // Closure is false here, this is how Cardano Ledger does it for upper bound
                )
                v1.Interval(lower, upper)
    }

    def getWithdrawals(
        withdrawals: util.List[Withdrawal]
    ): prelude.List[(v1.StakingCredential, BigInt)] = {
        // get sorted withdrawals
        val wdwls = mutable.TreeMap.empty[v1.StakingCredential.StakingHash, BigInt]
        for w <- withdrawals.asScala do
            val addr = Address(w.getRewardAddress)
            val cred = addr.getDelegationCredential.map(getCredential).get
            wdwls.put(v1.StakingCredential.StakingHash(cred), BigInt(w.getCoin))
        prelude.List.from(wdwls)
    }

    def getDCert(cert: Certificate): v1.DCert = {
        cert match
            case c: StakeRegistration =>
                v1.DCert.DelegRegKey(getStakingCredential(c.getStakeCredential))
            case c: StakeDeregistration =>
                v1.DCert.DelegDeRegKey(getStakingCredential(c.getStakeCredential))
            case c: StakeDelegation =>
                v1.DCert.DelegDelegate(
                  getStakingCredential(c.getStakeCredential),
                  v1.PubKeyHash(ByteString.fromArray(c.getStakePoolId.getPoolKeyHash))
                )
            case c: PoolRegistration =>
                v1.DCert.PoolRegister(
                  v1.PubKeyHash(ByteString.fromArray(c.getOperator)),
                  v1.PubKeyHash(ByteString.fromArray(c.getVrfKeyHash))
                )
            case c: PoolRetirement =>
                v1.DCert.PoolRetire(
                  v1.PubKeyHash(ByteString.fromArray(c.getPoolKeyHash)),
                  BigInt(c.getEpoch)
                )
            case c: GenesisKeyDelegation => v1.DCert.Genesis
            case c: MoveInstataneous     => v1.DCert.Mir
    }

    def getTxCertV3(cert: Certificate): v3.TxCert = {
        cert match
            case c: AuthCommitteeHotCert =>
                v3.TxCert.AuthHotCommittee(
                  getCredential(c.getCommitteeColdCredential),
                  getCredential(c.getCommitteeHotCredential)
                )
            case c: GenesisKeyDelegation =>
                throw new IllegalArgumentException("GenesisKeyDelegation not supported in V3")
            case c: MoveInstataneous =>
                throw new IllegalArgumentException("MoveInstantaneous not supported in V3")
            case c: PoolRegistration =>
                v3.TxCert.PoolRegister(
                  v1.PubKeyHash(ByteString.fromArray(c.getOperator)),
                  v1.PubKeyHash(ByteString.fromArray(c.getVrfKeyHash))
                )
            case c: PoolRetirement =>
                v3.TxCert.PoolRetire(
                  v1.PubKeyHash(ByteString.fromArray(c.getPoolKeyHash)),
                  BigInt(c.getEpoch)
                )
            case c: RegCert =>
                v3.TxCert.RegStaking(getCredential(c.getStakeCredential), prelude.Option.None)
            case c: RegDRepCert =>
                v3.TxCert.RegDRep(getCredential(c.getDrepCredential), BigInt(c.getCoin))
            case c: ResignCommitteeColdCert =>
                v3.TxCert.ResignColdCommittee(getCredential(c.getCommitteeColdCredential))
            case c: StakeDelegation =>
                v3.TxCert.DelegStaking(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.Stake(
                    v1.PubKeyHash(ByteString.fromArray(c.getStakePoolId.getPoolKeyHash))
                  )
                )
            case c: StakeDeregistration =>
                throw new IllegalArgumentException("StakeDeregistration not supported in V3")
            case c: StakeRegDelegCert =>
                v3.TxCert.RegDeleg(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.Stake(v1.PubKeyHash(ByteString.fromHex(c.getPoolKeyHash))),
                  BigInt(c.getCoin)
                )
            case c: StakeRegistration =>
                throw new IllegalArgumentException("StakeRegistration not supported in V3")
            case c: StakeVoteDelegCert =>
                v3.TxCert.DelegStaking(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.StakeVote(
                    v1.PubKeyHash(ByteString.fromHex(c.getPoolKeyHash)),
                    getDRep(c.getDrep)
                  )
                )
            case c: StakeVoteRegDelegCert =>
                v3.TxCert.RegDeleg(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.StakeVote(
                    v1.PubKeyHash(ByteString.fromHex(c.getPoolKeyHash)),
                    getDRep(c.getDrep)
                  ),
                  BigInt(c.getCoin)
                )
            case c: UnregCert =>
                v3.TxCert.UnRegStaking(getCredential(c.getStakeCredential), prelude.Option.None)
            case c: UnregDRepCert =>
                v3.TxCert.UnRegDRep(getCredential(c.getDrepCredential), BigInt(c.getCoin))
            case c: UpdateDRepCert =>
                v3.TxCert.UpdateDRep(getCredential(c.getDrepCredential))
            case c: VoteDelegCert =>
                val dRep = getDRep(c.getDrep)
                v3.TxCert.DelegStaking(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.Vote(dRep)
                )
            case c: VoteRegDelegCert =>
                val dRep = getDRep(c.getDrep)
                v3.TxCert.RegDeleg(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.Vote(dRep),
                  BigInt(c.getCoin)
                )
    }

    private def getDRep(drep: DRep): v3.DRep = {
        drep.getType match
            case DRepType.ADDR_KEYHASH =>
                v3.DRep.DRep(
                  v1.Credential.PubKeyCredential(
                    v1.PubKeyHash(ByteString.fromHex(drep.getHash))
                  )
                )
            case DRepType.SCRIPTHASH =>
                v3.DRep.DRep(
                  v1.Credential.ScriptCredential(ByteString.fromHex(drep.getHash))
                )
            case DRepType.ABSTAIN =>
                v3.DRep.AlwaysAbstain
            case DRepType.NO_CONFIDENCE =>
                v3.DRep.AlwaysNoConfidence
    }

    def getTxInfoV1(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v1.TxInfo = getTxInfoV1(
      tx,
      TransactionUtil.getTxHash(tx),
      datums,
      utxos,
      slotConfig,
      protocolVersion
    )

    def getTxInfoV1(
        tx: Transaction,
        txhash: String,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v1.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        v1.TxInfo(
          // sorted inputs
          inputs = prelude.List.from(body.getInputs.asScala.sorted.map(getTxInInfoV1(_, utxos))),
          // outputs as in the transaction
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV1)),
          fee = v1.Value.lovelace(body.getFee ?? BigInteger.ZERO),
          // sorted mint values
          mint = getMintValue(body.getMint ?? util.List.of()),
          // certificates as is
          dcert = prelude.List.from(certs.asScala.map(getDCert)),
          withdrawals = getWithdrawals(body.getWithdrawals ?? util.List.of()),
          validRange = getInterval(tx, slotConfig, protocolVersion),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala
                .map(ByteString.fromArray)
                .sorted
                .map(v1.PubKeyHash.apply)
                .toSeq
          ),
          data = prelude.List.from(datums.to(immutable.SortedMap)),
          id = v1.TxId(ByteString.fromHex(txhash))
        )
    }

    def getTxInfoV2(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v2.TxInfo = getTxInfoV2(
      tx,
      TransactionUtil.getTxHash(tx),
      datums,
      utxos,
      slotConfig,
      protocolVersion
    )

    def getTxInfoV2(
        tx: Transaction,
        txhash: String,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v2.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        val rdmrs = tx.getWitnessSet.getRedeemers ?? util.List.of()
        v2.TxInfo(
          inputs = prelude.List.from(body.getInputs.asScala.sorted.map(getTxInInfoV2(_, utxos))),
          referenceInputs = prelude.List.from(
            body.getReferenceInputs.asScala.sorted.map(getTxInInfoV2(_, utxos))
          ),
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV2)),
          fee = v1.Value.lovelace(body.getFee ?? BigInteger.ZERO),
          mint = getMintValue(body.getMint ?? util.List.of()),
          dcert = prelude.List.from(certs.asScala.map(getDCert)),
          withdrawals = SortedMap.fromList(getWithdrawals(body.getWithdrawals ?? util.List.of())),
          validRange = getInterval(tx, slotConfig, protocolVersion),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala
                .map(ByteString.fromArray)
                .sorted
                .map(v1.PubKeyHash.apply)
                .toSeq
          ),
          redeemers =
              SortedMap.unsafeFromList(prelude.List.from(rdmrs.asScala.sorted.map { redeemer =>
                  val purpose = getScriptPurposeV2(
                    redeemer,
                    body.getInputs,
                    body.getMint,
                    body.getCerts,
                    body.getWithdrawals
                  )
                  purpose -> toScalusData(redeemer.getData)
              })),
          data = SortedMap.fromList(prelude.List.from(datums)),
          id = v1.TxId(ByteString.fromHex(txhash))
        )
    }

    def getTxOutRefV1(input: TransactionInput): v1.TxOutRef = {
        v1.TxOutRef(
          v1.TxId(ByteString.fromHex(input.getTransactionId)),
          input.getIndex
        )
    }

    def getTxOutRefV3(input: TransactionInput): v3.TxOutRef = {
        v3.TxOutRef(
          v3.TxId(ByteString.fromHex(input.getTransactionId)),
          input.getIndex
        )
    }

    def getScriptPurposeV1(
        redeemer: Redeemer,
        tx: Transaction
    ): v1.ScriptPurpose =
        getScriptPurposeV1(
          redeemer,
          tx.getBody.getInputs,
          tx.getBody.getMint,
          tx.getBody.getCerts,
          tx.getBody.getWithdrawals
        )

    def getScriptPurposeV1(
        redeemer: Redeemer,
        inputs: util.List[TransactionInput],
        mint: util.List[MultiAsset],
        certificates: util.List[Certificate],
        withdrawals: util.List[Withdrawal]
    ): v1.ScriptPurpose =
        // Cardano Ledger code is stupidly complex and unreadable. We need to make sure this is correct
        val index = redeemer.getIndex.intValue
        redeemer.getTag match
            case RedeemerTag.Spend =>
                val ins = inputs.asScala.sorted
                if ins.isDefinedAt(index) then
                    val input = ins(index)
                    v1.ScriptPurpose.Spending(getTxOutRefV1(input))
                else throw new IllegalStateException(s"Input not found: $index in $inputs")
            case RedeemerTag.Mint =>
                val policyIds = mint.asScala.map(_.getPolicyId).sorted
                if policyIds.isDefinedAt(index) then
                    v1.ScriptPurpose.Minting(ByteString.fromHex(policyIds(index)))
                else throw new IllegalStateException(s"Wrong mint index: $index in $mint")
            case RedeemerTag.Cert =>
                val certs = certificates.asScala
                if certs.isDefinedAt(index) then v1.ScriptPurpose.Certifying(getDCert(certs(index)))
                else throw new IllegalStateException(s"Wrong cert index: $index in $certificates")
            case RedeemerTag.Reward =>
                val rewardAccounts = withdrawals.asScala
                    .map(ra => Address(ra.getRewardAddress))
                    .sortBy(a => ByteString.fromArray(a.getBytes)) // for ordering
                if rewardAccounts.isDefinedAt(index) then
                    val address = rewardAccounts(index)
                    if address.getAddressType == AddressType.Reward then
                        val cred = getCredential(address.getDelegationCredential.get)
                        v1.ScriptPurpose.Rewarding(v1.StakingCredential.StakingHash(cred))
                    else
                        throw new IllegalStateException(
                          s"Wrong reward address type: $address in $withdrawals"
                        )
                else throw new IllegalStateException(s"Wrong reward index: $index in $withdrawals")
            case _ =>
                throw new IllegalStateException(
                  s"Unsupported redeemer tag: ${redeemer.getTag} in PlutusV1/V2"
                )

    def getScriptPurposeV2(
        redeemer: Redeemer,
        inputs: util.List[TransactionInput],
        mint: util.List[MultiAsset],
        certificates: util.List[Certificate],
        withdrawals: util.List[Withdrawal]
    ): v1.ScriptPurpose = getScriptPurposeV1(redeemer, inputs, mint, certificates, withdrawals)

    def getScriptPurposeV2(redeemer: Redeemer, tx: Transaction): v2.ScriptPurpose = {
        getScriptPurposeV2(
          redeemer,
          tx.getBody.getInputs,
          tx.getBody.getMint,
          tx.getBody.getCerts,
          tx.getBody.getWithdrawals
        )
    }

    def getScriptPurposeV3(tx: Transaction, redeemer: Redeemer): v3.ScriptPurpose = {
        val inputs = tx.getBody.getInputs
        val mint = tx.getBody.getMint
        val certificates = tx.getBody.getCerts
        val withdrawals = tx.getBody.getWithdrawals
        // Cardano Ledger code is stupidly complex and unreadable. We need to make sure this is correct
        val index = redeemer.getIndex.intValue
        redeemer.getTag match
            case RedeemerTag.Spend =>
                val ins = inputs.asScala.sorted
                if ins.isDefinedAt(index) then
                    val input = ins(index)
                    v3.ScriptPurpose.Spending(getTxOutRefV3(input))
                else throw new IllegalStateException(s"Input not found: $index in $inputs")
            case RedeemerTag.Mint =>
                val policyIds = mint.asScala.map(_.getPolicyId).sorted
                if policyIds.isDefinedAt(index) then
                    v3.ScriptPurpose.Minting(ByteString.fromHex(policyIds(index)))
                else throw new IllegalStateException(s"Wrong mint index: $index in $mint")
            case RedeemerTag.Cert =>
                val certs = certificates.asScala
                if certs.isDefinedAt(index) then
                    v3.ScriptPurpose.Certifying(index, getTxCertV3(certs(index)))
                else throw new IllegalStateException(s"Wrong cert index: $index in $certificates")
            case RedeemerTag.Reward =>
                val rewardAccounts = withdrawals.asScala
                    .map(ra => Address(ra.getRewardAddress))
                    .sortBy(a => ByteString.fromArray(a.getBytes)) // for ordering
                if rewardAccounts.isDefinedAt(index) then
                    val address = rewardAccounts(index)
                    if address.getAddressType == AddressType.Reward then
                        val cred = getCredential(address.getDelegationCredential.get)
                        v3.ScriptPurpose.Rewarding(cred)
                    else
                        throw new IllegalStateException(
                          s"Wrong reward address type: $address in $withdrawals"
                        )
                else throw new IllegalStateException(s"Wrong reward index: $index in $withdrawals")

            case RedeemerTag.Proposing =>
                val proposals = certificates.asScala.collect { case p: ProposalProcedure => p }
                if proposals.isDefinedAt(index) then
                    v3.ScriptPurpose.Proposing(index, getProposalProcedureV3(proposals(index)))
                else
                    throw new IllegalStateException(
                      s"Wrong proposal index: $index in $certificates"
                    )
            case RedeemerTag.Voting =>
                val voting = tx.getBody.getVotingProcedures.getVoting.asScala.toSeq.sortBy(_._1)
                if voting.isDefinedAt(index) then
                    v3.ScriptPurpose.Voting(getVoterV3(voting(index)._1))
                else
                    throw new IllegalStateException(
                      s"Wrong voter index: $index in $certificates"
                    )
    }

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

    def getScriptContextV2(
        redeemer: Redeemer,
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v2.ScriptContext = getScriptContextV2(
      redeemer,
      tx,
      TransactionUtil.getTxHash(tx),
      utxos,
      slotConfig,
      protocolVersion
    )

    def getScriptContextV2(
        redeemer: Redeemer,
        tx: Transaction,
        txhash: String,
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v2.ScriptContext = {
        import scala.jdk.CollectionConverters.*
        val purpose = getScriptPurposeV2(
          redeemer,
          tx.getBody.getInputs,
          tx.getBody.getMint,
          tx.getBody.getCerts,
          tx.getBody.getWithdrawals
        )
        val datums = tx.getWitnessSet.getPlutusDataList.asScala.map { plutusData =>
            ByteString.fromArray(plutusData.getDatumHashAsBytes) -> Interop.toScalusData(plutusData)
        }
        val txInfo = getTxInfoV2(tx, txhash, datums, utxos, slotConfig, protocolVersion)
        val scriptContext = v2.ScriptContext(txInfo, purpose)
        scriptContext
    }

    def getProposalProcedureV3(proposal: ProposalProcedure): v3.ProposalProcedure = {
        v3.ProposalProcedure(
          proposal.getDeposit,
          getCredential(Address(proposal.getRewardAccount).getPaymentCredential.get),
          getGovernanceActionV3(proposal.getGovAction)
        )
    }

    def getGovernanceActionV3(action: GovAction): v3.GovernanceAction = {
        action match
            case a: ParameterChangeAction =>
                v3.GovernanceAction.ParameterChange(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId)),
                  parameters = a.getProtocolParamUpdate.toData,
                  constitutionScript = prelude.Option(a.getPolicyHash).map(ByteString.fromArray)
                )
            case a: TreasuryWithdrawalsAction =>
                v3.GovernanceAction.TreasuryWithdrawals(
                  withdrawals =
                      SortedMap.fromList(prelude.List.from(a.getWithdrawals.asScala.map { w =>
                          getCredential(
                            Address(w.getRewardAddress).getPaymentCredential.get
                          ) -> BigInt(
                            w.getCoin
                          )

                      }.toList)),
                  constitutionScript = prelude.Option(ByteString.fromArray(a.getPolicyHash))
                )
            case a: HardForkInitiationAction =>
                v3.GovernanceAction.HardForkInitiation(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId)),
                  protocolVersion = getProtocolVersion(a.getProtocolVersion)
                )
            case _: InfoAction => v3.GovernanceAction.InfoAction
            case a: NewConstitution =>
                v3.GovernanceAction.NewConstitution(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId)),
                  constitution =
                      if a.getConstitution.getScripthash != null then
                          prelude.Option.Some(ByteString.fromHex(a.getConstitution.getScripthash))
                      else prelude.Option.None
                )
            case a: NoConfidence =>
                v3.GovernanceAction.NoConfidence(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId))
                )
            case a: UpdateCommittee =>
                v3.GovernanceAction.UpdateCommittee(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId)),
                  removedMembers = prelude.List.from(a.getMembersForRemoval.asScala.map { m =>
                      getCredential(m)
                  }),
                  addedMembers = SortedMap.fromList(
                    prelude.List.from(a.getNewMembersAndTerms.asScala.map { (c, t) =>
                        getCredential(c) -> BigInt(t)
                    })
                  ),
                  newQuorum = prelude.Rational(
                    BigInt(a.getQuorumThreshold.getNumerator),
                    BigInt(a.getQuorumThreshold.getDenominator)
                  )
                )

    }

    def getProtocolVersion(version: ProtocolVersion): v3.ProtocolVersion = {
        v3.ProtocolVersion(version.getMajor, version.getMinor)
    }

    def getGovActionId(id: GovActionId): v3.GovernanceActionId = {
        v3.GovernanceActionId(
          v3.TxId(ByteString.fromHex(id.getTransactionId)),
          id.getGovActionIndex
        )
    }

    def getVoterV3(voter: Voter): v3.Voter = {
        voter.getType match
            case VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH |
                VoterType.CONSTITUTIONAL_COMMITTEE_HOT_SCRIPT_HASH =>
                v3.Voter.CommitteeVoter(getCredential(voter.getCredential))
            case VoterType.DREP_KEY_HASH | VoterType.DREP_SCRIPT_HASH =>
                v3.Voter.DRepVoter(getCredential(voter.getCredential))
            case VoterType.STAKING_POOL_KEY_HASH =>
                v3.Voter.StakePoolVoter(
                  v1.PubKeyHash(ByteString.fromArray(voter.getCredential.getBytes))
                )
    }

    def getVotingProcedures(
        voting: VotingProcedures
    ): SortedMap[v3.Voter, SortedMap[GovernanceActionId, v3.Vote]] = {
        if voting == null then return SortedMap.empty
        SortedMap.unsafeFromList(
          prelude.List.from(
            voting.getVoting.asScala.toSeq
                .sortBy(_._1)
                .map: (voter, procedures) =>
                    getVoterV3(voter) -> SortedMap.unsafeFromList(
                      prelude.List.from(
                        procedures.asScala.toSeq
                            .sortBy(_._1)
                            .map: (govActionId, procedure) =>
                                getGovActionId(govActionId) -> getVoteV3(procedure)
                      )
                    )
          )
        )
    }

    def getVoteV3(procedure: VotingProcedure): v3.Vote = {
        procedure.getVote match
            case Vote.YES     => v3.Vote.Yes
            case Vote.NO      => v3.Vote.No
            case Vote.ABSTAIN => v3.Vote.Abstain
    }

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

    def getTxInfoV3(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v3.TxInfo =
        getTxInfoV3(tx, TransactionUtil.getTxHash(tx), datums, utxos, slotConfig, protocolVersion)

    def getTxInfoV3(
        tx: Transaction,
        txhash: String,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v3.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        val rdmrs = tx.getWitnessSet.getRedeemers ?? util.List.of()
        val withdrawals =
            val wdvls = getWithdrawals(body.getWithdrawals ?? util.List.of())
            wdvls.map {
                case (v1.StakingCredential.StakingHash(cred), coin) => cred -> coin
                case w => throw new IllegalStateException(s"Invalid withdrawal: $w")
            }
        v3.TxInfo(
          inputs = prelude.List.from(body.getInputs.asScala.sorted.map(getTxInInfoV3(_, utxos))),
          referenceInputs = prelude.List.from(
            body.getReferenceInputs.asScala.sorted.map(getTxInInfoV3(_, utxos))
          ),
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV2)),
          fee = body.getFee ?? BigInteger.ZERO,
          mint = getValue(body.getMint ?? util.List.of()),
          certificates = prelude.List.from(certs.asScala.map(getTxCertV3)),
          withdrawals = SortedMap.fromList(withdrawals),
          validRange = getInterval(tx, slotConfig, protocolVersion),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala
                .map(ByteString.fromArray)
                .sorted
                .map(v1.PubKeyHash.apply)
                .toSeq
          ),
          redeemers =
              SortedMap.unsafeFromList(prelude.List.from(rdmrs.asScala.sorted.map { redeemer =>
                  val purpose = getScriptPurposeV3(tx, redeemer)
                  purpose -> toScalusData(redeemer.getData)
              })),
          data = SortedMap.fromList(prelude.List.from(datums)),
          id = v3.TxId(ByteString.fromHex(txhash)),
          votes = getVotingProcedures(body.getVotingProcedures),
          proposalProcedures = prelude.List
              .from(
                (body.getProposalProcedures ?? util.List.of()).asScala
                    .map(getProposalProcedureV3)
              ),
          currentTreasuryAmount =
              if tx.getBody.getCurrentTreasuryValue != null then
                  prelude.Option.Some(
                    BigInt(tx.getBody.getCurrentTreasuryValue)
                  )
              else prelude.Option.None,
          treasuryDonation =
              if tx.getBody.getDonation != null then
                  prelude.Option.Some(BigInt(tx.getBody.getDonation))
              else prelude.Option.None
        )
    }

    def getScriptContextV3(
        redeemer: Redeemer,
        datum: Option[Data],
        tx: Transaction,
        txhash: String,
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v3.ScriptContext = {
        import scala.jdk.CollectionConverters.*
        val scriptInfo = getScriptInfoV3(tx, redeemer, datum)
        val datums = tx.getWitnessSet.getPlutusDataList.asScala.map { plutusData =>
            ByteString.fromArray(plutusData.getDatumHashAsBytes) -> plutusData.toScalusData
        }
        val txInfo = getTxInfoV3(tx, txhash, datums, utxos, slotConfig, protocolVersion)
        val scriptContext = v3.ScriptContext(txInfo, redeemer.getData.toScalusData, scriptInfo)
        scriptContext
    }
}
