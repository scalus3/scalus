package scalus.examples.midgard

import scalus.cardano.onchain.plutus.prelude.{AssocMap, List, Option}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}

/** Port of the fixture builders in Midgard `validators/da-bond-pool.test.ak`.
  *
  * Names follow the Aiken file in camelCase, so each test reads like its Aiken twin.
  */
object DaBondPoolFixtures {
    import DaBondPoolConstants.*

    private def hash28(b: Int): ByteString = ByteString.fromArray(Array.fill(28)(b.toByte))
    private def hash32(b: Int): ByteString = ByteString.fromArray(Array.fill(32)(b.toByte))

    val poolPolicy: PolicyId = hash28(0xd0)
    val hubPolicy: PolicyId = hash28(0xe0)
    val daParamsPolicy: PolicyId = hash28(0xe1)
    val stateQueuePolicy: PolicyId = hash28(0xe2)
    val correctionLockScript: PolicyId = hash28(0xe3)
    val foreignPolicy: PolicyId = hash28(0xef)

    val owner1: PubKeyHash = PubKeyHash(hash28(0xc1))
    val owner2: PubKeyHash = PubKeyHash(hash28(0xc2))
    val owner3: PubKeyHash = PubKeyHash(hash28(0xc3))
    val nonOwner1: PubKeyHash = PubKeyHash(hash28(0xc8))
    val nonOwner2: PubKeyHash = PubKeyHash(hash28(0xc9))

    val committeeKey: ByteString = hash32(0xa1)

    val daBond: BigInt = 500_000_000
    val poolFloor: BigInt = 5_000_000
    val minTopUp: BigInt = 5_000_000

    /** Backing 600 ADA: more than one bond. */
    val poolLovelace: BigInt = 605_000_000
    val withdrawAmount: BigInt = 100_000_000
    val beginLower: BigInt = 1_000_000_000
    val withdrawingUnlockAt: BigInt = 2_000_000_000

    val parameters: ParametersV1 = ParametersV1(
      responseGeometry = ResponseGeometryV1(4_096, 4 * 1024 * 1024, 16),
      daBondLovelace = daBond,
      challengerBondLovelace = 10_000_000_000L,
      maxOpenFeeLovelace = 1,
      maxPublicationFeeLovelace = 1,
      maxSettlementFeeLovelace = 1,
      maxCloseFeeLovelace = 1,
      maxTimeoutFeeLovelace = 1,
      daSlashPenaltyLovelace = 100_000_000,
      daBondMinTopUpLovelace = minTopUp,
      daBondPoolFloorLovelace = poolFloor,
      challengeRecordLovelace = 27_000_000
    )

    /** `bytearray.push(#"00" * 31, seed)`: 31 zero bytes after the seed byte. */
    def outref(seed: Int, index: BigInt): TxOutRef =
        TxOutRef(TxId(ByteString.fromArray(seed.toByte +: Array.fill(31)(0.toByte))), index)

    val initRef: TxOutRef = outref(1, 0)
    val poolRef: TxOutRef = outref(2, 0)

    val params: DaBondPoolParams = DaBondPoolParams(initRef, hubPolicy, daParamsPolicy, parameters)

    def scriptAddress(hash: ByteString): Address =
        Address(Credential.ScriptCredential(hash), Option.None)

    val poolAddress: Address = scriptAddress(poolPolicy)

    def asset(policy: PolicyId, name: ByteString, qty: BigInt): Value = Value(policy, name, qty)

    val poolNft: Value = asset(poolPolicy, poolAssetName, 1)

    def poolValue(lovelace: BigInt): Value = Value.lovelace(lovelace) + poolNft

    val bonded: Data = DaBondPoolDatum.Bonded.toData
    def withdrawing(unlockAt: BigInt): Data = DaBondPoolDatum.Withdrawing(unlockAt).toData

    def poolOutput(datum: Data, lovelace: BigInt): TxOut =
        TxOut(poolAddress, poolValue(lovelace), OutputDatum.OutputDatum(datum), Option.None)

    def poolInput(datum: Data, lovelace: BigInt): TxInInfo =
        TxInInfo(poolRef, poolOutput(datum, lovelace))

    def walletInput(seed: Int): TxInInfo = TxInInfo(
      outref(seed, 0),
      TxOut(
        Address(Credential.PubKeyCredential(owner1), Option.None),
        Value.lovelace(700_000_000),
        OutputDatum.NoOutputDatum,
        Option.None
      )
    )

    /** Aiken `transaction.placeholder`: id is 32 zero bytes, every other field is empty. */
    val placeholderId: TxId = TxId(hash32(0))

    def poolTx(
        inputs: List[TxInInfo],
        referenceInputs: List[TxInInfo],
        outputs: List[TxOut],
        redeemers: List[(ScriptPurpose, Data)],
        signatories: List[PubKeyHash],
        validRange: Interval
    ): TxInfo = TxInfo(
      inputs = inputs,
      referenceInputs = referenceInputs,
      outputs = outputs,
      redeemers = AssocMap.fromList(redeemers),
      signatories = signatories,
      validRange = validRange,
      id = placeholderId
    )

    def withPoolOutput(tx: TxInfo, output: TxOut): TxInfo = tx.copy(outputs = List(output))

    def stakedAt(address: Address, key: PubKeyHash): Address =
        address.copy(stakingCredential =
            Option.Some(StakingCredential.StakingHash(Credential.PubKeyCredential(key)))
        )

    // ------------------------------------------------------------------------
    // mint / InitPool
    // ------------------------------------------------------------------------

    def initOutput(lovelace: BigInt): TxOut = poolOutput(bonded, lovelace)

    val honestInitTx: TxInfo = TxInfo(
      inputs = List(TxInInfo(initRef, walletInput(9).resolved)),
      outputs = List(initOutput(poolFloor)),
      mint = poolNft,
      id = placeholderId
    )

    // ------------------------------------------------------------------------
    // spend / TopUp
    // ------------------------------------------------------------------------

    val topUp: DaBondPoolSpendRedeemer = DaBondPoolSpendRedeemer.TopUp(0)

    def honestTopUpTx(datum: Data, increase: BigInt): TxInfo = poolTx(
      List(poolInput(datum, poolLovelace), walletInput(9)),
      List.Nil,
      List(poolOutput(datum, poolLovelace + increase)),
      List.Nil,
      List.Nil,
      Interval.always
    )

    // ------------------------------------------------------------------------
    // spend / Slash
    // ------------------------------------------------------------------------

    val challengedHeaderHash: ByteString = hash32(0x01)

    val challengeAssetName: ByteString =
        ByteString.fromHex("4441434802020202020202020202020202020202020202020202020202020202")

    /** A hub oracle datum whose only real entries are the state-queue policy and address. */
    val hubDatum: Data = {
        val f = foreignPolicy
        val other = scriptAddress(foreignPolicy)
        HubOracleDatum(
          registeredOperators = f,
          activeOperators = f,
          retiredOperators = f,
          scheduler = f,
          stateQueue = stateQueuePolicy,
          fraudProofCatalogue = f,
          fraudProof = f,
          deposit = f,
          withdrawal = f,
          txOrder = f,
          settlement = f,
          payout = f,
          registeredOperatorsAddr = other,
          activeOperatorsAddr = other,
          retiredOperatorsAddr = other,
          schedulerAddr = other,
          stateQueueAddr = scriptAddress(stateQueuePolicy),
          fraudProofCatalogueAddr = other,
          fraudProofAddr = other,
          depositAddr = other,
          withdrawalAddr = other,
          txOrderAddr = other,
          settlementAddr = other,
          reserveAddr = other,
          payoutAddr = other,
          reserveObserver = f
        ).toData
    }

    def hubRefInput(nftPolicy: PolicyId): TxInInfo = TxInInfo(
      outref(3, 0),
      TxOut(
        scriptAddress(hubPolicy),
        Value.lovelace(2_000_000) + asset(nftPolicy, hubOracleAssetName, 1),
        OutputDatum.OutputDatum(hubDatum),
        Option.None
      )
    )

    val lockIdle: Data = CorrectionLockDatum.Idle.toData

    val lockLocked: Data = CorrectionLockDatum
        .Locked(challengedHeaderHash, CorrectionIdentity.AvailabilityChallenge(challengeAssetName))
        .toData

    def correctionLockInput(datum: Data): TxInInfo = TxInInfo(
      outref(4, 0),
      TxOut(
        scriptAddress(correctionLockScript),
        Value.lovelace(2_000_000) + asset(hubPolicy, correctionLockAssetName, 1),
        OutputDatum.OutputDatum(datum),
        Option.None
      )
    )

    val removeUnavailableRedeemer: Data = StateQueueMintRedeemer
        .RemoveUnavailableBlockAfterTimeout(
          yieldToRefInputIndex = 1,
          unavailableHeaderHash = challengedHeaderHash,
          challengeAssetName = challengeAssetName,
          removalApproach = AttestationTimeoutRemovalApproach.RemoveTimedOutHead(outref(5, 0), 1)
        )
        .toData

    val removeUnattestedRedeemer: Data = StateQueueMintRedeemer
        .RemoveUnattestedBlockAfterTimeout(
          yieldToRefInputIndex = 1,
          timedOutHeaderHash = challengedHeaderHash,
          removalApproach =
              UnattestedTimeoutRemovalApproach.RemoveLastUnattestedBlock(outref(5, 0), 1)
        )
        .toData

    val slash: DaBondPoolSpendRedeemer = DaBondPoolSpendRedeemer.Slash(0, 1, 0, 0)

    def honestSlashTx(datum: Data, inLovelace: BigInt, outLovelace: BigInt): TxInfo = poolTx(
      List(correctionLockInput(lockIdle), poolInput(datum, inLovelace)),
      List(hubRefInput(hubPolicy)),
      List(poolOutput(datum, outLovelace)),
      List(
        (ScriptPurpose.Spending(poolRef), slash.toData),
        (ScriptPurpose.Minting(stateQueuePolicy), removeUnavailableRedeemer)
      ),
      List.Nil,
      Interval.always
    )

    val honestBondedSlashTx: TxInfo = honestSlashTx(bonded, poolLovelace, poolLovelace - daBond)

    // ------------------------------------------------------------------------
    // Owner quorum
    // ------------------------------------------------------------------------

    val daParams: DaParamsDatum = DaParamsDatum(
      committee = committeeKey,
      committeeSignersHash = blake2b_256(committeeKey),
      daThreshold = 1,
      owners = List(owner1, owner2, owner3),
      updateThreshold = 2
    )

    val daParamsRefInput: TxInInfo = TxInInfo(
      outref(6, 0),
      TxOut(
        scriptAddress(daParamsPolicy),
        Value.lovelace(2_000_000) + asset(daParamsPolicy, daParamsAssetName, 1),
        OutputDatum.OutputDatum(daParams.toData),
        Option.None
      )
    )

    val quorum: List[PubKeyHash] = List(owner1, owner3)

    def withSigners(tx: TxInfo, signers: List[PubKeyHash]): TxInfo = tx.copy(signatories = signers)

    def withParamsOutput(tx: TxInfo, output: TxOut): TxInfo =
        tx.copy(referenceInputs = List(daParamsRefInput.copy(resolved = output)))

    def paramsWithoutNft(tx: TxInfo): TxInfo =
        withParamsOutput(tx, daParamsRefInput.resolved.copy(value = Value.lovelace(2_000_000)))

    def paramsAtOtherCredential(tx: TxInfo): TxInfo =
        withParamsOutput(tx, daParamsRefInput.resolved.copy(address = scriptAddress(foreignPolicy)))

    def quorumTx(inDatum: Data, outDatum: Data, outLovelace: BigInt, validRange: Interval): TxInfo =
        poolTx(
          List(poolInput(inDatum, poolLovelace)),
          List(daParamsRefInput),
          List(poolOutput(outDatum, outLovelace)),
          List.Nil,
          quorum,
          validRange
        )

    // ------------------------------------------------------------------------
    // BeginWithdraw / CancelWithdraw / CompleteWithdraw
    // ------------------------------------------------------------------------

    val beginUpper: BigInt = beginLower + maxValidityRangeLength

    /** A validity range shaped as the ledger builds it: inclusive lower bound, exclusive upper
      * bound. `inclusiveUpper` is the last millisecond inside the range. Aiken normalizes it to the
      * same bounds as its `interval.between(lower, inclusiveUpper)` fixtures.
      */
    def ledgerRange(lower: BigInt, inclusiveUpper: BigInt): Interval =
        Interval(
          IntervalBound.finiteInclusive(lower),
          IntervalBound.finiteExclusive(inclusiveUpper + 1)
        )

    val beginRange: Interval = ledgerRange(beginLower, beginUpper)
    val beginUnlockAt: BigInt = beginUpper + daBondWithdrawDelayMs

    val begin: DaBondPoolSpendRedeemer = DaBondPoolSpendRedeemer.BeginWithdraw(0, 0)

    val honestBeginTx: TxInfo =
        quorumTx(bonded, withdrawing(beginUnlockAt), poolLovelace, beginRange)

    val cancelWithdraw: DaBondPoolSpendRedeemer = DaBondPoolSpendRedeemer.CancelWithdraw(0, 0)

    val honestCancelTx: TxInfo =
        quorumTx(withdrawing(withdrawingUnlockAt), bonded, poolLovelace, Interval.always)

    def complete(amount: BigInt): DaBondPoolSpendRedeemer =
        DaBondPoolSpendRedeemer.CompleteWithdraw(amount, 0, 0)

    def rangeFrom(lower: BigInt): Interval = ledgerRange(lower, lower + maxValidityRangeLength)

    def completeTx(inDatum: Data, outDatum: Data, outLovelace: BigInt, lower: BigInt): TxInfo =
        quorumTx(inDatum, outDatum, outLovelace, rangeFrom(lower))

    val honestCompleteTx: TxInfo = completeTx(
      withdrawing(withdrawingUnlockAt),
      bonded,
      poolLovelace - withdrawAmount,
      withdrawingUnlockAt + maxValidityRangeLength
    )

    // ------------------------------------------------------------------------
    // Script contexts
    // ------------------------------------------------------------------------

    def spendContext(redeemer: DaBondPoolSpendRedeemer, tx: TxInfo): ScriptContext =
        ScriptContext(tx, redeemer.toData, ScriptInfo.SpendingScript(poolRef, Option.None))

    def mintContext(tx: TxInfo): ScriptContext = ScriptContext(
      tx,
      DaBondPoolMintRedeemer.InitPool(0).toData,
      ScriptInfo.MintingScript(poolPolicy)
    )
}
