package scalus.examples.pricebet

import scalus.builtin.{ByteString, Data, FromData}
import scalus.cardano.address.Address as OffchainAddress
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.uplc.PlutusV3
import scalus.prelude.*

import java.time.Instant

case class PricebetTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    pricebetContract: PlutusV3[Data => Unit],
    oracleContract: PlutusV3[Data => Unit]
) {
    def pricebetScript: Script.PlutusV3 = pricebetContract.script
    val pricebetScriptAddress: OffchainAddress = pricebetContract.address(env.network)

    def oracleScript: Script.PlutusV3 = oracleContract.script
    val oracleScriptAddress: OffchainAddress = oracleContract.address(env.network)

    /** Create initial pricebet UTXO with owner's bet.
      */
    def initiatePricebet(
        ownerUtxos: Utxos,
        betAmount: Coin,
        ownerPkh: AddrKeyHash,
        oracleScriptHash: ByteString,
        deadline: PosixTime,
        exchangeRate: (BigInt, BigInt),
        changeAddress: OffchainAddress,
        signer: TransactionSigner
    ): Transaction = {
        val datum = PricebetState(
          owner = PubKeyHash(ownerPkh),
          oracleScriptHash = oracleScriptHash,
          player = Option.empty,
          deadline = deadline,
          exchangeRate = exchangeRate
        )

        val ownerUtxo = Utxo(ownerUtxos.head)

        TxBuilder(env)
            .spend(ownerUtxo)
            .payTo(pricebetScriptAddress, Value.lovelace(betAmount.value), datum)
            .complete(availableUtxos = ownerUtxos, sponsor = changeAddress)
            .sign(signer)
            .transaction
    }

    /** Player joins by matching the bet amount.
      */
    def join(
        utxos: Utxos,
        pricebetUtxo: Utxo,
        playerPkh: AddrKeyHash,
        sponsor: OffchainAddress,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.Join

        val oldDatum = previousStateInlineDatum[PricebetState](pricebetUtxo)
        val betAmount = pricebetUtxo.output.value.coin.value

        // Construct new datum with player
        val newDatum = oldDatum.copy(player = Option.Some(PubKeyHash(playerPkh)))

        TxBuilder(env, evaluator)
            .spend(pricebetUtxo, redeemer, pricebetScript, Set(playerPkh))
            .payTo(pricebetScriptAddress, Value.lovelace(betAmount * 2), newDatum)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    private def previousStateInlineDatum[A: FromData](pricebetUtxo: Utxo): A =
        pricebetUtxo.output.inlineDatum
            .getOrElse(throw IllegalStateException("Pricebet UTxO must have inline datum"))
            .to[A]

    /** Player claims pot if oracle rate exceeds bet rate.
      */
    def win(
        utxos: Utxos,
        pricebetUtxo: Utxo,
        oracleUtxo: Utxo,
        playerAddress: OffchainAddress,
        sponsor: OffchainAddress,
        validTo: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.Win

        val datum = previousStateInlineDatum[PricebetState](pricebetUtxo)
        val playerPkh = datum.player.get.hash

        TxBuilder(env, evaluator)
            .references(oracleUtxo)
            .spend(pricebetUtxo, redeemer, pricebetScript, Set(AddrKeyHash(playerPkh)))
            .payTo(playerAddress, pricebetUtxo.output.value)
            .validTo(validTo)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Owner reclaims pot after deadline.
      */
    def timeout(
        utxos: Utxos,
        pricebetUtxo: Utxo,
        ownerAddress: OffchainAddress,
        sponsor: OffchainAddress,
        validFrom: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.Timeout

        val datum = previousStateInlineDatum[PricebetState](pricebetUtxo)
        val ownerPkh = datum.owner.hash

        TxBuilder(env, evaluator)
            .spend(pricebetUtxo, redeemer, pricebetScript, Set(AddrKeyHash(ownerPkh)))
            .payTo(ownerAddress, pricebetUtxo.output.value)
            .validFrom(validFrom)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Mints the oracle beacon token and creates the initial oracle UTXO. The beacon is minted
      * using the oracle script as a minting policy.
      *
      * @param seedUtxo
      *   The UTXO that will be spent to authorize the one-time mint
      * @param beaconTokenName
      *   The name for the beacon token
      * @param authorizedSigner
      *   The pubkey hash authorized to update the oracle
      * @param initialTimestamp
      *   Initial timestamp for the oracle
      * @param initialRateNominator
      *   Initial exchange rate numerator
      * @param initialRateDenominator
      *   Initial exchange rate denominator
      * @param sponsor
      *   Address for change
      * @param signer
      *   Transaction signer
      */
    def mintBeaconAndCreateOracle(
        utxos: Utxos,
        seedUtxo: Utxo,
        beaconTokenName: ByteString,
        authorizedSigner: AddrKeyHash,
        initialTimestamp: PosixTime,
        initialRateNominator: BigInt,
        initialRateDenominator: BigInt,
        sponsor: OffchainAddress,
        signer: TransactionSigner
    ): Transaction = {
        // The beacon policy ID is the script hash of the oracle validator (used as minting policy)
        val beaconPolicyId = oracleScriptAddress.scriptHashOption.get

        val mintRedeemer: MintRedeemer = MintRedeemer(
          seedUtxo = scalus.ledger.api.v3.TxOutRef(
            scalus.ledger.api.v3.TxId(seedUtxo._1.transactionId),
            BigInt(seedUtxo._1.index.toLong)
          )
        )

        val datum = OracleState(
          timestamp = initialTimestamp,
          exchangeRateNominator = initialRateNominator,
          exchangeRateDenominator = initialRateDenominator,
          beaconPolicyId = beaconPolicyId,
          beaconTokenName = beaconTokenName,
          authorizedSigner = PubKeyHash(authorizedSigner)
        )

        // Create value with beacon token + min ADA
        val value =
            Value.ada(2) + Value.asset(
              ScriptHash.fromByteString(beaconPolicyId),
              AssetName(beaconTokenName),
              1
            )

        TxBuilder(env, evaluator)
            .spend(seedUtxo)
            .collaterals(seedUtxo)
            .mint(
              ScriptHash.fromByteString(beaconPolicyId),
              Map(AssetName(beaconTokenName) -> 1L),
              TwoArgumentPlutusScriptWitness.attached(oracleScript, mintRedeemer)
            )
            .payTo(oracleScriptAddress, value, datum)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Updates oracle with new exchange rate and timestamp.
      */
    def updateOracle(
        utxos: Utxos,
        oracleUtxo: Utxo,
        newTimestamp: PosixTime,
        newRateNominator: BigInt,
        newRateDenominator: BigInt,
        authorizedSigner: AddrKeyHash,
        sponsor: OffchainAddress,
        validFrom: Instant,
        validTo: Instant,
        oracleSigner: TransactionSigner,
        sponsorSigner: TransactionSigner
    ): Transaction = {
        val oldState = previousStateInlineDatum[OracleState](oracleUtxo)

        val newState = oldState.copy(
          timestamp = newTimestamp,
          exchangeRateNominator = newRateNominator,
          exchangeRateDenominator = newRateDenominator
        )

        TxBuilder(env, evaluator)
            .spend(oracleUtxo, Data.unit, oracleScript, Set(authorizedSigner))
            .payTo(oracleScriptAddress, oracleUtxo.output.value, newState)
            .validFrom(validFrom)
            .validTo(validTo)
            .complete(availableUtxos = utxos, sponsor)
            .sign(oracleSigner)
            .sign(sponsorSigner)
            .transaction
    }
}
