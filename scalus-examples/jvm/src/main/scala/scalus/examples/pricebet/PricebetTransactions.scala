package scalus.examples.pricebet

import scalus.uplc.builtin.{ByteString, Data, FromData}
import scalus.cardano.address.Address as OffchainAddress
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.onchain.plutus.v1.{PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.v3.TxOutRef
import scalus.cardano.onchain.plutus.prelude.*

import java.time.Instant

/** Transaction creator for Pricebet and Oracle contracts.
  *
  * The oracle contract is parameterized with OracleConfig which includes the seed UTXO, beacon
  * token info, and authorized signer. This config gets baked into the script hash, making each
  * oracle instance unique.
  *
  * @param env
  *   Cardano environment info
  * @param evaluator
  *   Plutus script evaluator
  * @param oracleConfig
  *   Configuration for the oracle (seed UTXO, beacon info, authorized signer)
  * @param pricebetConfig
  *   Configuration for the pricebet (oracle script hash)
  */
case class PricebetTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    oracleConfig: OracleConfig,
    pricebetConfig: PricebetConfig
) {
    // Build the parameterized scripts
    private val oracleContract = OracleContract(oracleConfig)
    private val pricebetContract = PriceBetContract(pricebetConfig)

    val oracleScript: Script.PlutusV3 = oracleContract.script
    val oracleScriptAddress: OffchainAddress = oracleContract.address(env.network)
    val beaconPolicyId: ByteString = oracleScriptAddress.scriptHashOption.get

    val pricebetScript: Script.PlutusV3 = pricebetContract.script
    val pricebetScriptAddress: OffchainAddress = pricebetContract.address(env.network)

    /** Mints the oracle beacon token and creates the initial oracle UTXO.
      *
      * The seed UTXO from the config will be spent to authorize the one-time mint.
      */
    def mintBeaconAndCreateOracle(
        utxos: Utxos,
        initialTimestamp: PosixTime,
        initialExchangeRate: Rational,
        sponsor: OffchainAddress,
        signer: TransactionSigner
    ): Transaction = {
        val mintRedeemer = MintOracleRedeemer.Mint

        val datum = OracleState(
          timestamp = initialTimestamp,
          exchangeRate = initialExchangeRate
        )

        // Create value with beacon token + min ADA
        val value =
            Value.ada(2) + Value.asset(
              ScriptHash.fromByteString(beaconPolicyId),
              AssetName(oracleConfig.beaconName),
              1
            )

        // Find the seed UTXO in the available UTXOs
        val seedUtxo = utxos
            .find { case (input, _) =>
                input.transactionId == oracleConfig.seedUtxo.id.hash &&
                input.index == oracleConfig.seedUtxo.idx.toInt
            }
            .map(Utxo(_))
            .getOrElse(throw IllegalStateException("Seed UTXO not found in available UTXOs"))

        TxBuilder(env, evaluator)
            .spend(seedUtxo)
            .collaterals(seedUtxo)
            .mint(
              ScriptHash.fromByteString(beaconPolicyId),
              Map(AssetName(oracleConfig.beaconName) -> 1L),
              TwoArgumentPlutusScriptWitness.attached(
                oracleScript,
                mintRedeemer,
                Set(AddrKeyHash(oracleConfig.authorizedSigner.hash))
              )
            )
            .payTo(oracleScriptAddress, value, datum)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Burns the oracle beacon token by spending the oracle UTXO. Both the spend validator (with
      * Burn redeemer) and mint validator (with Burn redeemer) run.
      */
    def burnBeacon(
        utxos: Utxos,
        oracleUtxo: Utxo,
        sponsor: OffchainAddress,
        signer: TransactionSigner
    ): Transaction = {
        val spendRedeemer = SpendOracleRedeemer.Burn
        val mintRedeemer = MintOracleRedeemer.Burn

        // Find a collateral UTXO from available UTXOs
        val collateralUtxo = Utxo(utxos.head)

        TxBuilder(env, evaluator)
            .spend(
              oracleUtxo,
              spendRedeemer,
              oracleScript,
              Set(AddrKeyHash(oracleConfig.authorizedSigner.hash))
            )
            .collaterals(collateralUtxo)
            .mint(
              ScriptHash.fromByteString(beaconPolicyId),
              Map(AssetName(oracleConfig.beaconName) -> -1L),
              TwoArgumentPlutusScriptWitness.attached(
                oracleScript,
                mintRedeemer,
                Set(AddrKeyHash(oracleConfig.authorizedSigner.hash))
              )
            )
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Updates oracle with new exchange rate and timestamp. */
    def updateOracle(
        utxos: Utxos,
        oracleUtxo: Utxo,
        newTimestamp: PosixTime,
        newExchangeRate: Rational,
        sponsor: OffchainAddress,
        validFrom: Instant,
        validTo: Instant,
        oracleSigner: TransactionSigner,
        sponsorSigner: TransactionSigner
    ): Transaction = {
        val newState = OracleState(
          timestamp = newTimestamp,
          exchangeRate = newExchangeRate
        )

        // The redeemer needs the output index where the oracle continuation will be
        // Since we're building the tx, we know it will be output index 0 (first payTo)
        val redeemer = SpendOracleRedeemer.Update(oracleUtxoIndex = BigInt(0))

        TxBuilder(env, evaluator)
            .spend(
              oracleUtxo,
              redeemer,
              oracleScript,
              Set(AddrKeyHash(oracleConfig.authorizedSigner.hash))
            )
            .payTo(oracleScriptAddress, oracleUtxo.output.value, newState)
            .validFrom(validFrom)
            .validTo(validTo)
            .complete(availableUtxos = utxos, sponsor)
            .sign(oracleSigner)
            .sign(sponsorSigner)
            .transaction
    }

    /** Create initial pricebet UTXO with owner's bet. */
    def initiatePricebet(
        ownerUtxos: Utxos,
        betAmount: Coin,
        ownerPkh: AddrKeyHash,
        deadline: PosixTime,
        exchangeRate: Rational,
        changeAddress: OffchainAddress,
        signer: TransactionSigner
    ): Transaction = {
        val datum = PricebetState(
          owner = PubKeyHash(ownerPkh),
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

    /** Player joins by matching the bet amount. */
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

    /** Player claims pot if oracle rate exceeds bet rate. */
    def win(
        utxos: Utxos,
        pricebetUtxo: Utxo,
        oracleUtxo: Utxo,
        playerAddress: OffchainAddress,
        sponsor: OffchainAddress,
        validFrom: Instant,
        validTo: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val datum = previousStateInlineDatum[PricebetState](pricebetUtxo)
        val playerPkh = datum.player.get.hash

        // The oracle will be the first (and only) reference input, so index 0
        val redeemer = Action.Win(oracleOut = BigInt(0))

        TxBuilder(env, evaluator)
            .references(oracleUtxo)
            .spend(pricebetUtxo, redeemer, pricebetScript, Set(AddrKeyHash(playerPkh)))
            .payTo(playerAddress, pricebetUtxo.output.value)
            .validFrom(validFrom)
            .validTo(validTo)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Owner reclaims pot after deadline. */
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

    private def previousStateInlineDatum[A: FromData](utxo: Utxo): A =
        utxo.output.inlineDatum
            .getOrElse(throw IllegalStateException("UTxO must have inline datum"))
            .to[A]
}
