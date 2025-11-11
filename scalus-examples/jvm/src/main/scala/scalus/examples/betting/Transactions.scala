package scalus.examples.betting

import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.ToData.*
import scalus.cardano.address.Address
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.cardano.blueprint.PlutusV3CompiledContract

class Transactions(
    context: BuilderContext,
    compiledContract: PlutusV3CompiledContract = BettingContract.defaultCompiledContract
):

    val wallet = context.wallet
    val script = compiledContract.script
    val scriptAddress = Address(context.env.network, Credential.ScriptHash(script.scriptHash))

    // utility initial script deployment tx
    def deploy(
        deploymentAddress: Address,
        feeUtxo: Utxo
    ): Either[String, Transaction] = PaymentBuilder(context)
        .withStep(
          TransactionBuilderStep.Spend(
            TransactionUnspentOutput(feeUtxo),
            PubKeyWitness
          )
        )
        .withStep(
          TransactionBuilderStep.Send(
            TransactionOutput(
              deploymentAddress,
              Value.zero,
              None,
              Some(ScriptRef(script))
            )
          )
        )
        .build()

    def init(
        bet: Long, // lovelace to bet by 'player1'
        player1: PubKeyHash,
        oracle: PubKeyHash,
        expiration: PosixTime,
        scriptUtxo: Utxo,
        beforeSlot: Long,
        player2: PubKeyHash = PubKeyHash(hex""), // supposed to be empty
        token: AssetName = AssetName(
          utf8"lucky_number_slevin"
        ), // minted token asset name, shouldn't matter
        amount: Long = 1L // minted token amount, shouldn't matter
    ): Either[String, Transaction] = PaymentBuilder(context)
        .withStep(
          TransactionBuilderStep.ReferenceOutput(TransactionUnspentOutput.apply.tupled(scriptUtxo))
        )
        .withStep(
          TransactionBuilderStep.Mint(
            script.scriptHash,
            token,
            amount,
            TwoArgumentPlutusScriptWitness( // must see a reference to a deployed script
              scriptSource = ScriptSource.PlutusScriptAttached,
              redeemer = Data.unit,
              additionalSigners = Set(ExpectedSigner(AddrKeyHash.fromByteString(player1.hash)))
            )
          )
        )
        .payToScript(
          scriptAddress,
          Value.asset(script.scriptHash, token, amount, Coin(bet)),
          BettingConfig(player1, player2, oracle, expiration).toData
        )
        .withStep( // ???: why test is not fail without valid range step
          TransactionBuilderStep.ValidityEndSlot(context.env.slotConfig.timeToSlot(beforeSlot))
        )
        .collateral
        .tupled(wallet.collateralInputs.head)
        .build()

    def join(
        bet: Long, // lovelace to bet by 'player2'
        player1: PubKeyHash,
        player2: PubKeyHash,
        oracle: PubKeyHash,
        expiration: PosixTime,
        scriptUtxo: Utxo,
        betUtxo: Utxo, // player1's lovelace bet & issued token
        beforeSlot: Long
    ): Either[String, Transaction] =
        val lovelace = Value.lovelace(bet)
        wallet
            .selectInputs(lovelace)
            .get
            .foldLeft(PaymentBuilder(context)):
                case (builder, (utxo, witness)) =>
                    builder.spendOutputs((utxo.input, utxo.output), witness)
            .withStep(
              TransactionBuilderStep.ReferenceOutput(
                TransactionUnspentOutput.apply.tupled(scriptUtxo)
              )
            )
            .payToScript(
              scriptAddress,
              betUtxo._2.value + lovelace,
              BettingConfig(player1, player2, oracle, expiration).toData
            )
            .withStep(
              TransactionBuilderStep.Spend(
                TransactionUnspentOutput(betUtxo),
                ThreeArgumentPlutusScriptWitness(
                  scriptSource = ScriptSource.PlutusScriptAttached,
                  redeemer = Action.Join.toData,
                  datum = Datum.DatumInlined,
                  additionalSigners = Set(ExpectedSigner(AddrKeyHash.fromByteString(player2.hash)))
                )
              )
            )
            .withStep(
              TransactionBuilderStep.ValidityEndSlot(
                context.env.slotConfig.timeToSlot(beforeSlot)
              )
            )
            .collateral
            .tupled(wallet.collateralInputs.head)
            .build()

    def win(
        isJoinWin: Boolean,
        player1: PubKeyHash,
        player2: PubKeyHash,
        oracle: PubKeyHash,
        scriptUtxo: Utxo,
        betUtxo: Utxo, // player2's lovelace bet & issued token
        feeUtxo: Utxo,
        afterSlot: Long
    ): Either[String, Transaction] =
        val payout = if isJoinWin then player2 else player1
        PaymentBuilder(context)
            .withStep(
              TransactionBuilderStep.Spend(
                TransactionUnspentOutput(feeUtxo),
                PubKeyWitness
              )
            )
            .withStep(
              TransactionBuilderStep.ReferenceOutput(
                TransactionUnspentOutput.apply.tupled(scriptUtxo)
              )
            )
            .withStep(
              TransactionBuilderStep.Spend(
                TransactionUnspentOutput(betUtxo),
                ThreeArgumentPlutusScriptWitness(
                  scriptSource = ScriptSource.PlutusScriptAttached,
                  redeemer = Action.AnnounceWinner(payout).toData,
                  datum = Datum.DatumInlined,
                  additionalSigners = Set(ExpectedSigner(AddrKeyHash.fromByteString(oracle.hash)))
                )
              )
            )
            .withStep(
              TransactionBuilderStep.ValidityStartSlot(context.env.slotConfig.timeToSlot(afterSlot))
            )
            .payTo(
              ShelleyAddress(
                network = CardanoInfo.mainnet.network,
                payment = ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(payout.hash)),
                delegation = ShelleyDelegationPart.Null
              ),
              betUtxo._2.value
            )
            .collateral
            .tupled(wallet.collateralInputs.head)
            .build()
