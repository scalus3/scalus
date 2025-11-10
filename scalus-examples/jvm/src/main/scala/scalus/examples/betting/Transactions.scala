package scalus.examples.betting

import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.*
import scalus.cardano.address.Address
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
    def deploy(deploymentAddress: Address): Either[String, Transaction] =
        wallet
            .selectInputs(
              Value.lovelace(32945265L)
            ) // FIXME: balanceFeeAndChange - script deployment fee
            .get
            .foldLeft(PaymentBuilder(context)):
                case (builder, (utxo, witness)) =>
                    builder.spendOutputs(Utxo(utxo.input, utxo.output), witness)
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
            .collateral
            .tupled(wallet.collateralInputs.head)
            .build()

    def init(
        bet: Long, // lovelace to bet by 'player1'
        player1: PubKeyHash,
        oracle: PubKeyHash,
        expiration: PosixTime,
        scriptUtxo: Utxo,
        player2: PubKeyHash = PubKeyHash(hex""), // supposed to be empty
        token: AssetName = AssetName(
          utf8"lucky_number_slevin"
        ), // minted token asset name, shouldn't matter
        amount: Long = 1L // minted token amount, shouldn't matter
    ): Either[String, Transaction] = PaymentBuilder(context)
        .withStep(
          TransactionBuilderStep.ReferenceOutput(scriptUtxo)
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
          Config(player1, player2, oracle, expiration).toData
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
        betUtxo: Utxo // player1's lovelace bet & issued token
    ): Either[String, Transaction] =
        val lovelace = Value.lovelace(bet)
        wallet
            .selectInputs(lovelace)
            .get
            .foldLeft(PaymentBuilder(context)):
                case (builder, (utxo, witness)) =>
                    builder.spendOutputs(utxo, witness)
            .withStep(
              TransactionBuilderStep.Spend(
                betUtxo,
                ThreeArgumentPlutusScriptWitness(
                  scriptSource = ScriptSource.PlutusScriptValue(script),
                  redeemer = Action.Join.toData,
                  datum = Datum.DatumInlined,
                  additionalSigners = Set(ExpectedSigner(AddrKeyHash.fromByteString(player2.hash)))
                )
              )
            )
            .payToScript(
              scriptAddress,
              betUtxo._2.value + lovelace,
              Config(player1, player2, oracle, expiration).toData
            )
            .collateral
            .tupled(wallet.collateralInputs.head)
            .build()

    def win(
        isJoinWin: Boolean,
        player1: PubKeyHash,
        player2: PubKeyHash,
        oracle: PubKeyHash,
        betUtxo: Utxo // player2's lovelace bet & issued token
    ): Either[String, Transaction] =
        val payout = if isJoinWin then player2 else player1
        PaymentBuilder(context)
            .withStep(
              TransactionBuilderStep.Spend(
                betUtxo,
                ThreeArgumentPlutusScriptWitness(
                  scriptSource = ScriptSource.PlutusScriptValue(script),
                  redeemer = Action.AnnounceWinner(payout).toData,
                  datum = Datum.DatumInlined,
                  additionalSigners = Set(ExpectedSigner(AddrKeyHash.fromByteString(oracle.hash)))
                )
              )
            )
            .payTo(Address.fromByteString(payout.hash), betUtxo._2.value)
            .collateral
            .tupled(wallet.collateralInputs.head)
            .build()
