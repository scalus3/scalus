package scalus.examples.betting

import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.ToData.*
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

    def init(
        bet: Long, // lovelace to bet by 'player1'
        player1: PubKeyHash,
        oracle: PubKeyHash,
        expiration: PosixTime,
        player2: PubKeyHash = PubKeyHash(hex""), // supposed to be empty
        token: AssetName = AssetName(
          utf8"lucky_number_slevin"
        ), // minted token asset name, shouldn't matter
        amount: Long = 1L // minted token amount, shouldn't matter
    ): Either[String, Transaction] = PaymentBuilder(context)
        .withStep(
          TransactionBuilderStep.Mint(
            script.scriptHash,
            token,
            amount,
            TwoArgumentPlutusScriptWitness(
              scriptSource = ScriptSource.PlutusScriptValue(script),
              redeemer = Data.unit,
              additionalSigners = Set(ExpectedSigner(AddrKeyHash.fromByteString(player1.hash)))
            )
          )
        )
        .payToScript(
          scriptAddress,
          Value.asset(script.scriptHash, token, amount, Coin(bet)),
          BetDatum(player1, player2, oracle, expiration).toData
        )
        .build()

    def join(
        bet: Long, // lovelace to bet by 'player2'
        player1: PubKeyHash,
        player2: PubKeyHash,
        oracle: PubKeyHash,
        expiration: PosixTime,
        tokenUtxo: (TransactionInput, TransactionOutput) // token issued by an initial bet
    ): Either[String, Transaction] = wallet
        .selectInputs(Value.lovelace(bet))
        .get
        .foldLeft(PaymentBuilder(context)):
            case (builder, (utxo, witness)) =>
                builder.spendOutputs((utxo.input, utxo.output), witness)
        .withStep(
          TransactionBuilderStep.Spend(
            TransactionUnspentOutput(tokenUtxo),
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
          tokenUtxo._2.value + Value.lovelace(bet),
          BetDatum(player1, player2, oracle, expiration).toData
        )
        .build()

    // winner: true - 'player2', false - 'player1'
    def win(
        winner: Boolean,
        player1: PubKeyHash,
        player2: PubKeyHash,
        oracle: PubKeyHash,
        expiration: PosixTime
    ): Either[String, Transaction] = ???
