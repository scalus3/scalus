package scalus.examples.htlc

import scalus.cardano.address.Address
import scalus.cardano.blueprint.PlutusV3CompiledContract
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}

import scala.collection.Map

class Transactions2(
    env: Environment,
    signers: Map[Address, TransactionSigner],
    compiledContract: PlutusV3CompiledContract = HtlcContract.defaultCompiledContract
) {

    val script = compiledContract.script
    val scriptAddress = Address(env.network, Credential.ScriptHash(script.scriptHash))

    def lock(
        utxo: Utxo,
        value: Value,
        committer: PubKeyHash,
        receiver: PubKeyHash,
        image: Image,
        timeout: PosixTime
    ): Either[Throwable, Transaction] = {
        val datum = Config(committer, receiver, image, timeout)
        val senderAddress = utxo.output.address

        for {
            signer <- signers
                .get(senderAddress)
                .toRight(
                  new RuntimeException(
                    s"Could not sign the lock transaction. Utxo address: ${senderAddress.toHex}"
                  )
                )

            signedBuilder <- TxBuilder(env)
                .spend(utxo)
                .payTo(scriptAddress, value, datum)
                .changeTo(senderAddress)
                .build()
                .sign(signer)
        } yield signedBuilder.transaction
    }

    def reveal(
        lockedUtxo: Utxo,
        collateralUtxo: Utxo,
        preimage: Preimage,
        recipientAddress: Address,
        receiverPkh: PubKeyHash,
        time: PosixTime
    ): Either[Throwable, Transaction] = {
        val redeemer = Action.Reveal(preimage)
        val receiverKeyHash = AddrKeyHash.fromByteString(receiverPkh.hash)

        for {
            receiverSigner <- signers
                .get(recipientAddress)
                .toRight(
                  new RuntimeException(
                    s"Could not sign the reveal transaction. Receiver address: ${recipientAddress.toHex}"
                  )
                )

            signedBuilder <- TxBuilder(env)
                .collaterals(collateralUtxo)
                .spend(lockedUtxo, redeemer, script, Set(receiverKeyHash))
                .payTo(recipientAddress, lockedUtxo.output.value)
                .validFrom(java.time.Instant.ofEpochMilli(time.toLong))
                .changeTo(recipientAddress)
                .build()
                .sign(receiverSigner)
        } yield signedBuilder.transaction
    }
}
