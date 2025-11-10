package scalus.cardano.txbuilder

import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.Provider
import scalus.cardano.wallet.Account

import java.time.Instant

case class Context(
    env: Environment,
)

case class TxBuilder(
    ctx: BuilderContext,
    steps: Seq[TransactionBuilderStep],
    fromAccount: Account,
    feePayer: Account
) extends Builder {
    override def build(): Builder = ???
    override def context: TransactionBuilder.Context = ???
    override def transaction: Transaction = ???
}

trait Builder {
    def spend(utxo: Utxo): Builder = ???
    def from(address: Address): Builder = ???
    def withdraw(credential: Credential.KeyHash): Builder = ???
    def withdraw(credential: Credential.ScriptHash): Builder = ???
    def references(utxos: Utxo*): Builder = ???
    def collaterals(utxos: Utxo*): Builder = ???
    def output(output: TransactionOutput): Builder = ???
    def payTo(address: Address, value: Value): Builder = ???
    def payTo(address: Address, value: Value, datum: Data): Builder = ???
    def payTo(address: Address, value: Value, datumHash: DataHash): Builder = ???
    def attach(script: Script): Builder = ???
    def attach(data: Data): Builder = ???
    def metadata(auxiliaryData: AuxiliaryData): Builder = ???
    def mint(mint: Mint): Builder = ???
    def minFee(minFee: Coin): Builder = ???
    def validDuring(interval: ValidityInterval): Builder = ???
    def validFrom(from: Instant): Builder = ???
    def validTo(to: Instant): Builder = ???
    def diffHandler(handler: (Long, Transaction) => Unit): Builder = ???
    def feePayer(address: Address): Builder = ???
    def changeTo(address: Address): Builder = ???
    def build(): Builder = ???
    def context: TransactionBuilder.Context = ???
    def sign(signers: TxSigner*): Builder = ???
    def transaction: Transaction = ???
    def provider: Provider = ???
}

trait TxSigner
