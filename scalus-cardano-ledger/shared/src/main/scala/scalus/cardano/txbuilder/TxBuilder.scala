package scalus.cardano.txbuilder

import com.bloxbean.cardano.client.account.Account
import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import java.time.{Duration, Instant}

case class Context(
    env: Environment,
)

trait Builder {
    def spend(utxo: Utxo): Builder = ???
    def from(address: Address): Builder = ???
    def from(account: Account): Builder = ???
    def withdraw(credential: Credential.KeyHash): Builder = ???
    def withdraw(credential: Credential.ScriptHash): Builder = ???
    def references(utxos: Utxo*): Builder = ???
    def collaterals(utxos: Utxo*): Builder = ???
    def output(address: Address, value: Value): Builder = ???
    def output(address: Address, value: Value, datum: DatumOption): Builder = ???
    def attach(script: Script): Builder = ???
    def attach(data: Data): Builder = ???
    def metadata(auxiliaryData: AuxiliaryData): Builder = ???
    def mint(mint: Mint): Builder = ???
    def minFee(minFee: Coin): Builder = ???
    def validity(interval: ValidityInterval): Builder = ???
    def validFrom(from: Instant): Builder = ???
    def validTo(to: Instant): Builder = ???
    def validFor(duration: Duration): Builder = ???
    def feePayer(address: Address): Builder = ???
    def changeTo(address: Address): Builder = ???
    def build(): Builder = ???
    def sign(): Transaction = ???
}
