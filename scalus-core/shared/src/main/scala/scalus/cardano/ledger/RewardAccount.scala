package scalus.cardano.ledger

import io.bullet.borer.*
import scalus.cardano.address.{Address, StakeAddress, StakePayload}
import scala.util.control.NonFatal

import scala.math.Ordered.orderingToOrdered

/** Represents a reward account in the Cardano blockchain.
  *
  * Reward accounts (also known as stake addresses) are used to receive staking rewards. They have a
  * specific format with bits 7-5 set to 111 and bit 4 indicating whether the credential is a key
  * hash or script hash.
  *
  * @param address
  *   The address of the reward account
  */
case class RewardAccount(address: StakeAddress) {
    def keyHashOption: Option[AddrKeyHash | StakeKeyHash] = address.keyHashOption
    def scriptHashOption: Option[ScriptHash] = address.scriptHashOption
}

object RewardAccount {

    /** Ordering matches the ledger's derived `Ord AccountAddress`, which is `(Network, AccountId)`
      * where `AccountId` wraps a `Credential`
      * (`libs/cardano-ledger-core/src/Cardano/Ledger/Address.hs:183-190`) and `Ord (Credential kr)`
      * is derived on `ScriptHashObj | KeyHashObj` (`Credential.hs:98-101`), so **script credentials
      * sort before key credentials**.
      *
      * This matters beyond serialisation: the ledger resolves a `Withdrawing`/`Rewarding` redeemer
      * index with `Map.elemAt` over `Map AccountAddress Coin` (`Conway/TxBody.hs:672-673`), so this
      * ordering decides which withdrawal an index names. Comparing only `(network, hash)` and
      * ignoring the constructor produced an order no node emits, and made us assign redeemer
      * indices the node resolves to a different withdrawal.
      */
    given Ordering[RewardAccount] with
        private def credentialTag(p: StakePayload): Int = p match
            case StakePayload.Script(_) => 0 // ScriptHashObj
            case StakePayload.Stake(_)  => 1 // KeyHashObj

        def compare(x: RewardAccount, y: RewardAccount): Int =
            (x.address, y.address) match
                case (StakeAddress(n1, p1), StakeAddress(n2, p2)) =>
                    n1.compare(n2) match
                        case 0 =>
                            credentialTag(p1).compare(credentialTag(p2)) match
                                case 0 => p1.asHash.compare(p2.asHash)
                                case c => c
                        case c => c

    given Encoder[RewardAccount] with
        def write(w: Writer, value: RewardAccount): Writer = {
            w.write(value.address: Address)
            w
        }

    /** CBOR decoder for Address */
    given Decoder[RewardAccount] with
        def read(r: Reader): RewardAccount = {
            try RewardAccount(r.read[Address]().asInstanceOf[StakeAddress])
            catch case NonFatal(exception) => r.validationFailure(exception.getMessage)
        }
}
