package scalus.regression.vault20251015

import scalus.Compile
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.Validator

/** Minimal reproduction of Vault validator for regression testing
  *
  * This minimal version tests the inheritance via inline override feature where a Validator only
  * implements the `spend` method and the compiler plugin should handle the other methods.
  *
  * Issue: When compiling a validator that extends Validator and only implements `spend`, the
  * compiler adds RuntimeException stubs for unimplemented methods (mint, reward, certify, vote,
  * propose).
  */
@Compile
object VaultMinimal extends Validator {

    case class Datum(
        owner: ByteString,
        amount: BigInt
    ) derives FromData,
          ToData

    enum Redeemer derives FromData, ToData:
        case Deposit
        case Withdraw

    inline override def spend(
        d: scalus.prelude.Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val datum = d.get.to[Datum]
        redeemer.to[Redeemer] match {
            case Redeemer.Deposit  => deposit(tx, ownRef, datum)
            case Redeemer.Withdraw => ()
        }
    }

    def deposit(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        ()
    }
}
