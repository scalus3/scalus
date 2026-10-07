package scalus.cardano.ledger

import io.bullet.borer.{Cbor, Decoder, Encoder, Reader, Writer}

/** The state of one registered stake account, as `ConwayAccountState` of the Haskell ledger
  * (`Cardano.Ledger.Conway.State.Account`).
  *
  * @param balance
  *   the reward balance of the account
  * @param deposit
  *   the deposit paid when the account was registered
  * @param stakePoolDelegation
  *   the pool the account delegates its stake to
  * @param dRepDelegation
  *   the DRep the account delegates its vote to
  */
case class ConwayAccountState(
    balance: Coin,
    deposit: Coin,
    stakePoolDelegation: Option[PoolKeyHash],
    dRepDelegation: Option[DRep]
) {

    /** Encodes the account as the Haskell ledger state does: `[balance, deposit, pool / null, drep
      * / null]`.
      */
    def toCbor: Array[Byte] = Cbor.encode(this).toByteArray
}

object ConwayAccountState {

    /** Decodes an account encoded as `[balance, deposit, pool / null, drep / null]`. */
    def fromCbor(cbor: Array[Byte]): ConwayAccountState =
        Cbor.decode(cbor).to[ConwayAccountState].value

    given Decoder[ConwayAccountState] with
        def read(r: Reader): ConwayAccountState =
            r.readArrayHeader(4)
            val balance = r.read[Coin]()
            val deposit = r.read[Coin]()
            // StrictMaybe is encoded as null (SNothing) or the value (SJust)
            val stakePoolDelegation =
                if r.tryReadNull() then None else Some(r.read[PoolKeyHash]())
            val dRepDelegation = if r.tryReadNull() then None else Some(r.read[DRep]())
            ConwayAccountState(balance, deposit, stakePoolDelegation, dRepDelegation)

    given Encoder[ConwayAccountState] with
        def write(w: Writer, value: ConwayAccountState): Writer =
            w.writeArrayHeader(4)
            w.write(value.balance)
            w.write(value.deposit)
            value.stakePoolDelegation match
                case None    => w.writeNull()
                case Some(v) => w.write(v)
            value.dRepDelegation match
                case None    => w.writeNull()
                case Some(v) => w.write(v)
}
