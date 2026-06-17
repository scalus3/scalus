package scalus.examples.anonymousdata

import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*

/** Anonymous on-chain data storage — implemented with **zero on-chain execution**.
  *
  * Specification (rosetta `anonymous_data`): "store data on-chain, associated with a cryptographic
  * hash, in a way that only the user who can generate that hash can retrieve it."
  *
  * On Cardano this needs no validator at all. Every transaction output can carry a **datum hash**
  * ([[scalus.cardano.ledger.DatumOption.Hash]]) — a 32-byte commitment whose preimage is NOT kept
  * on-chain. We commit to `Data.List([B(nonce), data])`:
  *
  *   - The chain stores only `blake2b_256(serialise([nonce, data]))`. Observers see a hash, not the
  *     data, and cannot tell two unrelated entries apart.
  *   - To **retrieve** an entry you reveal the preimage `(nonce, data)`; anyone can recompute the
  *     hash and check it against the on-chain UTxO. Only someone who already knows `(nonce, data)`
  *     can produce a matching preimage.
  *   - The `nonce` is what makes the commitment *hiding*: without it, low-entropy `data` (a vote, a
  *     yes/no flag, a small number) could be brute-forced by hashing every candidate. A fresh
  *     random nonce per entry also makes the same `data` stored twice produce two unlinkable
  *     hashes.
  *
  * This is a commitment scheme expressed in a native ledger feature. It is the whole point of the
  * example: this functionality requires no smart contract — just a datum hash on an ordinary UTxO.
  *
  * Note on anonymity: the storing transaction is still signed by *some* key, so on a public chain a
  * determined observer can link the storer's wallet to the UTxO they created. Hiding *that* link is
  * a different problem (it needs a relayer plus a zero-knowledge membership proof, e.g. a bilinear
  * accumulator or zk-SNARK). What a datum hash gives you, simply and cheaply, is data
  * confidentiality with selective disclosure: the *contents* stay private until their owner chooses
  * to reveal them.
  */
case class AnonymousDataTransactions(env: CardanoInfo) {

    /** The committed preimage for `data` under a secret `nonce`: `Data.List([B(nonce), data])`. */
    def commitment(nonce: ByteString, data: Data): Data =
        Data.List(PList.Cons(Data.B(nonce), PList.Cons(data, PList.Nil)))

    /** The on-chain footprint of an entry: `blake2b_256` of the CBOR-encoded commitment. */
    def commitmentHash(nonce: ByteString, data: Data): DataHash =
        DataHash.fromByteString(commitment(nonce, data).dataHash)

    /** Store `data`: create a UTxO whose datum is committed *by hash only*.
      *
      * The preimage `(nonce, data)` never touches the chain — only its 32-byte hash does. The UTxO
      * sits at `owner`, so only the owner's key can later spend it; the data itself is recoverable
      * only by someone who knows the preimage.
      */
    def store(
        utxos: Utxos,
        data: Data,
        nonce: ByteString,
        ada: Coin,
        owner: Address,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction =
        TxBuilder(env)
            .payTo(owner, Value(ada), commitmentHash(nonce, data))
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction

    /** Retrieve (open) a stored entry off-chain.
      *
      * Given a revealed `(nonce, data)` and the stored UTxO, return the data iff the preimage
      * hashes to the UTxO's committed datum hash. Pure verification — no transaction, no script.
      * Retrieval never needs to touch the chain: the commitment is already there, and revealing the
      * preimage to any verifier proves what was stored.
      */
    def open(storedUtxo: Utxo, nonce: ByteString, data: Data): Option[Data] =
        storedUtxo.output.datumOption match
            case Some(DatumOption.Hash(h)) if h == commitmentHash(nonce, data) => Some(data)
            case _                                                             => None
}
