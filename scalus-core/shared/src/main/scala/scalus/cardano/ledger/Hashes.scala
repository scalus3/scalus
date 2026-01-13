package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder}
import org.typelevel.paiges.Doc
import scalus.builtin.ByteString
import scalus.utils.{Pretty, Style}

trait Blake2b_256
trait Blake2b_224

case class HashSize[HF](size: Int)

object HashSize {
    given HashSize[Blake2b_224] = HashSize(28)
    given HashSize[Blake2b_256] = HashSize(32)
}

object HashPurpose {
    trait KeyHash
    trait ScriptHash
    trait DataHash
    trait ScriptDataHash
    trait MetadataHash
    trait AuxiliaryDataHash
    trait VrfKeyHash
    trait PoolKeyHash
    trait StakeKeyHash
    trait TransactionHash
    trait BlockHash
}

opaque type Hash[+HashFunction, +Purpose] <: ByteString = ByteString
object Hash {

    def apply[HF: HashSize, Purpose](bytes: ByteString): Hash[HF, Purpose] = {
        val size = summon[HashSize[HF]].size
        require(bytes.size == size, s"Hash must be $size bytes, got ${bytes.size}")
        bytes
    }

    def stakeKeyHash(bytes: ByteString): StakeKeyHash = Hash(bytes)
    def scriptHash(bytes: ByteString): ScriptHash = Hash(bytes)

    given Encoder[HF, Purpose]: Encoder[Hash[HF, Purpose]] = { (w, hash) =>
        // here we explicitly pass the ByteString encoder to avoid StackOverflowError
        // because here Encoder[Hash[]] is resolved as Encoder[ByteString]
        w.write[ByteString](hash)(using ByteString.given_Encoder_ByteString)
    }

    given Decoder[HF: HashSize, Purpose]: Decoder[Hash[HF, Purpose]] = { r =>
        // here we explicitly pass the ByteString decoder to avoid StackOverflowError
        // because here Decoder[Hash[]] is resolved as Decoder[ByteString]
        val bytes = r.read[ByteString]()(using ByteString.given_Decoder_ByteString)
        Hash[HF, Purpose](bytes)
    }

    given Ordering[HF, Purpose]: Ordering[Hash[HF, Purpose]] =
        (x: Hash[HF, Purpose], y: Hash[HF, Purpose]) =>
            ByteString.given_Ordering_ByteString.compare(x, y)

    /** Pretty prints a hash as its full hex string */
    given prettyHash[HF, Purpose]: Pretty[Hash[HF, Purpose]] with
        def pretty(a: Hash[HF, Purpose], style: Style): Doc = Doc.text(a.toHex)
}

trait HashConstructors[HashType >: Hash[HashFunction, Purpose], HashFunction: HashSize, Purpose] {

    type THashType = HashType
    type THashFunction = HashFunction
    type THashPurpose = Purpose

    def fromByteString(bytes: ByteString): HashType =
        Hash[HashFunction, Purpose](bytes)
    def fromHex(hex: String): HashType = fromByteString(ByteString.fromHex(hex))
    def fromArray(bytes: Array[Byte]): HashType = fromByteString(ByteString.fromArray(bytes))

}

type AnyHash = Hash[Any, Any]

/** Represents a key hash used in addresses in the Cardano blockchain.
  *
  * An AddrKeyHash is a 28-byte hash value that identifies a public key and is used in constructing
  * addresses.
  *
  * @param hash
  *   The 28-byte hash value
  */
type AddrKeyHash = Hash[Blake2b_224, HashPurpose.KeyHash]
object AddrKeyHash extends HashConstructors[AddrKeyHash, Blake2b_224, HashPurpose.KeyHash] {

    def apply(bytes: ByteString): AddrKeyHash = {
        require(bytes.size == 28, s"AddrKeyHash must be 28 bytes, got ${bytes.size}")
        Hash[Blake2b_224, HashPurpose.KeyHash](bytes)
    }

}

/** Represents a script hash in Cardano
  *
  * To compute a script hash, a tag must be prepended to the script bytes before hashing. The tag is
  * determined by the script language:
  *   - "\x00" for multisig scripts
  *   - "\x01" for Plutus V1 scripts
  *   - "\x02" for Plutus V2 scripts
  *   - "\x03" for Plutus V3 scripts
  */
type ScriptHash = Hash[Blake2b_224, HashPurpose.ScriptHash]
object ScriptHash extends HashConstructors[ScriptHash, Blake2b_224, HashPurpose.ScriptHash]

type DataHash = Hash[Blake2b_256, HashPurpose.DataHash]
object DataHash extends HashConstructors[DataHash, Blake2b_256, HashPurpose.DataHash]

type MetadataHash = Hash[Blake2b_256, HashPurpose.MetadataHash]
object MetadataHash extends HashConstructors[MetadataHash, Blake2b_256, HashPurpose.MetadataHash]

type BlockHash = Hash[Blake2b_256, HashPurpose.BlockHash]

object BlockHash extends HashConstructors[BlockHash, Blake2b_256, HashPurpose.BlockHash]

type AuxiliaryDataHash = Hash[Blake2b_256, HashPurpose.AuxiliaryDataHash]

object AuxiliaryDataHash
    extends HashConstructors[AuxiliaryDataHash, Blake2b_256, HashPurpose.AuxiliaryDataHash]

type VrfKeyHash = Hash[Blake2b_256, HashPurpose.VrfKeyHash]

object VrfKeyHash extends HashConstructors[VrfKeyHash, Blake2b_256, HashPurpose.VrfKeyHash]

type PoolKeyHash = Hash[Blake2b_224, HashPurpose.PoolKeyHash]

object PoolKeyHash extends HashConstructors[PoolKeyHash, Blake2b_224, HashPurpose.PoolKeyHash]

type StakeKeyHash = Hash[Blake2b_224, HashPurpose.StakeKeyHash]

object StakeKeyHash extends HashConstructors[StakeKeyHash, Blake2b_224, HashPurpose.StakeKeyHash]

type TransactionHash = Hash[Blake2b_256, HashPurpose.TransactionHash]

object TransactionHash
    extends HashConstructors[TransactionHash, Blake2b_256, HashPurpose.TransactionHash]

/** Represents a hash of the script data
  *
  * This hash includes the redeemers, datums, and language views from the transaction. It's used to
  * ensure the script's execution environment is consistent with what was intended.
  *
  * This is simply a type alias for Hash32 to provide better type safety and readability.
  */
type ScriptDataHash = Hash[Blake2b_256, HashPurpose.ScriptDataHash]

object ScriptDataHash
    extends HashConstructors[ScriptDataHash, Blake2b_256, HashPurpose.ScriptDataHash]

/** Represents a 28-byte Blake2b_224 hash value used in Cardano
  *
  * Hash28 is commonly used for address key hashes and script hashes
  */
type Hash28 = Hash[Blake2b_224, Any]

object Hash28 extends HashConstructors[Hash28, Blake2b_224, Any] {

    def apply(bytes: ByteString): Hash28 = {
        Hash[Blake2b_224, Any](bytes)
    }

}

type Hash32 = Hash[Blake2b_256, Any]

object Hash32 extends HashConstructors[Hash32, Blake2b_256, Any]

type PolicyHash = ScriptHash

type PolicyId = ScriptHash
