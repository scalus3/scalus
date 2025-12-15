package scalus.crypto.ed25519

import scalus.builtin.ByteString

/** Ed25519 verification key (public key) - 32 bytes. Subtype of ByteString for seamless interop.
  */
opaque type VerificationKey <: ByteString = ByteString

object VerificationKey:
    def fromByteString(bs: ByteString): Either[String, VerificationKey] =
        if bs.size == 32 then Right(bs) else Left(s"Invalid size: ${bs.size}, expected 32")

    def unsafeFromByteString(bs: ByteString): VerificationKey =
        require(bs.size == 32, s"Invalid verification key size: ${bs.size}, expected 32")
        bs

    def fromArray(bytes: Array[Byte]): Either[String, VerificationKey] =
        fromByteString(ByteString.fromArray(bytes))

    def unsafeFromArray(bytes: Array[Byte]): VerificationKey =
        unsafeFromByteString(ByteString.fromArray(bytes))

/** Ed25519 signature - 64 bytes. */
opaque type Signature <: ByteString = ByteString

object Signature:
    def fromByteString(bs: ByteString): Either[String, Signature] =
        if bs.size == 64 then Right(bs) else Left(s"Invalid size: ${bs.size}, expected 64")

    def unsafeFromByteString(bs: ByteString): Signature =
        require(bs.size == 64, s"Invalid signature size: ${bs.size}, expected 64")
        bs

    def fromArray(bytes: Array[Byte]): Either[String, Signature] =
        fromByteString(ByteString.fromArray(bytes))

    def unsafeFromArray(bytes: Array[Byte]): Signature =
        unsafeFromByteString(ByteString.fromArray(bytes))

/** Standard Ed25519 signing key - 32 bytes. */
opaque type SigningKey <: ByteString = ByteString

object SigningKey:
    def fromByteString(bs: ByteString): Either[String, SigningKey] =
        if bs.size == 32 then Right(bs) else Left(s"Invalid size: ${bs.size}, expected 32")

    def unsafeFromByteString(bs: ByteString): SigningKey =
        require(bs.size == 32, s"Invalid signing key size: ${bs.size}, expected 32")
        bs

    def fromArray(bytes: Array[Byte]): Either[String, SigningKey] =
        fromByteString(ByteString.fromArray(bytes))

    def unsafeFromArray(bytes: Array[Byte]): SigningKey =
        unsafeFromByteString(ByteString.fromArray(bytes))

/** Extended Ed25519 signing key (SLIP-001/HD wallets) - 64 bytes. */
opaque type ExtendedSigningKey <: ByteString = ByteString

object ExtendedSigningKey:
    def fromByteString(bs: ByteString): Either[String, ExtendedSigningKey] =
        if bs.size == 64 then Right(bs) else Left(s"Invalid size: ${bs.size}, expected 64")

    def unsafeFromByteString(bs: ByteString): ExtendedSigningKey =
        require(bs.size == 64, s"Invalid extended signing key size: ${bs.size}, expected 64")
        bs

    def fromArray(bytes: Array[Byte]): Either[String, ExtendedSigningKey] =
        fromByteString(ByteString.fromArray(bytes))

    def unsafeFromArray(bytes: Array[Byte]): ExtendedSigningKey =
        unsafeFromByteString(ByteString.fromArray(bytes))

    extension (esk: ExtendedSigningKey)
        /** Extract the first 32 bytes as a standard signing key. */
        def standardKey: SigningKey = SigningKey.unsafeFromByteString(esk.take(32))
