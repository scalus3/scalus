use aiken/builtin

pub type VerificationKey =
  ByteArray

pub type VerificationKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type Script =
  ByteArray

pub type ScriptHash =
  Hash<Blake2b_224, Script>

pub type Signature =
  ByteArray

pub type DataHash =
  Hash<Blake2b_256, Data>

/// A `Hash` is nothing more than a `ByteArray`, but it carries extra
/// information for readability.
///
/// On-chain, any hash digest value is represented as a plain 'ByteArray'.
/// Though in practice, hashes come from different sources and have
/// different semantics.
///
/// Hence, while this type-alias doesn't provide any strong type-guarantees,
/// it helps writing functions signatures with more meaningful types than mere
/// 'ByteArray'.
///
/// Compare for example:
///
/// ```aiken
/// pub type Credential {
///   VerificationKey(ByteArray)
///   Script(ByteArray)
/// }
/// ```
///
/// with
///
/// ```aiken
/// pub type Credential {
///   VerificationKey(Hash<Blake2b_224, VerificationKey>)
///   Script(Hash<Blake2b_224, Script>)
/// }
/// ```
///
/// Both are strictly equivalent, but the second reads much better.
pub type Hash<alg, a> =
  ByteArray

// ## Hashing

/// A blake2b-224 hash algorithm.
///
/// Typically used for:
///
/// - [`Credential`](../cardano/address.html#Credential)
/// - [`PolicyId`](../cardano/assets.html#PolicyId)
///
/// Note: there's no function to calculate blake2b-224 hash digests on-chain.
pub opaque type Blake2b_224 {
  Blake2b_224
}

/// Compute the blake2b-224 hash digest (28 bytes) of some data.
pub fn blake2b_224(bytes: ByteArray) -> Hash<Blake2b_224, a> {
  builtin.blake2b_224(bytes)
}

/// A blake2b-256 hash algorithm.
///
/// Typically used for:
///
/// - [`TransactionId`](../cardano/transaction.html#TransactionId)
pub opaque type Blake2b_256 {
  Blake2b_256
}

/// Compute the blake2b-256 hash digest (32 bytes) of some data.
pub fn blake2b_256(bytes: ByteArray) -> Hash<Blake2b_256, a> {
  builtin.blake2b_256(bytes)
}

/// A Keccak-256 hash algorithm.
pub opaque type Keccak_256 {
  Keccak_256
}

/// Compute the keccak-256 hash digest (32 bytes) of some data.
pub fn keccak_256(bytes: ByteArray) -> Hash<Keccak_256, a> {
  builtin.keccak_256(bytes)
}

/// A SHA2-256 hash algorithm.
pub opaque type Sha2_256 {
  Sha2_256
}

/// Compute the sha2-256 hash digest (32 bytes) of some data.
pub fn sha2_256(bytes: ByteArray) -> Hash<Sha2_256, a> {
  builtin.sha2_256(bytes)
}

/// A SHA3-256 hash algorithm.
pub opaque type Sha3_256 {
  Sha3_256
}

/// Compute the sha3-256 hash digest (32 bytes) of some data.
pub fn sha3_256(bytes: ByteArray) -> Hash<Sha3_256, a> {
  builtin.sha3_256(bytes)
}

// ## Verifying signatures

/// Verify an ECDCA signature (over secp256k1) using the given verification key.
/// Returns `True` when the signature is valid.
pub fn verify_ecdsa_signature(
  key: VerificationKey,
  msg: ByteArray,
  sig: Signature,
) -> Bool {
  builtin.verify_ecdsa_secp256k1_signature(key, msg, sig)
}

/// Verify an Ed25519 signature using the given verification key.
/// Returns `True` when the signature is valid.
pub fn verify_ed25519_signature(
  key: VerificationKey,
  msg: ByteArray,
  sig: Signature,
) -> Bool {
  builtin.verify_ed25519_signature(key, msg, sig)
}

/// Verify a Schnorr signature (over secp256k1) using the given verification key.
/// Returns `True` when the signature is valid.
pub fn verify_schnorr_signature(
  key: VerificationKey,
  msg: ByteArray,
  sig: Signature,
) -> Bool {
  builtin.verify_schnorr_secp256k1_signature(key, msg, sig)
}
