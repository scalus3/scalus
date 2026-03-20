use aiken/crypto.{
  Blake2b_224, Hash, Script, ScriptHash, VerificationKey, VerificationKeyHash,
}

/// A general structure for representing an on-chain `Credential`.
///
/// Credentials are always one of two kinds: a direct public/private key
/// pair, or a script (native or Plutus).
pub type Credential {
  VerificationKey(VerificationKeyHash)
  Script(ScriptHash)
}

// ## Constructing

/// A Cardano `Address` typically holding one or two credential references.
///
/// Note that legacy bootstrap addresses (a.k.a. 'Byron addresses') are
/// completely excluded from Plutus contexts. Thus, from an on-chain
/// perspective only exists addresses of type 00, 01, ..., 07 as detailed
/// in [CIP-0019 :: Shelley Addresses](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019/#shelley-addresses).
pub type Address {
  payment_credential: PaymentCredential,
  stake_credential: Option<StakeCredential>,
}

/// Smart-constructor for an [Address](#Address) from a [script](#Script) hash. The address has no delegation rights whatsoever.
pub fn from_script(script: Hash<Blake2b_224, Script>) -> Address {
  Address { payment_credential: Script(script), stake_credential: None }
}

/// Smart-constructor for an [Address](#Address) from a [verification key](#VerificationKey) hash. The resulting address has no delegation rights whatsoever.
pub fn from_verification_key(vk: Hash<Blake2b_224, VerificationKey>) -> Address {
  Address { payment_credential: VerificationKey(vk), stake_credential: None }
}

/// Set (or reset) the delegation part of an [Address](#Address) using a [verification key](#VerificationKey) hash. This is useful when combined with [`from_verification_key`](#from_verification_key) and/or [`from_script`](#from_script).
pub fn with_delegation_key(
  self: Address,
  vk: Hash<Blake2b_224, VerificationKey>,
) -> Address {
  Address {
    payment_credential: self.payment_credential,
    stake_credential: Some(Inline(VerificationKey(vk))),
  }
}

/// Set (or reset) the delegation part of an [Address](#Address) using a [script](#Script) hash. This is useful when combined with [`from_verification_key`](#from_verification_key) and/or [`from_script`](#from_script).
pub fn with_delegation_script(
  self: Address,
  script: Hash<Blake2b_224, Script>,
) -> Address {
  Address {
    payment_credential: self.payment_credential,
    stake_credential: Some(Inline(Script(script))),
  }
}

/// Represent a type of object that can be represented either inline (by hash)
/// or via a reference (i.e. a pointer to an on-chain location).
///
/// This is mainly use for capturing pointers to a stake credential
/// registration certificate in the case of so-called pointer addresses.
pub type Referenced<a> {
  Inline(a)
  Pointer { slot_number: Int, transaction_index: Int, certificate_index: Int }
}

/// A `StakeCredential` represents the delegation and rewards withdrawal conditions
/// associated with some stake address / account.
///
/// A `StakeCredential` is either provided inline, or, by reference using an
/// on-chain pointer.
///
/// Read more about pointers in [CIP-0019 :: Pointers](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019/#pointers).
pub type StakeCredential =
  Referenced<Credential>

/// A 'PaymentCredential' represents the spending conditions associated with
/// some output. Hence,
///
/// - a `VerificationKey` captures an output locked by a public/private key pair;
/// - and a `Script` captures an output locked by a native or Plutus script.
///
pub type PaymentCredential =
  Credential
