use aiken/collection/list
use aiken/crypto.{DataHash, ScriptHash, VerificationKeyHash}
use aiken/fuzz.{
  and_then, bool, byte, bytearray_between, constant, data, int, int_at_least,
  int_between, list_between, map, map2, map3, map4, option, set_between,
}
use aiken/primitive/bytearray
use cardano/address.{
  Address, Credential, Inline, Pointer, Script, StakeCredential, VerificationKey,
}
use cardano/address/credential
use cardano/assets.{AssetName, Lovelace, PolicyId, Value}
use cardano/certificate.{
  AlwaysAbstain, AlwaysNoConfidence, AuthorizeConstitutionalCommitteeProxy,
  Certificate, Delegate, DelegateBlockProduction, DelegateBoth,
  DelegateCredential, DelegateRepresentative, DelegateVote,
  RegisterAndDelegateCredential, RegisterCredential,
  RegisterDelegateRepresentative, RegisterStakePool, Registered,
  RetireFromConstitutionalCommittee, RetireStakePool, StakePoolId,
  UnregisterCredential, UnregisterDelegateRepresentative,
  UpdateDelegateRepresentative,
}
use cardano/transaction.{
  Datum, DatumHash, InlineDatum, Input, NoDatum, Output, OutputReference,
  TransactionId,
}

// ## Address

const credential_hash_size: Int = 28

/// Generate an arbitrary `Address` (other than pointer addresses).
pub fn address() -> Fuzzer<Address> {
  let address_type <- and_then(byte())
  if address_type < 45 {
    let payment <-
      map(bytearray_between(credential_hash_size, credential_hash_size))
    Address {
      payment_credential: VerificationKey(payment),
      stake_credential: None,
    }
  } else if address_type < 87 {
    let payment <-
      map(bytearray_between(credential_hash_size, credential_hash_size))
    Address { payment_credential: Script(payment), stake_credential: None }
  } else if address_type < 129 {
    let payment <-
      and_then(bytearray_between(credential_hash_size, credential_hash_size))
    let stake <-
      map(bytearray_between(credential_hash_size, credential_hash_size))
    Address {
      payment_credential: VerificationKey(payment),
      stake_credential: Some(Inline(VerificationKey(stake))),
    }
  } else if address_type < 171 {
    let payment <-
      and_then(bytearray_between(credential_hash_size, credential_hash_size))
    let stake <-
      map(bytearray_between(credential_hash_size, credential_hash_size))
    Address {
      payment_credential: VerificationKey(payment),
      stake_credential: Some(Inline(Script(stake))),
    }
  } else if address_type < 213 {
    let payment <-
      and_then(bytearray_between(credential_hash_size, credential_hash_size))
    let stake <-
      map(bytearray_between(credential_hash_size, credential_hash_size))
    Address {
      payment_credential: Script(payment),
      stake_credential: Some(Inline(VerificationKey(stake))),
    }
  } else {
    let payment <-
      and_then(bytearray_between(credential_hash_size, credential_hash_size))
    let stake <-
      map(bytearray_between(credential_hash_size, credential_hash_size))
    Address {
      payment_credential: Script(payment),
      stake_credential: Some(Inline(Script(stake))),
    }
  }
}

/// Generate an arbitrary `Address` by specifying its payment and stake credentials.
///
/// #### Examples
///
/// ```aiken
/// // No stake credential whatsoever
/// cardano.address_with(cardano.credential(), fuzz.constant(None))
///
/// // Only script addresses
/// cardano.address_with(
///   cardano.script(),
///   fuzz.option(cardano.stake_credential()),
/// )
/// ```
pub fn address_with(
  payment_credential: Fuzzer<Credential>,
  stake_credential: Fuzzer<Option<StakeCredential>>,
) -> Fuzzer<Address> {
  map2(
    payment_credential,
    stake_credential,
    fn(payment_credential, stake_credential) {
      Address { payment_credential, stake_credential }
    },
  )
}

// ### Address :: Credential

/// Generate an arbitrary `Credential`.
pub fn credential() -> Fuzzer<Credential> {
  map2(
    bool(),
    bytearray_between(credential_hash_size, credential_hash_size),
    fn(coin_flip, key_hash) {
      if coin_flip {
        VerificationKey(key_hash)
      } else {
        Script(key_hash)
      }
    },
  )
}

// ##### constructors

/// Generate an arbitrary `Script` credential.
pub fn script() -> Fuzzer<Credential> {
  script_with(script_hash())
}

/// Generate an arbitrary `ScriptHash`
pub fn script_hash() -> Fuzzer<ScriptHash> {
  bytearray_between(credential_hash_size, credential_hash_size)
}

/// Generate an arbitrary `Script` credential, specifying a hash `Fuzzer`.
///
/// #### Examples
///
/// ```aiken
/// // Choose amongst a list of known hashes.
/// cardano.script_with(fuzz.one_of([
///   "foo",
///   "bar",
/// ]))
/// ```
pub fn script_with(script_hash: Fuzzer<ScriptHash>) -> Fuzzer<Credential> {
  map(script_hash, fn(hash) { Script(hash) })
}

/// Generate an arbitrary `VerificationKey` credential.
pub fn verification_key() -> Fuzzer<Credential> {
  verification_key_with(verification_key_hash())
}

/// Generate an arbitrary `VerificationKeyHash`.
pub fn verification_key_hash() -> Fuzzer<VerificationKeyHash> {
  bytearray_between(credential_hash_size, credential_hash_size)
}

/// Generate an arbitrary `VerificationKey` credential, specifying a hash `Fuzzer`.
///
/// #### Examples
///
/// ```aiken
/// // Choose amongst a list of known hashes.
/// cardano.verification_key_with(fuzz.one_of([
///   "foo",
///   "bar",
/// ]))
/// ```
pub fn verification_key_with(
  verification_key_hash: Fuzzer<VerificationKeyHash>,
) -> Fuzzer<Credential> {
  map(verification_key_hash, fn(hash) { VerificationKey(hash) })
}

// ### Address :: StakeCredential

/// Generate an arbitrary stake credential.
pub fn stake_credential() -> Fuzzer<StakeCredential> {
  and_then(
    bool(),
    fn(bool) {
      if bool {
        inline()
      } else {
        pointer()
      }
    },
  )
}

// ##### constructors

/// Generate an arbitrary `Inline` stake credential.
pub fn inline() -> Fuzzer<StakeCredential> {
  inline_with(credential())
}

/// Generate an `Inline` stake credential, specifying a `Fuzzer` for the `Credential`.
pub fn inline_with(credential: Fuzzer<Credential>) -> Fuzzer<StakeCredential> {
  map(credential, fn(credential) { Inline(credential) })
}

/// Generate an arbitrary `Pointer` stake credential.
pub fn pointer() -> Fuzzer<StakeCredential> {
  map3(
    int_at_least(0),
    byte(),
    byte(),
    fn(slot_number, transaction_index, certificate_index) {
      Pointer(slot_number, transaction_index, certificate_index)
    },
  )
}

/// Generate an arbitrary `Pointer` stake credential, specifying fuzzers for the sub-elements.
pub fn pointer_with(
  slot_number: Fuzzer<Int>,
  transaction_index: Fuzzer<Int>,
  certificate_index: Fuzzer<Int>,
) -> Fuzzer<StakeCredential> {
  map3(
    slot_number,
    transaction_index,
    certificate_index,
    fn(slot_number, transaction_index, certificate_index) {
      Pointer(slot_number, transaction_index, certificate_index)
    },
  )
}

// ## Assets

// ### Assets :: AssetName

const asset_name_max_len = 32

/// Generate an arbitrary `AssetName`, between 0 and 32 bytes length.
pub fn asset_name() -> Fuzzer<AssetName> {
  bytearray_between(0, asset_name_max_len)
}

/// Generate an arbitrary `AssetName` from another `Fuzzer`, enforcing a length smaller than 32 bytes.
pub fn asset_name_with(asset_name: Fuzzer<ByteArray>) -> Fuzzer<AssetName> {
  let name <- fuzz.map(asset_name)
  expect bytearray.length(name) <= asset_name_max_len
  name
}

// ### Assets :: PolicyId

const policy_id_len = 28

/// Generate an arbitrary `PolicyId` (a.k.a script hash).
pub fn policy_id() -> Fuzzer<PolicyId> {
  bytearray_between(policy_id_len, policy_id_len)
}

/// Generate an arbitrary `PolicyId` from another `Fuzzer`, enforcing a length strictly equal to 28 bytes.
pub fn policy_id_with(policy_id: Fuzzer<PolicyId>) -> Fuzzer<PolicyId> {
  let policy <- fuzz.map(policy_id)
  expect bytearray.length(policy) == policy_id_len
  policy
}

// ### Assets :: Value

/// Generate a `Value` holding a **positive** (i.e. > 0) quantity of lovelace, but no assets.
pub fn lovelace() -> Fuzzer<Value> {
  lovelace_with(int_at_least(1))
}

/// Generate a `Value` with no assets, specifying a `Fuzzer` for the `Lovelace` quantity.
///
/// #### Examples
///
/// ```aiken
/// cardano.lovelace_with(int_at_least(1)) == cardano.lovelace()
/// ```
pub fn lovelace_with(lovelace: Fuzzer<Int>) -> Fuzzer<Value> {
  map(lovelace, assets.from_lovelace)
}

/// Generate an arbitrary `Value`, extending an existing one. Use
/// `assets.zero` or `cardano.lovelace()` as a simple starting point.
///
/// #### Examples
///
/// ```aiken
/// /// Some arbitrary value, possibly empty.
/// cardano.value(assets.zero)
///
/// /// At least some known NFT
/// cardano.value(assets.from_asset("my_policy", "my_asset_name", 1))
///
/// /// At least some lovelace
/// let quantity <- fuzz.and_then(int_at_least(1))
/// cardano.value(assets.from_lovelace(lovelace))
/// ```
pub fn value(self: Value) -> Fuzzer<Value> {
  value_with(
    self,
    list_between(policy_id(), 0, 3),
    fn(_) { asset_name() },
    fn(_, _) { int_at_least(1) },
  )
}

/// Extend an existing value using the given fuzzers for fine-grained control.
///
/// #### Examples
///
/// ```aiken
/// cardano.value_with(
///   assets.zero,
///   policies: fuzz.constant([my_policy_1, my_policy_2]),
///   asset_name: fn(policy) {
///     if policy == my_policy_1 {
///        fuzz.one_of([asset_1, asset_2, asset_3])
///     } else {
///        fuzz.constant(my_other_asset)
///     }
///   },
///   quantity: fn(_, _) { fuzz.constant(1) }
/// )
/// ```
pub fn value_with(
  self: Value,
  policies: Fuzzer<List<PolicyId>>,
  asset_name: fn(PolicyId) -> Fuzzer<AssetName>,
  quantity: fn(PolicyId, AssetName) -> Fuzzer<Int>,
) -> Fuzzer<Value> {
  let policies <- and_then(policies)
  list.foldr(
    policies,
    constant(self),
    fn(policy, step) {
      let value <- and_then(step)
      let asset_name <- and_then(asset_name(policy))
      let quantity <- map(quantity(policy, asset_name))
      value |> assets.add(policy, asset_name, quantity)
    },
  )
}

// ## Certificate

/// Generate an arbitrary `Certificate`.
pub fn certificate() -> Fuzzer<Certificate> {
  let certificate_type <- and_then(byte())
  if certificate_type < 117 {
    if certificate_type < 71 {
      if certificate_type < 48 {
        if certificate_type < 25 {
          register_credential()
        } else {
          unregister_credential()
        }
      } else {
        delegate_credential()
      }
    } else {
      if certificate_type < 94 {
        register_and_delegate_credential()
      } else {
        register_delegate_representative()
      }
    }
  } else {
    if certificate_type < 186 {
      if certificate_type < 163 {
        if certificate_type < 140 {
          update_delegate_representative()
        } else {
          unregister_delegate_representative()
        }
      } else {
        register_stake_pool()
      }
    } else {
      if certificate_type < 232 {
        if certificate_type < 209 {
          retire_stake_pool()
        } else {
          authorize_constitutional_committee_proxy()
        }
      } else {
        retire_from_constitutional_committee()
      }
    }
  }
}

// ##### constructors

pub fn authorize_constitutional_committee_proxy() -> Fuzzer<Certificate> {
  map2(credential(), credential(), AuthorizeConstitutionalCommitteeProxy)
}

pub fn authorize_constitutional_committee_proxy_with(
  constitutional_committee_member: Fuzzer<Credential>,
  proxy: Fuzzer<Credential>,
) -> Fuzzer<Certificate> {
  map2(
    constitutional_committee_member,
    proxy,
    AuthorizeConstitutionalCommitteeProxy,
  )
}

pub fn delegate_credential() -> Fuzzer<Certificate> {
  map2(credential(), delegate(), DelegateCredential)
}

pub fn delegate_credential_with(
  credential: Fuzzer<Credential>,
  delegate: Fuzzer<Delegate>,
) -> Fuzzer<Certificate> {
  map2(credential, delegate, DelegateCredential)
}

pub fn register_and_delegate_credential() -> Fuzzer<Certificate> {
  map3(credential(), delegate(), int(), RegisterAndDelegateCredential)
}

pub fn register_and_delegate_credential_with(
  credential: Fuzzer<Credential>,
  delegate: Fuzzer<Delegate>,
  deposit: Fuzzer<Lovelace>,
) -> Fuzzer<Certificate> {
  map3(credential, delegate, deposit, RegisterAndDelegateCredential)
}

pub fn register_credential() -> Fuzzer<Certificate> {
  map(credential(), fn(credential) { RegisterCredential(credential, Never) })
}

pub fn register_credential_with(
  credential: Fuzzer<Credential>,
) -> Fuzzer<Certificate> {
  map(credential, fn(credential) { RegisterCredential(credential, Never) })
}

pub fn register_delegate_representative() -> Fuzzer<Certificate> {
  map2(credential(), int(), RegisterDelegateRepresentative)
}

pub fn register_delegate_representative_with(
  credential: Fuzzer<Credential>,
  deposit: Fuzzer<Lovelace>,
) -> Fuzzer<Certificate> {
  map2(credential, deposit, RegisterDelegateRepresentative)
}

pub fn register_stake_pool() -> Fuzzer<Certificate> {
  map2(
    stake_pool_id(),
    bytearray_between(stake_pool_id_size, stake_pool_id_size),
    RegisterStakePool,
  )
}

pub fn register_stake_pool_with(
  stake_pool_id: Fuzzer<StakePoolId>,
  verification_key_hash: Fuzzer<VerificationKeyHash>,
) -> Fuzzer<Certificate> {
  map2(
    stake_pool_id,
    verification_key_hash,
    fn(s, v) { RegisterStakePool(s, v) },
  )
}

pub fn retire_from_constitutional_committee() -> Fuzzer<Certificate> {
  map(credential(), RetireFromConstitutionalCommittee)
}

pub fn retire_from_constitutional_committee_with(
  constitutional_committee_member: Fuzzer<Credential>,
) -> Fuzzer<Certificate> {
  map(constitutional_committee_member, RetireFromConstitutionalCommittee)
}

pub fn retire_stake_pool() -> Fuzzer<Certificate> {
  map2(stake_pool_id(), int(), RetireStakePool)
}

pub fn retire_stake_pool_with(
  stake_pool_id: Fuzzer<StakePoolId>,
  deposit: Fuzzer<Lovelace>,
) -> Fuzzer<Certificate> {
  map2(stake_pool_id, deposit, RetireStakePool)
}

pub fn unregister_credential() -> Fuzzer<Certificate> {
  map(credential(), fn(credential) { UnregisterCredential(credential, Never) })
}

pub fn unregister_credential_with(
  credential: Fuzzer<Credential>,
) -> Fuzzer<Certificate> {
  map(credential, fn(credential) { UnregisterCredential(credential, Never) })
}

pub fn unregister_delegate_representative() -> Fuzzer<Certificate> {
  map2(credential(), int(), UnregisterDelegateRepresentative)
}

pub fn unregister_delegate_representative_with(
  credential: Fuzzer<Credential>,
  deposit: Fuzzer<Lovelace>,
) -> Fuzzer<Certificate> {
  map2(credential, deposit, UnregisterDelegateRepresentative)
}

pub fn update_delegate_representative() -> Fuzzer<Certificate> {
  map(credential(), UpdateDelegateRepresentative)
}

pub fn update_delegate_representative_with(
  credential: Fuzzer<Credential>,
) -> Fuzzer<Certificate> {
  map(credential, UpdateDelegateRepresentative)
}

// ### Certificate :: Delegate

/// Generate an arbitrary `Delegate`.
pub fn delegate() -> Fuzzer<Delegate> {
  let delegate_type <- and_then(byte())
  if delegate_type < 86 {
    delegate_block_production()
  } else if delegate_type < 171 {
    delegate_vote()
  } else {
    delegate_both()
  }
}

// ##### constructors

/// Generate an arbitrary `DelegateBlockProduction` delegate.
pub fn delegate_block_production() -> Fuzzer<Delegate> {
  map(stake_pool_id(), DelegateBlockProduction)
}

/// Generate an arbitrary `DelegateBlockProduction` delegate, specifying a `Fuzzer` for the pool id.
pub fn delegate_block_production_with(
  stake_pool_id: Fuzzer<StakePoolId>,
) -> Fuzzer<Delegate> {
  map(stake_pool_id, DelegateBlockProduction)
}

/// Generate an arbitrary `DelegateBoth` delegate.
pub fn delegate_both() -> Fuzzer<Delegate> {
  map2(stake_pool_id(), delegate_representative(), DelegateBoth)
}

/// Generate an arbitrary `DelegateBoth` delegate, specifying a `Fuzzer` for the pool and drep.
pub fn delegate_both_with(
  stake_pool_id: Fuzzer<StakePoolId>,
  delegate_representative: Fuzzer<DelegateRepresentative>,
) -> Fuzzer<Delegate> {
  map2(stake_pool_id, delegate_representative, DelegateBoth)
}

/// Generate an arbitrary `DelegateVote` delegate.
pub fn delegate_vote() -> Fuzzer<Delegate> {
  map(delegate_representative(), DelegateVote)
}

/// Generate an arbitrary `DelegateVote` delegate, specifying a `Fuzzer` for the drep.
pub fn delegate_vote_with(
  delegate_representative: Fuzzer<DelegateRepresentative>,
) -> Fuzzer<Delegate> {
  map(delegate_representative, DelegateVote)
}

// ### Certificate :: Delegate Representative

/// Generate an arbitrary `DelegateRepresentative`.
pub fn delegate_representative() -> Fuzzer<DelegateRepresentative> {
  let drep_type <- and_then(byte())
  if drep_type < 86 {
    registered()
  } else if drep_type < 171 {
    always_abstain()
  } else {
    always_no_confidence()
  }
}

// ##### constructors

/// Generate an arbitrary `AlwaysAbstain` drep.
pub fn always_abstain() -> Fuzzer<DelegateRepresentative> {
  constant(AlwaysAbstain)
}

/// Generate an arbitrary `AlwaysNoConfidence` drep.
pub fn always_no_confidence() -> Fuzzer<DelegateRepresentative> {
  constant(AlwaysNoConfidence)
}

/// Generate an arbitrary `Registered` drep.
pub fn registered() -> Fuzzer<DelegateRepresentative> {
  map(credential(), Registered)
}

/// Generate an arbitrary `Registered` drep, specifying a `Fuzzer` for the credential.
pub fn registered_with(
  credential: Fuzzer<Credential>,
) -> Fuzzer<DelegateRepresentative> {
  map(credential, Registered)
}

// ### Certificate :: StakePoolId

const stake_pool_id_size: Int = 28

/// Generate an arbitrary `StakePoolId`.
pub fn stake_pool_id() -> Fuzzer<StakePoolId> {
  bytearray_between(stake_pool_id_size, stake_pool_id_size)
}

/// Generate an arbitrary `StakePoolId`, enforcing a pool id size of 28 bytes.
pub fn stake_pool_id_with(
  stake_pool_id: Fuzzer<ByteArray>,
) -> Fuzzer<StakePoolId> {
  map(
    stake_pool_id,
    fn(stake_pool_id) {
      if bytearray.length(stake_pool_id) == stake_pool_id_size {
        stake_pool_id
      } else {
        fail @"StakePoolId must be 28 bytes"
      }
    },
  )
}

// ## Transaction

const transaction_id_size: Int = 32

/// Generate an arbitrary `TransactionId`
pub fn transaction_id() -> Fuzzer<TransactionId> {
  bytearray_between(transaction_id_size, transaction_id_size)
}

// ### Tx :: Datum

const datum_hash_size: Int = 32

/// Generate an arbitrary `Datum`.
pub fn datum() -> Fuzzer<Datum> {
  let datum_type <- and_then(byte())
  if datum_type < 86 {
    no_datum()
  } else if datum_type < 171 {
    datum_hash()
  } else {
    inline_datum()
  }
}

// ##### constructors

pub fn datum_hash() -> Fuzzer<Datum> {
  datum_hash_with(bytearray_between(datum_hash_size, datum_hash_size))
}

pub fn datum_hash_with(datum_hash: Fuzzer<DataHash>) -> Fuzzer<Datum> {
  let hash <- map(datum_hash)
  DatumHash(hash)
}

pub fn inline_datum() -> Fuzzer<Datum> {
  inline_datum_with(data())
}

pub fn inline_datum_with(data: Fuzzer<Data>) -> Fuzzer<Datum> {
  let data <- map(data)
  InlineDatum(data)
}

pub fn no_datum() -> Fuzzer<Datum> {
  constant(NoDatum)
}

// ### Tx :: Input

/// Generate an arbitrary `Input`.
pub fn input() -> Fuzzer<Input> {
  input_with(output_reference(), output())
}

/// Generate an arbitrary `Input`, specifying fuzzers for sub-elements.
pub fn input_with(
  output_reference: Fuzzer<OutputReference>,
  output: Fuzzer<Output>,
) {
  map2(
    output_reference,
    output,
    fn(output_reference, output) { Input { output_reference, output } },
  )
}

// ### Tx :: Output

/// Generate an arbitrary `Output`.
pub fn output() -> Fuzzer<Output> {
  let lovelace <- and_then(lovelace())
  output_with(address(), value(lovelace), datum(), reference_script())
}

/// Generate an arbitrary `Output`, specifying fuzzers for sub-elements.
pub fn output_with(
  address: Fuzzer<Address>,
  value: Fuzzer<Value>,
  datum: Fuzzer<Datum>,
  reference_script: Fuzzer<Option<ScriptHash>>,
) -> Fuzzer<Output> {
  map4(
    address,
    value,
    datum,
    reference_script,
    fn(address, value, datum, reference_script) {
      Output { address, value, datum, reference_script }
    },
  )
}

// ### Tx :: OutputReference

/// Generate an arbitrary `OutputReference`, with reasonably sized output indexes.
pub fn output_reference() -> Fuzzer<OutputReference> {
  output_reference_with(transaction_id(), int_between(0, 300))
}

/// Generate an arbitrary `OutputReference`, specifying fuzzers for sub-elements.
pub fn output_reference_with(
  transaction_id: Fuzzer<TransactionId>,
  output_index: Fuzzer<Int>,
) -> Fuzzer<OutputReference> {
  map2(
    transaction_id,
    output_index,
    fn(transaction_id, output_index) {
      expect output_index >= 0
      OutputReference { transaction_id, output_index }
    },
  )
}

// ### Tx :: ReferenceScript

/// Generate an arbitrary `ReferenceScript`.
pub fn reference_script() -> Fuzzer<Option<ScriptHash>> {
  reference_script_with(
    bytearray_between(credential_hash_size, credential_hash_size),
  )
}

/// Generate an arbitrary `ReferenceScript`, specifying a `Fuzzer` for the script hash.
pub fn reference_script_with(
  script_hash: Fuzzer<ScriptHash>,
) -> Fuzzer<Option<ScriptHash>> {
  option(script_hash)
}

// ### Tx :: Withdrawals

/// Generate arbitrary withdrawals, ordered by ascending `Credential`.
pub fn withdrawals() -> Fuzzer<Pairs<Credential, Lovelace>> {
  withdrawals_extending([])
}

/// Generate a valid sequence of withdrawals, extending from an existing set.
pub fn withdrawals_extending(
  self: Pairs<Credential, Lovelace>,
) -> Fuzzer<Pairs<Credential, Lovelace>> {
  map(
    // It's reasonable to assume 'credential' to lead unique credentials here
    // so `set_between` is unnecessary.
    list_between(
      map2(credential(), int_at_least(1), fn(c, i) { Pair(c, i) }),
      0,
      3,
    ),
    fn(xs) {
      xs
        |> list.concat(self)
        |> list.sort(fn(e1, e2) { credential.compare(e1.1st, e2.1st) })
    },
  )
}

/// Generate arbitrary withdrawals, specifying a `Fuzzer` for credentials and quantity for fine-grained control.
pub fn withdrawals_with(
  credential: Fuzzer<Credential>,
  quantity: fn(Credential) -> Fuzzer<Lovelace>,
) -> Fuzzer<Pairs<Credential, Lovelace>> {
  // We don't know how users may generate credential, so we must guarantee uniqueness.
  let credentials <- and_then(set_between(credential, 0, 3))
  list.foldr(
    list.sort(credentials, credential.compare),
    constant([]),
    fn(credential, step) {
      let xs <- and_then(step)
      let n <- map(quantity(credential))
      [Pair(credential, n), ..xs]
    },
  )
}
