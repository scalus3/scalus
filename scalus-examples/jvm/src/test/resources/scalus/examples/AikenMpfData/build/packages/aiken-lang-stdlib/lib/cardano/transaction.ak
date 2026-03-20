use aiken/builtin
use aiken/collection.{Index}
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{
  Blake2b_256, DataHash, Hash, ScriptHash, VerificationKeyHash, blake2b_256,
}
use aiken/interval.{Interval}
use aiken/option
use cardano/address.{Address, Credential, Script, VerificationKey}
use cardano/assets.{Lovelace, PolicyId, Value}
use cardano/certificate.{Certificate}
use cardano/governance.{GovernanceActionId, ProposalProcedure, Vote, Voter}

pub type TransactionId =
  Hash<Blake2b_256, Transaction>

/// Characterizes the script purpose.
pub type ScriptPurpose {
  /// For scripts executed as minting/burning policies, to insert
  /// or remove assets from circulation. It's parameterized by the identifier
  /// of the associated policy.
  Mint(PolicyId)
  /// For scripts that are used as payment credentials for addresses in
  /// transaction outputs. They govern the rule by which the output they
  /// reference can be spent.
  Spend(OutputReference)
  /// For scripts that validate reward withdrawals from a reward account.
  ///
  /// The argument identifies the target reward account.
  Withdraw(Credential)
  /// Needed when delegating to a pool using stake credentials defined as a
  /// custom script. This purpose is also triggered when de-registering such
  /// stake credentials.
  ///
  /// The Int is a 0-based index of the given `Certificate` in `certificates`.
  Publish { at: Index, certificate: Certificate }
  /// Voting for a type of voter using a governance action id to vote
  /// yes / no / abstain inside a transaction.
  ///
  /// The voter is who is doing the governance action.
  Vote(Voter)
  /// Used to propose a governance action.
  ///
  /// A 0-based index of the given `ProposalProcedure` in `proposal_procedures`.
  Propose { at: Index, proposal_procedure: ProposalProcedure }
}

/// A Cardano `Transaction`, as seen by on-chain scripts.
///
/// Note that this is a representation of a transaction, and not the 1:1
/// translation of the transaction as seen by the ledger. In particular,
/// on-chain scripts can't see inputs locked by bootstrap addresses, outputs
/// to bootstrap addresses or just transaction metadata.
pub type Transaction {
  inputs: List<Input>,
  reference_inputs: List<Input>,
  outputs: List<Output>,
  fee: Lovelace,
  mint: Value,
  certificates: List<Certificate>,
  /// > [!IMPORTANT]
  /// > Withdrawals are ordered by ascending [Credential](./credential.html#Credential). Yet, note that [`Script`](./credential.html#Credential) credentials are treated as **lower values** than [`VerificationKey`](./credential.html#Credential) credentials.
  withdrawals: Pairs<Credential, Lovelace>,
  validity_range: ValidityRange,
  extra_signatories: List<VerificationKeyHash>,
  /// > [!IMPORTANT]
  /// > Redeemers are ordered by ascending [ScriptPurpose](./transaction.html#ScriptPurpose).
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  datums: Dict<DataHash, Data>,
  id: TransactionId,
  /// > [!IMPORTANT]
  /// > Votes are ordered by ascending [Voter](./governance.html#Voter) and [GovernanceActionId](./governance.html#GovernanceActionId).<br/>First constructor variants in a type are treated as lower indices; except for [Credential](./credential.html#Credential) where [`Script`](./credential.html#Credential) credentials are treated as **lower values** than [`VerificationKey`](./credential.html#Credential) credentials.
  votes: Pairs<Voter, Pairs<GovernanceActionId, Vote>>,
  proposal_procedures: List<ProposalProcedure>,
  current_treasury_amount: Option<Lovelace>,
  treasury_donation: Option<Lovelace>,
}

/// An interval of POSIX time, measured in **number of milliseconds** since 1970-01-01T00:00:00Z.
pub type ValidityRange =
  Interval<Int>

/// An `Input` made of an output reference and, the resolved value associated with that output.
pub type Input {
  output_reference: OutputReference,
  output: Output,
}

/// An `OutputReference` is a unique reference to an output on-chain. The `output_index`
/// corresponds to the position in the output list of the transaction (identified by its id)
/// that produced that output
pub type OutputReference {
  transaction_id: Hash<Blake2b_256, Transaction>,
  output_index: Int,
}

/// A transaction `Output`, with an address, a value and optional datums and script references.
pub type Output {
  address: Address,
  value: Value,
  datum: Datum,
  reference_script: Option<ScriptHash>,
}

/// An output `Datum`.
pub type Datum {
  NoDatum
  /// A datum referenced by its hash digest.
  DatumHash(DataHash)
  /// A datum completely inlined in the output.
  InlineDatum(Data)
}

/// A type-alias for Redeemers, passed to scripts for validation. The `Data` is
/// opaque because it is user-defined and it is the script's responsibility to
/// parse it into its expected form.
pub type Redeemer =
  Data

// ## Querying

/// Find an input by its [`OutputReference`](#OutputReference). This is typically used in
/// combination with the `Spend` [`ScriptPurpose`](#ScriptPurpose) to find a script's own
/// input.
///
/// ```aiken
/// validator {
///   spend(datum, redeemer, my_output_reference, self) {
///     expect Some(input) =
///       self.inputs
///         |> transaction.find_input(my_output_reference)
///   }
/// }
/// ```
pub fn find_input(
  inputs: List<Input>,
  output_reference: OutputReference,
) -> Option<Input> {
  inputs
    |> list.find(fn(input) { input.output_reference == output_reference })
}

/// Find a [`Datum`](#Datum) by its hash, if present. The function looks first for
/// datums in the witness set, and then for inline datums if it doesn't find any in
/// witnesses.
pub fn find_datum(
  outputs: List<Output>,
  datums: Dict<DataHash, Data>,
  datum_hash: DataHash,
) -> Option<Data> {
  datums
    |> dict.get(datum_hash)
    |> option.or_try(
        fn() {
          outputs
            |> list.filter_map(
                fn(output) {
                  when output.datum is {
                    InlineDatum(data) ->
                      if blake2b_256(builtin.serialise_data(data)) == datum_hash {
                        Some(data)
                      } else {
                        None
                      }
                    _ -> None
                  }
                },
              )
            |> list.head
        },
      )
}

/// Find all outputs that are paying into the given script hash, if any. This is useful for
/// contracts running over multiple transactions.
pub fn find_script_outputs(
  outputs: List<Output>,
  script_hash: ScriptHash,
) -> List<Output> {
  outputs
    |> list.filter(
        fn(output) {
          when output.address.payment_credential is {
            Script(addr_script_hash) -> script_hash == addr_script_hash
            VerificationKey(_) -> False
          }
        },
      )
}

// ## Testing

/// A placeholder / empty `Transaction` to serve as a base in a transaction
/// builder. This is particularly useful for constructing test transactions.
///
/// Every field is empty or null, and we have in particular:
///
/// ```aiken
/// use aiken/interval
///
/// transaction.placeholder.id ==
///   #"0000000000000000000000000000000000000000000000000000000000000000"
///
/// transaction.placeholder.validity_range == interval.everything
/// ```
pub const placeholder: Transaction =
  Transaction {
    inputs: [],
    reference_inputs: [],
    outputs: [],
    fee: 0,
    mint: assets.zero,
    certificates: [],
    withdrawals: [],
    validity_range: interval.everything,
    extra_signatories: [],
    redeemers: [],
    datums: dict.empty,
    id: #"0000000000000000000000000000000000000000000000000000000000000000",
    votes: [],
    proposal_procedures: [],
    current_treasury_amount: None,
    treasury_donation: None,
  }
