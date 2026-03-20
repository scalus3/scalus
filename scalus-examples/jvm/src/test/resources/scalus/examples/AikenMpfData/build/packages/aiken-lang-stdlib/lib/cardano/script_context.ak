//// This module contains utilities for manually dealing with [`ScriptContext`](#ScriptContext). This is only ever useful for writing custom `else` handlers in validators.
////
//// > [!NOTE]
//// > Unless you know what you're doing, you should prefer using named handlers:
//// >
//// > - `mint`
//// > - `spend`
//// > - `withdraw`
//// > - `publish`
//// > - `vote`
//// > - `propose`

use aiken/collection.{Index}
use cardano/address.{Credential}
use cardano/assets.{PolicyId}
use cardano/certificate.{Certificate}
use cardano/governance.{ProposalProcedure, Voter}
use cardano/transaction.{OutputReference, Redeemer, Transaction}

/// A context given to a script by the Cardano ledger when being executed.
///
/// The context contains information about the entire transaction that contains
/// the script. The transaction may also contain other scripts; to distinguish
/// between multiple scripts, the [`ScriptContext`](#ScriptContext) contains a
/// [`ScriptInfo`](#ScriptInfo) which indicates which script (or, for what
/// purpose) the transaction is being executed.
pub type ScriptContext {
  transaction: Transaction,
  redeemer: Redeemer,
  info: ScriptInfo,
}

/// Characterizes the script information. The main (and only) difference with [`ScriptPurpose`](./transaction.html#ScriptPurpose) resides in the `Spending` variant which here contains a second field `datum: Option<Data>`.
pub type ScriptInfo {
  /// For scripts executed as minting/burning policies, to insert
  /// or remove assets from circulation. It's parameterized by the identifier
  /// of the associated policy.
  Minting(PolicyId)
  /// For scripts that are used as payment credentials for addresses in
  /// transaction outputs. They govern the rule by which the output they
  /// reference can be spent.
  Spending { output: OutputReference, datum: Option<Data> }
  /// For scripts that validate reward withdrawals from a reward account.
  ///
  /// The argument identifies the target reward account.
  Withdrawing(Credential)
  /// Needed when delegating to a pool using stake credentials defined as a
  /// custom script. This purpose is also triggered when de-registering such
  /// stake credentials.
  ///
  /// The Int is a 0-based index of the given `Certificate` in `certificates`.
  Publishing { at: Index, certificate: Certificate }
  /// Voting for a type of voter using a governance action id to vote
  /// yes / no / abstain inside a transaction.
  ///
  /// The voter is who is doing the governance action.
  Voting(Voter)
  /// Used to propose a governance action.
  ///
  /// A 0-based index of the given `ProposalProcedure` in `proposal_procedures`.
  Proposing { at: Index, proposal_procedure: ProposalProcedure }
}
