use aiken/collection.{Index}
use aiken/crypto.{Blake2b_256, Hash, ScriptHash, VerificationKeyHash}
use aiken/math/rational.{Rational}
use cardano/address.{Credential}
use cardano/assets.{Lovelace}
use cardano/governance/protocol_parameters.{ProtocolParametersUpdate}

pub type ProposalProcedure {
  deposit: Lovelace,
  return_address: Credential,
  governance_action: GovernanceAction,
}

pub type GovernanceAction {
  ProtocolParameters {
    /// The last governance action of type 'ProtocolParameters'. They must all
    /// form a chain.
    ancestor: Option<GovernanceActionId>,
    /// The new proposed protocol parameters. Only values set to `Some` are relevant.
    new_parameters: ProtocolParametersUpdate,
    /// The optional guardrails script defined in the constitution. The script
    /// is executed by the ledger in addition to the hard-coded ledger rules.
    ///
    /// It must pass for the new protocol parameters to be deemed valid.
    guardrails: Option<ScriptHash>,
  }
  HardFork {
    /// The last governance action of type `HardFork`. They must all
    /// form a chain.
    ancestor: Option<GovernanceActionId>,
    /// The new proposed version. Few rules apply to proposing new versions:
    ///
    /// - The `major` component, if incremented, must be exactly one more than the current.
    /// - The `minor` component, if incremented, must be exactly one more than the current.
    /// - If the `major` component is incremented, `minor` must be set to `0`.
    /// - Neither `minor` nor `major` can be decremented.
    new_version: ProtocolVersion,
  }
  TreasuryWithdrawal {
    /// A collection of beneficiaries, which can be plain verification key
    /// hashes or script hashes (e.g. DAO).
    beneficiaries: Pairs<Credential, Lovelace>,
    /// The optional guardrails script defined in the constitution. The script
    /// is executed by the ledger in addition to the hard-coded ledger rules.
    ///
    /// It must pass for the withdrawals to be authorized.
    guardrails: Option<ScriptHash>,
  }
  NoConfidence {
    /// The last governance action of type `NoConfidence` or
    /// `ConstitutionalCommittee`. They must all / form a chain.
    ancestor: Option<GovernanceActionId>,
  }
  ConstitutionalCommittee {
    /// The last governance action of type `NoConfidence` or
    /// `ConstitutionalCommittee`. They must all / form a chain.
    ancestor: Option<GovernanceActionId>,
    /// Constitutional members to be removed.
    evicted_members: List<Credential>,
    /// Constitutional members to be added.
    added_members: Pairs<Credential, Mandate>,
    /// The new quorum value, as a ratio of a numerator and a denominator. The
    /// quorum specifies the threshold of 'Yes' votes necessary for the
    /// constitutional committee to accept a proposal procedure.
    quorum: Rational,
  }
  NewConstitution {
    /// The last governance action of type `Constitution` or
    /// `ConstitutionalCommittee`. They must all / form a chain.
    ancestor: Option<GovernanceActionId>,
    /// The new proposed constitution.
    constitution: Constitution,
  }
  NicePoll
}

pub type Vote {
  No
  Yes
  Abstain
}

pub type TransactionId =
  Hash<Blake2b_256, ByteArray>

pub type GovernanceActionId {
  transaction: TransactionId,
  proposal_procedure: Index,
}

pub type ProtocolVersion {
  major: Int,
  minor: Int,
}

pub type Constitution {
  guardrails: Option<ScriptHash>,
}

/// An epoch number after which constitutional committee member
/// mandate expires.
pub type Mandate =
  Int

pub type Voter {
  ConstitutionalCommitteeMember(Credential)
  DelegateRepresentative(Credential)
  StakePool(VerificationKeyHash)
}
