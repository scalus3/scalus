use aiken/math/rational.{Rational}
use cardano/assets.{Lovelace}

pub opaque type ProtocolParametersUpdate {
  inner: Pairs<ProtocolParametersIndex, Data>,
}

pub type ScriptExecutionPrices {
  memory: Rational,
  cpu: Rational,
}

pub type ExecutionUnits {
  memory: Int,
  cpu: Int,
}

pub type StakePoolOperatorVotingThresholds {
  motion_of_no_confidence: Rational,
  constitutional_committee: ConstitutionalCommitteeThresholds,
  hard_fork: Rational,
  protocol_parameters: ProtocolParametersThresholds<
    Rational,
    Void,
    Void,
    Void,
    Void,
  >,
}

pub type DelegateRepresentativeVotingThresholds {
  motion_of_no_confidence: Rational,
  constitutional_committee: ConstitutionalCommitteeThresholds,
  constitution: Rational,
  hard_fork: Rational,
  protocol_parameters: ProtocolParametersThresholds<
    Void,
    Rational,
    Rational,
    Rational,
    Rational,
  >,
  treasury_withdrawal: Rational,
}

pub type ProtocolParametersThresholds<
  security,
  network,
  economic,
  technical,
  governance,
> {
  security_group: security,
  network_group: network,
  economic_group: economic,
  technical_group: technical,
  governance_group: governance,
}

pub type ConstitutionalCommitteeThresholds {
  default: Rational,
  under_no_confidence: Rational,
}

/// The linear coefficient that intervenes in the transaction fee calculation.
/// It is multiplied by the size of the transaction in bytes to obtain a Lovelace value.
pub fn min_fee_coefficient(self: ProtocolParametersUpdate) -> Option<Int> {
  get_protocol_param(self.inner, 0, into_int)
}

/// The constant factor that intervenes in the transaction fee calculation. It is
/// a flat cost of lovelace that is added to every fee calculation.
pub fn min_fee_constant(self: ProtocolParametersUpdate) -> Option<Int> {
  get_protocol_param(self.inner, 1, into_int)
}

/// The maximum size of a serialized block body, expressed in bytes.
pub fn max_block_body_size(self: ProtocolParametersUpdate) -> Option<Int> {
  get_protocol_param(self.inner, 2, into_int)
}

/// The maximum size of a serialized transaction (body + witnesses), expressed in bytes.
pub fn max_transaction_size(self: ProtocolParametersUpdate) -> Option<Int> {
  get_protocol_param(self.inner, 3, into_int)
}

/// The maximum size of a serialized block header, expressed in bytes.
pub fn max_block_header_size(self: ProtocolParametersUpdate) -> Option<Int> {
  get_protocol_param(self.inner, 4, into_int)
}

/// The required deposit amount when registering stake credentials, expressed in Lovelace.
pub fn stake_credential_deposit(
  self: ProtocolParametersUpdate,
) -> Option<Lovelace> {
  get_protocol_param(self.inner, 5, into_int)
}

/// The required deposit amount when registering a stake pool, expressed in Lovelace.
pub fn stake_pool_deposit(self: ProtocolParametersUpdate) -> Option<Lovelace> {
  get_protocol_param(self.inner, 6, into_int)
}

/// The maximum number of epoch in the future allowed for a stake pool retirement to be scheduled.
pub fn stake_pool_retirement_horizon(
  self: ProtocolParametersUpdate,
) -> Option<Int> {
  get_protocol_param(self.inner, 7, into_int)
}

/// The desired/optimal number of fully saturated stake pools in the system. Also known as the _'k-parameter'_.
pub fn desired_number_of_stake_pools(
  self: ProtocolParametersUpdate,
) -> Option<Int> {
  get_protocol_param(self.inner, 8, into_int)
}

/// A parameter controlling the influence of an pool owner's pledge on the rewards. Also known as _'a0'_.
pub fn stake_pool_pledge_influence(
  self: ProtocolParametersUpdate,
) -> Option<Rational> {
  get_protocol_param(self.inner, 9, into_rational)
}

/// The monetary expansion parameter, controlling the fraction of Ada put in circulation on every epoch through the incentivies model. Also known as _'ρ'_.
pub fn monetary_expansion(self: ProtocolParametersUpdate) -> Option<Rational> {
  get_protocol_param(self.inner, 10, into_rational)
}

/// The parameter controlling what fraction (%) of available rewards is sent to the treasury on every epoch. Also known as _'τ'_.
pub fn treasury_expansion(self: ProtocolParametersUpdate) -> Option<Rational> {
  get_protocol_param(self.inner, 11, into_rational)
}

/// Minimum authorized constant cost that stake pools can declare when registering, expressed in Lovelace.
pub fn min_stake_pool_cost(self: ProtocolParametersUpdate) -> Option<Lovelace> {
  get_protocol_param(self.inner, 16, into_int)
}

/// The linear coefficient that intervenes in the calculation of the minimum Ada value that any UTxO must hold. It is expressed in Lovelace per Byte, and is also known as the 'coins per utxo byte' parameter.
pub fn min_utxo_deposit_coefficient(
  self: ProtocolParametersUpdate,
) -> Option<Int> {
  get_protocol_param(self.inner, 17, into_int)
}

/// The costs associated with the various operations of the Plutus Virtual Machine, which can be different for each Plutus version.
pub fn cost_models(self: ProtocolParametersUpdate) -> Option<Data> {
  get_protocol_param(self.inner, 18, identity)
}

/// The price, in Lovelace per unit, of the execution units corresponding to cpu and memory usage of on-chain scripts.
pub fn script_execution_prices(
  self: ProtocolParametersUpdate,
) -> Option<ScriptExecutionPrices> {
  get_protocol_param(self.inner, 19, into_script_execution_prices)
}

/// The maximum execution units allowed for a single transaction.
pub fn max_transaction_execution_units(
  self: ProtocolParametersUpdate,
) -> Option<ExecutionUnits> {
  get_protocol_param(self.inner, 20, into_execution_units)
}

/// The maximum execution units allowed for a single block.
pub fn max_block_execution_units(
  self: ProtocolParametersUpdate,
) -> Option<ExecutionUnits> {
  get_protocol_param(self.inner, 21, into_execution_units)
}

/// The maximum size of a serialized value in a transaction output. This effectively limits
/// the maximum kinds of assets that can be sent in a single output. It is expressed in bytes.
pub fn max_value_size(self: ProtocolParametersUpdate) -> Option<Int> {
  get_protocol_param(self.inner, 22, into_int)
}

/// The scaling factor applied to the transaction cost for defining the minimum collateral
/// amount. It is expressed in percent points (so 100 = 100%).
pub fn collateral_percentage(self: ProtocolParametersUpdate) -> Option<Int> {
  get_protocol_param(self.inner, 23, into_int)
}

/// The maximum number of collateral inputs allowed in the transaction.
pub fn max_collateral_inputs(self: ProtocolParametersUpdate) -> Option<Int> {
  get_protocol_param(self.inner, 24, into_int)
}

/// The various governance voting thresholds pertaining to stake pool operators.
pub fn stake_pool_operator_voting_thresholds(
  self: ProtocolParametersUpdate,
) -> Option<StakePoolOperatorVotingThresholds> {
  get_protocol_param(self.inner, 25, into_spo_voting_thresholds)
}

/// The various governance voting thresholds pertaining to delegate representatives
/// (a.k.a DReps).
pub fn delegate_representative_voting_thresholds(
  self: ProtocolParametersUpdate,
) -> Option<DelegateRepresentativeVotingThresholds> {
  get_protocol_param(self.inner, 26, into_drep_voting_thresholds)
}

/// The minimum number of members in the constitutional committee. Any updates of the committee
/// must leave at least this number of members.
pub fn min_constitutional_committee_size(
  self: ProtocolParametersUpdate,
) -> Option<Int> {
  get_protocol_param(self.inner, 27, into_int)
}

/// The maximum length of a constitutional committee member, expressed in number of epochs.
pub fn max_constitutional_committee_mandate(
  self: ProtocolParametersUpdate,
) -> Option<Int> {
  get_protocol_param(self.inner, 28, into_int)
}

/// The lifetime of any governance proposal. An action that hasn't been approved beyond that
/// period is considered inactive and discarded. It is expressed in number of epochs.
pub fn governance_proposal_lifetime(
  self: ProtocolParametersUpdate,
) -> Option<Int> {
  get_protocol_param(self.inner, 29, into_int)
}

/// The required deposit amount for governance proposal procedures, expressed in Lovelace.
pub fn governance_proposal_deposit(
  self: ProtocolParametersUpdate,
) -> Option<Lovelace> {
  get_protocol_param(self.inner, 30, into_int)
}

/// The required deposit amount when registering as a delegate representative, expressed in
/// Lovelace.
pub fn delegate_representative_deposit(
  self: ProtocolParametersUpdate,
) -> Option<Lovelace> {
  get_protocol_param(self.inner, 31, into_int)
}

/// The maximum number of epochs that a delegate representative can stay inactive (i.e. no
/// voting) without becoming _inactive_ and removed from thresholds calculations.
pub fn delegate_representative_max_idle_time(
  self: ProtocolParametersUpdate,
) -> Option<Int> {
  get_protocol_param(self.inner, 32, into_int)
}

/// The base tier fee coefficient for reference scripts. Reference scripts gets increasingly
/// more expensives every ~24KB, the base coefficient is a multiplicating factor which grows
/// exponentially with each tier.
pub fn reference_scripts_tier_fee_initial_factor(
  self: ProtocolParametersUpdate,
) -> Option<Rational> {
  get_protocol_param(self.inner, 33, into_rational)
}

// Internals -------------------------------------------------------------------

type ProtocolParametersIndex =
  Int

fn get_protocol_param(
  self: Pairs<ProtocolParametersIndex, Data>,
  ix: ProtocolParametersIndex,
  into: fn(Data) -> a,
) -> Option<a> {
  when self is {
    [] -> None
    [Pair(jx, param), ..tail] ->
      if ix == jx {
        Some(into(param))
      } else {
        get_protocol_param(tail, ix, into)
      }
  }
}

fn into_int(param: Data) -> Int {
  expect param: Int = param
  param
}

fn into_rational(param: Data) -> Rational {
  expect [numerator, denominator]: List<Int> = param
  expect Some(r) = rational.new(numerator, denominator)
  r
}

fn into_execution_units(param: Data) -> ExecutionUnits {
  expect [memory, cpu]: List<Int> = param
  ExecutionUnits { memory, cpu }
}

fn into_script_execution_prices(param: Data) -> ScriptExecutionPrices {
  expect [memory, cpu]: List<Data> = param
  let memory = into_rational(memory)
  let cpu = into_rational(cpu)
  ScriptExecutionPrices { memory, cpu }
}

fn into_spo_voting_thresholds(param: Data) -> StakePoolOperatorVotingThresholds {
  expect [
    motion_of_no_confidence, constitutional_committee,
    constitutional_committee_under_no_confidence, hard_fork,
    protocol_parameters_security_group,
  ]: List<Data> = param

  StakePoolOperatorVotingThresholds {
    motion_of_no_confidence: into_rational(motion_of_no_confidence),
    constitutional_committee: ConstitutionalCommitteeThresholds {
      default: into_rational(constitutional_committee),
      under_no_confidence: into_rational(
        constitutional_committee_under_no_confidence,
      ),
    },
    hard_fork: into_rational(hard_fork),
    protocol_parameters: ProtocolParametersThresholds {
      security_group: into_rational(protocol_parameters_security_group),
      network_group: Void,
      economic_group: Void,
      technical_group: Void,
      governance_group: Void,
    },
  }
}

fn into_drep_voting_thresholds(
  param: Data,
) -> DelegateRepresentativeVotingThresholds {
  expect [
    motion_of_no_confidence, constitutional_committee,
    constitutional_committee_under_no_confidence, constitution, hard_fork,
    protocol_parameters_network_group, protocol_parameters_economic_group,
    protocol_parameters_technical_group, protocol_parameters_governance_group,
    treasury_withdrawal,
  ]: List<Data> = param

  DelegateRepresentativeVotingThresholds {
    motion_of_no_confidence: into_rational(motion_of_no_confidence),
    constitutional_committee: ConstitutionalCommitteeThresholds {
      default: into_rational(constitutional_committee),
      under_no_confidence: into_rational(
        constitutional_committee_under_no_confidence,
      ),
    },
    constitution: into_rational(constitution),
    hard_fork: into_rational(hard_fork),
    protocol_parameters: ProtocolParametersThresholds {
      security_group: Void,
      network_group: into_rational(protocol_parameters_network_group),
      economic_group: into_rational(protocol_parameters_economic_group),
      technical_group: into_rational(protocol_parameters_technical_group),
      governance_group: into_rational(protocol_parameters_governance_group),
    },
    treasury_withdrawal: into_rational(treasury_withdrawal),
  }
}
