//// A testing framework for running valid and invalid scenarii on a validator. The
//// framework instruments the generation and execution of arbitrarily generated test
//// scenarii working at the transaction-level for maximum reproducibility.
////
//// A full tutorial is coming soon. For now, see examples:
////
//// - [pragma-org/amaru-treasury](https://github.com/pragma-org/amaru-treasury/blob/main/validators)
//// - [mpizenberg/cardano-badges](https://github.com/mpizenberg/cardano-badges/tree/main/aiken-badges/validators/check_badges.tests.ak)
//// - [cardanosolutions/zhuli](https://github.com/CardanoSolutions/zhuli/blob/main/validators/zhuli.test.ak)

use aiken/cbor
use aiken/collection/dict.{Dict}
use aiken/collection/list.{for_each}
use aiken/collection/pairs
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/fuzz.{and_then, byte, constant as return, map}
use aiken/primitive/bytearray
use aiken/primitive/string
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId}
use cardano/certificate.{
  AuthorizeConstitutionalCommitteeProxy, Certificate, DelegateCredential,
  RegisterAndDelegateCredential, RegisterDelegateRepresentative,
  RetireFromConstitutionalCommittee, UnregisterCredential,
  UnregisterDelegateRepresentative, UpdateDelegateRepresentative,
}
use cardano/governance.{
  ConstitutionalCommitteeMember, DelegateRepresentative, Voter,
}
use cardano/script_context.{ScriptContext, ScriptInfo}
use cardano/transaction.{
  DatumHash, InlineDatum, Input, Mint, NoDatum, OutputReference, Publish,
  ScriptPurpose, Spend, Transaction, Vote, Withdraw,
}

const max_tries: Int = 50

const max_byte: Int = 255

pub type Label =
  String

pub type Scenario<st> {
  Done
  Step(List<Label>, st, Transaction)
}

// ## Running

/// Run a given scenario for a given set of handlers. See also: [`ok`](#ok) and
/// [`ko`](#ko) for generating scenarii.
///
/// Any missing handler from the validator can be plucked using one of the following:
///
/// - [`into_mint_handler`](#into_mint_handler)
/// - [`into_spend_handler`](#into_spend_handler)
/// - [`into_withdraw_handler`](#into_withdraw_handler)
/// - [`into_publish_handler`](#into_publish_handler)
/// - [`into_vote_handler`](#into_vote_handler)
pub fn run(
  scenario: List<Transaction>,
  script: ScriptHash,
  mint: MintHandler,
  spend: SpendHandler,
  withdraw: WithdrawHandler,
  publish: PublishHandler,
  vote: VoteHandler,
) {
  when scenario is {
    [] -> Void
    [head, ..tail] -> {
      expect mint_handlers(head, script, mint)
      expect spend_handlers(head, script, spend)
      expect withdraw_handlers(head, script, withdraw)
      expect publish_handlers(head, script, publish)
      expect vote_handlers(head, script, vote)
      run(tail, script, mint, spend, withdraw, publish, vote)
    }
  }
}

/// Generate only _K.O._ scenarii from an initial state and a stepping function.
///
/// ```aiken
/// test prop_end_to_end_ko((labels, scenario) via scenario.ko(initial_state, step)) {
///   list.for_each(labels, fuzz.label)
///   scenario.run(
///     scenario,
///     my_script,
///     handlers.mint,
///     handlers.spend,
///     scenario.into_withdraw_handler(handlers.else),
///     scenario.into_publish_handler(handlers.else),
///     scenario.into_vote_handler(handlers.else),
///   )
/// }
/// ```
pub fn ko(
  initial_state: st,
  step: fn(st) -> Fuzzer<Scenario<st>>,
) -> Fuzzer<(List<Label>, List<Transaction>)> {
  gen_scenario_repeatedly(False, initial_state, step, 0)
}

/// Generate only _O.K._ scenarii from an initial state and a stepping function.
///
/// ```aiken
/// test prop_end_to_end_ok(scenario via scenario.ok(initial_state, step)) {
///   scenario.run(
///     scenario,
///     my_script,
///     handlers.mint,
///     handlers.spend,
///     scenario.into_withdraw_handler(handlers.else),
///     scenario.into_publish_handler(handlers.else),
///     scenario.into_vote_handler(handlers.else),
///   )
/// }
/// ```
pub fn ok(
  initial_state: st,
  step: fn(st) -> Fuzzer<Scenario<st>>,
) -> Fuzzer<List<Transaction>> {
  map(gen_scenario_repeatedly(True, initial_state, step, 0), fn(x) { x.2nd })
}

// ## Inspecting

pub type Outcome {
  Ok
  Ko
}

/// A generator meant to analyze the scenario generator, to control that it is
/// relatively well-balanced between O.K. and K.O. scenarios.
///
/// ```aiken
/// test prop_scenario_coverage(
///   outcome via scenario.report_coverage(initial_state, step),
/// ) {
///   fuzz.label_when(outcome == scenario.Ok, @"O.K.", @"K.O.")
/// }
/// ```
pub fn report_coverage(
  initial_state: st,
  step: fn(st) -> Fuzzer<Scenario<st>>,
) -> Fuzzer<Outcome> {
  let scenario <- map(gen_scenario(initial_state, step))
  when scenario is {
    ([], []) -> fail @"generated empty scenario?"
    ([], _txs) -> Ok
    (_labels, _txs) -> Ko
  }
}

/// Repeatedly run a K.O. scenario fuzzer to ensure that all labels eventually
/// appear in scenarii. Fails after a while if it takes too long.
///
/// ```aiken
/// const all_failures: List<String> = [ @"...", @"..." ]
///
/// test ko_coverage() {
///   scenario.check_coverage(
///     all_failures,
///     scenario.ko(default_state, step)
///   )
/// }
/// ```
pub fn check_coverage(labels: List<String>, fuzzer: Fuzzer<(List<String>, a)>) {
  let labels = list.map(labels, bytearray.from_string)

  let collected =
    do_check_coverage(
      Seeded { seed: blake2b_256(""), choices: "" },
      fuzzer,
      labels,
      dict.empty,
    )

  when list.difference(dict.keys(collected), labels) is {
    [] -> Void
    unseen_labels -> {
      trace @"scenario yielded more labels than expected:"
      for_each(
        unseen_labels,
        fn(label) {
          trace bytearray.to_string(label)
          Void
        },
      )
      fail
    }
  }
}

fn do_check_coverage(
  prng: PRNG,
  fuzzer: Fuzzer<(List<String>, a)>,
  labels: List<ByteArray>,
  accum: Dict<String, Void>,
) -> Dict<String, Void> {
  expect Seeded { choices, .. } = prng
  if bytearray.length(choices) > 250000 {
    trace @"uncovered labels:"
    for_each(
      labels,
      fn(label) {
        trace bytearray.to_string(label)
        Void
      },
    )
    fail @"took too long to find coverage for all labels"
  } else {
    when labels is {
      [] -> accum
      _ -> {
        expect Some((prng, (seen_labels, _))) = fuzzer(prng)
        do_check_coverage(
          prng,
          fuzzer,
          list.difference(labels, list.map(seen_labels, bytearray.from_string)),
          insert_all(accum, seen_labels),
        )
      }
    }
  }
}

fn insert_all(set: Dict<String, Void>, labels: List<String>) {
  list.foldl(
    labels,
    set,
    fn(label, set) { dict.insert(set, bytearray.from_string(label), Void) },
  )
}

/// A helper for classifying scenarios based on some given predicates.
///
/// ```aiken
/// []
///   |> classify(
///       @"✓ more authorizations than necessary",
///       spent_badges_count + ref_badges_count > list.length(policies),
///     )
///   |> classify(
///       @"✓ at least one spent authorization",
///       spent_badges_count > 0,
///     )
///   |> classify(
///       @"✓ at least one reference authorization",
///       ref_badges_count > 0,
///     )
///   |> classify(
///       @"✓ more than one badge checked",
///       list.length(policies) > 1,
///     )
///   |> for_each(label)
/// ```
pub fn classify(
  labels: List<Label>,
  label: String,
  predicate: Bool,
) -> List<Label> {
  if predicate {
    [label, ..labels]
  } else {
    labels
  }
}

// ### Plumbing

// ------------------------------------------------------------------------ Mint

pub type MintHandler =
  fn(Data, PolicyId, Transaction) -> Bool

/// Turn a `else` handler into a [`MintHandler`](#MintHandler).
pub fn into_mint_handler(fallback: fn(ScriptContext) -> Bool) -> MintHandler {
  fn(redeemer: Data, policy_id: PolicyId, transaction: Transaction) -> Bool {
    fallback(
      ScriptContext {
        transaction,
        redeemer,
        info: ScriptInfo.Minting(policy_id),
      },
    )
  }
}

fn mint_handlers(
  self: Transaction,
  script: ScriptHash,
  mint: MintHandler,
) -> Bool {
  list.foldr(
    assets.policies(self.mint),
    True,
    fn(policy_id, st) {
      if policy_id == script {
        and {
          st,
          expect_redeemer(self.redeemers, Mint(policy_id))
            |> mint(policy_id, self),
        }
      } else {
        st
      }
    },
  )
}

// ----------------------------------------------------------------------- Spend

pub type SpendHandler =
  fn(Option<Data>, Data, OutputReference, Transaction) -> Bool

/// Turn a `else` handler into a [`SpendHandler`](#SpendHandler).
pub fn into_spend_handler(fallback: fn(ScriptContext) -> Bool) -> SpendHandler {
  fn(
    datum: Option<Data>,
    redeemer: Data,
    output: OutputReference,
    transaction: Transaction,
  ) -> Bool {
    fallback(
      ScriptContext {
        transaction,
        redeemer,
        info: ScriptInfo.Spending { output, datum },
      },
    )
  }
}

fn spend_handlers(
  self: Transaction,
  script: ScriptHash,
  spend: fn(Option<Data>, Data, OutputReference, Transaction) -> Bool,
) -> Bool {
  list.foldl(
    self.inputs,
    True,
    fn(input, st) {
      if input.output.address.payment_credential == Script(script) {
        and {
          st,
          {
            let redeemer =
              expect_redeemer(self.redeemers, Spend(input.output_reference))

            let datum =
              when input.output.datum is {
                NoDatum -> None
                DatumHash(digest) -> dict.get(self.datums, digest)
                InlineDatum(datum) -> Some(datum)
              }

            spend(datum, redeemer, input.output_reference, self)
          },
        }
      } else {
        st
      }
    },
  )
}

// -------------------------------------------------------------------- Withdraw

pub type WithdrawHandler =
  fn(Data, Credential, Transaction) -> Bool

/// Turn a `else` handler into a [`WithdrawHandler`](#WithdrawHandler).
pub fn into_withdraw_handler(
  fallback: fn(ScriptContext) -> Bool,
) -> WithdrawHandler {
  fn(redeemer: Data, account: Credential, transaction: Transaction) -> Bool {
    fallback(
      ScriptContext {
        transaction,
        redeemer,
        info: ScriptInfo.Withdrawing(account),
      },
    )
  }
}

fn withdraw_handlers(
  self: Transaction,
  script: ScriptHash,
  withdraw: fn(Data, Credential, Transaction) -> Bool,
) -> Bool {
  pairs.foldr(
    self.withdrawals,
    True,
    fn(account, _amount, st) {
      if account == Script(script) {
        and {
          st,
          expect_redeemer(self.redeemers, Withdraw(account))
            |> withdraw(account, self),
        }
      } else {
        st
      }
    },
  )
}

// --------------------------------------------------------------------- Publish

pub type PublishHandler =
  fn(Data, Certificate, Transaction) -> Bool

/// Turn a `else` handler into a [`PublishHandler`](#PublishHandler).
pub fn into_publish_handler(
  fallback: fn(ScriptContext) -> Bool,
) -> PublishHandler {
  fn(redeemer: Data, certificate: Certificate, transaction: Transaction) -> Bool {
    // NOTE: This function is only called by the runner when the certificate is already found.
    expect Some(at) =
      transaction.certificates
        |> list.index_of(certificate)
    fallback(
      ScriptContext {
        transaction,
        redeemer,
        info: ScriptInfo.Publishing { at, certificate },
      },
    )
  }
}

fn publish_handlers(
  self: Transaction,
  script: ScriptHash,
  publish: fn(Data, Certificate, Transaction) -> Bool,
) -> Bool {
  list.indexed_foldr(
    self.certificates,
    True,
    fn(ix, certificate, st) {
      when certificate is {
        UnregisterCredential { credential, .. } |
        DelegateCredential { credential, .. } |
        RegisterAndDelegateCredential { credential, .. } |
        RegisterDelegateRepresentative {
          delegate_representative: credential,
          ..
        } |
        UpdateDelegateRepresentative { delegate_representative: credential } |
        UnregisterDelegateRepresentative {
          delegate_representative: credential,
          ..
        } |
        AuthorizeConstitutionalCommitteeProxy {
          constitutional_committee_member: credential,
          ..
        } |
        RetireFromConstitutionalCommittee {
          constitutional_committee_member: credential,
        } ->
          if credential == Script(script) {
            and {
              st,
              expect_redeemer(self.redeemers, Publish(ix, certificate))
                |> publish(certificate, self),
            }
          } else {
            st
          }

        _ -> st
      }
    },
  )
}

// ------------------------------------------------------------------------ Vote

pub type VoteHandler =
  fn(Data, Voter, Transaction) -> Bool

/// Turn a `else` handler into a [`VoteHandler`](#VoteHandler).
pub fn into_vote_handler(fallback: fn(ScriptContext) -> Bool) -> VoteHandler {
  fn(redeemer: Data, voter: Voter, transaction: Transaction) -> Bool {
    fallback(
      ScriptContext { transaction, redeemer, info: ScriptInfo.Voting(voter) },
    )
  }
}

fn vote_handlers(
  self: Transaction,
  script: ScriptHash,
  vote: fn(Data, Voter, Transaction) -> Bool,
) -> Bool {
  pairs.foldr(
    self.votes,
    True,
    fn(voter, _vote, st) {
      let relevant = or {
          voter == DelegateRepresentative(Script(script)),
          voter == ConstitutionalCommitteeMember(Script(script)),
        }

      if relevant {
        and {
          st,
          expect_redeemer(self.redeemers, Vote(voter))
            |> vote(voter, self),
        }
      } else {
        st
      }
    },
  )
}

// ## Fuzzing

/// Fork between two scenarios using with a frequency proportional to the given
/// pivot.
///
/// The `pivot` must be value between $0$ and $255$ which represents the
/// probability of NOT forking the baseline scenario into a branch.
///
/// It needs to be high enough to have a high probability chance of
/// generating valid scenarios despite many forks, but small enough that forks
/// still occur. Use [`report_coverage`](#report_coverage) to analyze your scenarios
/// and tweak the weights accordingly.
///
/// Ideally, if $f$ is the total number of forks one wants:
///
/// $$
/// \frac{255 - pivot}{255}^{f} \approx 0.5
/// $$
pub fn fork(
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch: fn() -> Fuzzer<a>,
) -> Fuzzer<a> {
  fork_and_then(pivot, baseline, branch, return)
}

/// Like [`fork`](#fork), but take a continuation immediately. This is mostly a
/// convenient way to avoid wrapping all calls to `.fork` inside `and_then(...)`.
///
/// ```aiken
/// let (st, inputs) <- fork_and_then(
///   weights.scenario_input,
///   scenario_baseline(st),
///   scenario_inject_fault(st),
/// )
///
/// // equivalent to
///
/// let (st, inputs) <- fuzz.and_then(fork(
///   weights.scenario_input,
///   scenario_baseline(st),
///   scenario_inject_fault(st),
/// ))
/// ```
pub fn fork_and_then(
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch: fn() -> Fuzzer<a>,
  continue: fn(a) -> Fuzzer<b>,
) -> Fuzzer<b> {
  let toss <- and_then(byte())
  if toss < pivot {
    and_then(baseline(), continue)
  } else {
    and_then(branch(), continue)
  }
}

/// Like [`fork`](#fork), but only forks if the `predicate` is `True`. Runs the
/// `baseline` scenario otherwise.
///
/// Useful to selectively fork into a scenario only when specific conditions are
/// verified.
pub fn fork_if(
  predicate: Bool,
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch: fn() -> Fuzzer<a>,
) -> Fuzzer<a> {
  if predicate {
    fork_and_then(pivot, baseline, branch, return)
  } else {
    baseline()
  }
}

/// Like [`fork_if`](#fork_if) but takes a continuation.
///
/// See also [`fork_and_then`](#fork_and_then).
pub fn fork_if_and_then(
  predicate: Bool,
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch: fn() -> Fuzzer<a>,
  continue: fn(a) -> Fuzzer<b>,
) -> Fuzzer<b> {
  if predicate {
    fork_and_then(pivot, baseline, branch, continue)
  } else {
    and_then(baseline(), continue)
  }
}

/// Like [`fork`](#fork), but allows multiple alternative scenarios with an equal
/// probability.
pub fn fork2(
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
) -> Fuzzer<a> {
  fork2_and_then(pivot, baseline, branch1, branch2, return)
}

/// Like [`fork_and_then`](#fork_and_then), but allows multiple alternative
/// scenarios with an equal probability.
pub fn fork2_and_then(
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  continue: fn(a) -> Fuzzer<b>,
) -> Fuzzer<b> {
  let toss <- and_then(byte())
  let step = ( max_byte - pivot ) / 2
  if toss < pivot {
    and_then(baseline(), continue)
  } else if toss < pivot + step {
    and_then(branch1(), continue)
  } else {
    and_then(branch2(), continue)
  }
}

/// Like [`fork_if`](#fork), but allows multiple alternative scenarios with an equal
/// probability.
pub fn fork2_if(
  predicate: Bool,
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
) -> Fuzzer<a> {
  if predicate {
    fork2_and_then(pivot, baseline, branch1, branch2, return)
  } else {
    baseline()
  }
}

/// Like [`fork_if_and_then`](#fork_if_and_then), but allows multiple
/// alternative scenarios with an equal probability.
pub fn fork2_if_and_then(
  predicate: Bool,
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  continue: fn(a) -> Fuzzer<b>,
) -> Fuzzer<b> {
  if predicate {
    fork2_and_then(pivot, baseline, branch1, branch2, continue)
  } else {
    and_then(baseline(), continue)
  }
}

/// Like [`fork`](#fork), but allows multiple alternative scenarios with an equal
/// probability.
pub fn fork3(
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  branch3: fn() -> Fuzzer<a>,
) -> Fuzzer<a> {
  fork3_and_then(pivot, baseline, branch1, branch2, branch3, return)
}

/// Like [`fork_and_then`](#fork_and_then), but allows multiple alternative
/// scenarios with an equal probability.
pub fn fork3_and_then(
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  branch3: fn() -> Fuzzer<a>,
  continue: fn(a) -> Fuzzer<b>,
) -> Fuzzer<b> {
  let toss <- and_then(byte())
  let step = ( max_byte - pivot ) / 3
  if toss < pivot {
    and_then(baseline(), continue)
  } else if toss < pivot + step {
    and_then(branch1(), continue)
  } else if toss < pivot + 2 * step {
    and_then(branch2(), continue)
  } else {
    and_then(branch3(), continue)
  }
}

/// Like [`fork_if`](#fork), but allows multiple alternative scenarios with an equal
/// probability.
pub fn fork3_if(
  predicate: Bool,
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  branch3: fn() -> Fuzzer<a>,
) -> Fuzzer<a> {
  if predicate {
    fork3_and_then(pivot, baseline, branch1, branch2, branch3, return)
  } else {
    baseline()
  }
}

/// Like [`fork_if_and_then`](#fork_if_and_then), but allows multiple
/// alternative scenarios with an equal probability.
pub fn fork3_if_and_then(
  predicate: Bool,
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  branch3: fn() -> Fuzzer<a>,
  continue: fn(a) -> Fuzzer<b>,
) -> Fuzzer<b> {
  if predicate {
    fork3_and_then(pivot, baseline, branch1, branch2, branch3, continue)
  } else {
    and_then(baseline(), continue)
  }
}

/// Like [`fork`](#fork), but allows multiple alternative scenarios with an equal
/// probability.
pub fn fork4(
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  branch3: fn() -> Fuzzer<a>,
  branch4: fn() -> Fuzzer<a>,
) -> Fuzzer<a> {
  fork4_and_then(pivot, baseline, branch1, branch2, branch3, branch4, return)
}

/// Like [`fork_and_then`](#fork_and_then), but allows multiple alternative
/// scenarios with an equal probability.
pub fn fork4_and_then(
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  branch3: fn() -> Fuzzer<a>,
  branch4: fn() -> Fuzzer<a>,
  continue: fn(a) -> Fuzzer<b>,
) -> Fuzzer<b> {
  let toss <- and_then(byte())
  let step = ( max_byte - pivot ) / 4
  if toss < pivot {
    and_then(baseline(), continue)
  } else if toss < pivot + 2 * step {
    if toss < pivot + step {
      and_then(branch1(), continue)
    } else {
      and_then(branch2(), continue)
    }
  } else {
    if toss < pivot + 3 * step {
      and_then(branch3(), continue)
    } else {
      and_then(branch4(), continue)
    }
  }
}

/// Like [`fork_if`](#fork), but allows multiple alternative scenarios with an equal
/// probability.
pub fn fork4_if(
  predicate: Bool,
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  branch3: fn() -> Fuzzer<a>,
  branch4: fn() -> Fuzzer<a>,
) -> Fuzzer<a> {
  if predicate {
    fork4_and_then(pivot, baseline, branch1, branch2, branch3, branch4, return)
  } else {
    baseline()
  }
}

/// Like [`fork_if_and_then`](#fork_if_and_then), but allows multiple
/// alternative scenarios with an equal probability.
pub fn fork4_if_and_then(
  predicate: Bool,
  pivot: Int,
  baseline: fn() -> Fuzzer<a>,
  branch1: fn() -> Fuzzer<a>,
  branch2: fn() -> Fuzzer<a>,
  branch3: fn() -> Fuzzer<a>,
  branch4: fn() -> Fuzzer<a>,
  continue: fn(a) -> Fuzzer<b>,
) -> Fuzzer<b> {
  if predicate {
    fork4_and_then(
      pivot,
      baseline,
      branch1,
      branch2,
      branch3,
      branch4,
      continue,
    )
  } else {
    and_then(baseline(), continue)
  }
}

// Internal

fn expect_redeemer(
  self: Pairs<ScriptPurpose, Data>,
  purpose: ScriptPurpose,
) -> Data {
  when pairs.get_first(self, purpose) is {
    None ->
      fail string.concat(@"missing redeemer for: ", cbor.diagnostic(purpose))
    Some(redeemer) -> redeemer
  }
}

fn gen_scenario_repeatedly(
  keep_ok: Bool,
  initial_state: st,
  step: fn(st) -> Fuzzer<Scenario<st>>,
  n: Int,
) -> Fuzzer<(List<Label>, List<Transaction>)> {
  if n > max_tries {
    if keep_ok {
      fail @"fail to generate O.K. scenarios: too many K.O. ones"
    } else {
      fail @"fail to generate K.O. scenarios: too many O.K. ones"
    }
  } else {
    let scenario <- and_then(gen_scenario(initial_state, step))
    when scenario is {
      ([], []) -> return(([], []))
      ([], txs) ->
        if keep_ok {
          return(([], txs))
        } else {
          gen_scenario_repeatedly(keep_ok, initial_state, step, n + 1)
        }
      (labels, txs) ->
        if keep_ok {
          gen_scenario_repeatedly(keep_ok, initial_state, step, n + 1)
        } else {
          return((labels, txs))
        }
    }
  }
}

fn gen_scenario(
  st: st,
  step: fn(st) -> Fuzzer<Scenario<st>>,
) -> Fuzzer<(List<Label>, List<Transaction>)> {
  let next <- and_then(step(st))
  when next is {
    Done -> return(([], []))
    Step([], st_new, tx) -> {
      let (labels, txs) <- and_then(gen_scenario(st_new, step))
      return((labels, [tx, ..txs]))
    }
    Step(labels, _, tx) -> return((labels, [tx]))
  }
}
