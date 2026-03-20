use aiken/primitive/bytearray
use aiken/primitive/int
use cardano/address.{Script, VerificationKey}
use cardano/address/credential
use cardano/certificate.{RegisterCredential}
use cardano/governance.{NicePoll, ProposalProcedure, StakePool}
use cardano/governance/voter
use cardano/transaction.{
  Mint, OutputReference, Propose, Publish, ScriptPurpose, Spend, Vote, Withdraw,
}
use cardano/transaction/output_reference

pub fn compare(left: ScriptPurpose, right: ScriptPurpose) -> Ordering {
  when left is {
    Mint(left) ->
      when right is {
        Mint(right) -> bytearray.compare(left, right)
        _ -> Less
      }

    Spend(left) ->
      when right is {
        Spend(right) -> output_reference.compare(left, right)
        Mint(_) -> Greater
        _ -> Less
      }

    Withdraw(left) ->
      when right is {
        Withdraw(right) -> credential.compare(left, right)
        Spend(_) | Mint(_) -> Greater
        _ -> Less
      }

    Publish(left, _) ->
      when right is {
        Publish(right, _) -> int.compare(left, right)
        Spend(_) | Mint(_) | Withdraw(_) -> Greater
        _ -> Less
      }

    Vote(left) ->
      when right is {
        Vote(right) -> voter.compare(left, right)
        Propose(..) -> Less
        _ -> Greater
      }

    Propose(left, _) ->
      when right is {
        Propose(right, _) -> int.compare(left, right)
        _ -> Greater
      }
  }
}

test compare_matrix() {
  let mint0 = Mint("0")
  let mint1 = Mint("1")

  let spend0 = Spend(OutputReference("", 0))
  let spend1 = Spend(OutputReference("", 1))

  let withdraw0 = Withdraw(VerificationKey("0"))
  let withdraw1 = Withdraw(VerificationKey("1"))

  let publish0 = Publish(0, RegisterCredential(Script(""), Never))
  let publish1 = Publish(1, RegisterCredential(Script(""), Never))

  let vote0 = Vote(StakePool("0"))
  let vote1 = Vote(StakePool("1"))

  let propose0 = Propose(0, ProposalProcedure(0, Script(""), NicePoll))
  let propose1 = Propose(1, ProposalProcedure(0, Script(""), NicePoll))

  and {
    (compare(mint0, mint0) == Equal)?,
    (compare(mint0, mint1) == Less)?,
    (compare(mint1, mint0) == Greater)?,
    (compare(mint0, spend0) == Less)?,
    (compare(mint0, withdraw0) == Less)?,
    (compare(mint0, publish0) == Less)?,
    (compare(mint0, vote0) == Less)?,
    (compare(mint0, propose0) == Less)?,
    (compare(spend0, spend0) == Equal)?,
    (compare(spend0, spend1) == Less)?,
    (compare(spend1, spend0) == Greater)?,
    (compare(spend0, mint0) == Greater)?,
    (compare(spend0, withdraw0) == Less)?,
    (compare(spend0, publish0) == Less)?,
    (compare(spend0, vote0) == Less)?,
    (compare(spend0, propose0) == Less)?,
    (compare(withdraw0, withdraw0) == Equal)?,
    (compare(withdraw0, withdraw1) == Less)?,
    (compare(withdraw1, withdraw0) == Greater)?,
    (compare(withdraw0, mint0) == Greater)?,
    (compare(withdraw0, spend0) == Greater)?,
    (compare(withdraw0, publish0) == Less)?,
    (compare(withdraw0, vote0) == Less)?,
    (compare(withdraw0, propose0) == Less)?,
    (compare(publish0, publish0) == Equal)?,
    (compare(publish0, publish1) == Less)?,
    (compare(publish1, publish0) == Greater)?,
    (compare(publish0, mint0) == Greater)?,
    (compare(publish0, spend0) == Greater)?,
    (compare(publish0, withdraw0) == Greater)?,
    (compare(publish0, vote0) == Less)?,
    (compare(publish0, propose0) == Less)?,
    (compare(vote0, vote0) == Equal)?,
    (compare(vote0, vote1) == Less)?,
    (compare(vote1, vote0) == Greater)?,
    (compare(vote0, mint0) == Greater)?,
    (compare(vote0, spend0) == Greater)?,
    (compare(vote0, withdraw0) == Greater)?,
    (compare(vote0, publish0) == Greater)?,
    (compare(vote0, propose0) == Less)?,
    (compare(propose0, propose0) == Equal)?,
    (compare(propose0, propose1) == Less)?,
    (compare(propose1, propose0) == Greater)?,
    (compare(propose0, mint0) == Greater)?,
    (compare(propose0, spend0) == Greater)?,
    (compare(propose0, withdraw0) == Greater)?,
    (compare(propose0, publish0) == Greater)?,
    (compare(propose0, vote0) == Greater)?,
  }
}
