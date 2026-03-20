use aiken/collection/list
use aiken/fuzz.{label}
use cardano/address.{Address, Inline, Script, VerificationKey}
use cardano/address/credential
use cardano/certificate.{Certificate, Delegate, DelegateRepresentative}
use cardano/fuzz as cardano
use cardano/transaction.{Datum}

test prop_address_distribution(a via cardano.address()) {
  label(
    when a is {
      Address { payment_credential: VerificationKey(_), stake_credential: None } ->
        @"VerificationKey_NoStake"
      Address { payment_credential: Script(_), stake_credential: None } ->
        @"Script_NoStake"
      Address {
        payment_credential: VerificationKey(_),
        stake_credential: Some(Inline(VerificationKey(_))),
      } -> @"VerificationKey_InlineVerificationKey"
      Address {
        payment_credential: VerificationKey(_),
        stake_credential: Some(Inline(Script(_))),
      } -> @"VerificationKey_InlineScript"
      Address {
        payment_credential: Script(_),
        stake_credential: Some(Inline(VerificationKey(_))),
      } -> @"Script_InlineVerificationKey"
      Address {
        payment_credential: Script(_),
        stake_credential: Some(Inline(Script(_))),
      } -> @"Script_InlineScript"
      _ -> fail @"Unexpected Address from address fuzzer"
    },
  )
}

test prop_credential_distribution(credential via cardano.credential()) {
  label(
    when credential is {
      Script(_) -> @"Script"
      VerificationKey(_) -> @"VerificationKey"
    },
  )
}

test prop_certificate_distribution(cert via cardano.certificate()) {
  // TODO: Present to supress a wrong 'unused' warning. To be removed in 1.1.17;
  let cert: Certificate = cert
  label(
    when cert is {
      Certificate.RegisterCredential { .. } -> @"RegisterCredential"
      Certificate.UnregisterCredential { .. } -> @"UnregisterCredential"
      Certificate.DelegateCredential { .. } -> @"DelegateCredential"
      Certificate.RegisterAndDelegateCredential { .. } ->
        @"RegisterAndDelegateCredential"
      Certificate.RegisterDelegateRepresentative { .. } ->
        @"RegisterDelegateRepresentative"
      Certificate.UpdateDelegateRepresentative { .. } ->
        @"UpdateDelegateRepresentative"
      Certificate.UnregisterDelegateRepresentative { .. } ->
        @"UnregisterDelegateRepresentative"
      Certificate.RegisterStakePool { .. } -> @"RegisterStakePool"
      Certificate.RetireStakePool { .. } -> @"RetireStakePool"
      Certificate.AuthorizeConstitutionalCommitteeProxy { .. } ->
        @"AuthorizeConstitutionalCommitteeProxy"
      Certificate.RetireFromConstitutionalCommittee { .. } ->
        @"RetireFromConstitutionalCommittee"
    },
  )
}

test prop_delegate_distribution(delegate via cardano.delegate()) {
  // TODO: Present to supress a wrong 'unused' warning. To be removed in 1.1.17;
  let delegate: Delegate = delegate
  label(
    when delegate is {
      Delegate.DelegateBlockProduction { .. } -> @"DelegateBlockProduction"
      Delegate.DelegateVote { .. } -> @"DelegateVote"
      Delegate.DelegateBoth { .. } -> @"DelegateBoth"
    },
  )
}

test prop_delegate_representative_distribution(
  drep via cardano.delegate_representative(),
) {
  // TODO: Present to supress a wrong 'unused' warning. To be removed in 1.1.17;
  let drep: DelegateRepresentative = drep
  label(
    when drep is {
      DelegateRepresentative.Registered(_) -> @"Registered"
      DelegateRepresentative.AlwaysAbstain -> @"AlwaysAbstain"
      DelegateRepresentative.AlwaysNoConfidence -> @"AlwaysNoConfidence"
    },
  )
}

test prop_datum_distribution(datum via cardano.datum()) {
  // TODO: Present to supress a wrong 'unused' warning. To be removed in 1.1.17;
  let datum: Datum = datum
  label(
    when datum is {
      Datum.NoDatum -> @"NoDatum"
      Datum.DatumHash { .. } -> @"DatumHash"
      Datum.InlineDatum { .. } -> @"InlineDatum"
    },
  )
}

const arbitrary_withdrawals =
  cardano.withdrawals_with(cardano.credential(), fn(_) { fuzz.constant(1) })

test prop_withdrawals_with_in_order(withdrawals via arbitrary_withdrawals) {
  let sorted =
    withdrawals
      |> list.sort(fn(left, right) { credential.compare(left.1st, right.1st) })
  sorted == withdrawals
}
