use aiken/primitive/bytearray
use cardano/address.{Credential, Script, VerificationKey}

pub fn compare(left: Credential, right: Credential) -> Ordering {
  when left is {
    Script(left) ->
      when right is {
        Script(right) -> bytearray.compare(left, right)
        _ -> Less
      }
    VerificationKey(left) ->
      when right is {
        Script(_) -> Greater
        VerificationKey(right) -> bytearray.compare(left, right)
      }
  }
}

test compare_matrix() {
  and {
    (compare(Script(""), Script("")) == Equal)?,
    (compare(VerificationKey(""), VerificationKey("")) == Equal)?,
    (compare(Script(""), VerificationKey("")) == Less)?,
    (compare(VerificationKey(""), Script("")) == Greater)?,
    (compare(Script("01"), Script("02")) == Less)?,
    (compare(Script("02"), Script("01")) == Greater)?,
    (compare(VerificationKey("01"), VerificationKey("02")) == Less)?,
    (compare(VerificationKey("02"), VerificationKey("01")) == Greater)?,
  }
}
