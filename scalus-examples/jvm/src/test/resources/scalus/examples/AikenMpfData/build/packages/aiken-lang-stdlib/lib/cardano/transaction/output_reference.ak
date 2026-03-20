use aiken/primitive/bytearray
use aiken/primitive/int
use cardano/transaction.{OutputReference}

pub fn compare(left: OutputReference, right: OutputReference) -> Ordering {
  when bytearray.compare(left.transaction_id, right.transaction_id) is {
    Equal -> int.compare(left.output_index, right.output_index)
    ordering -> ordering
  }
}

test compare_matrix() {
  and {
    (compare(OutputReference("", 0), OutputReference("", 0)) == Equal)?,
    (compare(OutputReference("00", 42), OutputReference("00", 42)) == Equal)?,
    (compare(OutputReference("00", 0), OutputReference("01", 0)) == Less)?,
    (compare(OutputReference("01", 0), OutputReference("00", 0)) == Greater)?,
    (compare(OutputReference("00", 42), OutputReference("01", 14)) == Less)?,
    (compare(OutputReference("01", 14), OutputReference("00", 42)) == Greater)?,
    (compare(OutputReference("", 42), OutputReference("", 14)) == Greater)?,
    (compare(OutputReference("", 14), OutputReference("", 42)) == Less)?,
  }
}
