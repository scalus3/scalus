use aiken/fuzz.{bytearray, list_between}
use aiken/merkle_patricia_forestry/helpers.{combine}
use aiken/merkle_patricia_forestry/merkling.{merkle_4}

// -----------------------------------------------------------------------------
// -------------------------------------------------------------------- merkle_4
// -----------------------------------------------------------------------------

test prop_merkle_4(nodes via list_between(bytearray(), 4, 4)) {
  expect [a, b, c, d] = nodes

  let root = combine(combine(a, b), combine(c, d))

  and {
    merkle_4(0, a, combine(c, d), b) == root,
    merkle_4(1, b, combine(c, d), a) == root,
    merkle_4(2, c, combine(a, b), d) == root,
    merkle_4(3, d, combine(a, b), c) == root,
  }
}
