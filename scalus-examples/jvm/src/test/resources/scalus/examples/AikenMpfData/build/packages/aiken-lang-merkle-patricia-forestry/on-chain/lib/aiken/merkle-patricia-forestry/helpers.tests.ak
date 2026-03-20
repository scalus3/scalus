use aiken/builtin.{index_bytearray}
use aiken/fuzz.{both, int_between}
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{
  null_hash, null_hash_2, null_hash_4, null_hash_8,
}

// -----------------------------------------------------------------------------
// --------------------------------------------------------------------- combine
// -----------------------------------------------------------------------------

test combine_null_hash() {
  and {
    combine(null_hash, null_hash) == null_hash_2,
    combine(null_hash_2, null_hash_2) == null_hash_4,
    combine(null_hash_4, null_hash_4) == null_hash_8,
  }
}

// -----------------------------------------------------------------------------
// ---------------------------------------------------------------------- suffix
// -----------------------------------------------------------------------------

test examples_suffix() {
  and {
    suffix(#"abcd456789", 0) == #"ffabcd456789",
    suffix(#"abcd456789", 1) == #"000bcd456789",
    suffix(#"abcd456789", 2) == #"ffcd456789",
    suffix(#"abcd456789", 4) == #"ff456789",
    suffix(#"abcd456789", 5) == #"00056789",
    suffix(#"abcd456789", 10) == #"ff",
  }
}

test prop_suffix_prefix(params via both(fuzz.bytearray(), int_between(0, 31))) {
  let str = suffix(params.1st, params.2nd)
  let head = index_bytearray(str, 0)

  if head == 0 {
    index_bytearray(str, 1) < 16
  } else {
    head == 255
  }
}

// -----------------------------------------------------------------------------
// --------------------------------------------------------------------- nibbles
// -----------------------------------------------------------------------------

test examples_nibbles() {
  and {
    nibbles(#"0123456789", 2, 2) == #[],
    nibbles(#"0123456789", 2, 3) == #[2],
    nibbles(#"0123456789", 4, 8) == #[4, 5, 6, 7],
    nibbles(#"0123456789", 3, 6) == #[3, 4, 5],
    nibbles(#"0123456789", 1, 7) == #[1, 2, 3, 4, 5, 6],
  }
}

// -----------------------------------------------------------------------------
// ---------------------------------------------------------------------- nibble
// -----------------------------------------------------------------------------

test examples_nibble() {
  and {
    nibble(#"ab", 0) == 10,
    nibble(#"ab", 1) == 11,
  }
}

test prop_nearby_nibble(params via both(fuzz.bytearray(), int_between(0, 31))) {
  let (bytes, index) = params

  let msb = nibble(bytes, index * 2)
  let lsb = nibble(bytes, index * 2 + 1)

  msb * 16 + lsb == index_bytearray(bytes, index)
}

test prop_nibble_size(params via both(fuzz.bytearray(), int_between(0, 63))) {
  let (bytes, index) = params
  let digit = nibble(bytes, index)
  and {
    digit >= 0,
    digit <= 15,
  }
}
