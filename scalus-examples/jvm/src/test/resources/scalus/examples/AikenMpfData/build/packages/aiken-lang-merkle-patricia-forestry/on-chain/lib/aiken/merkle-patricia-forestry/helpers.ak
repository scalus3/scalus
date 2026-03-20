//// @hidden
////
//// This module contains internal functions used by the library. They are only
//// exposed for internal testing and module splitting.

use aiken/builtin.{blake2b_256}
use aiken/primitive/bytearray

// -----------------------------------------------------------------------------
// --------------------------------------------------------------------- combine
// -----------------------------------------------------------------------------

pub fn combine(left: ByteArray, right: ByteArray) -> ByteArray {
  blake2b_256(bytearray.concat(left, right))
}

// -----------------------------------------------------------------------------
// ---------------------------------------------------------------------- suffix
// -----------------------------------------------------------------------------

pub fn suffix(path, cursor) {
  if cursor % 2 == 0 {
    bytearray.drop(path, cursor / 2)
      |> bytearray.push(0xff)
  } else {
    bytearray.drop(path, ( cursor + 1 ) / 2)
      |> bytearray.push(nibble(path, cursor))
      |> bytearray.push(0)
  }
}

// -----------------------------------------------------------------------------
// --------------------------------------------------------------------- nibbles
// -----------------------------------------------------------------------------

/// Computes nibbles for a given branch node, from the start and end
/// location of two position in a path.
pub fn nibbles(path: ByteArray, start: Int, end: Int) -> ByteArray {
  if start >= end {
    #[]
  } else {
    bytearray.push(nibbles(path, start + 1, end), nibble(path, start))
  }
}

// -----------------------------------------------------------------------------
// ---------------------------------------------------------------------- nibble
// -----------------------------------------------------------------------------

pub fn nibble(self: ByteArray, index: Int) -> Int {
  if index % 2 == 0 {
    bytearray.at(self, index / 2) / 16
  } else {
    bytearray.at(self, index / 2) % 16
  }
}
