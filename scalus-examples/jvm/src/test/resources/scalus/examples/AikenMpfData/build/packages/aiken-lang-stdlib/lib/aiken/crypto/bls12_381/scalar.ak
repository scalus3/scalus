//// This module implements arithmetic operations in the scalar field associated with the BLS12-381 elliptic curve.
//// The scalar field, defined over a prime number `q`, is derived from the order of the subgroup G1.
////
//// More explicitly, we have the identity:
////
//// ```aiken
//// builtin.bls12_381_g1_scalar_mul(q, bls12_381_g1_generator) == 1
//// ```
////
//// where,
////
//// ```aiken
//// q = 52435875175126190479447740508185965837690552500527637822603658699938581184513
//// ```
////
//// This module provides functionality for basic arithmetic operations (addition, subtraction, multiplication, division) within this scalar field.
//// Additionally, it includes advanced operations such as exponentiation and calculation of multiplicative inverses, tailored for cryptographic applications.

use aiken/builtin

/// The prime number defining the scalar field of the BLS12-381 curve.
pub const field_prime =
  52435875175126190479447740508185965837690552500527637822603658699938581184513

/// Represents the additive identity (zero) in the `Scalar` field.
pub const zero: Scalar = Scalar(0)

/// Represents the multiplicative identity (one) in the `Scalar` field.
pub const one: Scalar = Scalar(1)

/// Opaque type representing an element of the finite field `Scalar`.
pub opaque type Scalar {
  integer: Int,
}

// ## Constructing

/// Constructs a new `Scalar` element from an integer, ensuring it's within the valid range of the field.
/// Returns `None` if the integer is negative or greater than the prime number defining the field.
pub fn new(n: Int) -> Option<Scalar> {
  if n >= 0 && n < field_prime {
    Some(Scalar(n))
  } else {
    None
  }
}

test new_1() {
  and {
    new(-1) == None,
    new(field_prime) == None,
    new(834884848) == Some(Scalar(834884848)),
  }
}

/// Constructs a new `Scalar` element from a Big-Endian (most-significant bits first) `ByteArray`.
pub fn from_bytearray_big_endian(bytes: ByteArray) -> Option<Scalar> {
  new(builtin.bytearray_to_integer(True, bytes))
}

test from_bytearray_big_endian_1() {
  from_bytearray_big_endian(#"ffff00") == Some(Scalar(16776960))
}

/// Constructs a new `Scalar` element from a Little-Endian (least-significant bits first) `ByteArray`.
pub fn from_bytearray_little_endian(bytes: ByteArray) -> Option<Scalar> {
  new(builtin.bytearray_to_integer(False, bytes))
}

test from_bytearray_little_endian_1() {
  from_bytearray_little_endian(#"ffff00") == Some(Scalar(65535))
}

// ## Modifying

/// Exponentiates an `Scalar` element by a non-negative integer exponent, using repeated squaring.
/// Note that this function returns `scalar.zero` for negative exponents.
/// A dedicated builtin function for this is in the making, see CIP 109.
pub fn scale(self: Scalar, e: Int) -> Scalar {
  if e < 0 {
    zero
  } else if e == 0 {
    one
  } else if e % 2 == 0 {
    scale(mul(self, self), e / 2)
  } else {
    mul(self, scale(mul(self, self), ( e - 1 ) / 2))
  }
}

test scale_1() {
  and {
    scale(Scalar(834884848), -1) == zero,
    scale(Scalar(834884848), 0) == one,
    scale(Scalar(834884848), 1) == Scalar(834884848),
    scale(Scalar(834884848), 2) == Scalar(697032709419983104),
    scale(Scalar(834884848), 3) == Scalar(581942047655130761945608192),
    scale(Scalar(field_prime - 4), 200) == Scalar(
      12843927705572658539565969578937286576443167978938369866871449552629978143484,
    ),
  }
}

/// A faster version of `scale` for the case where the exponent is a power of two.
/// That is, the exponent `e = 2^k` for some non-negative integer `k`. Which is used alot in zk-SNARKs.
pub fn scale2(self: Scalar, k: Int) -> Scalar {
  if k < 0 {
    zero
  } else {
    do_scale2(self, k)
  }
}

fn do_scale2(self: Scalar, k: Int) -> Scalar {
  if k == 0 {
    self
  } else {
    do_scale2(mul(self, self), k - 1)
  }
}

test scale2_1() {
  and {
    scale2(Scalar(834884848), -1) == zero,
    scale2(Scalar(834884848), 0) == scale(Scalar(834884848), 1),
    scale2(Scalar(834884848), 1) == scale(Scalar(834884848), 2),
    scale2(Scalar(834884848), 2) == scale(Scalar(834884848), 4),
    scale2(Scalar(834884848), 3) == scale(Scalar(834884848), 8),
    scale2(Scalar(834884848), 4) == scale(Scalar(834884848), 16),
  }
}

// ## Combining

/// Adds two `Scalar` elements, ensuring the result stays within the finite field range.
pub fn add(left: Scalar, right: Scalar) -> Scalar {
  Scalar(( left.integer + right.integer ) % field_prime)
}

test add_1() {
  and {
    (add(Scalar(834884848), Scalar(834884848)) == Scalar(1669769696))?,
    (add(Scalar(field_prime - 1), Scalar(1)) == Scalar(0))?,
    (add(Scalar(3), Scalar(field_prime)) == Scalar(3))?,
  }
}

/// Divides one `Scalar` element by another, returning `None` if the divisor is zero.
pub fn div(left: Scalar, right: Scalar) -> Option<Scalar> {
  if right == zero {
    None
  } else {
    Some(mul(left, scale(right, field_prime - 2)))
  }
}

test div_1() {
  and {
    div(Scalar(834884848), Scalar(834884848)) == Some(Scalar(1)),
    div(Scalar(834884848), zero) == None,
    div(Scalar(field_prime - 1), Scalar(2)) == Some(
      Scalar(
        26217937587563095239723870254092982918845276250263818911301829349969290592256,
      ),
    ),
  }
}

/// Multiplies two `Scalar` elements, with the result constrained within the finite field.
pub fn mul(left: Scalar, right: Scalar) -> Scalar {
  Scalar(left.integer * right.integer % field_prime)
}

test mul_1() {
  and {
    mul(Scalar(834884848), Scalar(834884848)) == Scalar(697032709419983104),
    mul(zero, Scalar(834884848)) == zero,
    mul(Scalar(field_prime - 1), Scalar(2)) == Scalar(
      52435875175126190479447740508185965837690552500527637822603658699938581184511,
    ),
  }
}

/// Calculates the additive inverse of a `Scalar` element.
pub fn neg(self: Scalar) -> Scalar {
  // this is basicly sub(zero, self), but more efficient as it saves one modulo operation
  if self.integer == 0 {
    self
  } else {
    Scalar(field_prime - self.integer)
  }
}

test neg_1() {
  and {
    neg(Scalar(834884848)) == Scalar(
      52435875175126190479447740508185965837690552500527637822603658699937746299665,
    ),
    neg(zero) == zero,
    neg(one) == Scalar(field_prime - 1),
  }
}

/// Calculates the multiplicative inverse of an `Scalar` element, returning `None` if the element is zero.
pub fn recip(self: Scalar) -> Option<Scalar> {
  div(one, self)
}

test recip_1() {
  and {
    recip(Scalar(834884848)) == Some(
      Scalar(
        35891248691642227249400403463796410930702563777316955162085759263735363466421,
      ),
    ),
    recip(zero) == None,
  }
}

/// Subtracts one `Scalar` element from another, with the result wrapped within the finite field range.
pub fn sub(left: Scalar, right: Scalar) -> Scalar {
  Scalar(( left.integer - right.integer ) % field_prime)
}

test sub_1() {
  and {
    (sub(Scalar(834884848), Scalar(834884848)) == zero)?,
    (sub(zero, Scalar(5)) == Scalar(field_prime - 5))?,
  }
}

// ## Transforming

/// Converts a `Scalar` element back to its integer representation.
pub fn to_int(self: Scalar) -> Int {
  self.integer
}

test to_int_1() {
  to_int(Scalar(834884848)) == 834884848
}

/// Converts a `Scalar` element to a Big-Endian (most-significant bits first) `ByteArray`.
pub fn to_bytearray_big_endian(self: Scalar, size: Int) -> ByteArray {
  builtin.integer_to_bytearray(True, size, self.integer)
}

/// Converts a `Scalar` element to a Little-Endian (least-significant bits first) `ByteArray`.
pub fn to_bytearray_little_endian(self: Scalar, size: Int) -> ByteArray {
  builtin.integer_to_bytearray(False, size, self.integer)
}

test to_bytearray_1() {
  to_bytearray_big_endian(Scalar(16777215), 3) == #"ffffff"
}
