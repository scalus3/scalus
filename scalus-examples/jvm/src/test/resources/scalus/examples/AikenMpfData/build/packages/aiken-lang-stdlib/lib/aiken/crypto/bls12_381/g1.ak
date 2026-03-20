//// This module is designed for cryptographic operations involving the BLS12-381 elliptic curve, particularly focusing on the G1 group of the curve.
////
//// The key functionalities provided by this module include:
//// - Defining the generator of the G1 group, which is a fixed base point on the elliptic curve used for various cryptographic computations.
//// - Implementing the additive identity (zero) in the G1 group, which plays a crucial role in elliptic curve arithmetic.
//// - Providing functions to compress and decompress points in the G1 group. Compression reduces the size of the point representation, which is useful for efficient storage and transmission. Decompression restores the original point from its compressed form.
//// - Implementing basic arithmetic operations on the points in the G1 group, such as addition and subtraction.
//// - Enabling the exponentiation of a point in the G1 group with a scalar, which is a fundamental operation in elliptic curve cryptography.
//// - Offering a function to hash arbitrary data to a point in the G1 group, a process important in several cryptographic protocols.
////
//// This module ensures that all operations respect the properties of the BLS12-381 curve and the mathematical structure of the G1 group.

use aiken/builtin
use aiken/crypto/bls12_381/scalar.{Scalar}

/// The compressed generator of the G1 group of the BLS12-381 curve.
/// This constant represents a fixed base point on the elliptic curve.
/// Note that flat encoded plutus does not allow for the direct usage of BLS12-381 points.
/// More explicit, any points in plutus data or scripts must be decompressed before usage onchain.
pub const generator: G1Element =
  #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

test generator_1() {
  builtin.bls12_381_g1_scalar_mul(scalar.field_prime, generator) == #<Bls12_381, G1>"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
}

/// Represents the additive identity (zero) in the G1 group.
/// Note that flat encoded plutus does not allow for the direct usage of BLS12-381 points.
/// More explicit, any points in plutus data or scripts must be decompressed before usage onchain.
pub const zero: G1Element =
  #<Bls12_381, G1>"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

test zero_1() {
  and {
    zero == builtin.bls12_381_g1_scalar_mul(scalar.field_prime, generator),
    zero == builtin.bls12_381_g1_scalar_mul(
      scalar.field_prime,
      #<Bls12_381, G1>"88c7e388ee58f1db9a24d7098b01d13634298bebf2d159254975bd450cb0d287fcc622eb71edde8b469a8513551baf1f",
    ),
    zero == builtin.bls12_381_g1_scalar_mul(
      scalar.field_prime,
      #<Bls12_381, G1>"a6ac32e625dc30b8d31bacf5f4c89c27b0388b15f57ae10de8d5cec02dd1f113c9a31077be05ab587ca57a88d34deb75",
    ),
  }
}

/// Compresses a point in the G1 group into a more compact representation.
/// The compressed representation is a 48-byte string, corresponding to a modified `x` coordinate.
/// The leading most significant 3 bits of this string indicate how to reconstruct the `y` coordinate.
///
/// > [!NOTE]
/// > More explicitly via [Zcash's spec](https://github.com/supranational/blst#serialization-format):
/// >
/// > <i>The most-significant three bits of a G1 or G2 encoding should be masked away before the coordinate(s) are interpreted. These bits are used to unambiguously represent the underlying element:
/// >
/// > - The most significant bit, when set, indicates that the point is in compressed form. Otherwise, the point is in uncompressed form.
/// > - The second-most significant bit indicates that the point is at infinity. If this bit is set, the remaining bits of the group element's encoding should be set to zero.
/// > - The third-most significant bit is set if (and only if) this point is in compressed form and it is not the point at infinity and its y-coordinate is the lexicographically largest of the two associated with the encoded x-coordinate.</i>
pub fn compress(point) {
  builtin.bls12_381_g1_compress(point)
}

test compress_1() {
  compress(
    #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
  ) == #"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
}

/// Decompresses a point in the G1 group from its compressed form.
pub fn decompress(bytes) {
  builtin.bls12_381_g1_uncompress(bytes)
}

pub fn equal(left, right) {
  builtin.bls12_381_g1_equal(left, right)
}

test equal_1() {
  equal(generator, generator)
}

/// Adds two points in the G1 group.
pub fn add(left, right) {
  builtin.bls12_381_g1_add(left, right)
}

/// Subtracts one point in the G1 group from another.
pub fn sub(left, right) {
  builtin.bls12_381_g1_add(left, builtin.bls12_381_g1_neg(right))
}

test sub_1() {
  generator == sub(add(generator, generator), generator)
}

/// Exponentiates a point in the G1 group with a `scalar`.
/// This operation is equivalent to the repeated addition of the point with itself `e` times.
pub fn scale(point, e: Scalar) {
  builtin.bls12_381_g1_scalar_mul(scalar.to_int(e), point)
}

test scale_1() {
  expect Some(x) = scalar.new(2)
  builtin.bls12_381_g1_add(generator, generator) == scale(generator, x)
}

/// Hashes arbitrary data to a point in the G1 group.
/// You can use the `domain_separation_tag` parameter to cryptographically separate different uses of the hash function between applications.
pub fn hash_to_group(bytes: ByteArray, domain_separation_tag: ByteArray) {
  builtin.bls12_381_g1_hash_to_group(bytes, domain_separation_tag)
}

test hash_to_group_1() {
  hash_to_group("hello", "world") == #<Bls12_381, G1>"89223b03c629cc6bcbbdccbba46b6679bc6a79db82f2d3bd115899a45a5a38c391587b59d3d1e297f977d1c4ee9e3388"
}
