//// This module is designed for cryptographic operations involving the BLS12-381 elliptic curve, particularly focusing on the G2 group of the curve.
////
//// The key functionalities provided by this module include:
//// - Defining the generator of the G2 group, which is a fixed base point on the elliptic curve used for various cryptographic computations.
//// - Implementing the additive identity (zero) in the G2 group, which plays a crucial role in elliptic curve arithmetic.
//// - Providing functions to compress and decompress points in the G2 group. Compression reduces the size of the point representation, which is useful for efficient storage and transmission. Decompression restores the original point from its compressed form.
//// - Implementing basic arithmetic operations on the points in the G2 group, such as addition and subtraction.
//// - Enabling the exponentiation of a point in the G2 group with a scalar, which is a fundamental operation in elliptic curve cryptography.
//// - Offering a function to hash arbitrary data to a point in the G2 group, a process important in several cryptographic protocols.
////
//// This module ensures that all operations respect the properties of the BLS12-381 curve and the mathematical structure of the G2 group.

use aiken/builtin
use aiken/crypto/bls12_381/scalar.{Scalar}

/// The compressed generator of the G2 group of the BLS12-381 curve.
/// This constant represents a fixed base point on the elliptic curve.
/// Note that flat encoded plutus does not allow for the direct usage of BLS12-381 points.
/// More explicit, any points in plutus data or scripts must be decompressed before usage onchain.
pub const generator: G2Element =
  #<Bls12_381, G2>"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

test generator_1() {
  builtin.bls12_381_g2_scalar_mul(scalar.field_prime, generator) == #<Bls12_381, G2>"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
}

/// Represents the additive identity (zero) in the G2 group.
/// Note that flat encoded plutus does not allow for the direct usage of BLS12-381 points.
/// More explicit, any points in plutus data or scripts must be decompressed before usage onchain.
pub const zero: G2Element =
  #<Bls12_381, G2>"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

test zero_1() {
  and {
    zero == builtin.bls12_381_g2_scalar_mul(scalar.field_prime, generator),
    zero == builtin.bls12_381_g2_scalar_mul(
      scalar.field_prime,
      #<Bls12_381, G2>"9964a9ac2ee28a4dab595ff0970d446373bf46701c5d0b29ce8e1ba995d811a1c7b193c928269192c64ba1fbe4b1940207c251e086b452b920bc72e3cebab46ce672b9b088ca620a471d3b888d9737f6abd165319aa457dbf8835e3d34196051",
    ),
    zero == builtin.bls12_381_g2_scalar_mul(
      scalar.field_prime,
      #<Bls12_381, G2>"a900e25cb53cf1eeb1a82c0c83292937c49c97966351273767a204256a7ef6e95aa391404387075d361e7b13ccd694db03aa73ee0e1bd2c3dd735582b99fdf71696de72e4eda18ae99ea45995f1c9605aa0057008ee9a4da604b5716fb4a345b",
    ),
  }
}

/// Compresses a point in the G2 group into a more compact representation.
/// The compressed representation is the concatenation of two 48-byte strings, corresponding to a modified and complexified `x` coordinate.
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
  builtin.bls12_381_g2_compress(point)
}

test compress_1() {
  let g2 =
    #<Bls12_381, G2>"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
  compress(g2) == #"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
}

/// Decompresses a point in the G2 group from its compressed form.
pub fn decompress(bytes) {
  builtin.bls12_381_g2_uncompress(bytes)
}

test decompress_1() {
  let g2 =
    #<Bls12_381, G2>"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
  generator == g2
}

pub fn equal(left, right) {
  builtin.bls12_381_g2_equal(left, right)
}

test equal_1() {
  equal(
    generator,
    #<Bls12_381, G2>"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
  )
}

/// Adds two points in the G2 group.
pub fn add(left, right) {
  builtin.bls12_381_g2_add(left, right)
}

/// Subtracts one point in the G2 group from another.
pub fn sub(left, right) {
  builtin.bls12_381_g2_add(left, builtin.bls12_381_g2_neg(right))
}

test sub_1() {
  generator == sub(add(generator, generator), generator)
}

/// Exponentiates a point in the G2 group with a `scalar`.
/// This operation is equivalent to the repeated addition of the point with itself `e` times.
pub fn scale(point, e: Scalar) {
  builtin.bls12_381_g2_scalar_mul(scalar.to_int(e), point)
}

test scale_1() {
  expect Some(x) = scalar.new(2)
  builtin.bls12_381_g2_add(generator, generator) == scale(generator, x)
}

/// Hashes arbitrary data to a point in the G2 group.
/// You can use the `domain_separation_tag` parameter to cryptographically separate different uses of the hash function between applications.
pub fn hash_to_group(bytes, domain_separation_tag) {
  builtin.bls12_381_g2_hash_to_group(bytes, domain_separation_tag)
}

test hash_to_group_1() {
  hash_to_group("hello", "world") == #<Bls12_381, G2>"a18486bba1dc8321f4998ed4268c6df8dfa5618dd5c91595844059d517f8104bf8031d3e766f9c99db1d6f58b201ee9614de92fc08f9e5cc3a6cd814e871857cb6e3924e8a4fa48775116c5f158d58ceda63614d62f6b7bc47db798d656969a5"
}
