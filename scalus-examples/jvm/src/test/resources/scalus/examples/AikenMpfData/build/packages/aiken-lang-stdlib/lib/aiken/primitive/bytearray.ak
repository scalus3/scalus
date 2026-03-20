use aiken/builtin
use aiken/math
use aiken/option

pub type Byte =
  Int

// ## Constructing

/// Encode an integer value as a Big-Endian (most-significant bytes first) `ByteArray`.
/// The size is the expected size in number of bytes.
///
/// > [!IMPORTANT]
/// > This function fails (i.e. halts the program) if the value cannot fit in the given size. When the
/// > size is _too large_, the array is left-padded with zeroes.
///
/// ```aiken
/// bytearray.from_int_big_endian(1_000_000, 3) == #"0f4240"
/// bytearray.from_int_big_endian(1_000_000, 5) == #"00000f4240"
/// bytearray.from_int_big_endian(0, 8) == #"0000000000000000"
/// bytearray.from_int_big_endian(1_000_000, 1) => 💥
/// ```
pub fn from_int_big_endian(self: Int, size: Int) -> ByteArray {
  builtin.integer_to_bytearray(True, size, self)
}

test from_int_big_endian_1() {
  from_int_big_endian(1_000_000, 3) == #"0f4240"
}

test from_int_big_endian_2() {
  from_int_big_endian(1_000_000, 5) == #"00000f4240"
}

test from_int_big_endian_3() {
  from_int_big_endian(0, 8) == #"0000000000000000"
}

test from_int_big_endian_4() fail {
  from_int_big_endian(1_000_000, 1) == #"40"
}

/// Encode an integer value as a Little-Endian (least-significant bytes first) `ByteArray`.
/// The size is the expected size in number of bytes.
///
/// > [!IMPORTANT]
/// > This function fails (i.e. halts the program) if the value cannot fit in the given size. When the
/// > size is _too large_, the array is right-padded with zeroes.
///
/// ```aiken
/// bytearray.from_int_little_endian(1_000_000, 3) == #"40420f"
/// bytearray.from_int_little_endian(1_000_000, 5) == #"40420f0000"
/// bytearray.from_int_little_endian(0, 8) == #"0000000000000000"
/// bytearray.from_int_little_endian(1_000_000, 1) => 💥
/// ```
pub fn from_int_little_endian(self: Int, size: Int) -> ByteArray {
  builtin.integer_to_bytearray(False, size, self)
}

test from_int_little_endian_1() {
  from_int_little_endian(1_000_000, 3) == #"40420f"
}

test from_int_little_endian_2() {
  from_int_little_endian(1_000_000, 5) == #"40420f0000"
}

test from_int_little_endian_3() {
  from_int_little_endian(0, 8) == #"0000000000000000"
}

test from_int_little_endian_4() fail {
  from_int_little_endian(1_000_000, 1) == #"40"
}

/// Convert a `String` into a `ByteArray`.
///
/// ```aiken
/// bytearray.from_string(@"ABC") == #"414243"
/// ```
pub fn from_string(str: String) -> ByteArray {
  builtin.encode_utf8(str)
}

test from_string_1() {
  from_string(@"") == ""
}

test from_string_2() {
  from_string(@"ABC") == #"414243"
}

/// Add a byte element in front of a `ByteArray`. When the given byte is
/// greater than 255, it wraps-around. **PlutusV2 behavior** So 256 is mapped to 0, 257 to 1, and so
/// forth.
/// In PlutusV3 this will error instead of wrapping around.
///
/// ```aiken
/// bytearray.push(#"", 0) == #"00"
/// bytearray.push(#"0203", 1) == #"010203"
/// bytearray.push(#"0203", 257) == #"010203"
/// ```
pub fn push(self: ByteArray, byte: Byte) -> ByteArray {
  builtin.cons_bytearray(byte, self)
}

test push_1() {
  push(#[], 0) == #[0]
}

test push_2() {
  push(#[2, 3], 1) == #[1, 2, 3]
}

test push_3() fail {
  let x = 257
  push(#[2, 3], x) == #[1, 2, 3]
}

// ## Inspecting

/// Get the `Byte` at the given index, or crash.
///
/// > [!WARNING]
/// > This functions fails (i.e. halts the program) if there's no byte at the given index.
pub fn at(self: ByteArray, index: Int) -> Byte {
  builtin.index_bytearray(self, index)
}

/// Search the start and end positions of a sub-array in a `ByteArray`.
///
/// ```aiken
/// bytearray.index_of("Hello, World!", "World") == Some((7, 11))
/// bytearray.index_of("Hello, World!", "foo") == None
/// bytearray.index_of("Hello, World!", "!") == Some((12, 12))
/// bytearray.index_of("Hello, World!", "o") == Some((4, 4))
/// bytearray.index_of("Hello, World!", "Hello, World!") == Some((0, 12))
/// ```
pub fn index_of(self: ByteArray, bytes: ByteArray) -> Option<(Int, Int)> {
  let offset = length(bytes)

  do_index_of(self, bytes, 0, offset, length(self))
    |> option.map(fn(ix) { (ix, ix + offset - 1) })
}

fn do_index_of(
  self: ByteArray,
  bytes: ByteArray,
  cursor: Int,
  offset: Int,
  size: Int,
) -> Option<Int> {
  if cursor + offset > size {
    None
  } else {
    if builtin.slice_bytearray(cursor, offset, self) == bytes {
      Some(cursor)
    } else {
      do_index_of(self, bytes, cursor + 1, offset, size)
    }
  }
}

test index_of_1() {
  index_of("Hello, World!", "World") == Some((7, 11))
}

test index_of_2() {
  index_of("Hello, World!", "foo") == None
}

test index_of_3() {
  index_of("Hello, World!", "!") == Some((12, 12))
}

test index_of_4() {
  index_of("Hello, World!", "o") == Some((4, 4))
}

test index_of_5() {
  index_of("Hello, World!", "Hello, World!") == Some((0, 12))
}

/// Returns `True` when the given `ByteArray` is empty.
///
/// ```aiken
/// bytearray.is_empty(#"") == True
/// bytearray.is_empty(#"00ff") == False
/// ```
pub fn is_empty(self: ByteArray) -> Bool {
  builtin.length_of_bytearray(self) == 0
}

test is_empty_1() {
  is_empty(#"") == True
}

test is_empty_2() {
  is_empty(#"01") == False
}

/// Returns the number of bytes in a `ByteArray`.
///
/// ```aiken
/// bytearray.length(#[1, 2, 3]) == 3
/// ```
pub fn length(self: ByteArray) -> Int {
  builtin.length_of_bytearray(self)
}

test length_1() {
  length(#"") == 0
}

test length_2() {
  length(#"010203") == 3
}

/// Checks whether a bit (Most-Significant-Bit first) is set in the given 'ByteArray'.
///
/// For example, consider the following bytearray: `#"8b765f"`. It can also be written as the
/// following bits sequence:
///
/// `8`    | `b`    | `7`    | `6`    | `5`    | `f`
/// ---    | ---    | ---    | ---    | ---    | ---
/// `1000` | `1011` | `0111` | `0110` | `0101` | `1111`
///
/// And thus, we have:
///
/// ```aiken
/// test_bit(#"8b765f", 0) == True
/// test_bit(#"8b765f", 1) == False
/// test_bit(#"8b765f", 2) == False
/// test_bit(#"8b765f", 3) == False
/// test_bit(#"8b765f", 7) == True
/// test_bit(#"8b765f", 8) == False
/// test_bit(#"8b765f", 20) == True
/// test_bit(#"8b765f", 21) == True
/// test_bit(#"8b765f", 22) == True
/// test_bit(#"8b765f", 23) == True
/// ```
pub fn test_bit(self: ByteArray, ix: Int) -> Bool {
  builtin.less_than_equals_bytearray(
    #[128],
    builtin.cons_bytearray(
      builtin.index_bytearray(self, ix / 8) * math.pow2(ix % 8) % 256,
      "",
    ),
  )
}

test test_bit_0() {
  test_bit(#"8b765f", 0)
}

test test_bit_1() {
  !test_bit(#"8b765f", 1)
}

test test_bit_2() {
  !test_bit(#"8b765f", 2)
}

test test_bit_3() {
  !test_bit(#"8b765f", 3)
}

test test_bit_7() {
  test_bit(#"8b765f", 7)
}

test test_bit_8() {
  !test_bit(#"8b765f", 8)
}

test test_bit_20_21_22_23() {
  and {
    test_bit(#"8b765f", 20),
    test_bit(#"8b765f", 21),
    test_bit(#"8b765f", 22),
    test_bit(#"8b765f", 23),
  }
}

// ## Modifying

/// Returns the suffix of a `ByteArray` after `n` elements.
///
/// ```aiken
/// bytearray.drop(#[1, 2, 3], n: 2) == #[3]
/// ```
pub fn drop(self: ByteArray, n: Int) -> ByteArray {
  builtin.slice_bytearray(n, builtin.length_of_bytearray(self) - n, self)
}

test drop_1() {
  let x = #"01020304050607"
  drop(x, 2) == #"0304050607"
}

test drop_2() {
  let x = #"01020304050607"
  drop(x, 0) == x
}

test drop_3() {
  let x = #"01"
  drop(x, 1) == #""
}

test drop_4() {
  let x = #""
  drop(x, 2) == #""
}

/// Extract a `ByteArray` as a slice of another `ByteArray`.
///
/// Indexes are 0-based and inclusive.
///
/// ```aiken
/// bytearray.slice(#[0, 1, 2, 3, 4, 5, 6], start: 1, end: 3) == #[1, 2, 3]
/// ```
pub fn slice(self: ByteArray, start: Int, end: Int) -> ByteArray {
  builtin.slice_bytearray(start, end - start + 1, self)
}

test slice_1() {
  slice(#"", 1, 2) == #""
}

test slice_2() {
  slice(#"010203", 1, 2) == #"0203"
}

test slice_3() {
  slice(#"010203", 0, 42) == #"010203"
}

test slice_4() {
  slice(#[0, 1, 2, 3, 4], 0, 3) == #[0, 1, 2, 3]
}

test slice_5() {
  slice(#[0, 1, 2, 3, 4], 1, 2) == #[1, 2]
}

/// Returns the n-length prefix of a `ByteArray`.
///
/// ```aiken
/// bytearray.take(#[1, 2, 3], n: 2) == #[1, 2]
/// ```
pub fn take(self: ByteArray, n: Int) -> ByteArray {
  builtin.slice_bytearray(0, n, self)
}

test take_1() {
  let x = #"01020304050607"
  take(x, 2) == #"0102"
}

test take_2() {
  let x = #"01020304050607"
  take(x, 0) == #""
}

test take_3() {
  let x = #"01"
  take(x, 1) == x
}

test take_4() {
  let x = #"010203"
  take(x, 0) == #""
}

// ## Combining

/// Combine two `ByteArray` together.
///
/// ```aiken
/// bytearray.concat(left: #[1, 2, 3], right: #[4, 5, 6]) == #[1, 2, 3, 4, 5, 6]
/// ```
pub fn concat(left: ByteArray, right: ByteArray) -> ByteArray {
  builtin.append_bytearray(left, right)
}

test concat_1() {
  concat(#"", #"") == #""
}

test concat_2() {
  concat(#"", #"01") == #"01"
}

test concat_3() {
  concat(#"0102", #"") == #"0102"
}

test concat_4() {
  concat(#"0102", #"0304") == #"01020304"
}

/// Compare two bytearrays lexicographically.
///
/// ```aiken
/// bytearray.compare(#"00", #"FF") == Less
/// bytearray.compare(#"42", #"42") == Equal
/// bytearray.compare(#"FF", #"00") == Greater
/// ```
pub fn compare(left: ByteArray, right: ByteArray) -> Ordering {
  if builtin.less_than_bytearray(left, right) {
    Less
  } else if builtin.equals_bytearray(left, right) {
    Equal
  } else {
    Greater
  }
}

// ## Transforming

/// Left-fold over bytes of a [`ByteArray`](https://aiken-lang.github.io/prelude/aiken.html#ByteArray). Note that every byte given to the callback function is comprised between 0 and 255.
///
/// ```aiken
/// bytearray.foldl(#"acab", 0, fn(byte, acc) { acc * 256 + byte }) == 44203
/// bytearray.foldl(#[1, 2, 3], #"", flip(bytearray.push)) == #[3, 2, 1]
/// ```
pub fn foldl(
  self: ByteArray,
  zero: result,
  with: fn(Int, result) -> result,
) -> result {
  do_foldl(self, zero, builtin.length_of_bytearray(self), 0, with)
}

fn do_foldl(
  self: ByteArray,
  zero: result,
  len: Int,
  cursor: Int,
  with: fn(Int, result) -> result,
) -> result {
  if cursor == len {
    zero
  } else {
    do_foldl(
      self,
      with(builtin.index_bytearray(self, cursor), zero),
      len,
      cursor + 1,
      with,
    )
  }
}

test foldl_1() {
  foldl(#[], 42, fn(byte, acc) { byte + acc }) == 42
}

test foldl_2() {
  foldl(#"acab", 0, fn(byte, acc) { acc * 256 + byte }) == 44203
}

test foldl_3() {
  foldl(
    #"356cf088720a169dae0ce0bb1df8588944389fa43322f0d6ef4ed8c069bfd405",
    0,
    fn(byte, acc) { acc * 256 + byte },
  ) == 24165060555594911913195642527692216679757672038384202527929620681761931383813
}

test foldl_4() {
  foldl(#[1, 2, 3, 4, 5], #"", flip(push)) == #[5, 4, 3, 2, 1]
}

/// Right-fold over bytes of a [`ByteArray`](https://aiken-lang.github.io/prelude/aiken.html#ByteArray). Note that every byte given to the callback function is comprised between 0 and 255.
///
/// ```aiken
/// bytearray.foldr(#"acab", 0, fn(byte, acc) { acc * 256 + byte }) == 43948
/// bytearray.foldl(#[1, 2, 3], #"", flip(bytearray.push)) == #[1, 2, 3]
/// ```
pub fn foldr(
  self: ByteArray,
  zero: result,
  with: fn(Int, result) -> result,
) -> result {
  do_foldr(self, zero, builtin.length_of_bytearray(self) - 1, with)
}

fn do_foldr(
  self: ByteArray,
  zero: result,
  cursor: Int,
  with: fn(Int, result) -> result,
) -> result {
  if cursor < 0 {
    zero
  } else {
    do_foldr(
      self,
      with(builtin.index_bytearray(self, cursor), zero),
      cursor - 1,
      with,
    )
  }
}

test foldr_1() {
  foldr(#[], 42, fn(byte, acc) { byte + acc }) == 42
}

test foldr_2() {
  foldr(#"acab", 0, fn(byte, acc) { acc * 256 + byte }) == 43948
}

test foldr_3() {
  foldr(#[1, 2, 3, 4, 5], #"", flip(push)) == #[1, 2, 3, 4, 5]
}

/// Reduce bytes in a ByteArray from left to right using the accumulator as left operand.
/// Said differently, this is [`foldl`](#foldl) with callback arguments swapped.
///
/// ```aiken
/// bytearray.reduce(#[1,2,3], #[], bytearray.push) == #[3, 2, 1]
/// ```
pub fn reduce(
  self: ByteArray,
  zero: result,
  with: fn(result, Int) -> result,
) -> result {
  foldl(self, zero, flip(with))
}

test reduce_1() {
  reduce(#[], #[], push) == #[]
}

test reduce_2() {
  reduce(#[1, 2, 3], #[], push) == #[3, 2, 1]
}

/// Interpret a Big-Endian (most-significant bytes first) `ByteArray` as an `Int`.
///
/// ```aiken
/// bytearray.to_int_big_endian(#"0f4240") == 1_000_000
/// bytearray.to_int_big_endian(#"00000f4240") == 1_000_000
/// bytearray.to_int_big_endian(#"0000000000000000") == 0
/// ```
pub fn to_int_big_endian(self: ByteArray) -> Int {
  builtin.bytearray_to_integer(True, self)
}

test to_int_big_endian_1() {
  to_int_big_endian(#"0f4240") == 1_000_000
}

test to_int_big_endian_2() {
  to_int_big_endian(#"00000f4240") == 1_000_000
}

test to_int_big_endian_3() {
  to_int_big_endian(#"0000000000000000") == 0
}

/// Interpret a Little-Endian (least-significant bytes first) `ByteArray` as an `Int`.
///
/// ```aiken
/// bytearray.to_int_big_endian(#"40420f") == 1_000_000
/// bytearray.to_int_big_endian(#"40420f0000") == 1_000_000
/// bytearray.to_int_big_endian(#"0000000000000000") == 0
/// ```
pub fn to_int_little_endian(self: ByteArray) -> Int {
  builtin.bytearray_to_integer(False, self)
}

test to_int_little_endian_1() {
  to_int_little_endian(#"40420f") == 1_000_000
}

test to_int_little_endian_2() {
  to_int_little_endian(#"40420f0000") == 1_000_000
}

test to_int_little_endian_3() {
  to_int_little_endian(#"0000000000000000") == 0
}

/// Convert a `ByteArray` into a `String`.
///
/// > [!WARNING]
/// > This functions fails (i.e. halts the program) if the underlying `ByteArray` isn't UTF-8-encoded. In particular, you cannot convert arbitrary hash digests using this function.
/// >
/// > For converting arbitrary `ByteArray`s, use [bytearray.to_hex](#to_hex).
///
/// ```aiken
/// bytearray.to_string(#"414243") == "ABC"
/// bytearray.to_string(some_hash) => 💥
/// ```
pub fn to_string(self: ByteArray) -> String {
  builtin.decode_utf8(self)
}

test to_string_1() {
  to_string("") == @""
}

test to_string_2() {
  to_string("ABC") == @"ABC"
}

/// Encode a `ByteArray` as a hexidecimal `String`.
///
/// ```aiken
/// bytearray.to_hex("Hello world!") == @"48656c6c6f20776f726c6421"
/// ```
pub fn to_hex(self: ByteArray) -> String {
  self
    |> encode_base16(builtin.length_of_bytearray(self) - 1, "")
    |> builtin.decode_utf8
}

test to_hex_1() {
  to_hex("Hello world!") == @"48656C6C6F20776F726C6421"
}

test to_hex_2() {
  to_hex("The quick brown fox jumps over the lazy dog") == @"54686520717569636B2062726F776E20666F78206A756D7073206F76657220746865206C617A7920646F67"
}

/// Checks whether a `ByteArray` starts with a given prefix.
///
/// ```aiken
/// bytearray.starts_with("Hello, World!", prefix: "Hello") == True
/// bytearray.starts_with("", prefix: "") == True
/// bytearray.starts_with("Hello", prefix: "Hello, World!") == False
/// ```
pub fn starts_with(self: ByteArray, prefix: ByteArray) -> Bool {
  let prefix_length = length(prefix)
  if length(self) < prefix_length {
    False
  } else {
    take(self, prefix_length) == prefix
  }
}

test starts_with_1() {
  starts_with("", "")
}

test starts_with_2() {
  starts_with("Hello, World!", "Hello, World!")
}

test starts_with_3() {
  !starts_with("Hello, World!", "hello")
}

test starts_with_4() {
  !starts_with("", "World")
}

test starts_with_5() {
  starts_with("Hello, World", "Hello")
}

test starts_with_6() {
  !starts_with("foo", "foo_")
}
