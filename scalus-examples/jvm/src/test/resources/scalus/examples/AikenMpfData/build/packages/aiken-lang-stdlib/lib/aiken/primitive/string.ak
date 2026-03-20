use aiken/builtin.{
  append_bytearray, append_string, decode_utf8, encode_utf8, length_of_bytearray,
}

// ## Constructing

/// Convert a `ByteArray` into a `String`
///
/// > [!WARNING]
/// > This functions fails if the underlying `ByteArray` isn't UTF-8-encoded. In particular, you cannot convert arbitrary hash digests using this function.
/// >
/// > For converting arbitrary `ByteArray`s, use [bytearray.to_hex](./bytearray.html#to_hex).
///
/// ```aiken
/// string.from_bytearray("foo") == @"foo"
/// string.from_bytearray(#"666f6f") == @"foo"
/// string.from_bytearray(some_hash) -> fail
/// ```
pub fn from_bytearray(bytes: ByteArray) -> String {
  decode_utf8(bytes)
}

test from_bytearray_1() {
  from_bytearray(#[]) == @""
}

test from_bytearray_2() {
  from_bytearray(#[65, 66, 67]) == @"ABC"
}

test from_bytearray_3() {
  from_bytearray("ABC") == @"ABC"
}

/// Convert an `Int` to its `String` representation.
///
/// ```aiken
/// string.from_int(42) == @"42"
/// ```
pub fn from_int(n: Int) -> String {
  diagnostic(n, "") |> decode_utf8
}

test from_int_1() {
  from_int(0) == @"0"
}

test from_int_2() {
  from_int(5) == @"5"
}

test from_int_3() {
  from_int(42) == @"42"
}

test from_int_4() {
  from_int(200) == @"200"
}

// ## Combining

/// Combine two `String` together.
///
/// ```aiken
/// string.concat(left: @"Hello", right: @", World!") == @"Hello, World!"
/// ```
pub fn concat(left: String, right: String) -> String {
  append_string(left, right)
}

test concat_1() {
  concat(@"", @"") == @""
}

test concat_2() {
  concat(@"", @"foo") == concat(@"foo", @"")
}

test concat_3() {
  concat(left: @"Hello", right: @", World!") == @"Hello, World!"
}

/// Join a list of strings, separated by a given _delimiter_.
///
/// ```aiken
/// string.join([], @"+") == @""
/// string.join([@"a", @"b", @"c"], @",") == @"a,b,c"
/// ```
pub fn join(list: List<String>, delimiter: String) -> String {
  do_join(list, encode_utf8(delimiter), #"")
    |> decode_utf8
}

fn do_join(xs, delimiter, bytes) {
  when xs is {
    [] -> bytes
    [x, ..rest] ->
      do_join(
        rest,
        delimiter,
        if length_of_bytearray(bytes) == 0 {
          encode_utf8(x)
        } else {
          append_bytearray(bytes, append_bytearray(delimiter, encode_utf8(x)))
        },
      )
  }
}

test join_1() {
  join([], @",") == @""
}

test join_2() {
  join([@"a", @"b", @"c"], @",") == @"a,b,c"
}

// ## Transforming

/// Convert a `String` into a `ByteArray`
///
/// ```aiken
/// string.to_bytearray(@"foo") == "foo"
/// ```
pub fn to_bytearray(self: String) -> ByteArray {
  encode_utf8(self)
}

test to_bytearray_1() {
  to_bytearray(@"") == ""
}

test to_bytearray_2() {
  to_bytearray(@"ABC") == #[65, 66, 67]
}

test to_bytearray_3() {
  to_bytearray(@"ABC") == "ABC"
}
