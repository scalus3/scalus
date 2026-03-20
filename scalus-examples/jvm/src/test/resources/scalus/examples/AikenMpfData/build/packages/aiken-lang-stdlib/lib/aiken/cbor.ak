use aiken
use aiken/builtin.{decode_utf8, serialise_data}
use aiken/primitive/bytearray

/// Obtain a String representation of _anything_. This is particularly (and only) useful for tracing
/// and debugging. This function is expensive and should not be used in any production code as it
/// will very likely explodes the validator's budget.
///
/// The output is a [CBOR diagnostic](https://www.rfc-editor.org/rfc/rfc8949#name-diagnostic-notation)
/// of the underlying on-chain binary representation of the data. It's not as
/// easy to read as plain Aiken code, but it is handy for troubleshooting values
/// _at runtime_. Incidentally, getting familiar with reading CBOR diagnostic is
/// a good idea in the Cardano world.
///
/// ```aiken
/// cbor.diagnostic(42) == "42"
/// cbor.diagnostic(#"a1b2") == "h'A1B2'"
/// cbor.diagnostic([1, 2, 3]) == "[_ 1, 2, 3]"
/// cbor.diagnostic([]) == "[]"
/// cbor.diagnostic((1, 2)) == "[_ 1, 2]"
/// cbor.diagnostic((1, #"ff", 3)) == "[_ 1, h'FF', 3]"
/// cbor.diagnostic([(1, #"ff")]) == "{_ 1: h'FF' }"
/// cbor.diagnostic(Some(42)) == "121([_ 42])"
/// cbor.diagnostic(None) == "122([])"
/// ```
pub fn diagnostic(self: Data) -> String {
  aiken.diagnostic(self, #"")
    |> decode_utf8
}

/// Deserialise a [CBOR](https://www.rfc-editor.org/rfc/rfc8949) Data. This is the reverse operation of [serialise](#serialise).
/// In particular, we have the following property:
///
/// ```aiken
/// cbor.deserialise(cbor.serialise(any_data)) == Some(any_data)
/// ```
///
/// > [!CAUTION]
/// > Unfortunately, this function isn't derived from a builtin primitive. It
/// > is therefore an order of magnitude more expensive than its counterpart
/// > and shall be used with care.
/// >
/// > In general, one might prefer avoiding deserialisation unless truly necessary.
/// > Yet, it may come in handy for testing and in rare scenarios.
pub fn deserialise(bytes: ByteArray) -> Option<Data> {
  let length = bytearray.length(bytes)

  let peek =
    fn(offset: Int, callback: fn(Byte) -> Decoder<Data>) -> Decoder<Data> {
      fn(cursor) {
        if 0 >= cursor {
          deserialise_failure
        } else {
          callback(bytearray.at(bytes, length - cursor))(cursor - offset)
        }
      }
    }

  let take =
    fn(n: Int, callback: fn(ByteArray) -> Decoder<Data>) -> Decoder<Data> {
      fn(cursor) {
        if 0 >= cursor {
          deserialise_failure
        } else {
          callback(builtin.slice_bytearray(length - cursor, n, bytes))(
            cursor - n,
          )
        }
      }
    }

  if length == 0 {
    None
  } else {
    let Pair(result, consumed) = decode_data(peek, take)(length)
    if consumed != 0 {
      None
    } else {
      Some(result)
    }
  }
}

/// Serialise any value to binary, encoding using [CBOR](https://www.rfc-editor.org/rfc/rfc8949).
///
/// This is particularly useful in combination with hashing functions, as a way
/// to obtain a byte representation that matches the serialised representation
/// used by the ledger in the context of on-chain code.
///
/// Note that the output matches the output of [`diagnostic`](#diagnostic),
/// though with a different encoding. [`diagnostic`](#diagnostic) is merely a
/// textual representation of the CBOR encoding that is human friendly and
/// useful for debugging.
///
/// ```aiken
/// cbor.serialise(42) == #"182a"
/// cbor.serialise(#"a1b2") == #"42a1b2"
/// cbor.serialise([]) == #"80"
/// cbor.serialise((1, 2)) == #"9f0102ff"
/// cbor.serialise((1, #"ff", 3)) == #"9f0141ff03ff"
/// cbor.serialise([(1, #"ff")]) == #"a10141ff"
/// cbor.serialise(Some(42)) == #"d8799f182aff"
/// cbor.serialise(None) == #"d87a80"
/// ```
pub fn serialise(self: Data) -> ByteArray {
  serialise_data(self)
}

type Byte =
  Int

type Decoder<a> =
  fn(Int) -> Pair<a, Int>

type Peek<a> =
  fn(Int, fn(Byte) -> Decoder<a>) -> Decoder<a>

type Take<a> =
  fn(Int, fn(ByteArray) -> Decoder<a>) -> Decoder<a>

fn return(data: Data) -> Decoder<Data> {
  fn(cursor) { Pair(data, cursor) }
}

const deserialise_failure: Pair<Data, Int> = {
    let empty: Data = ""
    Pair(empty, -1)
  }

const token_begin_bytes = 0x5f

const token_begin_list = 0x9f

const token_begin_map = 0xbf

const token_break = 0xff

fn decode_data(peek: Peek<Data>, take: Take<Data>) -> Decoder<Data> {
  let next <- peek(1)
  let major_type = next / 32
  if major_type <= 2 {
    if major_type == 0 {
      let i <- decode_uint(peek, take, next)
      return(builtin.i_data(i))
    } else if major_type == 1 {
      let i <- decode_uint(peek, take, next - 32)
      return(builtin.i_data(-i - 1))
    } else {
      if next == token_begin_bytes {
        let b <- decode_chunks(peek, take)
        return(builtin.b_data(b))
      } else {
        let b <- decode_bytes(peek, take, next - 64)
        return(builtin.b_data(b))
      }
    }
  } else if major_type == 6 {
    let tag <- decode_uint(peek, take, next - 192)
    let next <- peek(1)
    if tag == 102 {
      fn(_) { deserialise_failure }
    } else {
      let ix =
        if tag >= 1280 {
          tag - 1280 + 7
        } else {
          tag - 121
        }
      if next == token_begin_list {
        let fields <- decode_indefinite(peek, take, decode_data)
        return(builtin.constr_data(ix, fields))
      } else {
        let size <- decode_uint(peek, take, next - 128)
        let fields <- decode_definite(peek, take, decode_data, size)
        return(builtin.constr_data(ix, fields))
      }
    }
  } else if major_type == 4 {
    if next == token_begin_list {
      let xs <- decode_indefinite(peek, take, decode_data)
      return(builtin.list_data(xs))
    } else {
      let size <- decode_uint(peek, take, next - 128)
      let xs <- decode_definite(peek, take, decode_data, size)
      return(builtin.list_data(xs))
    }
  } else if major_type == 5 {
    if next == token_begin_map {
      let xs <- decode_indefinite(peek, take, decode_pair)
      return(builtin.map_data(xs))
    } else {
      let size <- decode_uint(peek, take, next - 160)
      let xs <- decode_definite(peek, take, decode_pair, size)
      return(builtin.map_data(xs))
    }
  } else {
    fn(_) { deserialise_failure }
  }
}

fn decode_pair(peek: Peek<Data>, take: Take<Data>) -> Decoder<Pair<Data, Data>> {
  fn(cursor) {
    let Pair(k, cursor) = decode_data(peek, take)(cursor)
    let Pair(v, cursor) = decode_data(peek, take)(cursor)
    Pair(Pair(k, v), cursor)
  }
}

fn decode_uint(
  peek: Peek<Data>,
  take: Take<Data>,
  header: Int,
  and_then: fn(Int) -> Decoder<Data>,
) -> Decoder<Data> {
  if header < 24 {
    and_then(header)
  } else if header == 24 {
    let payload <- peek(1)
    and_then(payload)
  } else if header < 28 {
    let width = bytearray.at(#[2, 4, 8], header - 25)
    let payload <- take(width)
    and_then(bytearray.to_int_big_endian(payload))
  } else {
    fn(_) { deserialise_failure }
  }
}

fn decode_bytes(
  peek: Peek<Data>,
  take: Take<Data>,
  header: Int,
  and_then: fn(ByteArray) -> Decoder<Data>,
) -> Decoder<Data> {
  let width <- decode_uint(peek, take, header)
  let bytes <- take(width)
  and_then(bytes)
}

fn decode_chunks(
  peek: Peek<Data>,
  take: Take<Data>,
  and_then: fn(ByteArray) -> Decoder<Data>,
) -> Decoder<Data> {
  let next <- peek(1)
  if next == token_break {
    and_then("")
  } else {
    let chunk <- decode_bytes(peek, take, next - 64)
    let chunks <- decode_chunks(peek, take)
    and_then(builtin.append_bytearray(chunk, chunks))
  }
}

fn decode_definite(
  peek: Peek<Data>,
  take: Take<Data>,
  decode_one: fn(Peek<Data>, Take<Data>) -> Decoder<a>,
  size: Int,
  and_then: fn(List<a>) -> Decoder<Data>,
) -> Decoder<Data> {
  if size <= 0 {
    and_then([])
  } else {
    fn(cursor) {
      let Pair(elem, cursor) = decode_one(peek, take)(cursor)
      {
        let elems <- decode_definite(peek, take, decode_one, size - 1)
        and_then([elem, ..elems])
      }(cursor)
    }
  }
}

fn decode_indefinite(
  peek: Peek<Data>,
  take: Take<Data>,
  decode_one: fn(Peek<Data>, Take<Data>) -> Decoder<a>,
  and_then: fn(List<a>) -> Decoder<Data>,
) -> Decoder<Data> {
  let next <- peek(1)
  if next == token_break {
    and_then([])
  } else {
    fn(cursor) {
      let Pair(elem, cursor) = decode_one(peek, take)(cursor + 1)
      {
        let elems <- decode_indefinite(peek, take, decode_one)
        and_then([elem, ..elems])
      }(cursor)
    }
  }
}
