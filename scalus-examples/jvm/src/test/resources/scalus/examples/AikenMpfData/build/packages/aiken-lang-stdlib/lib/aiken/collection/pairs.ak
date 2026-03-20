//// A module for working with associative lists (a.k.a `Pairs`).
////
//// While any function that works on `List` also work on `Pairs`, this module provides some extra helpers
//// that are specifically tailored to working with associative lists. Fundamentally, a `Pairs<k, v>` is
//// a type-alias to `List<Pair<k, v>>`.
////
//// > [!CAUTION]
//// >
//// > Unlike dictionnaries (a.k.a. [`Dict`](./dict.html#Dict), associative lists make no assumption
//// > about the ordering of elements within the list. As a result, lookup
//// > functions do traverse the entire list when invoked. They are also not _sets_,
//// > and thus allow for duplicate keys. This is reflected in the functions used
//// > to interact with them.

use aiken/builtin
use aiken/primitive/bytearray

// ## Inspecting

/// Get all values in the alist associated with a given key.
///
/// ```aiken
/// pairs.get_all([], "a") == []
/// pairs.get_all([Pair("a", 1)], "a") == [1]
/// pairs.get_all([Pair("a", 1), Pair("b", 2)], "a") == [1]
/// pairs.get_all([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == [1, 3]
/// ```
pub fn get_all(self: Pairs<key, value>, key k: key) -> List<value> {
  when self is {
    [] -> []
    [Pair(k2, v), ..rest] ->
      if k == k2 {
        [v, ..get_all(rest, k)]
      } else {
        get_all(rest, k)
      }
  }
}

test get_all_1() {
  get_all([], "a") == []
}

test get_all_2() {
  get_all([Pair("a", 1)], "a") == [1]
}

test get_all_3() {
  get_all([Pair("a", 1), Pair("b", 2)], "a") == [1]
}

test get_all_4() {
  get_all([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == [1, 3]
}

test get_all_5() {
  get_all([Pair("a", 1), Pair("b", 2), Pair("c", 3)], "d") == []
}

/// Get the value in the alist by its key.
/// If multiple values with the same key exist, only the first one is returned.
///
/// ```aiken
/// pairs.get_first([], "a") == None
/// pairs.get_first([Pair("a", 1)], "a") == Some(1)
/// pairs.get_first([Pair("a", 1), Pair("b", 2)], "a") == Some(1)
/// pairs.get_first([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == Some(1)
/// ```
pub fn get_first(self: Pairs<key, value>, key k: key) -> Option<value> {
  when self is {
    [] -> None
    [Pair(k2, v), ..rest] ->
      if k == k2 {
        Some(v)
      } else {
        get_first(rest, k)
      }
  }
}

test get_first_1() {
  get_first([], "a") == None
}

test get_first_2() {
  get_first([Pair("a", 1)], "a") == Some(1)
}

test get_first_3() {
  get_first([Pair("a", 1), Pair("b", 2)], "a") == Some(1)
}

test get_first_4() {
  get_first([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == Some(1)
}

test get_first_5() {
  get_first([Pair("a", 1), Pair("b", 2), Pair("c", 3)], "d") == None
}

/// Get the value in the alist by its key.
/// If multiple values with the same key exist, only the last one is returned.
///
/// ```aiken
/// pairs.get_last([], "a") == None
/// pairs.get_last([Pair("a", 1)], "a") == Some(1)
/// pairs.get_last([Pair("a", 1), Pair("b", 2)], "a") == Some(1)
/// pairs.get_last([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == Some(3)
/// ```
pub fn get_last(self: Pairs<key, value>, key k: key) -> Option<value> {
  when self is {
    [] -> None
    [Pair(k2, v), ..rest] ->
      if k == k2 {
        when get_last(rest, k) is {
          None -> Some(v)
          some -> some
        }
      } else {
        get_last(rest, k)
      }
  }
}

test get_last_1() {
  get_last([], "a") == None
}

test get_last_2() {
  get_last([Pair("a", 1)], "a") == Some(1)
}

test get_last_3() {
  get_last([Pair("a", 1), Pair("b", 2)], "a") == Some(1)
}

test get_last_4() {
  get_last([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == Some(3)
}

test get_last_5() {
  get_last([Pair("a", 1), Pair("b", 2), Pair("c", 3)], "d") == None
}

/// Finds all keys in the alist associated with a given value.
///
/// ```aiken
/// pairs.find_all([], 1) == []
/// pairs.find_all([Pair("a", 1)], 1) == ["a"]
/// pairs.find_all([Pair("a", 1), Pair("b", 2)], 1) == ["a"]
/// pairs.find_all([Pair("a", 1), Pair("b", 2), Pair("c", 1)], 1) == ["a", "c"]
/// ```
pub fn find_all(self: Pairs<key, value>, v: value) -> List<key> {
  when self is {
    [] -> []
    [Pair(k2, v2), ..rest] ->
      if v == v2 {
        [k2, ..find_all(rest, v)]
      } else {
        find_all(rest, v)
      }
  }
}

test find_all_1() {
  find_all([], "a") == []
}

test find_all_2() {
  find_all([Pair("a", 14)], 14) == ["a"]
}

test find_all_3() {
  find_all([Pair("a", 14)], 42) == []
}

test find_all_4() {
  find_all([Pair("a", 14), Pair("b", 42), Pair("c", 14)], 14) == ["a", "c"]
}

/// Finds the first key in the alist associated with a given value, if any.
///
/// ```aiken
/// pairs.find_first([], 1) == None
/// pairs.find_first([Pair("a", 1)], 1) == Some("a")
/// pairs.find_first([Pair("a", 1), Pair("b", 2)], 1) == Some("a")
/// pairs.find_first([Pair("a", 1), Pair("b", 2), Pair("c", 1)], 1) == Some("a")
/// ```
pub fn find_first(self: Pairs<key, value>, v: value) -> Option<key> {
  when self is {
    [] -> None
    [Pair(k2, v2), ..rest] ->
      if v == v2 {
        Some(k2)
      } else {
        find_first(rest, v)
      }
  }
}

test find_first_1() {
  find_first([], "a") == None
}

test find_first_2() {
  find_first([Pair("a", 14)], 14) == Some("a")
}

test find_first_3() {
  find_first([Pair("a", 14)], 42) == None
}

test find_first_4() {
  find_first([Pair("a", 14), Pair("b", 42), Pair("c", 14)], 14) == Some("a")
}

/// Finds the last key in the alist associated with a given value, if any.
///
/// ```aiken
/// pairs.find_last([], 1) == None
/// pairs.find_last([Pair("a", 1)], 1) == Some("a")
/// pairs.find_last([Pair("a", 1), Pair("b", 2)], 1) == Some("a")
/// pairs.find_last([Pair("a", 1), Pair("b", 2), Pair("c", 1)], 1) == Some("c")
/// ```
pub fn find_last(self: Pairs<key, value>, v: value) -> Option<key> {
  when self is {
    [] -> None
    [Pair(k2, v2), ..rest] ->
      if v == v2 {
        when find_last(rest, v) is {
          None -> Some(k2)
          some -> some
        }
      } else {
        find_last(rest, v)
      }
  }
}

test find_last_1() {
  find_last([], "a") == None
}

test find_last_2() {
  find_last([Pair("a", 14)], 14) == Some("a")
}

test find_last_3() {
  find_last([Pair("a", 14)], 42) == None
}

test find_last_4() {
  find_last([Pair("a", 14), Pair("b", 42), Pair("c", 14)], 14) == Some("c")
}

/// Check if a key exists in the pairs.
///
/// ```aiken
/// pairs.has_key([], "a") == False
/// pairs.has_key([Pair("a", 1)], "a") == True
/// pairs.has_key([Pair("a", 1), Pair("b", 2)], "a") == True
/// pairs.has_key([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == True
/// ```
pub fn has_key(self: Pairs<key, value>, k: key) -> Bool {
  when self is {
    [] -> False
    // || is lazy so this is fine
    [Pair(k2, _), ..rest] -> k == k2 || has_key(rest, k)
  }
}

test has_key_1() {
  !has_key([], "a")
}

test has_key_2() {
  has_key([Pair("a", 14)], "a")
}

test has_key_3() {
  !has_key([Pair("a", 14)], "b")
}

test has_key_4() {
  has_key([Pair("a", 14), Pair("b", 42)], "b")
}

test has_key_5() {
  has_key([Pair("a", 14), Pair("b", 42), Pair("a", 42)], "a")
}

/// Extract all the keys present in a given `Pairs`.
///
/// ```aiken
/// pairs.keys([]) == []
/// pairs.keys([Pair("a", 1)]) == ["a"]
/// pairs.keys([Pair("a", 1), Pair("b", 2)]) == ["a", "b"]
/// pairs.keys([Pair("a", 1), Pair("b", 2), Pair("a", 3)]) == ["a", "b", "a"]
/// ```
pub fn keys(self: Pairs<key, value>) -> List<key> {
  when self is {
    [] -> []
    [Pair(k, _), ..rest] -> [k, ..keys(rest)]
  }
}

test keys_1() {
  keys([]) == []
}

test keys_2() {
  keys([Pair("a", 0)]) == ["a"]
}

test keys_3() {
  keys([Pair("a", 0), Pair("b", 0)]) == ["a", "b"]
}

/// Extract all the values present in a given `Pairs`.
///
/// ```aiken
/// pairs.values([]) == []
/// pairs.values([Pair("a", 1)]) == [1]
/// pairs.values([Pair("a", 1), Pair("b", 2)]) == [1, 2]
/// pairs.values([Pair("a", 1), Pair("b", 2), Pair("a", 3)]) == [1, 2, 3]
/// ```
pub fn values(self: Pairs<key, value>) -> List<value> {
  when self is {
    [] -> []
    [Pair(_, v), ..rest] -> [v, ..values(rest)]
  }
}

test values_1() {
  values([]) == []
}

test values_2() {
  values([Pair("a", 1)]) == [1]
}

test values_3() {
  values([Pair("a", 1), Pair("b", 2)]) == [1, 2]
}

test values_4() {
  values([Pair("a", 1), Pair("b", 2), Pair("a", 3)]) == [1, 2, 3]
}

// ## Modifying

/// Remove all key-value pairs matching the key from the Pairs. If the key is not found, no changes are made.
///
/// ```aiken
/// pairs.delete_all([], "a") == []
/// pairs.delete_all([Pair("a", 1)], "a") == []
/// pairs.delete_all([Pair("a", 1), Pair("b", 2)], "a") == [Pair("b", 2)]
/// pairs.delete_all([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == [Pair("b", 2)]
/// ```
pub fn delete_all(self: Pairs<key, value>, key k: key) -> Pairs<key, value> {
  when self is {
    [] -> []
    [Pair(k2, v2), ..rest] ->
      if k == k2 {
        delete_all(rest, k)
      } else {
        [Pair(k2, v2), ..delete_all(rest, k)]
      }
  }
}

test delete_all_1() {
  delete_all([], "a") == []
}

test delete_all_2() {
  delete_all([Pair("a", 14)], "a") == []
}

test delete_all_3() {
  let fixture = [Pair("a", 14)]
  delete_all(fixture, "b") == fixture
}

test delete_all_4() {
  let fixture = [Pair("a", 1), Pair("b", 2), Pair("a", 3)]
  delete_all(fixture, "a") == [Pair("b", 2)]
}

/// Remove a single key-value pair from the `Pairs`. If the key is not found, no changes are made.
/// Duplicate keys are not deleted. Only the **first** key found is deleted.
///
/// ```aiken
/// pairs.delete_first([], "a") == []
/// pairs.delete_first([Pair("a", 1)], "a") == []
/// pairs.delete_first([Pair("a", 1), Pair("b", 2)], "a") == [Pair("b", 2)]
/// pairs.delete_first([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == [Pair("b", 2), Pair("a", 3)]
/// ```
pub fn delete_first(self: Pairs<key, value>, key k: key) -> Pairs<key, value> {
  when self is {
    [] -> []
    [Pair(k2, v2), ..rest] ->
      if k == k2 {
        rest
      } else {
        [Pair(k2, v2), ..delete_first(rest, k)]
      }
  }
}

test delete_first_1() {
  delete_first([], "a") == []
}

test delete_first_2() {
  delete_first([Pair("a", 14)], "a") == []
}

test delete_first_3() {
  let fixture = [Pair("a", 14)]
  delete_first(fixture, "b") == fixture
}

test delete_first_4() {
  let fixture = [Pair("a", 1), Pair("b", 2), Pair("a", 3)]
  delete_first(fixture, "a") == [Pair("b", 2), Pair("a", 3)]
}

/// Remove a single key-value pair from the Pairs. If the key is not found, no changes are made.
/// Duplicate keys are not deleted. Only the **last** key found is deleted.
///
/// ```aiken
/// pairs.delete_last([], "a") == []
/// pairs.delete_last([Pair("a", 1)], "a") == []
/// pairs.delete_last([Pair("a", 1), Pair("b", 2)], "a") == [Pair("b", 2)]
/// pairs.delete_last([Pair("a", 1), Pair("b", 2), Pair("a", 3)], "a") == [Pair("a", 1), Pair("b", 2)]
/// ```
pub fn delete_last(self: Pairs<key, value>, key k: key) -> Pairs<key, value> {
  when self is {
    [] -> []
    [Pair(k2, v2), ..rest] ->
      if k == k2 {
        let tail = delete_last(rest, k)
        if tail == rest {
          rest
        } else {
          [Pair(k2, v2), ..tail]
        }
      } else {
        [Pair(k2, v2), ..delete_last(rest, k)]
      }
  }
}

test delete_last_1() {
  delete_last([], "a") == []
}

test delete_last_2() {
  delete_last([Pair("a", 14)], "a") == []
}

test delete_last_3() {
  let fixture = [Pair("a", 14)]
  delete_last(fixture, "b") == fixture
}

test delete_last_4() {
  let fixture = [Pair("a", 1), Pair("b", 2), Pair("a", 3)]
  delete_last(fixture, "a") == [Pair("a", 1), Pair("b", 2)]
}

/// Insert a value in the `Pairs` at a given key. If the key already exists,
/// the value is added in front.
///
/// > [!CAUTION]
/// > The list is only traversed up to the given key and the traversal
/// > stops as soon as a higher key is encountered. Said differently, the list
/// > is assumed to **be ordered by ascending keys**! If it is not, expect the
/// > unexpected.
///
/// ```aiken
/// use aiken/primitive/bytearray
///
/// let result =
///   []
///     |> pairs.insert_by_ascending_key(key: "foo", value: 1, compare: bytearray.compare)
///     |> pairs.insert_by_ascending_key(key: "bar", value: 2, compare: bytearray.compare)
///     |> pairs.insert_by_ascending_key(key: "foo", value: 3, compare: bytearray.compare)
///
/// result == [Pair("bar", 2), Pair("foo", 3), Pair("foo", 1)]
/// ```
pub fn insert_by_ascending_key(
  self: Pairs<key, value>,
  key k: key,
  value v: value,
  compare: fn(key, key) -> Ordering,
) -> Pairs<key, value> {
  when self is {
    [] -> [Pair(k, v)]
    [Pair(k2, v2), ..rest] ->
      if compare(k, k2) == Less {
        [Pair(k, v), ..self]
      } else {
        if k == k2 {
          [Pair(k, v), ..self]
        } else {
          [Pair(k2, v2), ..insert_by_ascending_key(rest, k, v, compare)]
        }
      }
  }
}

test insert_by_ascending_key_1() {
  let m =
    []
      |> insert_by_ascending_key("foo", 42, bytearray.compare)
      |> insert_by_ascending_key("foo", 14, bytearray.compare)

  m == [Pair("foo", 14), Pair("foo", 42)]
}

test insert_by_ascending_key_2() {
  let m =
    []
      |> insert_by_ascending_key("foo", 42, bytearray.compare)
      |> insert_by_ascending_key("bar", 14, bytearray.compare)
      |> insert_by_ascending_key("baz", 1337, bytearray.compare)

  m == [Pair("bar", 14), Pair("baz", 1337), Pair("foo", 42)]
}

/// Like [`insert_by_ascending_key`](#insert_by_ascending_key) but specifies
/// how to combine two values on a key conflict.
///
/// > [!CAUTION]
/// > The list is only traversed up to the given key and the traversal
/// > stops as soon as a higher key is encountered. Said differently, the list
/// > is assumed to **be ordered by ascending keys**! If it is not, expect the
/// > unexpected.
///
/// ```aiken
/// use aiken/primitive/bytearray
///
/// let add_integer = fn(x, y) { x + y }
///
/// let result =
///   []
///     |> pairs.insert_with_by_ascending_key(key: "foo", value: 1, compare: bytearray.compare, with: add_integer)
///     |> pairs.insert_with_by_ascending_key(key: "bar", value: 2, compare: bytearray.compare, with: add_integer)
///     |> pairs.insert_with_by_ascending_key(key: "foo", value: 3, compare: bytearray.compare, with: add_integer)
///
/// result == [Pair("bar", 2), Pair("foo", 4)]
/// ```
pub fn insert_with_by_ascending_key(
  self: Pairs<key, value>,
  key k: key,
  value v: value,
  compare: fn(key, key) -> Ordering,
  with: fn(value, value) -> value,
) -> Pairs<key, value> {
  when self is {
    [] -> [Pair(k, v)]
    [Pair(k2, v2), ..rest] ->
      if compare(k, k2) == Less {
        [Pair(k, v), ..self]
      } else {
        if k == k2 {
          [Pair(k, with(v, v2)), ..rest]
        } else {
          [
            Pair(k2, v2),
            ..insert_with_by_ascending_key(rest, k, v, compare, with)
          ]
        }
      }
  }
}

test insert_with_by_ascending_key_1() {
  let compare_un_b_data =
    fn(l, r) {
      bytearray.compare(l |> builtin.un_b_data, r |> builtin.un_b_data)
    }

  let m =
    []
      |> insert_with_by_ascending_key(
          "foo" |> builtin.b_data,
          42,
          compare_un_b_data,
          builtin.add_integer,
        )
      |> insert_with_by_ascending_key(
          "foo" |> builtin.b_data,
          14,
          compare_un_b_data,
          builtin.add_integer,
        )

  m == [Pair("foo" |> builtin.b_data, 56)]
}

test insert_with_by_ascending_key_2() {
  let compare_un_b_data =
    fn(l, r) {
      bytearray.compare(l |> builtin.un_b_data, r |> builtin.un_b_data)
    }

  let m =
    []
      |> insert_with_by_ascending_key(
          "foo" |> builtin.b_data,
          42,
          compare_un_b_data,
          builtin.add_integer,
        )
      |> insert_with_by_ascending_key(
          "bar" |> builtin.b_data,
          14,
          compare_un_b_data,
          builtin.add_integer,
        )
      |> insert_with_by_ascending_key(
          "baz" |> builtin.b_data,
          1337,
          compare_un_b_data,
          builtin.add_integer,
        )

  m == [
    Pair("bar" |> builtin.b_data, 14),
    Pair("baz" |> builtin.b_data, 1337),
    Pair("foo" |> builtin.b_data, 42),
  ]
}

test insert_with_by_ascending_key_3() {
  let compare_un_b_data =
    fn(l, r) {
      bytearray.compare(l |> builtin.un_b_data, r |> builtin.un_b_data)
    }

  let result =
    []
      |> insert_with_by_ascending_key(
          "foo" |> builtin.b_data,
          1,
          compare_un_b_data,
          builtin.add_integer,
        )
      |> insert_with_by_ascending_key(
          "bar" |> builtin.b_data,
          2,
          compare_un_b_data,
          builtin.add_integer,
        )
      |> insert_with_by_ascending_key(
          "foo" |> builtin.b_data,
          3,
          compare_un_b_data,
          builtin.add_integer,
        )

  result == [Pair("bar" |> builtin.b_data, 2), Pair("foo" |> builtin.b_data, 4)]
}

/// Apply a function to all key-value pairs in a alist, replacing the values.
///
/// ```aiken
/// let fixture = [Pair("a", 100), Pair("b", 200)]
///
/// pairs.map(fixture, fn(_k, v) { v * 2 }) == [Pair("a", 200), Pair("b", 400)]
/// ```
pub fn map(
  self: Pairs<key, value>,
  with: fn(key, value) -> result,
) -> Pairs<key, result> {
  when self is {
    [] -> []
    [Pair(k, v), ..rest] -> [Pair(k, with(k, v)), ..map(rest, with)]
  }
}

test map_1() {
  let fixture = [Pair("a", 1), Pair("b", 2)]

  map(fixture, with: fn(k, _) { k }) == [Pair("a", "a"), Pair("b", "b")]
}

test map_2() {
  let fixture = [Pair("a", 1), Pair("b", 2)]

  map(fixture, with: fn(_, v) { v + 1 }) == [Pair("a", 2), Pair("b", 3)]
}

/// Insert a value in the `Pairs` at a given key. If the key already exists,
/// its value is replaced.
///
/// > [!CAUTION]
/// > The list is only traversed up to the given key and the traversal
/// > stops as soon as a higher key is encountered. Said differently, the list
/// > is assumed to **be ordered by ascending keys**! If it is not, expect the
/// > unexpected.
///
/// ```aiken
/// use aiken/primitive/bytearray
///
/// let result =
///   []
///     |> pairs.repsert_by_ascending_key(key: "foo", value: 1, compare: bytearray.compare)
///     |> pairs.repsert_by_ascending_key(key: "bar", value: 2, compare: bytearray.compare)
///     |> pairs.repsert_by_ascending_key(key: "foo", value: 3, compare: bytearray.compare)
///
/// result == [Pair("bar", 2), Pair("foo", 3)]
/// ```
pub fn repsert_by_ascending_key(
  self: Pairs<key, value>,
  key k: key,
  value v: value,
  compare: fn(key, key) -> Ordering,
) -> Pairs<key, value> {
  when self is {
    [] -> [Pair(k, v)]
    [Pair(k2, v2), ..rest] ->
      if compare(k, k2) == Less {
        [Pair(k, v), ..self]
      } else {
        if k == k2 {
          [Pair(k, v), ..rest]
        } else {
          [Pair(k2, v2), ..repsert_by_ascending_key(rest, k, v, compare)]
        }
      }
  }
}

test repsert_by_ascending_key_1() {
  let m =
    []
      |> repsert_by_ascending_key("foo", 42, bytearray.compare)
      |> repsert_by_ascending_key("foo", 14, bytearray.compare)

  m == [Pair("foo", 14)]
}

test repsert_by_ascending_key_2() {
  let m =
    []
      |> repsert_by_ascending_key("foo", 42, bytearray.compare)
      |> repsert_by_ascending_key("bar", 14, bytearray.compare)
      |> repsert_by_ascending_key("baz", 1337, bytearray.compare)

  m == [Pair("bar", 14), Pair("baz", 1337), Pair("foo", 42)]
}

// ## Transforming

/// Fold over the key-value pairs in a pairs. The fold direction follows keys
/// in ascending order and is done from left-to-right.
///
/// ```aiken
/// let fixture = [
///   Pair(1, 100),
///   Pair(2, 200),
///   Pair(3, 300),
/// ]
///
/// pairs.foldl(fixture, 0, fn(k, v, result) { k * v + result }) == 1400
/// ```
pub fn foldl(
  self: Pairs<key, value>,
  zero: result,
  with: fn(key, value, result) -> result,
) -> result {
  when self is {
    [] -> zero
    [Pair(k, v), ..rest] -> foldl(rest, with(k, v, zero), with)
  }
}

test foldl_1() {
  foldl([], 14, fn(_, _, _) { 42 }) == 14
}

test foldl_2() {
  foldl(
    [Pair("a", 42), Pair("b", 14)],
    zero: 0,
    with: fn(_, v, total) { v + total },
  ) == 56
}

/// Fold over the key-value pairs in a Pairs. The fold direction follows the
/// order of elements in the Pairs and is done from right-to-left.
///
/// ```aiken
/// let fixture = [
///   Pair(1, 100),
///   Pair(2, 200),
///   Pair(3, 300),
/// ]
///
/// pairs.foldr(fixture, 0, fn(k, v, result) { k * v + result }) == 1400
/// ```
pub fn foldr(
  self: Pairs<key, value>,
  zero: result,
  with: fn(key, value, result) -> result,
) -> result {
  when self is {
    [] -> zero
    [Pair(k, v), ..rest] -> with(k, v, foldr(rest, zero, with))
  }
}

test foldr_1() {
  foldr([], 14, fn(_, _, _) { 42 }) == 14
}

test foldr_2() {
  foldr(
    [Pair("a", 42), Pair("b", 14)],
    zero: 0,
    with: fn(_, v, total) { v + total },
  ) == 56
}

test foldr_3() {
  let fixture = [Pair(1, 100), Pair(2, 200), Pair(3, 300)]

  foldr(fixture, 0, fn(k, v, result) { k * v + result }) == 1400
}
