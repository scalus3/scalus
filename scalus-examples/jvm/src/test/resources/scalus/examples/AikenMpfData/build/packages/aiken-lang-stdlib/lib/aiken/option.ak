//// A type to capture optional results; useful for handling errors.
////
//// Note that the `Option` type and its constructors are readily available in Aiken. They are part of the [Prelude](https://aiken-lang.github.io/prelude/aiken.html#Option) module imported by default in every module.

// ## Inspecting

/// Asserts whether an option is `None`.
pub fn is_none(self: Option<a>) -> Bool {
  when self is {
    Some(_) -> False
    _ -> True
  }
}

test is_none_1() {
  is_none(Some(0)) == False
}

test is_none_2() {
  is_none(None) == True
}

/// Asserts whether an option is `Some`, irrespective of the value it contains.
pub fn is_some(self: Option<a>) -> Bool {
  when self is {
    Some(_) -> True
    _ -> False
  }
}

test is_some_1() {
  is_some(Some(0)) == True
}

test is_some_2() {
  is_some(None) == False
}

// ## Combining

/// Chain together many computations that may fail.
///
/// ```aiken
/// self
///   |> dict.get(policy_id)
///   |> option.and_then(dict.get(_, asset_name))
///   |> option.or_else(0)
/// ```
pub fn and_then(
  self: Option<a>,
  then: fn(a) -> Option<result>,
) -> Option<result> {
  when self is {
    None -> None
    Some(a) -> then(a)
  }
}

fn try_decrement(n: Int) -> Option<Int> {
  if n > 0 {
    Some(n - 1)
  } else {
    None
  }
}

test and_then_1() {
  let result =
    None
      |> and_then(try_decrement)
  result == None
}

test and_then_2() {
  let result =
    Some(14)
      |> and_then(try_decrement)
  result == Some(13)
}

test and_then_3() {
  let result =
    Some(0)
      |> and_then(try_decrement)
  result == None
}

/// Picks the first element which is not None. If there's no such element, return None.
///
/// ```aiken
/// option.choice([]) == None
/// option.choice([Some(14), Some(42)]) == Some(14)
/// option.choice([None, Some(42)]) == Some(42)
/// option.choice([None, None]) == None
/// ```
pub fn choice(self: List<Option<a>>) -> Option<a> {
  when self is {
    [] -> None
    [head, ..others] ->
      when head is {
        None -> choice(others)
        _ -> head
      }
  }
}

test choice_1() {
  Some(1) == choice([Some(1), Some(2)])
}

test choice_2() {
  None == choice([])
}

test choice_3() {
  Some(1) == choice([None, Some(1)])
}

/// Converts from `Option<Option<a>>` to `Option<a>`.
///
/// ```aiken
/// option.flatten(Some(Some(42))) == Some(42)
/// option.flatten(Some(None)) == None
/// option.flatten(None) == None
/// ```
///
/// Flattening only removes one level of nesting at a time:
///
/// ```aiken
/// flatten(Some(Some(Some(42)))) == Some(Some(42))
/// Some(Some(Some(42))) |> flatten |> flatten == Some(42)
/// ```
pub fn flatten(opt: Option<Option<a>>) -> Option<a> {
  when opt is {
    Some(inner) -> inner
    None -> None
  }
}

test flatten_1() {
  let x: Option<Option<Int>> = Some(Some(6))
  Some(6) == flatten(x)
}

test flatten_2() {
  let x: Option<Option<Int>> = Some(None)
  None == flatten(x)
}

test flatten_3() {
  let x: Option<Option<Int>> = None
  None == flatten(x)
}

test flatten_4() {
  let x: Option<Option<Option<Int>>> = Some(Some(Some(6)))

  let result =
    x
      |> flatten
      |> flatten

  Some(6) == result
}

/// Apply a function to the inner value of an [`Option`](#option)
///
/// ```aiken
/// option.map(None, fn(n) { n * 2 }) == None
/// option.map(Some(14), fn(n) { n * 2 }) == Some(28)
/// ```
pub fn map(self: Option<a>, with: fn(a) -> result) -> Option<result> {
  when self is {
    None -> None
    Some(a) -> Some(with(a))
  }
}

test map_1() {
  map(None, fn(_) { Void }) == None
}

test map_2() {
  map(Some(14), fn(n) { n + 1 }) == Some(15)
}

/// Combine two [`Option`](#option) together.
///
/// ```aiken
/// type Foo {
///   Foo(Int, Int)
/// }
///
/// option.map2(Some(14), Some(42), Foo) == Some(Foo(14, 42))
/// option.map2(None, Some(42), Foo) == None
/// option.map2(Some(14), None, Foo) == None
/// ```
pub fn map2(
  opt_a: Option<a>,
  opt_b: Option<b>,
  with: fn(a, b) -> result,
) -> Option<result> {
  when opt_a is {
    None -> None
    Some(a) ->
      when opt_b is {
        None -> None
        Some(b) -> Some(with(a, b))
      }
  }
}

test map2_1() {
  map2(None, Some(42), fn(_, _) { 14 }) == None
}

test map2_2() {
  map2(Some(42), None, fn(_, _) { 14 }) == None
}

test map2_3() {
  map2(Some(14), Some(42), fn(a, b) { (a, b) }) == Some((14, 42))
}

/// Combine three [`Option`](#option) together.
///
/// ```aiken
/// type Foo {
///   Foo(Int, Int, Int)
/// }
///
/// option.map3(Some(14), Some(42), Some(1337), Foo) == Some(Foo(14, 42, 1337))
/// option.map3(None, Some(42), Some(1337), Foo) == None
/// option.map3(Some(14), None, None, Foo) == None
/// ```
pub fn map3(
  opt_a: Option<a>,
  opt_b: Option<b>,
  opt_c: Option<c>,
  with: fn(a, b, c) -> result,
) -> Option<result> {
  when opt_a is {
    None -> None
    Some(a) ->
      when opt_b is {
        None -> None
        Some(b) ->
          when opt_c is {
            None -> None
            Some(c) -> Some(with(a, b, c))
          }
      }
  }
}

test map3_1() {
  map3(None, Some(42), None, fn(_, _, _) { 14 }) == None
}

test map3_2() {
  map3(Some(42), None, None, fn(_, _, _) { 14 }) == None
}

test map3_3() {
  map3(Some(14), Some(42), Some(1337), fn(a, b, c) { c - a + b }) == Some(1365)
}

/// Like [`or_else`](#or_else) but allows returning an `Option`.
/// This is effectively mapping the error branch.
///
/// ```aiken
/// option.or_try(None, fn(_) { Some("aiken") }) == Some("aiken")
/// option.or_try(Some(42), fn(_) { Some(14) }) == Some(42)
/// option.or_try(None, fn (_) { fail }) => 💥
/// ```
pub fn or_try(self: Option<a>, compute_default: fn() -> Option<a>) -> Option<a> {
  when self is {
    None -> compute_default()
    _ -> self
  }
}

test or_try_1() {
  or_try(None, fn() { Some("aiken") }) == Some("aiken")
}

test or_try_2() {
  or_try(Some(42), fn() { fail }) == Some(42)
}

// ## Transforming

/// Provide a default value, turning an optional value into a normal value.
///
/// ```aiken
/// option.or_else(None, "aiken") == "aiken"
/// option.or_else(Some(42), 14) == 42
/// ```
pub fn or_else(self: Option<a>, default: a) -> a {
  when self is {
    None -> default
    Some(a) -> a
  }
}

test or_else_1() {
  or_else(None, "aiken") == "aiken"
}

test or_else_2() {
  or_else(Some(42), 14) == 42
}
