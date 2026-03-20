use aiken/builtin
use aiken/collection/dict.{Dict, from_ascending_pairs_with}
use aiken/collection/list
use aiken/crypto.{Blake2b_224, Hash, Script}
use aiken/option

/// Lovelace is now a type wrapper for Int.
pub type Lovelace =
  Int

/// A type-alias for a `PolicyId`. A `PolicyId` is always 28-byte long
pub type PolicyId =
  Hash<Blake2b_224, Script>

/// Ada, the native currency, isn't associated with any `PolicyId` (it's not
/// possible to mint Ada!).
///
/// By convention, it is an empty `ByteArray`.
pub const ada_policy_id = ""

/// A type-alias for 'AssetName`, which are free-form byte-arrays between
/// 0 and 32 bytes.
pub type AssetName =
  ByteArray

/// Ada, the native currency, isn't associated with any `AssetName` (it's not
/// possible to mint Ada!).
///
/// By convention, it is an empty `ByteArray`.
pub const ada_asset_name = ""

/// A multi-asset output `Value`. Contains tokens indexed by [PolicyId](#PolicyId) and [AssetName](#AssetName).
///
/// > [!IMPORTANT]
/// > This type maintain some invariants by construction; in particular, a `Value` will never contain a
/// zero quantity of a particular token.
pub opaque type Value {
  inner: Dict<PolicyId, Dict<AssetName, Int>>,
}

// ## Constructing

/// Construct a `Value` from an asset identifier (i.e. `PolicyId` + `AssetName`)
/// and a given quantity.
pub fn from_asset(
  policy_id: PolicyId,
  asset_name: AssetName,
  quantity: Int,
) -> Value {
  if quantity == 0 {
    zero
  } else {
    let asset =
      dict.empty
        |> dict.insert(asset_name, quantity)
    dict.empty
      |> dict.insert(policy_id, asset)
      |> Value
  }
}

/// Promote an arbitrary list of assets into a `Value`. This function fails
/// (i.e. halts the program execution) if:
///
/// - there's any duplicate amongst `PolicyId`;
/// - there's any duplicate amongst `AssetName`;
/// - the `AssetName` aren't sorted in ascending lexicographic order; or
/// - any asset quantity is null.
///
/// This function is meant to turn arbitrary user-defined `Data` into safe `Value`,
/// while checking for internal invariants.
pub fn from_asset_list(xs: Pairs<PolicyId, Pairs<AssetName, Int>>) -> Value {
  xs
    |> list.foldr(
        dict.empty,
        fn(inner, acc) {
          expect Pair(p, [_, ..] as x) = inner
          x
            |> from_ascending_pairs_with(fn(v) { v != 0 })
            |> dict.insert_with(
                acc,
                p,
                _,
                fn(_, _, _) {
                  fail @"Duplicate policy in the asset list."
                },
              )
        },
      )
    |> Value
}

test from_asset_list_1() {
  let v = from_asset_list([])
  v == zero
}

test from_asset_list_2() fail {
  let v = from_asset_list([Pair(#"33", [])])
  v == zero
}

test from_asset_list_3() fail {
  let v = from_asset_list([Pair(#"33", [Pair(#"", 0)])])
  v != zero
}

test from_asset_list_4() {
  let v = from_asset_list([Pair(#"33", [Pair(#"", 1)])])
  flatten(v) == [(#"33", #"", 1)]
}

test from_asset_list_5() {
  let v = from_asset_list([Pair(#"33", [Pair(#"", 1), Pair(#"33", 1)])])
  flatten(v) == [(#"33", #"", 1), (#"33", #"33", 1)]
}

test from_asset_list_6() fail {
  let v =
    from_asset_list(
      [
        Pair(#"33", [Pair(#"", 1), Pair(#"33", 1)]),
        Pair(#"33", [Pair(#"", 1), Pair(#"33", 1)]),
      ],
    )
  v != zero
}

test from_asset_list_7() fail {
  let v =
    from_asset_list(
      [
        Pair(#"33", [Pair(#"", 1), Pair(#"33", 1)]),
        Pair(#"34", [Pair(#"", 1), Pair(#"", 1)]),
      ],
    )
  v != zero
}

test from_asset_list_8() {
  let v =
    from_asset_list(
      [
        Pair(#"33", [Pair(#"", 1), Pair(#"33", 1)]),
        Pair(#"34", [Pair(#"31", 1)]), Pair(#"35", [Pair(#"", 1)]),
      ],
    )
  flatten(v) == [
    (#"33", #"", 1), (#"33", #"33", 1), (#"34", #"31", 1), (#"35", #"", 1),
  ]
}

test from_asset_list_9() {
  let v =
    from_asset_list(
      [
        Pair(#"35", [Pair(#"", 1)]), Pair(#"33", [Pair(#"", 1), Pair(#"33", 1)]),
        Pair(#"34", [Pair(#"31", 1)]),
      ],
    )
  flatten(v) == [
    (#"33", #"", 1), (#"33", #"33", 1), (#"34", #"31", 1), (#"35", #"", 1),
  ]
}

/// Construct a `Value` from a lovelace quantity.
///
/// Friendly reminder: 1 Ada = 1.000.000 Lovelace
pub fn from_lovelace(quantity: Int) -> Value {
  from_asset(ada_policy_id, ada_asset_name, quantity)
}

/// Construct an empty `Value` with nothing in it.
pub const zero: Value = Value { inner: dict.empty }

// ## Inspecting

/// Check is a `Value` is zero. That is, it has no assets and holds no Ada/Lovelace.
pub fn is_zero(self: Value) -> Bool {
  self == zero
}

/// Efficiently compare two values together, allowing a custom behaviour for Ada/Lovelace.
/// The second parameter is provided as `Data`, allowing to conveniently compare serialized
/// datums or similar structurually equivalent types (such as `Pairs<PolicyId, Pairs<AssetName, Lovelace>>`).
///
/// The third argument is a callback function to assert the left and right lovelace
/// quantities. Its first argument refers to the quantity of the first argument of
/// `match`, and the second argument of the callback to the quantity of the second
/// argument of `match`. In the absence of lovelace in any value, it defaults to `0`.
///
/// ```aiken
/// const value: Value =
///   assets.from_lovelace(30)
///     |> assets.add("foo", "bar", 1)
///     |> assets.add("foo", "baz", 42)
///
/// const datum: Data =
///   assets.from_lovelace(20)
///     |> assets.add("foo", "bar", 1)
///     |> assets.add("foo", "baz", 42)
///
/// True == assets.match(value, datum, >=)
///
/// False == assets.match(value, datum, ==)
///
/// True == assets.match(value, datum, fn(value_lovelace, datum_lovelace) {
///   2 * datum_lovelace >= value_lovelace
/// })
/// ```
pub fn match(
  left: Value,
  right: Data,
  assert_lovelace: fn(Lovelace, Lovelace) -> Bool,
) -> Bool {
  builtin.choose_data(
    right,
    False,
    {
      let (left_lovelace, left_assets) = dict.pop(left.inner, ada_policy_id)
      let left_assets: Data = left_assets
      let left_lovelace =
        when left_lovelace is {
          Some(tokens) -> builtin.head_list(dict.to_pairs(tokens)).2nd
          None -> 0
        }
      when builtin.un_map_data(right) is {
        [] -> left_assets == right && assert_lovelace(left_lovelace, 0)
        [first_asset, ..right_assets] ->
          if first_asset.1st == builtin.b_data(ada_policy_id) {
            and {
              assert_lovelace(
                left_lovelace,
                builtin.un_i_data(
                  builtin.head_list(builtin.un_map_data(first_asset.2nd)).2nd,
                ),
              ),
              left_assets == builtin.map_data(right_assets),
            }
          } else {
            and {
              assert_lovelace(left_lovelace, 0),
              left_assets == right,
            }
          }
      }
    },
    False,
    False,
    False,
  )
}

const fixture_match_value: Value =
  zero
    |> add(ada_policy_id, ada_asset_name, 42)
    |> add("foo", "01", 1)
    |> add("foo", "02", 1)
    |> add("bar", "01", 42)

const fixture_match_data: Data =
  zero
    |> add(ada_policy_id, ada_asset_name, 14)
    |> add("foo", "01", 1)
    |> add("foo", "02", 1)
    |> add("bar", "01", 42)

const fixture_match_data_missing_foo_02: Data =
  zero
    |> add(ada_policy_id, ada_asset_name, 14)
    |> add("foo", "01", 1)
    |> add("bar", "01", 42)

const fixture_match_data_altered_foo_01: Data =
  zero
    |> add(ada_policy_id, ada_asset_name, 14)
    |> add("foo", "01", 14)
    |> add("foo", "02", 1)
    |> add("bar", "01", 42)

const fixture_match_data_missing_bar: Data =
  zero
    |> add(ada_policy_id, ada_asset_name, 14)
    |> add("foo", "01", 1)
    |> add("foo", "02", 1)

const fixture_match_data_extra_policy: Data =
  zero
    |> add(ada_policy_id, ada_asset_name, 14)
    |> add("foo", "01", 1)
    |> add("foo", "02", 1)
    |> add("bar", "01", 42)
    |> add("baz", "01", 1)

const fixture_match_data_extra_asset: Data =
  zero
    |> add(ada_policy_id, ada_asset_name, 14)
    |> add("foo", "01", 1)
    |> add("foo", "02", 1)
    |> add("foo", "03", 1)
    |> add("bar", "01", 42)

const fixture_match_data_no_assets: Data =
  zero
    |> add(ada_policy_id, ada_asset_name, 14)

test match_1() {
  match(fixture_match_value, fixture_match_data, fn(_, _) { True })
}

test match_2() {
  !match(
    fixture_match_value,
    fixture_match_data,
    fn(source, target) { source == target },
  )
}

test match_3() {
  !match(
    fixture_match_value,
    fixture_match_data_missing_foo_02,
    fn(_, _) { True },
  )
}

test match_4() {
  !match(fixture_match_value, fixture_match_data_missing_bar, fn(_, _) { True })
}

test match_5() {
  !match(
    fixture_match_value,
    fixture_match_data_altered_foo_01,
    fn(_, _) { True },
  )
}

test match_6() {
  !match(
    fixture_match_value,
    fixture_match_data_extra_policy,
    fn(_, _) { True },
  )
}

test match_7() {
  !match(fixture_match_value, fixture_match_data_extra_asset, fn(_, _) { True })
}

test match_8() {
  !match(fixture_match_value, fixture_match_data_no_assets, fn(_, _) { True })
}

test match_9() {
  match(zero, zero, ==)
}

test match_10() {
  match(
    without_lovelace(fixture_match_value),
    without_lovelace(fixture_match_value),
    fn(left, right) { left == 0 && right == 0 },
  )
}

test match_11() {
  match(
    without_lovelace(fixture_match_value),
    fixture_match_value,
    fn(left, right) { left == 0 && right > 0 },
  )
}

test match_12() {
  match(
    fixture_match_value,
    without_lovelace(fixture_match_value),
    fn(left, right) { left > 0 && right == 0 },
  )
}

test match_13() {
  match(
    zero |> add(ada_policy_id, ada_asset_name, 42),
    zero,
    fn(left, right) { left == 42 && right == 0 },
  )
}

test match_14() {
  match(
    zero,
    zero |> add(ada_policy_id, ada_asset_name, 42),
    fn(left, right) { left == 0 && right == 42 },
  )
}

const fixture_match_benchmark_left: Value =
  zero
    |> add(ada_policy_id, ada_asset_name, 1337)
    |> add(
        #"0246a14d04c3a0e9b65f6b90a3d1aa5faee5d56ab1e30ec7e8b02f29",
        "MATTR",
        200,
      )
    |> add(
        #"0a9e126256cb38c4865cdac6eb2ada51c328ba0df2ebde22ae126c0d",
        "ProphecyPoster076",
        1,
      )
    |> add(
        #"1774343241680e4daef7cbfe3536fc857ce23fb66cd0b66320b2e3dd",
        "BISON",
        12_004_999_999,
      )
    |> add(
        #"279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f",
        "SNEK",
        1486,
      )
    |> add(
        #"651dfc074202423585996ffa717cb45237d307e705e2cc3dab1ccabd",
        "MAYZSilverFoundersEdition0035",
        1,
      )
    |> add(
        #"63df49056617dd14034986cf7c250bad6552fd2f0f9c71d797932008",
        "CardanoSpaceSession",
        20,
      )
    |> add(
        #"5b01968867e13432afaa2f814e1d15e332d6cd0aa77e350972b0967d",
        "ADAOGovernanceToken",
        1,
      )
    |> add(
        #"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
        "HOSKY",
        400_001_000,
      )
    |> add(
        #"da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24",
        "LQ",
        10_635_899,
      )
    |> add(
        #"95d9a98c2f7999a3d5e0f4d795cb1333837c09eb0f24835cd2ce954c",
        "GrandmasterAdventurer659",
        1,
      )
    |> add(
        #"702cbdb06a81ef2fa4f85f9e32159c03f502539d762a71194fc11eb3",
        "AdventurerOfThiolden8105",
        1,
      )
    |> add(
        #"d0112837f8f856b2ca14f69b375bc394e73d146fdadcc993bb993779",
        "DiscoSolaris3725",
        1,
      )
    |> add(
        #"8dd5717e7d4d993019dbd788c19837910e3fcf647ab282f828c80a7a",
        "CardaWorld535",
        1,
      )
    |> add(
        #"8dd5717e7d4d993019dbd788c19837910e3fcf647ab282f828c80a7a",
        "CardaWorld1213",
        1,
      )
    |> add(
        #"8dd5717e7d4d993019dbd788c19837910e3fcf647ab282f828c80a7a",
        "CardaWorld1518",
        1,
      )
    |> add(
        #"8dd5717e7d4d993019dbd788c19837910e3fcf647ab282f828c80a7a",
        "CardaWorld1537",
        1,
      )
    |> add(
        #"8dd5717e7d4d993019dbd788c19837910e3fcf647ab282f828c80a7a",
        "CardaWorld4199",
        1,
      )
    |> add(
        #"8dd5717e7d4d993019dbd788c19837910e3fcf647ab282f828c80a7a",
        "CardaWorld3767",
        1,
      )
    |> add(
        #"7597444754551a8c17edbf7291cdaeca898ca02ee4e732b09a949396",
        "Algae1",
        1,
      )
    |> add(
        #"7597444754551a8c17edbf7291cdaeca898ca02ee4e732b09a949396",
        "Algae2",
        1,
      )

const fixture_match_benchmark_right: Data = fixture_match_benchmark_left

test match_benchmark() {
  match(fixture_match_benchmark_left, fixture_match_benchmark_right, ==)
}

test match_benchmark_vs() {
  let data: Data = fixture_match_benchmark_right
  expect pairs: Pairs<PolicyId, Pairs<AssetName, Int>> = data
  fixture_match_benchmark_left == from_asset_list(pairs)
}

/// A specialized version of `quantity_of` for the Ada currency.
pub fn lovelace_of(self: Value) -> Int {
  quantity_of(self, ada_policy_id, ada_asset_name)
}

/// A list of all token policies in that Value with non-zero tokens.
pub fn policies(self: Value) -> List<PolicyId> {
  dict.keys(self.inner)
}

/// Extract the quantity of a given asset.
pub fn quantity_of(
  self: Value,
  policy_id: PolicyId,
  asset_name: AssetName,
) -> Int {
  self.inner
    |> dict.get(policy_id)
    |> option.and_then(dict.get(_, asset_name))
    |> option.or_else(0)
}

/// Get all tokens associated with a given policy.
pub fn tokens(self: Value, policy_id: PolicyId) -> Dict<AssetName, Int> {
  self.inner
    |> dict.get(policy_id)
    |> option.or_else(dict.empty)
}

// ## Combining

/// Add a (positive or negative) quantity of a single token to a assets.
/// This is more efficient than [`merge`](#merge) for a single asset.
pub fn add(
  self: Value,
  policy_id: PolicyId,
  asset_name: AssetName,
  quantity: Int,
) -> Value {
  if quantity == 0 {
    self
  } else {
    let helper =
      fn(_, left, _right) {
        let inner_result =
          dict.insert_with(
            left,
            asset_name,
            quantity,
            fn(_k, ql, qr) {
              let q = ql + qr
              if q == 0 {
                None
              } else {
                Some(q)
              }
            },
          )
        if dict.is_empty(inner_result) {
          None
        } else {
          Some(inner_result)
        }
      }

    Value(
      dict.insert_with(
        self.inner,
        policy_id,
        dict.from_ascending_pairs([Pair(asset_name, quantity)]),
        helper,
      ),
    )
  }
}

test add_1() {
  let v =
    zero
      |> add(#"acab", #"beef", 321)
      |> add(#"acab", #"beef", -321)
  v == zero
}

test add_2() {
  let v =
    from_lovelace(123)
      |> add(#"acab", #"beef", 321)
      |> add(#"acab", #"beef", -1 * 321)
  v == from_lovelace(123)
}

test add_3() {
  let v =
    from_lovelace(1)
      |> add(ada_policy_id, ada_asset_name, 2)
      |> add(ada_policy_id, ada_asset_name, 3)
  v == from_lovelace(6)
}

test add_4() {
  let v =
    zero
      |> add(#"acab", #"beef", 0)
  v == zero
}

test add_5() {
  let v =
    zero
      |> add(#"acab", #"beef", 0)
      |> add(#"acab", #"beef", 0)
  v == zero
}

/// Combine two `Value` together.
pub fn merge(left v0: Value, right v1: Value) -> Value {
  Value(
    dict.union_with(
      v0.inner,
      v1.inner,
      fn(_, a0, a1) {
        let result =
          dict.union_with(
            a0,
            a1,
            fn(_, q0, q1) {
              let q = q0 + q1
              if q == 0 {
                None
              } else {
                Some(q)
              }
            },
          )
        if dict.is_empty(result) {
          None
        } else {
          Some(result)
        }
      },
    ),
  )
}

test merge_1() {
  let v1 = from_lovelace(1)
  let v2 = from_lovelace(-1)
  merge(v1, v2) == zero
}

test merge_2() {
  let v1 = from_asset(#"00", #"", 1)
  let v2 = from_asset(#"01", #"", 2)
  let v3 = from_asset(#"02", #"", 3)
  let v =
    from_lovelace(42)
      |> merge(v3)
      |> merge(v1)
      |> merge(v2)

  flatten(v) == [
    (#"", #"", 42), (#"00", #"", 1), (#"01", #"", 2), (#"02", #"", 3),
  ]
}

test merge_3() {
  let v1 = from_asset(#"00", #"", 1)
  let v2 = from_asset(#"00", #"", -1)
  let v3 = from_asset(#"01", #"", 1)

  let v =
    zero
      |> merge(v1)
      |> merge(v2)
      |> merge(v3)

  flatten(v) == [(#"01", #"", 1)]
}

test merge_4() {
  let v1 = from_asset(#"00", #"", 1)
  let v2 = from_asset(#"00", #"", -1)

  merge(v1, v2) == zero
}

test merge_5() {
  let v =
    zero
      |> add(#"acab", #"beef", 0)

  merge(zero, v) == zero
}

/// Negates quantities of all tokens (including Ada) in that `Value`.
///
/// ```
/// v1
///   |> assets.negate
///   |> assets.merge(v1)
///   |> assets.is_zero
/// // True
/// ```
pub fn negate(self: Value) -> Value {
  dict.map(self.inner, fn(_, a) { dict.map(a, fn(_, q) { 0 - q }) })
    |> Value
}

/// Get a subset of the assets restricted to the given policies.
pub fn restricted_to(self: Value, mask: List<PolicyId>) -> Value {
  list.foldr(
    policies(self),
    zero,
    fn(policy_id, value) {
      if list.has(mask, policy_id) {
        dict.foldr(
          tokens(self, policy_id),
          value,
          fn(asset_name, quantity, value) {
            add(value, policy_id, asset_name, quantity)
          },
        )
      } else {
        value
      }
    },
  )
}

test restricted_to_1() {
  let self = from_lovelace(42) |> add("foo", "", 1)
  restricted_to(self, []) == zero
}

test restricted_to_2() {
  let self = from_lovelace(42) |> add("foo", "", 1)
  restricted_to(self, [ada_policy_id]) == from_lovelace(42)
}

test restricted_to_3() {
  let self = from_lovelace(42) |> add("foo", "", 1) |> add("bar", "", 1)
  restricted_to(self, ["foo", "bar"]) == without_lovelace(self)
}

test restricted_to_4() {
  let self = from_lovelace(42) |> add("foo", "bar", 1) |> add("foo", "baz", 1)
  restricted_to(self, ["foo"]) == without_lovelace(self)
}

test restricted_to_5() {
  let self = from_lovelace(42) |> add("foo", "bar", 1) |> add("foo", "baz", 1)
  restricted_to(self, [ada_policy_id, "foo"]) == self
}

/// Get a `Value` excluding Ada.
pub fn without_lovelace(self: Value) -> Value {
  dict.delete(self.inner, ada_policy_id)
    |> Value
}

test without_lovelace_1() {
  let v = from_lovelace(1000000)
  without_lovelace(v) == zero
}

test without_lovelace_2() {
  let v = from_lovelace(1000000)
  let v2 = from_lovelace(50000000)
  without_lovelace(v) == without_lovelace(v2)
}

test without_lovelace_3() {
  let v =
    from_asset(#"010203", #"040506", 100)
      |> add(ada_policy_id, ada_asset_name, 100000000)
  let v2 = from_asset(#"010203", #"040506", 100)
  without_lovelace(v) == without_lovelace(v2) && without_lovelace(v) == v2
}

// ## Transforming

/// Flatten a `Value` as list of 3-tuple `(PolicyId, AssetName, Quantity)`.
///
/// Handy to manipulate values as uniform lists.
pub fn flatten(self: Value) -> List<(PolicyId, AssetName, Int)> {
  dict.foldr(
    self.inner,
    [],
    fn(policy_id, asset_list, value) {
      dict.foldr(
        asset_list,
        value,
        fn(asset_name, quantity, xs) {
          [(policy_id, asset_name, quantity), ..xs]
        },
      )
    },
  )
}

/// Flatten a `Value` as a list of results, possibly discarding some along the way.
///
/// When the transform function returns `None`, the result is discarded altogether.
pub fn flatten_with(
  self: Value,
  with: fn(PolicyId, AssetName, Int) -> Option<result>,
) -> List<result> {
  dict.foldr(
    self.inner,
    [],
    fn(policy_id, asset_list, value) {
      dict.foldr(
        asset_list,
        value,
        fn(asset_name, quantity, xs) {
          when with(policy_id, asset_name, quantity) is {
            None -> xs
            Some(x) -> [x, ..xs]
          }
        },
      )
    },
  )
}

test flatten_with_1() {
  flatten_with(zero, fn(p, a, q) { Some((p, a, q)) }) == []
}

test flatten_with_2() {
  let v =
    zero
      |> add("a", "1", 14)
      |> add("b", "", 42)
      |> add("a", "2", 42)

  flatten_with(
    v,
    fn(p, a, q) {
      if q == 42 {
        Some((p, a))
      } else {
        None
      }
    },
  ) == [("a", "2"), ("b", "")]
}

/// Reduce a value into a single result
///
/// ```
/// assets.zero
///  |> assets.add("a", "1", 10)
///  |> assets.add("b", "2", 20)
///  |> assets.reduce(v, 0, fn(_, _, quantity, acc) { acc + quantity })
/// // 30
/// ```
pub fn reduce(
  self: Value,
  start: result,
  with: fn(PolicyId, AssetName, Int, result) -> result,
) -> result {
  dict.foldr(
    self.inner,
    start,
    fn(policy_id, asset_list, result) {
      dict.foldr(asset_list, result, with(policy_id, _, _, _))
    },
  )
}

test reduce_1() {
  let v =
    zero
      |> add("a", "1", 10)
      |> add("b", "2", 20)
  let result = reduce(v, 0, fn(_, _, quantity, acc) { acc + quantity })
  result == 30
}

test reduce_2() {
  let v =
    zero
      |> add("a", "1", 5)
      |> add("a", "2", 15)
      |> add("b", "", 10)
  let result =
    reduce(
      v,
      [],
      fn(policy_id, asset_name, _, acc) { [(policy_id, asset_name), ..acc] },
    )
  result == [("a", "1"), ("a", "2"), ("b", "")]
}

test reduce_3() {
  let v = zero
  let result = reduce(v, 1, fn(_, _, quantity, acc) { acc + quantity })
  result == 1
}

/// Convert the value into a dictionary of dictionaries.
pub fn to_dict(self: Value) -> Dict<PolicyId, Dict<AssetName, Int>> {
  self.inner
}
