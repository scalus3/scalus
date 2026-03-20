use aiken/collection/list
use aiken/fuzz.{
  and_then, bool, byte, bytearray, constant, either3, either4, either5, either6,
  either7, either8, either9, int, int_between, label, list_between,
  list_with_elem, map, map2, map3, map4, map5, map6, map7, map8, map9, one_of,
  set, set_between, sublist, such_that, tuple, tuple3, tuple4, tuple5, tuple6,
  tuple7, tuple8, tuple9,
}
use aiken/math
use aiken/primitive/bytearray
use aiken/primitive/string

test prop_int_distribution(n via int()) {
  label(
    if n < -255 {
      fail @"n < -max_u8"
    } else if n < 0 {
      @"[-255; 0["
    } else if n == 0 {
      @"0"
    } else if n < 256 {
      @"]0; 255]"
    } else if n < 16383 {
      @"[256; 16383]"
    } else {
      fail @"n > 16383"
    },
  )

  True
}

test prop_int_between_distribution(n via int_between(-100, 100)) {
  buckets(n, -100, 100, fn(n) { n + 25 })
  n >= -100 && n <= 100
}

test prop_int_between_large(
  n via int_between(0, 340_282_366_920_938_463_463_374_607_431_768_211_455),
) fail {
  n <= 18_446_744_073_709_551_615
}

test prop_bytearray_simplify(bytes via fuzz.bytearray()) fail once {
  bytes == ""
}

test prop_bool_distribution(is_true via bool()) {
  label(
    if is_true {
      @"True"
    } else {
      @"False"
    },
  )

  True
}

test prop_list_distribution_small(xs via list_between(int(), 0, 10)) {
  let len = list.length(xs)
  buckets(len, 0, 11, fn(n) { n + 1 })
  len >= 0 && len <= 10
}

fn is_even(x: Int) -> Bool {
  x % 2 == 0
}

test prop_such_that(x via such_that(int(), is_even)) {
  is_even(x)
}

test prop_list_exactly(xs via list_between(int(), 3, 3)) {
  list.length(xs) == 3
}

fn list_at_least() -> Fuzzer<(Int, List<Int>)> {
  let min <- and_then(int_between(0, 30))
  let xs <- map(fuzz.list_at_least(int(), min))
  (min, xs)
}

test prop_list_at_least(params via list_at_least()) {
  let (min, xs) = params
  list.length(xs) >= min
}

fn list_at_most() -> Fuzzer<(Int, List<Int>)> {
  let max <- and_then(int_between(-10, 10))
  let xs <- map(fuzz.list_at_most(int(), max))
  (max, xs)
}

test prop_list_at_most(params via list_at_most()) {
  let (max, xs) = params
  list.length(xs) <= math.max(0, max)
}

fn int_at_least() -> Fuzzer<(Int, Int)> {
  let min <- and_then(int())
  let n <- map(fuzz.int_at_least(min))
  (min, n)
}

test prop_int_at_least(params via int_at_least()) {
  let (min, n) = params
  n >= min
}

fn int_at_most() -> Fuzzer<(Int, Int)> {
  let max <- and_then(int())
  let n <- map(fuzz.int_at_most(max))
  (max, n)
}

test prop_int_at_most(params via int_at_most()) {
  let (max, n) = params
  n <= max
}

test prop_list_with_elem(xs via list_with_elem(int())) {
  let (xs, x) = xs
  list.has(xs, x)
}

fn list_with_sublist() -> Fuzzer<(List<Int>, List<Int>)> {
  let xs <- and_then(fuzz.list(int()))
  let sub <- map(sublist(xs))
  (xs, sub)
}

test prop_list_with_subset(params via list_with_sublist()) {
  let (xs, sub) = params
  list.all(sub, fn(x) { list.has(xs, x) })
}

fn bytearray_between() -> Fuzzer<(Int, Int, ByteArray)> {
  let min <- and_then(int_between(0, 50))
  let max <- and_then(int_between(min, 5 * min))
  let bytes <- map(fuzz.bytearray_between(min, max))
  (min, max, bytes)
}

test prop_bytearray_between(params via bytearray_between()) {
  let (min, max, bytes) = params

  let len = bytearray.length(bytes)

  let mid = ( min + max ) / 2

  label(
    if len < min {
      @"< min (impossible)"
    } else if len <= mid {
      @"<= mid (~50%)"
    } else if len <= max {
      @"<= max (~50%)"
    } else {
      @"> max (impossible)"
    },
  )

  len >= min && len <= max
}

test prop_int_between_boundary_up(n via int_between(0, 2)) fail once {
  n < 2
}

test prop_int_between_smallest_range(n via int_between(0, 1)) {
  label(
    if n == 0 {
      @"= 0"
    } else {
      @"= 1"
    },
  )
  n == 0 || n == 1
}

test prop_int_between_boundary_down(n via int_between(0, 10)) fail once {
  n > 0
}

test prop_one_of_upper(i via one_of([1, 3, 5, 7])) fail once {
  i != 7
}

test prop_set(xs via set(int())) {
  let ys =
    list.reduce(
      xs,
      [],
      fn(known, x) {
        expect !list.has(known, x)
        [x, ..known]
      },
    )

  list.length(ys) == list.length(xs)
}

test prop_set_between_distribution(n via set_between(int_between(0, 50), 3, 13)) {
  let len = n |> list.length
  label(len |> string.from_int)
  True
}

// This property simply illustrate a case where the `set`
// fuzzer would fail and not loop forever after not being
// able to satisfy the demand (not enough entropy in the
// input domain).
//
// test prop_set_exhausted(xs via set(int_between(0, 3))) {
//   True
// }

test prop_either3(
  lbl via either3(constant(@"a"), constant(@"b"), constant(@"c")),
) {
  label(lbl)
}

test prop_either4(
  lbl via either4(
    constant(@"a"),
    constant(@"b"),
    constant(@"c"),
    constant(@"d"),
  ),
) {
  label(lbl)
}

test prop_either5(
  lbl via either5(
    constant(@"a"),
    constant(@"b"),
    constant(@"c"),
    constant(@"d"),
    constant(@"e"),
  ),
) {
  label(lbl)
}

test prop_either6(
  lbl via either6(
    constant(@"a"),
    constant(@"b"),
    constant(@"c"),
    constant(@"d"),
    constant(@"e"),
    constant(@"f"),
  ),
) {
  label(lbl)
}

test prop_either7(
  lbl via either7(
    constant(@"a"),
    constant(@"b"),
    constant(@"c"),
    constant(@"d"),
    constant(@"e"),
    constant(@"f"),
    constant(@"g"),
  ),
) {
  label(lbl)
}

test prop_either8(
  lbl via either8(
    constant(@"a"),
    constant(@"b"),
    constant(@"c"),
    constant(@"d"),
    constant(@"e"),
    constant(@"f"),
    constant(@"g"),
    constant(@"h"),
  ),
) {
  label(lbl)
}

test prop_either9(
  lbl via either9(
    constant(@"a"),
    constant(@"b"),
    constant(@"c"),
    constant(@"d"),
    constant(@"e"),
    constant(@"f"),
    constant(@"g"),
    constant(@"h"),
    constant(@"i"),
  ),
) {
  label(lbl)
}

/// A small function for automatically labelling a range of ints.
fn buckets(n, start, end, increment) -> Void {
  expect n >= start
  let next = increment(start)
  if next >= end {
    label(
      [string.from_int(start), @"->", string.from_int(end)]
        |> string.join(@" "),
    )
  } else if n < next {
    label(
      [string.from_int(start), @"->", string.from_int(next)]
        |> string.join(@" "),
    )
  } else {
    buckets(n, next, end, increment)
  }
}

// ## Tuple Fuzzer Tests

fn data_fuzzer() -> Fuzzer<Data> {
  either4(
    {
      let bool <- map(bool())
      let data: Data = bool
      data
    },
    {
      let byte <- map(byte())
      let data: Data = byte
      data
    },
    {
      let bytearray <- map(bytearray())
      let data: Data = bytearray
      data
    },
    {
      let int <- map(int())
      let data: Data = int
      data
    },
  )
}

fn prop_tuple2() -> Fuzzer<Bool> {
  let a <- and_then(data_fuzzer())
  let b <- and_then(data_fuzzer())

  let fuzzer_a = a |> constant
  let fuzzer_b = b |> constant

  let l_tuple <- and_then(tuple(fuzzer_a, fuzzer_b))
  let r_tuple <- and_then(map2(fuzzer_a, fuzzer_b, fn(a, b) { (a, b) }))

  constant(l_tuple == r_tuple)
}

fn prop_tuple3() -> Fuzzer<Bool> {
  let a <- and_then(data_fuzzer())
  let b <- and_then(data_fuzzer())
  let c <- and_then(data_fuzzer())

  let fuzzer_a = a |> constant
  let fuzzer_b = b |> constant
  let fuzzer_c = c |> constant

  let l_tuple <- and_then(tuple3(fuzzer_a, fuzzer_b, fuzzer_c))
  let r_tuple <-
    and_then(map3(fuzzer_a, fuzzer_b, fuzzer_c, fn(a, b, c) { (a, b, c) }))

  constant(l_tuple == r_tuple)
}

fn prop_tuple4() -> Fuzzer<Bool> {
  let a <- and_then(data_fuzzer())
  let b <- and_then(data_fuzzer())
  let c <- and_then(data_fuzzer())
  let d <- and_then(data_fuzzer())

  let fuzzer_a = a |> constant
  let fuzzer_b = b |> constant
  let fuzzer_c = c |> constant
  let fuzzer_d = d |> constant

  let l_tuple <- and_then(tuple4(fuzzer_a, fuzzer_b, fuzzer_c, fuzzer_d))
  let r_tuple <-
    and_then(
      map4(
        fuzzer_a,
        fuzzer_b,
        fuzzer_c,
        fuzzer_d,
        fn(a, b, c, d) { (a, b, c, d) },
      ),
    )

  constant(l_tuple == r_tuple)
}

fn prop_tuple5() -> Fuzzer<Bool> {
  let a <- and_then(data_fuzzer())
  let b <- and_then(data_fuzzer())
  let c <- and_then(data_fuzzer())
  let d <- and_then(data_fuzzer())
  let e <- and_then(data_fuzzer())

  let fuzzer_a = a |> constant
  let fuzzer_b = b |> constant
  let fuzzer_c = c |> constant
  let fuzzer_d = d |> constant
  let fuzzer_e = e |> constant

  let l_tuple <-
    and_then(tuple5(fuzzer_a, fuzzer_b, fuzzer_c, fuzzer_d, fuzzer_e))
  let r_tuple <-
    and_then(
      map5(
        fuzzer_a,
        fuzzer_b,
        fuzzer_c,
        fuzzer_d,
        fuzzer_e,
        fn(a, b, c, d, e) { (a, b, c, d, e) },
      ),
    )

  constant(l_tuple == r_tuple)
}

fn prop_tuple6() -> Fuzzer<Bool> {
  let a <- and_then(data_fuzzer())
  let b <- and_then(data_fuzzer())
  let c <- and_then(data_fuzzer())
  let d <- and_then(data_fuzzer())
  let e <- and_then(data_fuzzer())
  let f <- and_then(data_fuzzer())

  let fuzzer_a = a |> constant
  let fuzzer_b = b |> constant
  let fuzzer_c = c |> constant
  let fuzzer_d = d |> constant
  let fuzzer_e = e |> constant
  let fuzzer_f = f |> constant

  let l_tuple <-
    and_then(tuple6(fuzzer_a, fuzzer_b, fuzzer_c, fuzzer_d, fuzzer_e, fuzzer_f))
  let r_tuple <-
    and_then(
      map6(
        fuzzer_a,
        fuzzer_b,
        fuzzer_c,
        fuzzer_d,
        fuzzer_e,
        fuzzer_f,
        fn(a, b, c, d, e, f) { (a, b, c, d, e, f) },
      ),
    )

  constant(l_tuple == r_tuple)
}

fn prop_tuple7() -> Fuzzer<Bool> {
  let a <- and_then(data_fuzzer())
  let b <- and_then(data_fuzzer())
  let c <- and_then(data_fuzzer())
  let d <- and_then(data_fuzzer())
  let e <- and_then(data_fuzzer())
  let f <- and_then(data_fuzzer())
  let g <- and_then(data_fuzzer())

  let fuzzer_a = a |> constant
  let fuzzer_b = b |> constant
  let fuzzer_c = c |> constant
  let fuzzer_d = d |> constant
  let fuzzer_e = e |> constant
  let fuzzer_f = f |> constant
  let fuzzer_g = g |> constant

  let l_tuple <-
    and_then(
      tuple7(
        fuzzer_a,
        fuzzer_b,
        fuzzer_c,
        fuzzer_d,
        fuzzer_e,
        fuzzer_f,
        fuzzer_g,
      ),
    )
  let r_tuple <-
    and_then(
      map7(
        fuzzer_a,
        fuzzer_b,
        fuzzer_c,
        fuzzer_d,
        fuzzer_e,
        fuzzer_f,
        fuzzer_g,
        fn(a, b, c, d, e, f, g) { (a, b, c, d, e, f, g) },
      ),
    )

  constant(l_tuple == r_tuple)
}

fn prop_tuple8() -> Fuzzer<Bool> {
  let a <- and_then(data_fuzzer())
  let b <- and_then(data_fuzzer())
  let c <- and_then(data_fuzzer())
  let d <- and_then(data_fuzzer())
  let e <- and_then(data_fuzzer())
  let f <- and_then(data_fuzzer())
  let g <- and_then(data_fuzzer())
  let h <- and_then(data_fuzzer())

  let fuzzer_a = a |> constant
  let fuzzer_b = b |> constant
  let fuzzer_c = c |> constant
  let fuzzer_d = d |> constant
  let fuzzer_e = e |> constant
  let fuzzer_f = f |> constant
  let fuzzer_g = g |> constant
  let fuzzer_h = h |> constant

  let l_tuple <-
    and_then(
      tuple8(
        fuzzer_a,
        fuzzer_b,
        fuzzer_c,
        fuzzer_d,
        fuzzer_e,
        fuzzer_f,
        fuzzer_g,
        fuzzer_h,
      ),
    )
  let r_tuple <-
    and_then(
      map8(
        fuzzer_a,
        fuzzer_b,
        fuzzer_c,
        fuzzer_d,
        fuzzer_e,
        fuzzer_f,
        fuzzer_g,
        fuzzer_h,
        fn(a, b, c, d, e, f, g, h) { (a, b, c, d, e, f, g, h) },
      ),
    )

  constant(l_tuple == r_tuple)
}

fn prop_tuple9() -> Fuzzer<Bool> {
  let a <- and_then(data_fuzzer())
  let b <- and_then(data_fuzzer())
  let c <- and_then(data_fuzzer())
  let d <- and_then(data_fuzzer())
  let e <- and_then(data_fuzzer())
  let f <- and_then(data_fuzzer())
  let g <- and_then(data_fuzzer())
  let h <- and_then(data_fuzzer())
  let i <- and_then(data_fuzzer())

  let fuzzer_a = a |> constant
  let fuzzer_b = b |> constant
  let fuzzer_c = c |> constant
  let fuzzer_d = d |> constant
  let fuzzer_e = e |> constant
  let fuzzer_f = f |> constant
  let fuzzer_g = g |> constant
  let fuzzer_h = h |> constant
  let fuzzer_i = i |> constant

  let l_tuple <-
    and_then(
      tuple9(
        fuzzer_a,
        fuzzer_b,
        fuzzer_c,
        fuzzer_d,
        fuzzer_e,
        fuzzer_f,
        fuzzer_g,
        fuzzer_h,
        fuzzer_i,
      ),
    )
  let r_tuple <-
    and_then(
      map9(
        fuzzer_a,
        fuzzer_b,
        fuzzer_c,
        fuzzer_d,
        fuzzer_e,
        fuzzer_f,
        fuzzer_g,
        fuzzer_h,
        fuzzer_i,
        fn(a, b, c, d, e, f, g, h, i) { (a, b, c, d, e, f, g, h, i) },
      ),
    )

  constant(l_tuple == r_tuple)
}

fn prop_tuple2_to_tuple9() -> Fuzzer<Bool> {
  let tuple2_map2 <- and_then(prop_tuple2())
  let tuple3_map3 <- and_then(prop_tuple3())
  let tuple4_map4 <- and_then(prop_tuple4())
  let tuple5_map5 <- and_then(prop_tuple5())
  let tuple6_map6 <- and_then(prop_tuple6())
  let tuple7_map7 <- and_then(prop_tuple7())
  let tuple8_map8 <- and_then(prop_tuple8())
  let tuple9_map9 <- and_then(prop_tuple9())

  constant(and {
      tuple2_map2,
      tuple3_map3,
      tuple4_map4,
      tuple5_map5,
      tuple6_map6,
      tuple7_map7,
      tuple8_map8,
      tuple9_map9,
    })
}

test prop_tuples(match via prop_tuple2_to_tuple9()) {
  match
}

test property_test(vs via fuzz.list_between(fuzz.constant(Void), 5, 10)) fail {
  // DO NOT MODIFY BELOW //
  let len = vs |> list.length
  len < 5
  // DO NOT MODIFY ABOVE //
}
