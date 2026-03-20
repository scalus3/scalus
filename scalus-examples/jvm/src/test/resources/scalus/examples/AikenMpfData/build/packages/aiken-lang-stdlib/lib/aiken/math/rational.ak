//// This module implements operations between rational numbers.
////
//// > [!CAUTION]
//// > Internally, rational aren't automatically reduced as this is **only done on-demand**.
//// >
//// > Thus, for example:
//// >
//// > ```aiken
//// > rational.new(2, 3) != rational.new(4, 6)
//// > ```
//// >
//// > Comparing rational values should, therefore, only happen after reduction (see [reduce](#reduce)) or via the [compare](#compare) method.

use aiken/builtin
use aiken/collection/list
use aiken/math
use aiken/option

/// Opaque type used to ensure the sign of the Rational is managed strictly in the numerator.
pub opaque type Rational {
  numerator: Int,
  denominator: Int,
}

// ## Constructing

/// Create a new `Rational` from an `Int`.
///
/// ```aiken
/// Some(rational.from_int(14)) == rational.new(14, 1)
/// Some(rational.from_int(-5)) == rational.new(-5, 1)
/// Some(rational.from_int(0)) == rational.new(0, 1)
/// ```
pub fn from_int(numerator: Int) -> Rational {
  Rational { numerator, denominator: 1 }
}

test from_int_1() {
  and {
    (from_int(14) == ratio(14, 1))?,
    (from_int(-5) == ratio(-5, 1))?,
    (from_int(0) == ratio(0, 1))?,
  }
}

/// An unsafe constructor for `Rational` values. Assumes that the following invariants are
/// enforced:
///
/// - the denominator is positive (the sign is managed in the numerator);
/// - the denominator is not null.
///
/// This function is mainly used as a quick way to construct rationals from literal values.
fn ratio(numerator: Int, denominator: Int) -> Rational {
  Rational { numerator, denominator }
}

/// Make a `Rational` number from the ratio of two integers.
///
/// Returns `None` when the denominator is null.
///
/// ```aiken
/// rational.new(14, 42) == Some(r)
/// rational.new(14, 0) == None
/// ```
pub fn new(numerator: Int, denominator: Int) -> Option<Rational> {
  if denominator == 0 {
    None
  } else if denominator < 0 {
    Some(Rational { numerator: -numerator, denominator: -denominator })
  } else {
    Some(Rational { numerator, denominator })
  }
}

test new_1() {
  and {
    (new(2, 0) == None)?,
    (new(2, 3) == Some(ratio(2, 3)))?,
    (new(-2, 3) == Some(ratio(-2, 3)))?,
    (new(2, -3) == Some(ratio(-2, 3)))?,
    (new(2, 4) == Some(ratio(2, 4)))?,
    (new(-2, -3) == Some(ratio(2, 3)))?,
    (new(-2, -4) == Some(ratio(2, 4)))?,
  }
}

/// A null `Rational`.
pub const zero: Rational = Rational { numerator: 0, denominator: 1 }

test zero_1() {
  zero == ratio(0, 1)
}

// ## Inspecting

/// Get the denominator of a rational value.
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// rational.denominator(x) == 3
/// ```
pub fn denominator(self: Rational) -> Int {
  self.denominator
}

test denominator_1() {
  expect Some(x) = new(2, 3)
  expect Some(y) = new(-2, 3)
  expect Some(z) = new(2, -3)
  expect Some(w) = new(-2, -3)
  and {
    (denominator(x) == 3)?,
    (denominator(y) == 3)?,
    (denominator(z) == 3)?,
    (denominator(w) == 3)?,
  }
}

/// Get the numerator of a rational value.
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// rational.numerator(x) == 2
/// ```
pub fn numerator(self: Rational) -> Int {
  self.numerator
}

test numerator_1() {
  expect Some(x) = new(2, 3)
  expect Some(y) = new(-2, 3)
  expect Some(z) = new(2, -3)
  expect Some(w) = new(-2, -3)

  and {
    (numerator(x) == 2)?,
    (numerator(y) == -2)?,
    (numerator(z) == -2)?,
    (numerator(w) == 2)?,
  }
}

// ## Modifying

/// Absolute value of a `Rational`.
///
/// ```aiken
/// expect Some(x) = rational.new(3, 2)
/// expect Some(y) = rational.new(-3, 2)
///
/// rational.abs(x) == x
/// rational.abs(y) == x
/// ```
pub fn abs(self: Rational) -> Rational {
  let Rational { numerator: a_n, denominator: a_d } = self
  Rational { numerator: math.abs(a_n), denominator: a_d }
}

test abs_examples() {
  and {
    (abs(ratio(5, 2)) == ratio(5, 2))?,
    (abs(ratio(-5, 2)) == ratio(5, 2))?,
    (abs(ratio(5, 2)) == abs(ratio(-5, 2)))?,
  }
}

/// Change the sign of a `Rational`.
///
/// ```aiken
/// expect Some(x) = rational.new(3, 2)
/// expect Some(y) = rational.new(-3, 2)
///
/// rational.negate(x) == y
/// rational.negate(y) == x
/// ```
pub fn negate(a: Rational) -> Rational {
  let Rational { numerator: a_n, denominator: a_d } = a
  Rational { numerator: -a_n, denominator: a_d }
}

test negate_1() {
  and {
    (negate(ratio(5, 2)) == ratio(-5, 2))?,
    (negate(ratio(-5, 2)) == ratio(5, 2))?,
    (negate(negate(ratio(5, 2))) == ratio(5, 2))?,
  }
}

/// Reciprocal of a `Rational` number. That is, a new `Rational` where the
/// numerator and denominator have been swapped.
///
/// ```aiken
/// expect Some(x) = rational.new(2, 5)
/// rational.reciprocal(x) == rational.new(5, 2)
///
/// let y = rational.zero
/// rational.reciprocal(y) == None
/// ```
pub fn reciprocal(self: Rational) -> Option<Rational> {
  let Rational { numerator: a_n, denominator: a_d } = self
  if a_n < 0 {
    Some(Rational { numerator: -a_d, denominator: -a_n })
  } else if a_n > 0 {
    Some(Rational { numerator: a_d, denominator: a_n })
  } else {
    None
  }
}

test reciprocal_1() {
  and {
    (reciprocal(ratio(5, 2)) == new(2, 5))?,
    (reciprocal(ratio(-5, 2)) == new(-2, 5))?,
    (reciprocal(ratio(0, 2)) == None)?,
    (reciprocal(ratio(2, 3)) == new(3, 2))?,
    (reciprocal(ratio(-2, 3)) == new(-3, 2))?,
  }
}

/// Reduce a rational to its irreducible form. This operation makes the
/// numerator and denominator coprime.
///
/// ```aiken
/// expect Some(x) = rational.new(80, 200)
/// Some(rational.reduce(x)) == rational.new(2, 5)
/// ```
pub fn reduce(self: Rational) -> Rational {
  let Rational { numerator: a_n, denominator: a_d } = self
  let d = math.gcd(a_n, a_d)
  Rational { numerator: a_n / d, denominator: a_d / d }
}

test reduce_1() {
  and {
    (reduce(ratio(80, 200)) == ratio(2, 5))?,
    (reduce(ratio(-5, 1)) == ratio(-5, 1))?,
    (reduce(ratio(0, 3)) == ratio(0, 1))?,
  }
}

// ## Combining

// ### Arithmetic operations

/// Addition: sum of two rational values
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// expect Some(y) = rational.new(3, 4)
///
/// Some(rational.add(x, y)) == rational.new(17, 12)
/// ```
pub fn add(left: Rational, right: Rational) -> Rational {
  let Rational { numerator: a_n, denominator: a_d } = left
  let Rational { numerator: b_n, denominator: b_d } = right
  Rational { numerator: a_n * b_d + b_n * a_d, denominator: a_d * b_d }
}

test add_1() {
  add(ratio(2, 3), ratio(3, 4)) == ratio(17, 12)
}

test add_2() {
  add(ratio(-2, 3), ratio(3, 4)) == ratio(1, 12)
}

/// Division: quotient of two rational values. Returns `None` when the second
/// value is null.
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// expect Some(y) = rational.new(3, 4)
///
/// rational.div(x, y) == rational.new(8, 9)
/// ```
pub fn div(left: Rational, right: Rational) -> Option<Rational> {
  reciprocal(right) |> option.map(mul(left, _))
}

test div_1() {
  div(ratio(2, 3), ratio(3, 4)) == new(8, 9)
}

test div_2() {
  div(ratio(2, 3), ratio(-3, 4)) == new(-8, 9)
}

/// Multiplication: the product of two rational values.
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// expect Some(y) = rational.new(3, 4)
///
/// Some(rational.mul(x, y)) == rational.new(6, 12)
/// ```
pub fn mul(left: Rational, right: Rational) -> Rational {
  let Rational { numerator: a_n, denominator: a_d } = left
  let Rational { numerator: b_n, denominator: b_d } = right
  Rational { numerator: a_n * b_n, denominator: a_d * b_d }
}

test mul_1() {
  mul(ratio(2, 3), ratio(3, 4)) == ratio(6, 12)
}

test mul_2() {
  mul(ratio(-2, 3), ratio(-3, 4)) == ratio(6, 12)
}

test mul_3() {
  let result =
    ratio(2, 5)
      |> mul(ratio(1, 8))
      |> mul(ratio(3, 10))
      |> mul(ratio(21, 100))
      |> mul(ratio(3, 5))
      |> mul(ratio(2, 8))
      |> mul(ratio(4, 10))
      |> mul(ratio(22, 100))
      |> reduce

  result == ratio(2079, 50000000)
}

/// Subtraction: difference of two rational values
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// expect Some(y) = rational.new(3, 4)
///
/// Some(rational.sub(x, y)) == rational.new(-1, 12)
/// ```
pub fn sub(left: Rational, right: Rational) -> Rational {
  let Rational { numerator: a_n, denominator: a_d } = left
  let Rational { numerator: b_n, denominator: b_d } = right
  Rational { numerator: a_n * b_d - b_n * a_d, denominator: a_d * b_d }
}

test sub_1() {
  sub(ratio(2, 3), ratio(3, 4)) == ratio(-1, 12)
}

test sub_2() {
  sub(ratio(2, 3), ratio(-3, 4)) == ratio(17, 12)
}

test sub_3() {
  sub(ratio(-2, 3), ratio(3, 4)) == ratio(-17, 12)
}

// ### Ordering

/// Compare two rationals for an ordering. This is safe to use even for
/// non-reduced rationals.
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// expect Some(y) = rational.new(3, 4)
/// expect Some(z) = rational.new(4, 6)
///
/// compare(x, y) == Less
/// compare(y, x) == Greater
/// compare(x, x) == Equal
/// compare(x, z) == Equal
/// ```
pub fn compare(left: Rational, right: Rational) -> Ordering {
  let Rational { numerator: a_n, denominator: a_d } = left
  let Rational { numerator: b_n, denominator: b_d } = right

  let l = a_n * b_d
  let r = b_n * a_d

  if l < r {
    Less
  } else if l > r {
    Greater
  } else {
    Equal
  }
}

test compare_1() {
  expect Some(x) = new(2, 3)
  expect Some(y) = new(3, 4)
  expect Some(z) = new(4, 6)
  and {
    compare(x, y) == Less,
    compare(y, x) == Greater,
    compare(x, x) == Equal,
    compare(x, z) == Equal,
  }
}

/// Comparison of two rational values using a chosen heuristic. For example:
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// expect Some(y) = rational.new(3, 4)
///
/// rational.compare_with(x, >, y) == False
/// rational.compare_with(y, >, x) == True
/// rational.compare_with(x, >, x) == False
/// rational.compare_with(x, >=, x) == True
/// rational.compare_with(x, ==, x) == True
/// rational.compare_with(x, ==, y) == False
/// ```
pub fn compare_with(
  left: Rational,
  with: fn(Int, Int) -> Bool,
  right: Rational,
) -> Bool {
  let Rational { numerator: a_n, denominator: a_d } = left
  let Rational { numerator: b_n, denominator: b_d } = right
  with(a_n * b_d, b_n * a_d)
}

// TODO: Rewrite tests using binary-operator as first-class functions once aiken-lang/aiken#619 is merged.

test compare_with_eq() {
  let eq =
    compare_with(_, fn(l, r) { l == r }, _)

  expect Some(x) = new(2, 3)
  expect Some(y) = new(3, 4)

  !eq(x, y)? && !eq(y, x)? && eq(x, x)?
}

test compare_with_neq() {
  let neq =
    compare_with(_, fn(l, r) { l != r }, _)

  expect Some(x) = new(2, 3)
  expect Some(y) = new(3, 4)

  neq(x, y)? && neq(y, x)? && !neq(x, x)?
}

test compare_with_gte() {
  let gte =
    compare_with(_, fn(l, r) { l >= r }, _)

  expect Some(x) = new(2, 3)
  expect Some(y) = new(3, 4)

  !gte(x, y)? && gte(y, x)? && gte(x, x)?
}

test compare_with_gt() {
  let gt =
    compare_with(_, fn(l, r) { l > r }, _)

  expect Some(x) = new(2, 3)
  expect Some(y) = new(3, 4)

  !gt(x, y)? && gt(y, x)? && !gt(x, x)?
}

test compare_with_lte() {
  let lte =
    compare_with(_, fn(l, r) { l <= r }, _)

  expect Some(x) = new(2, 3)
  expect Some(y) = new(3, 4)

  lte(x, y)? && !lte(y, x)? && lte(x, x)?
}

test compare_with_lt() {
  let lt =
    compare_with(_, fn(l, r) { l < r }, _)

  expect Some(x) = new(2, 3)
  expect Some(y) = new(3, 4)

  lt(x, y)? && !lt(y, x)? && !lt(x, x)?
}

// ### Means

/// Calculate the arithmetic mean between two `Rational` values.
///
/// ```aiken
/// let x = rational.from_int(0)
/// let y = rational.from_int(1)
/// let z = rational.from_int(2)
///
/// expect Some(result) = rational.arithmetic_mean([x, y, z])
///
/// rational.compare(result, y) == Equal
/// ```
pub fn arithmetic_mean(self: List<Rational>) -> Option<Rational> {
  div(list.foldr(self, zero, add), from_int(list.length(self)))
}

test arithmetic_mean_1() {
  let x = ratio(1, 2)
  let y = ratio(1, 2)
  expect Some(z) = arithmetic_mean([x, y])
  reduce(z) == ratio(1, 2)
}

test arithmetic_mean_2() {
  let x = ratio(1, 1)
  let y = ratio(2, 1)
  expect Some(z) = arithmetic_mean([x, y])
  reduce(z) == ratio(3, 2)
}

test arithmetic_mean_3() {
  let xs =
    [
      ratio(1, 1),
      ratio(2, 1),
      ratio(3, 1),
      ratio(4, 1),
      ratio(5, 1),
      ratio(6, 1),
    ]
  expect Some(z) = arithmetic_mean(xs)
  reduce(z) == ratio(7, 2)
}

/// Calculate the geometric mean between two `Rational` values. This returns
/// either the exact result or the smallest integer nearest to the square root
/// for the numerator and denominator.
///
/// ```aiken
/// expect Some(x) = rational.new(1, 3)
/// expect Some(y) = rational.new(1, 6)
///
/// rational.geometric_mean(x, y) == rational.new(1, 4)
/// ```
pub fn geometric_mean(left: Rational, right: Rational) -> Option<Rational> {
  let Rational { numerator: a_n, denominator: a_d } = left
  let Rational { numerator: b_n, denominator: b_d } = right
  when math.sqrt(a_n * b_n) is {
    Some(numerator) ->
      when math.sqrt(a_d * b_d) is {
        Some(denominator) -> Some(Rational { numerator, denominator })
        None -> None
      }
    None -> None
  }
}

test geometric_mean1() {
  expect Some(x) = new(1, 2)
  expect Some(y) = new(1, 2)
  geometric_mean(x, y) == new(1, 2)
}

test geometric_mean2() {
  expect Some(x) = new(-1, 2)
  expect Some(y) = new(1, 2)
  geometric_mean(x, y) == None
}

test geometric_mean3() {
  expect Some(x) = new(1, 2)
  expect Some(y) = new(-1, 2)
  geometric_mean(x, y) == None
}

test geometric_mean4() {
  expect Some(x) = new(1, 3)
  expect Some(y) = new(1, 6)
  geometric_mean(x, y) == new(1, 4)
}

test geometric_mean5() {
  expect Some(x) = new(67, 2500)
  expect Some(y) = new(35331, 1000)
  expect Some(yi) = reciprocal(y)
  geometric_mean(x, yi) == new(258, 9398)
}

// ## Transforming

/// Returns the smallest `Int` not less than a given `Rational`
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// rational.ceil(x) == 1
///
/// expect Some(y) = rational.new(44, 14)
/// rational.ceil(y) == 4
///
/// expect Some(z) = rational.new(-14, 3)
/// rational.ceil(z) == -4
/// ```
pub fn ceil(self: Rational) -> Int {
  let Rational { numerator, denominator } = self
  if builtin.remainder_integer(numerator, denominator) > 0 {
    builtin.quotient_integer(numerator, denominator) + 1
  } else {
    builtin.quotient_integer(numerator, denominator)
  }
}

test ceil_1() {
  and {
    (ceil(ratio(13, 5)) == 3)?,
    (ceil(ratio(15, 5)) == 3)?,
    (ceil(ratio(16, 5)) == 4)?,
    (ceil(ratio(-3, 5)) == 0)?,
    (ceil(ratio(-5, 5)) == -1)?,
    (ceil(ratio(-14, 3)) == -4)?,
    (ceil(ratio(-14, 6)) == -2)?,
    (ceil(ratio(44, 14)) == 4)?,
  }
}

/// Returns the greatest `Int` no greater than a given `Rational`
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// rational.floor(x) == 0
///
/// expect Some(y) = rational.new(44, 14)
/// rational.floor(y) == 3
///
/// expect Some(z) = rational.new(-14, 3)
/// rational.floor(z) == -5
/// ```
pub fn floor(self: Rational) -> Int {
  let Rational { numerator: a_n, denominator: a_d } = self
  a_n / a_d
}

test floor_1() {
  and {
    (floor(ratio(5, 2)) == 2)?,
    (floor(ratio(5, 3)) == 1)?,
    (floor(ratio(5, 4)) == 1)?,
    (floor(ratio(5, 5)) == 1)?,
    (floor(ratio(5, 6)) == 0)?,
    (floor(ratio(8, 3)) == 2)?,
    (floor(ratio(-14, 3)) == -5)?,
  }
}

/// Computes the rational number x raised to the power y. Returns `None` for
/// invalid exponentiation.
///
/// ```aiken
/// expect Some(x) = rational.new(50, 2500)
/// rational.reduce(rational.pow(x, 3)) == rational.new(1, 125000)
///
/// expect Some(x) = rational.new(50, 2500)
/// rational.reduce(rational.pow(x, -3)) == rational.new(125000, 1)
/// ```
pub fn pow(x: Rational, y: Int) -> Option<Rational> {
  let Rational { numerator: a, denominator: b } = x

  if a == 0 && y <= 0 {
    None
  } else if y > 0 {
    Some(Rational { numerator: math.pow(a, y), denominator: math.pow(b, y) })
  } else if y < 0 {
    Some(Rational { numerator: math.pow(b, -y), denominator: math.pow(a, -y) })
  } else {
    Some(Rational { numerator: 1, denominator: 1 })
  }
}

test pow_negative_exponent_non_zero_fraction() {
  expect Some(base) = new(50, 2500)
  expect Some(calculated_result) = pow(base, -3)
  expect Some(expected_result) = new(125000, 1)
  reduce(calculated_result) == expected_result
}

test pow_positive_exponent() {
  expect Some(base) = new(50, 2500)
  expect Some(calculated_result) = pow(base, 3)
  expect Some(expected_result) = new(1, 125000)
  reduce(calculated_result) == expected_result
}

test pow_exponent_zero() {
  expect Some(base) = new(50, 2500)
  pow(base, 0) == new(1, 1)
}

test pow_rational_zero_exponent_zero() {
  expect Some(base) = new(0, 1)
  pow(base, 0) == None
}

/// Returns the proper fraction of a given `Rational` `r`. That is, a 2-tuple of
/// an `Int` and `Rational` (n, f) such that:
///
/// - `r = n + f`;
/// - `n` and `f` have the same sign as `r`;
/// - `f` has an absolute value less than 1.
pub fn proper_fraction(self: Rational) -> (Int, Rational) {
  let Rational { numerator, denominator } = self
  (
    builtin.quotient_integer(numerator, denominator),
    Rational {
      numerator: builtin.remainder_integer(numerator, denominator),
      denominator,
    },
  )
}

test proper_fraction_1() {
  let r = ratio(10, 7)
  let (n, f) = proper_fraction(r)
  and {
    (n == 1)?,
    (f == ratio(3, 7))?,
    (r == add(from_int(n), f))?,
  }
}

test proper_fraction_2() {
  let r = ratio(-10, 7)
  let (n, f) = proper_fraction(r)
  and {
    (n == -1)?,
    (f == ratio(-3, 7))?,
    (r == add(from_int(n), f))?,
  }
}

test proper_fraction_3() {
  let r = ratio(4, 2)
  let (n, f) = proper_fraction(r)
  and {
    (n == 2)?,
    (f == ratio(0, 2))?,
    (r == add(from_int(n), f))?,
  }
}

/// Round the argument to the nearest whole number. If the argument is
/// equidistant between two values, the greater value is returned (it
/// rounds half towards positive infinity).
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// rational.round(x) == 1
///
/// expect Some(y) = rational.new(3, 2)
/// rational.round(y) == 2
///
/// expect Some(z) = rational.new(-3, 2)
/// rational.round(z) == -1
/// ```
///
/// > [!CAUTION]
/// > This behaves differently than _Haskell_. If you're coming from `PlutusTx`, beware that in Haskell, rounding on equidistant values depends on the whole number being odd or even.
/// > If you need this behaviour, use [`round_even`](#round_even).
pub fn round(self: Rational) -> Int {
  let (n, f) = proper_fraction(self)

  let is_negative = f.numerator < 0

  when compare(abs(f), ratio(1, 2)) is {
    Less -> n
    Equal ->
      if is_negative {
        n
      } else {
        n + 1
      }
    Greater ->
      if is_negative {
        n - 1
      } else {
        n + 1
      }
  }
}

test round_1() {
  and {
    (round(ratio(10, 7)) == 1)?,
    (round(ratio(11, 7)) == 2)?,
    (round(ratio(3, 2)) == 2)?,
    (round(ratio(5, 2)) == 3)?,
    (round(ratio(-3, 2)) == -1)?,
    (round(ratio(-2, 3)) == -1)?,
    (round(ratio(-10, 7)) == -1)?,
    (round(ratio(4, 2)) == 2)?,
  }
}

/// Round the argument to the nearest whole number. If the argument is
/// equidistant between two values, it returns the value that is even (it
/// rounds half to even, also known as 'banker's rounding').
///
/// ```aiken
/// expect Some(w) = rational.new(2, 3)
/// rational.round_even(w) == 1
///
/// expect Some(x) = rational.new(3, 2)
/// rational.round_even(x) == 2
///
/// expect Some(y) = rational.new(5, 2)
/// rational.round_even(y) == 2
///
/// expect Some(y) = rational.new(-3, 2)
/// rational.round_even(y) == -2
/// ```
pub fn round_even(self: Rational) -> Int {
  let (n, f) = proper_fraction(self)

  let m =
    when compare(f, ratio(0, 1)) is {
      Less -> -1
      _ -> 1
    }

  let is_even = n % 2 == 0

  when compare(abs(f), ratio(1, 2)) is {
    Less -> n
    Equal ->
      if is_even {
        n
      } else {
        n + m
      }
    Greater -> n + m
  }
}

test round_even_1() {
  and {
    (round_even(ratio(10, 7)) == 1)?,
    (round_even(ratio(11, 7)) == 2)?,
    (round_even(ratio(3, 2)) == 2)?,
    (round_even(ratio(5, 2)) == 2)?,
    (round_even(ratio(-3, 2)) == -2)?,
    (round_even(ratio(-2, 3)) == -1)?,
    (round_even(ratio(-10, 7)) == -1)?,
    (round_even(ratio(4, 2)) == 2)?,
  }
}

/// Returns the nearest `Int` between zero and a given `Rational`.
///
/// ```aiken
/// expect Some(x) = rational.new(2, 3)
/// rational.truncate(x) == 0
///
/// expect Some(y) = rational.new(44, 14)
/// rational.truncate(y) == 3
///
/// expect Some(z) = rational.new(-14, 3)
/// rational.truncate(z) == -4
/// ```
pub fn truncate(self: Rational) -> Int {
  let Rational { numerator: a_n, denominator: a_d } = self
  builtin.quotient_integer(a_n, a_d)
}

test truncate_1() {
  and {
    (truncate(ratio(5, 2)) == 2)?,
    (truncate(ratio(5, 3)) == 1)?,
    (truncate(ratio(5, 4)) == 1)?,
    (truncate(ratio(5, 5)) == 1)?,
    (truncate(ratio(5, 6)) == 0)?,
    (truncate(ratio(8, 3)) == 2)?,
    (truncate(ratio(-14, 3)) == -4)?,
  }
}
