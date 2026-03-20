use aiken/fuzz.{both, either, map}
use aiken/math/rational.{Rational, new, pow}

const any_positive_rational: Fuzzer<Rational> =
  either(
    map(
      both(fuzz.int_at_least(1), fuzz.int_at_least(1)),
      fn((num, den)) {
        expect Some(new_fraction) = new(num, den)
        new_fraction
      },
    ),
    map(
      both(fuzz.int_at_most(-1), fuzz.int_at_most(-1)),
      fn((num, den)) {
        expect Some(new_fraction) = new(num, den)
        new_fraction
      },
    ),
  )

const any_negative_rational: Fuzzer<Rational> =
  either(
    map(
      both(fuzz.int_at_most(-1), fuzz.int_at_least(1)),
      fn((num, den)) {
        expect Some(new_fraction) = new(num, den)
        new_fraction
      },
    ),
    map(
      both(fuzz.int_at_least(1), fuzz.int_at_most(-1)),
      fn((num, den)) {
        expect Some(new_fraction) = new(num, den)
        new_fraction
      },
    ),
  )

const any_non_zero_rational: Fuzzer<Rational> =
  either(any_negative_rational, any_positive_rational)

test prop_power_of_zero_returns_one(rational via any_non_zero_rational) {
  expect Some(calculated_result) = pow(rational, 0)
  expect Some(expected_result) = new(1, 1)
  calculated_result == expected_result
}

test prop_power_of_one_returns_same_fraction(rational via any_non_zero_rational) {
  expect Some(calculated_result) = pow(rational, 1)
  calculated_result == rational
}

test prop_power_numerator_zero_exponent_negative_returns_none(
  (denominator, exponent) via both(fuzz.int_at_least(1), fuzz.int_at_most(-1)),
) {
  expect Some(fraction) = new(0, denominator)
  expect None = pow(fraction, exponent)
}

test prop_power_unit_fraction_is_immutable(exponent via fuzz.int()) {
  expect Some(unit) = new(1, 1)
  expect Some(calculated_result) = pow(unit, exponent)
  calculated_result == unit
}
