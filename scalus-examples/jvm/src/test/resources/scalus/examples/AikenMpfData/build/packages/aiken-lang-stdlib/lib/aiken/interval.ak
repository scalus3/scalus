//// In a eUTxO-based blockchain like Cardano, the management of time can be
//// finicky.
////
//// Indeed, in order to maintain a complete determinism in the execution of
//// scripts, it is impossible to introduce a notion of _"current time"_ since
//// the execution would then depend on factor that are external to the
//// transaction itself: the ineluctable stream of time flowing in our universe.
////
//// Hence, to work around that, we typically define time intervals, which gives
//// window -- a.k.a intervals -- within which the transaction can be executed.
//// From within a script, it isn't possible to know when exactly the script is
//// executed, but we can reason about the interval bounds to validate pieces of
//// logic.

// TODO: Replace 'Int' with a generic 'a' once we have comparable traits.

/// A type to represent intervals of values. Interval are inhabited by a type
/// `a` which is useful for non-infinite intervals that have a finite
/// lower-bound and/or upper-bound.
///
/// This allows to represent all kind of mathematical intervals:
///
/// ```aiken
/// // [1; 10]
/// let i0: Interval<Int> = Interval
///   { lower_bound:
///       IntervalBound { bound_type: Finite(1), is_inclusive: True }
///   , upper_bound:
///       IntervalBound { bound_type: Finite(10), is_inclusive: True }
///   }
/// ```
///
/// ```aiken
/// // (20; infinity)
/// let i1: Interval<Int> = Interval
///   { lower_bound:
///       IntervalBound { bound_type: Finite(20), is_inclusive: False }
///   , upper_bound:
///       IntervalBound { bound_type: PositiveInfinity, is_inclusive: False }
///   }
/// ```
pub type Interval<a> {
  lower_bound: IntervalBound<a>,
  upper_bound: IntervalBound<a>,
}

/// An interval bound, either inclusive or exclusive.
pub type IntervalBound<a> {
  bound_type: IntervalBoundType<a>,
  is_inclusive: Bool,
}

/// A type of interval bound. Where finite, a value of type `a` must be
/// provided. `a` will typically be an `Int`, representing a number of seconds or
/// milliseconds.
pub type IntervalBoundType<a> {
  NegativeInfinity
  Finite(a)
  PositiveInfinity
}

// ## Constructing

/// Create an interval that includes all values greater than the given bound. i.e [lower_bound, +INF)
///
/// ```aiken
/// interval.after(10) == Interval {
///   lower_bound: IntervalBound { bound_type: Finite(10), is_inclusive: True },
///   upper_bound: IntervalBound { bound_type: PositiveInfinity, is_inclusive: True },
/// }
/// ```
pub fn after(lower_bound: a) -> Interval<a> {
  Interval {
    lower_bound: IntervalBound {
      bound_type: Finite(lower_bound),
      is_inclusive: True,
    },
    upper_bound: IntervalBound {
      bound_type: PositiveInfinity,
      is_inclusive: True,
    },
  }
}

/// Create an interval that includes all values after (and not including) the given bound. i.e (lower_bound, +INF)
///
/// ```aiken
/// interval.entirely_after(10) == Interval {
///   lower_bound: IntervalBound { bound_type: Finite(10), is_inclusive: False },
///   upper_bound: IntervalBound { bound_type: PositiveInfinity, is_inclusive: True },
/// }
/// ```
pub fn entirely_after(lower_bound: a) -> Interval<a> {
  Interval {
    lower_bound: IntervalBound {
      bound_type: Finite(lower_bound),
      is_inclusive: False,
    },
    upper_bound: IntervalBound {
      bound_type: PositiveInfinity,
      is_inclusive: True,
    },
  }
}

/// Create an interval that includes all values before (and including) the given bound. i.e (-INF, upper_bound]
///
/// ```aiken
/// interval.before(100) == Interval {
///   lower_bound: IntervalBound { bound_type: NegativeInfinity, is_inclusive: True },
///   upper_bound: IntervalBound { bound_type: Finite(100), is_inclusive: True },
/// }
/// ```
pub fn before(upper_bound: a) -> Interval<a> {
  Interval {
    lower_bound: IntervalBound {
      bound_type: NegativeInfinity,
      is_inclusive: True,
    },
    upper_bound: IntervalBound {
      bound_type: Finite(upper_bound),
      is_inclusive: True,
    },
  }
}

/// Create an interval that includes all values before (and not including) the given bound. i.e (-INF, upper_bound)
///
/// ```aiken
/// interval.entirely_before(10) == Interval {
///   lower_bound: IntervalBound { bound_type: NegativeInfinity, is_inclusive: True },
///   upper_bound: IntervalBound { bound_type: Finite(10), is_inclusive: False },
/// }
/// ```
pub fn entirely_before(upper_bound: a) -> Interval<a> {
  Interval {
    lower_bound: IntervalBound {
      bound_type: NegativeInfinity,
      is_inclusive: True,
    },
    upper_bound: IntervalBound {
      bound_type: Finite(upper_bound),
      is_inclusive: False,
    },
  }
}

/// Create an interval that includes all values between two bounds, including the bounds. i.e. [lower_bound, upper_bound]
///
/// ```aiken
/// interval.between(10, 100) == Interval {
///   lower_bound: IntervalBound { bound_type: Finite(10), is_inclusive: True },
///   upper_bound: IntervalBound { bound_type: Finite(100), is_inclusive: True },
/// }
/// ```
pub fn between(lower_bound: a, upper_bound: a) -> Interval<a> {
  Interval {
    lower_bound: IntervalBound {
      bound_type: Finite(lower_bound),
      is_inclusive: True,
    },
    upper_bound: IntervalBound {
      bound_type: Finite(upper_bound),
      is_inclusive: True,
    },
  }
}

/// Create an interval that includes all values between two bounds, excluding the bounds. i.e. (lower_bound, upper_bound)
///
/// ```aiken
/// interval.entirely_between(10, 100) == Interval {
///   lower_bound: IntervalBound { bound_type: Finite(10), is_inclusive: False },
///   upper_bound: IntervalBound { bound_type: Finite(100), is_inclusive: False },
/// }
/// ```
pub fn entirely_between(lower_bound: a, upper_bound: a) -> Interval<a> {
  Interval {
    lower_bound: IntervalBound {
      bound_type: Finite(lower_bound),
      is_inclusive: False,
    },
    upper_bound: IntervalBound {
      bound_type: Finite(upper_bound),
      is_inclusive: False,
    },
  }
}

/// Create an empty interval that contains no value.
///
/// ```aiken
/// interval.contains(empty, 0) == False
/// interval.contains(empty, 1000) == False
/// ```
pub const empty: Interval<a> =
  Interval {
    lower_bound: IntervalBound {
      bound_type: PositiveInfinity,
      is_inclusive: True,
    },
    upper_bound: IntervalBound {
      bound_type: NegativeInfinity,
      is_inclusive: True,
    },
  }

/// Create an interval that contains every possible values. i.e. (-INF, +INF)
///
/// ```aiken
/// interval.contains(everything, 0) == True
/// interval.contains(everything, 1000) == True
/// ```
pub const everything: Interval<a> =
  Interval {
    lower_bound: IntervalBound {
      bound_type: NegativeInfinity,
      is_inclusive: True,
    },
    upper_bound: IntervalBound {
      bound_type: PositiveInfinity,
      is_inclusive: True,
    },
  }

// ## Inspecting

/// Checks whether an element is contained within the interval.
///
/// ```aiken
/// let iv =
///   Interval {
///     lower_bound: IntervalBound {
///       bound_type: Finite(14),
///       is_inclusive: True
///     },
///     upper_bound: IntervalBound {
///       bound_type: Finite(42),
///       is_inclusive: False
///     },
///   }
///
/// interval.contains(iv, 25) == True
/// interval.contains(iv, 0) == False
/// interval.contains(iv, 14) == True
/// interval.contains(iv, 42) == False
/// ```
pub fn contains(self: Interval<Int>, elem: Int) -> Bool {
  let is_greater_than_lower_bound =
    when self.lower_bound.bound_type is {
      NegativeInfinity -> True
      Finite(lower_bound) ->
        if self.lower_bound.is_inclusive {
          elem >= lower_bound
        } else {
          elem > lower_bound
        }
      PositiveInfinity -> False
    }

  let is_smaller_than_upper_bound =
    when self.upper_bound.bound_type is {
      NegativeInfinity -> False
      Finite(upper_bound) ->
        if self.upper_bound.is_inclusive {
          elem <= upper_bound
        } else {
          elem < upper_bound
        }
      PositiveInfinity -> True
    }

  is_greater_than_lower_bound && is_smaller_than_upper_bound
}

test contains_1() {
  let iv = everything
  contains(iv, 14)
}

test contains_2() {
  let iv = entirely_before(15)
  contains(iv, 14)
}

test contains_3() {
  let iv = before(14)
  contains(iv, 14)
}

test contains_4() {
  let iv = entirely_before(14)
  !contains(iv, 14)
}

test contains_5() {
  let iv = entirely_after(13)
  contains(iv, 14)
}

test contains_6() {
  let iv = after(14)
  contains(iv, 14)
}

test contains_7() {
  let iv = entirely_after(14)
  !contains(iv, 14)
}

test contains_8() {
  let iv = between(42, 1337)
  !contains(iv, 14)
}

test contains_9() {
  let iv = between(0, 42)
  contains(iv, 14)
}

test contains_10() {
  let iv = between(0, 42)
  contains(iv, 42)
}

test contains_11() {
  let iv = entirely_between(0, 42)
  !contains(iv, 0)
}

test contains_12() {
  let iv = empty
  !contains(iv, 14)
}

/// Tells whether an interval is empty; i.e. that is contains no value.
///
/// ```aiken
/// let iv1 = interval.empty
///
/// let iv2 = Interval {
///     lower_bound: IntervalBound { bound_type: Finite(0), is_inclusive: False },
///     upper_bound: IntervalBound { bound_type: Finite(0), is_inclusive: False },
///   }
///
/// let iv3 = Interval {
///     lower_bound: IntervalBound { bound_type: Finite(0), is_inclusive: False },
///     upper_bound: IntervalBound { bound_type: Finite(100), is_inclusive: False },
///   }
///
/// interval.is_empty(iv1) == True
/// interval.is_empty(iv2) == True
/// interval.is_empty(iv3) == False
///
/// // Note: Two empty intervals are not necessarily equal.
/// iv1 != iv2
/// ```
pub fn is_empty(self: Interval<Int>) -> Bool {
  let ordering =
    compare_bound_type(self.lower_bound.bound_type, self.upper_bound.bound_type)

  when ordering is {
    Greater -> True
    Equal -> !(self.lower_bound.is_inclusive && self.upper_bound.is_inclusive)
    Less -> {
      let is_open_interval =
        !self.lower_bound.is_inclusive && !self.upper_bound.is_inclusive
      if is_open_interval {
        when (self.lower_bound.bound_type, self.upper_bound.bound_type) is {
          (Finite(lower_bound), Finite(upper_bound)) ->
            lower_bound + 1 == upper_bound
          _ -> False
        }
      } else {
        False
      }
    }
  }
}

/// Check whether the interval is entirely after the point "a"
///
/// ```aiken
/// interval.is_entirely_after(interval.after(10), 5) == True
/// interval.is_entirely_after(interval.after(10), 10) == False
/// interval.is_entirely_after(interval.after(10), 15) == False
/// interval.is_entirely_after(interval.between(10, 20), 30) == False
/// interval.is_entirely_after(interval.between(10, 20), 5) == True
pub fn is_entirely_after(self: Interval<Int>, point: Int) -> Bool {
  when self.lower_bound.bound_type is {
    Finite(low) ->
      if self.lower_bound.is_inclusive {
        point < low
      } else {
        point <= low
      }
    _ -> False
  }
}

test is_entirely_after_1() {
  is_entirely_after(after(10), 5)
}

test is_entirely_after_2() {
  !is_entirely_after(after(10), 10)
}

test is_entirely_after_3() {
  !is_entirely_after(after(10), 15)
}

test is_entirely_after_4() {
  !is_entirely_after(between(10, 20), 30)
}

test is_entirely_after_5() {
  is_entirely_after(between(10, 20), 5)
}

test is_entirely_after_6() {
  is_entirely_after(entirely_after(10), 10)
}

test is_entirely_after_7() {
  !is_entirely_after(before(10), 5)
}

test is_entirely_after_8() {
  !is_entirely_after(before(10), 15)
}

test is_entirely_after_9() {
  !is_entirely_after(entirely_before(10), 5)
}

/// Check whether the interval is entirely before the point "a"
///
/// ```aiken
/// interval.is_entirely_before(interval.before(10), 15) == True
/// interval.is_entirely_before(interval.before(10), 10) == False
/// interval.is_entirely_before(interval.before(10), 5) == False
/// interval.is_entirely_before(interval.between(10, 20), 30) == True
/// interval.is_entirely_before(interval.between(10, 20), 5) == False
pub fn is_entirely_before(self: Interval<Int>, point: Int) -> Bool {
  when self.upper_bound.bound_type is {
    Finite(hi) ->
      if self.upper_bound.is_inclusive {
        hi < point
      } else {
        hi <= point
      }
    _ -> False
  }
}

test is_entirely_before_1() {
  is_entirely_before(before(10), 15)
}

test is_entirely_before_2() {
  !is_entirely_before(before(10), 10)
}

test is_entirely_before_3() {
  !is_entirely_before(before(10), 5)
}

test is_entirely_before_4() {
  is_entirely_before(between(10, 20), 30)
}

test is_entirely_before_5() {
  !is_entirely_before(between(10, 20), 5)
}

test is_entirely_before_6() {
  is_entirely_before(entirely_before(10), 10)
}

test is_entirely_before_7() {
  !is_entirely_before(after(10), 15)
}

test is_entirely_before_8() {
  !is_entirely_before(after(10), 5)
}

test is_entirely_before_9() {
  !is_entirely_before(entirely_after(10), 5)
}

// ## Combining

/// Computes the smallest interval containing the two given intervals, if any
///
/// ```aiken
/// let iv1 = between(0, 10)
/// let iv2 = between(2, 14)
/// hull(iv1, iv2) == between(0, 14)
///
/// let iv1 = between(5, 10)
/// let iv2 = before(0)
/// hull(iv1, iv2) == before(10)
///
/// let iv1 = entirely_after(0)
/// let iv2 = between(10, 42)
/// hull(iv1, iv2) = entirely_after(0)
/// ```
pub fn hull(iv1: Interval<Int>, iv2: Interval<Int>) -> Interval<Int> {
  Interval {
    lower_bound: min(iv1.lower_bound, iv2.lower_bound),
    upper_bound: max(iv1.upper_bound, iv2.upper_bound),
  }
}

test hull_1() {
  let iv1 = between(0, 10)
  let iv2 = between(2, 14)
  hull(iv1, iv2) == between(0, 14)
}

test hull_2() {
  let iv1 = between(5, 10)
  let iv2 = before(0)
  hull(iv1, iv2) == before(10)
}

test hull_3() {
  let iv1 = entirely_after(0)
  let iv2 = between(10, 42)
  hull(iv1, iv2) == entirely_after(0)
}

/// Computes the largest interval contains in the two given intervals, if any.
///
/// ```aiken
/// let iv1 = interval.between(0, 10)
/// let iv2 = interval.between(2, 14)
/// interval.intersection(iv1, iv2) == interval.between(2, 10)
///
/// let iv1 = interval.entirely_before(10)
/// let iv2 = interval.entirely_after(0)
/// interval.intersection(iv1, iv2) == interval.entirely_between(0, 10)
///
/// let iv1 = interval.between(0, 1)
/// let iv2 = interval.between(2, 3)
/// interval.intersection(iv1, iv2) |> interval.is_empty
/// ```
pub fn intersection(iv1: Interval<Int>, iv2: Interval<Int>) -> Interval<Int> {
  Interval {
    lower_bound: max(iv1.lower_bound, iv2.lower_bound),
    upper_bound: min(iv1.upper_bound, iv2.upper_bound),
  }
}

test intersection_1() {
  let iv1 = between(0, 10)
  let iv2 = between(2, 14)
  intersection(iv1, iv2) == between(2, 10)
}

test intersection_2() {
  let iv1 = between(0, 1)
  let iv2 = between(1, 2)
  intersection(iv1, iv2) == between(1, 1)
}

test intersection_3() {
  let iv1 = between(0, 1)
  let iv2 = entirely_between(1, 2)
  intersection(iv1, iv2)
    |> is_empty
}

test intersection_4() {
  let iv1 = entirely_between(0, 1)
  let iv2 = entirely_between(1, 2)
  intersection(iv1, iv2)
    |> is_empty
}

test intersection_5() {
  let iv1 = between(0, 10)
  let iv2 = before(4)
  intersection(iv1, iv2) == between(0, 4)
}

test intersection_6() {
  let iv1 = entirely_before(10)
  let iv2 = entirely_after(0)
  intersection(iv1, iv2) == entirely_between(0, 10)
}

/// Return the highest bound of the two.
///
/// ```aiken
/// let ib1 = IntervalBound { bound_type: Finite(0), is_inclusive: False }
/// let ib2 = IntervalBound { bound_type: Finite(1), is_inclusive: False }
///
/// interval.max(ib1, ib2) == ib2
/// ```
pub fn max(
  left: IntervalBound<Int>,
  right: IntervalBound<Int>,
) -> IntervalBound<Int> {
  when compare_bound(left, right) is {
    Less -> right
    Equal -> left
    Greater -> left
  }
}

/// Return the smallest bound of the two.
///
/// ```aiken
/// let ib1 = IntervalBound { bound_type: Finite(0), is_inclusive: False }
/// let ib2 = IntervalBound { bound_type: Finite(1), is_inclusive: False }
///
/// interval.min(ib1, ib2) == ib1
/// ```
pub fn min(
  left: IntervalBound<Int>,
  right: IntervalBound<Int>,
) -> IntervalBound<Int> {
  when compare_bound(left, right) is {
    Less -> left
    Equal -> left
    Greater -> right
  }
}

fn compare_bound(
  left: IntervalBound<Int>,
  right: IntervalBound<Int>,
) -> Ordering {
  when compare_bound_type(left.bound_type, right.bound_type) is {
    Less -> Less
    Greater -> Greater
    Equal ->
      if left.is_inclusive == right.is_inclusive {
        Equal
      } else if left.is_inclusive {
        Greater
      } else {
        Less
      }
  }
}

fn compare_bound_type(
  left: IntervalBoundType<Int>,
  right: IntervalBoundType<Int>,
) -> Ordering {
  when left is {
    NegativeInfinity ->
      when right is {
        NegativeInfinity -> Equal
        _ -> Less
      }
    PositiveInfinity ->
      when right is {
        PositiveInfinity -> Equal
        _ -> Greater
      }
    Finite(left) ->
      when right is {
        NegativeInfinity -> Greater
        PositiveInfinity -> Less
        Finite(right) ->
          if left < right {
            Less
          } else if left == right {
            Equal
          } else {
            Greater
          }
      }
  }
}
