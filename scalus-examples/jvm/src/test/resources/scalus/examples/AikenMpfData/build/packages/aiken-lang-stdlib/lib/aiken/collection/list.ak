use aiken/builtin
use aiken/primitive/bytearray
use aiken/primitive/int

// ## Constructing

/// Add an element in front of the list. Sometimes useful when combined with
/// other functions.
///
/// ```aiken
/// list.push([2, 3], 1) == [1, ..[2, 3]] == [1, 2, 3]
/// ```
pub fn push(self: List<a>, elem: a) -> List<a> {
  [elem, ..self]
}

test push_1() {
  push([], 0) == [0]
}

test push_2() {
  push([2, 3], 1) == [1, 2, 3]
}

/// Construct a list of a integer from a given range.
///
/// ```aiken
/// list.range(0, 3) == [0, 1, 2, 3]
/// list.range(-1, 1) == [-1, 0, 1]
/// ```
pub fn range(from: Int, to: Int) -> List<Int> {
  if from > to {
    []
  } else {
    [from, ..range(from + 1, to)]
  }
}

test range_1() {
  range(0, 3) == [0, 1, 2, 3]
}

test range_2() {
  range(-1, 1) == [-1, 0, 1]
}

/// Construct a list filled with n copies of a value.
///
/// ```aiken
/// list.repeat("na", 3) == ["na", "na", "na"]
/// ```
pub fn repeat(elem: a, n_times: Int) -> List<a> {
  if n_times <= 0 {
    []
  } else {
    [elem, ..repeat(elem, n_times - 1)]
  }
}

test repeat_1() {
  repeat(42, 0) == []
}

test repeat_2() {
  repeat(14, 3) == [14, 14, 14]
}

// ## Inspecting

/// Determine if all elements of the list satisfy the given predicate.
///
/// Note: an empty list always satisfies the predicate.
///
/// ```aiken
/// list.all([], fn(n) { n > 0 }) == True
/// list.all([1, 2, 3], fn(n) { n > 0 }) == True
/// list.all([1, 2, 3], fn(n) { n == 2 }) == False
/// ```
pub fn all(self: List<a>, predicate: fn(a) -> Bool) -> Bool {
  when self is {
    [] -> True
    [x, ..xs] -> predicate(x) && all(xs, predicate)
  }
}

test all_1() {
  all([1, 2, 3], fn(n) { n > 0 }) == True
}

test all_2() {
  all([1, 2, 3], fn(n) { n > 42 }) == False
}

test all_3() {
  all([], fn(n) { n == 42 }) == True
}

/// Determine if at least one element of the list satisfies the given predicate.
///
/// Note: an empty list never satisfies the predicate.
///
/// ```aiken
/// list.any([], fn(n) { n > 2 }) == False
/// list.any([1, 2, 3], fn(n) { n > 0 }) == True
/// list.any([1, 2, 3], fn(n) { n == 2 }) == True
/// list.any([1, 2, 3], fn(n) { n < 0 }) == False
/// ```
pub fn any(self: List<a>, predicate: fn(a) -> Bool) -> Bool {
  when self is {
    [] -> False
    [x, ..xs] -> predicate(x) || any(xs, predicate)
  }
}

test any_1() {
  any([1, 2, 3], fn(n) { n > 0 }) == True
}

test any_2() {
  any([1, 2, 3], fn(n) { n > 42 }) == False
}

test any_3() {
  any([], fn(n) { n == 42 }) == False
}

/// Return Some(item) at the index or None if the index is out of range. The index is 0-based.
///
/// ```aiken
/// list.at([1, 2, 3], 1) == Some(2)
/// list.at([1, 2, 3], 42) == None
/// ```
pub fn at(self: List<a>, index: Int) -> Option<a> {
  when self is {
    [] -> None
    [x, ..xs] ->
      if index == 0 {
        Some(x)
      } else {
        at(xs, index - 1)
      }
  }
}

test at_1() {
  at([1, 2, 3], -1) == None
}

test at_2() {
  at([], 0) == None
}

test at_3() {
  at([1, 2, 3], 3) == None
}

test at_4() {
  at([1], 0) == Some(1)
}

test at_5() {
  at([1, 2, 3], 2) == Some(3)
}

/// Count how many items in the list satisfy the given predicate.
///
/// ```aiken
/// list.count([], fn(a) { a > 2}) == 0
/// list.count([1, 2, 3], fn(a) { n > 0 }) == 3
/// list.count([1, 2, 3], fn(a) { n >= 2 }) == 2
/// list.count([1, 2, 3], fn(a) { n > 5 }) == 0
/// ```
pub fn count(self: List<a>, predicate: fn(a) -> Bool) -> Int {
  foldr(
    self,
    0,
    fn(item, total) {
      if predicate(item) {
        total + 1
      } else {
        total
      }
    },
  )
}

test count_empty() {
  count([], fn(a) { a > 2 }) == 0
}

test count_all() {
  count([1, 2, 3], fn(a) { a > 0 }) == 3
}

test count_some() {
  count([1, 2, 3], fn(a) { a >= 2 }) == 2
}

test count_none() {
  count([1, 2, 3], fn(a) { a > 5 }) == 0
}

/// Find the first element satisfying the given predicate, if any.
///
/// ```aiken
/// list.find([1, 2, 3], fn(x) { x == 2 }) == Some(2)
/// list.find([4, 5, 6], fn(x) { x == 2 }) == None
/// ```
pub fn find(self: List<a>, predicate: fn(a) -> Bool) -> Option<a> {
  when self is {
    [] -> None
    [x, ..xs] ->
      if predicate(x) {
        Some(x)
      } else {
        find(xs, predicate)
      }
  }
}

test find_1() {
  find([1, 2, 3], fn(x) { x == 1 }) == Some(1)
}

test find_2() {
  find([1, 2, 3], fn(x) { x > 42 }) == None
}

test find_3() {
  find([], fn(_) { True }) == None
}

/// Figures out whether a list contain the given element.
///
/// ```aiken
/// list.has([1, 2, 3], 2) == True
/// list.has([1, 2, 3], 14) == False
/// list.has([], 14) == False
/// ```
pub fn has(self: List<a>, elem: a) -> Bool {
  when self is {
    [] -> False
    [x, ..xs] ->
      if x == elem {
        True
      } else {
        has(xs, elem)
      }
  }
}

test has_1() {
  has([1, 2, 3], 1) == True
}

test has_2() {
  has([1, 2, 3], 14) == False
}

test has_3() {
  has([], 14) == False
}

/// Get the first element of a list
///
/// ```aiken
/// list.head([1, 2, 3]) == Some(1)
/// list.head([]) == None
/// ```
pub fn head(self: List<a>) -> Option<a> {
  when self is {
    [] -> None
    _ -> Some(builtin.head_list(self))
  }
}

test head_1() {
  head([1, 2, 3]) == Some(1)
}

test head_2() {
  head([]) == None
}

/// Checks whether a list is empty.
///
/// ```aiken
/// list.is_empty([]) == True
/// list.is_empty([1, 2, 3]) == False
/// ```
pub fn is_empty(self: List<a>) -> Bool {
  when self is {
    [] -> True
    _ -> False
  }
}

test is_empty_1() {
  is_empty([]) == True
}

test is_empty_2() {
  is_empty([1, 2, 3]) == False
}

/// Gets the index of an element of a list, if any. Otherwise, returns None.
///
/// ```aiken
/// list.index_of([1, 5, 2], 2) == Some(2)
/// list.index_of([1, 7, 3], 4) == None
/// list.index_of([1, 0, 9, 6], 6) == 3
/// list.index_of([], 6) == None
/// ```
pub fn index_of(self: List<a>, elem: a) -> Option<Int> {
  do_index_of(self, elem, 0)
}

fn do_index_of(self: List<a>, elem: a, i: Int) -> Option<Int> {
  when self is {
    [] -> None
    [x, ..xs] ->
      if x == elem {
        Some(i)
      } else {
        do_index_of(xs, elem, i + 1)
      }
  }
}

test index_of_1() {
  index_of([1, 5, 2], 2) == Some(2)
}

test index_of_2() {
  index_of([1, 7, 3], 4) == None
}

test index_of_3() {
  index_of([1, 0, 9, 6], 6) == Some(3)
}

test index_of_4() {
  index_of([], 6) == None
}

/// Get the last in the given list, if any.
///
/// ```aiken
/// list.last([]) == None
/// list.last([1, 2, 3]) == Some(3)
/// ```
pub fn last(self: List<a>) -> Option<a> {
  when self is {
    [] -> None
    [x] -> Some(x)
    [_, ..xs] -> last(xs)
  }
}

test last_1() {
  last([]) == None
}

test last_2() {
  last([1]) == Some(1)
}

test last_3() {
  last([1, 2, 3, 4]) == Some(4)
}

/// Get the number of elements in the given list.
///
/// ```aiken
/// list.length([]) == 0
/// list.length([1, 2, 3]) == 3
/// ```
pub fn length(self: List<a>) -> Int {
  when self is {
    [] -> 0
    [_, ..xs] -> 1 + length(xs)
  }
}

test length_1() {
  length([]) == 0
}

test length_2() {
  length([1, 2, 3]) == 3
}

// ## Modifying

// ### Extracting

/// Remove the first occurrence of the given element from the list.
///
/// ```aiken
/// list.delete([1, 2, 3, 1], 1) == [2, 3, 1]
/// list.delete([1, 2, 3], 14) == [1, 2, 3]
/// ```
pub fn delete(self: List<a>, elem: a) -> List<a> {
  when self is {
    [] -> []
    [x, ..xs] ->
      if x == elem {
        xs
      } else {
        [x, ..delete(xs, elem)]
      }
  }
}

test delete_1() {
  delete([], 42) == []
}

test delete_2() {
  delete([1, 2, 3, 1], 1) == [2, 3, 1]
}

test delete_3() {
  delete([1, 2, 3], 14) == [1, 2, 3]
}

test delete_4() {
  delete([2], 2) == []
}

/// Drop the first `n` elements of a list.
///
/// ```aiken
/// list.drop([1, 2, 3], 2) == [3]
/// list.drop([], 42) == []
/// list.drop([1, 2, 3], 42) == []
/// ```
pub fn drop(self: List<a>, n: Int) -> List<a> {
  if n <= 0 {
    self
  } else {
    when self is {
      [] -> []
      [_x, ..xs] -> drop(xs, n - 1)
    }
  }
}

test drop_1() {
  drop([], 42) == []
}

test drop_2() {
  drop([1, 2, 3], 2) == [3]
}

/// Returns the suffix of the given list after removing all elements that satisfy the predicate.
///
/// ```aiken
/// list.drop_while([1, 2, 3], fn(x) { x < 2 }) == [2, 3]
/// list.drop_while([], fn(x) { x > 2 }) == []
/// list.drop_while([1, 2, 3], fn(x) { x == 3 }) == [1, 2, 3]
/// ```
pub fn drop_while(self: List<a>, predicate: fn(a) -> Bool) -> List<a> {
  when self is {
    [] -> []
    [x, ..xs] ->
      if predicate(x) {
        drop_while(xs, predicate)
      } else {
        self
      }
  }
}

test drop_while_1() {
  drop_while([], fn(x) { x > 2 }) == []
}

test drop_while_2() {
  let xs = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  drop_while(xs, fn(x) { x > 5 }) == [5, 4, 3, 2, 1]
}

test drop_while_3() {
  let xs = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  drop_while(xs, fn(x) { x == 42 }) == xs
}

test drop_while_4() {
  let xs = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  drop_while(xs, fn(x) { x < 42 }) == []
}

/// Produce a list of elements that satisfy a predicate.
///
/// ```aiken
/// list.filter([1, 2, 3], fn(x) { x >= 2 }) == [2, 3]
/// list.filter([], fn(x) { x > 2 }) == []
/// list.filter([1, 2, 3], fn(x) { x == 3 }) == [3]
/// ```
pub fn filter(self: List<a>, predicate: fn(a) -> Bool) -> List<a> {
  when self is {
    [] -> []
    [x, ..xs] ->
      if predicate(x) {
        [x, ..filter(xs, predicate)]
      } else {
        filter(xs, predicate)
      }
  }
}

test filter_1() {
  filter([], fn(x) { x > 0 }) == []
}

test filter_2() {
  let xs = [1, 2, 3, 4, 5, 6]
  filter(xs, fn(x) { builtin.mod_integer(x, 2) == 0 }) == [2, 4, 6]
}

test filter_3() {
  let filter_foldr =
    fn(xs, f) {
      foldr(
        xs,
        [],
        fn(x, ys) {
          if f(x) {
            [x, ..ys]
          } else {
            ys
          }
        },
      )
    }

  let is_odd =
    fn(n) { builtin.mod_integer(n, 2) != 0 }

  filter_foldr([1, 2, 3], is_odd) == filter([1, 2, 3], is_odd)
}

/// Produce a list of transformed elements that satisfy a predicate.
///
/// ```aiken
/// let transform = fn(x) { if x % 2 == 0 { None } else { Some(3*x) } }
/// list.filter_map([1, 2, 3], transform) == [3, 9]
/// ```
pub fn filter_map(self: List<a>, predicate: fn(a) -> Option<b>) -> List<b> {
  when self is {
    [] -> []
    [x, ..xs] ->
      when predicate(x) is {
        None -> filter_map(xs, predicate)
        Some(y) -> [y, ..filter_map(xs, predicate)]
      }
  }
}

test filter_map_1() {
  filter_map([], fn(_) { Some(42) }) == []
}

test filter_map_2() {
  filter_map(
    [1, 2, 3, 4, 5, 6],
    fn(x) {
      if builtin.mod_integer(x, 2) != 0 {
        Some(3 * x)
      } else {
        None
      }
    },
  ) == [3, 9, 15]
}

/// Return all elements except the last one.
///
/// ```aiken
/// list.init([]) == None
/// list.init([1, 2, 3]) == Some([1, 2])
/// ```
pub fn init(self: List<a>) -> Option<List<a>> {
  when self is {
    [] -> None
    _ -> Some(do_init(self))
  }
}

fn do_init(self: List<a>) -> List<a> {
  when self is {
    [] -> fail @"unreachable"
    [_] -> []
    [x, ..xs] -> [x, ..do_init(xs)]
  }
}

test init_1() {
  init([]) == None
}

test init_2() {
  init([1]) == Some([])
}

test init_3() {
  init([1, 2, 3, 4]) == Some([1, 2, 3])
}

/// Returns a tuple with all elements that satisfy the predicate at first
/// element, and the rest as second element.
///
/// ```aiken
/// list.partition([1, 2, 3, 4], fn(x) { x % 2 == 0 }) == ([2, 4], [1, 3])
/// ```
pub fn partition(self: List<a>, predicate: fn(a) -> Bool) -> (List<a>, List<a>) {
  when self is {
    [] -> ([], [])
    [x, ..xs] -> {
      let (left, right) = partition(xs, predicate)
      if predicate(x) {
        ([x, ..left], right)
      } else {
        (left, [x, ..right])
      }
    }
  }
}

test partition_1() {
  partition([], fn(x) { x > 2 }) == ([], [])
}

test partition_2() {
  let xs = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  partition(xs, fn(x) { x > 5 }) == ([10, 9, 8, 7, 6], [5, 4, 3, 2, 1])
}

test partition_3() {
  let xs = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  partition(xs, fn(x) { x == 42 }) == ([], xs)
}

test partition_4() {
  let xs = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  partition(xs, fn(x) { x < 42 }) == (xs, [])
}

test partition_5() {
  partition([1, 2, 3, 4], fn(x) { x % 2 == 0 }) == ([2, 4], [1, 3])
}

/// Extract a sublist from the given list using 0-based indexes. Negative
/// indexes wrap over, so `-1` refers to the last element of the list.
///
/// ```aiken
/// list.slice([1, 2, 3, 4, 5, 6], from: 2, to: 4) == [3, 4, 5]
/// list.slice([1, 2, 3, 4, 5, 6], from: -2, to: -1) == [5, 6]
/// list.slice([1, 2, 3, 4, 5, 6], from: 1, to: -1) == [2, 3, 4, 5, 6]
/// ```
pub fn slice(self: List<a>, from: Int, to: Int) -> List<a> {
  let (i, l) =
    if from >= 0 {
      (from, None)
    } else {
      let l = length(self)
      (l + from, Some(l))
    }

  let j =
    if to >= 0 {
      to - i + 1
    } else {
      when l is {
        Some(l) -> l + to - i + 1
        None -> length(self) + to - i + 1
      }
    }

  self
    |> drop(i)
    |> take(j)
}

test slice_1() {
  slice([1, 2, 3], 0, 2) == [1, 2, 3]
}

test slice_2() {
  slice([1, 2, 3, 4, 5, 6], from: 2, to: 4) == [3, 4, 5]
}

test slice_3() {
  slice([1, 2, 3, 4, 5, 6], from: -2, to: -1) == [5, 6]
}

test slice_4() {
  slice([1, 2, 3, 4, 5, 6], from: 1, to: -1) == [2, 3, 4, 5, 6]
}

test slice_5() {
  slice([1, 2, 3, 4, 5, 6], from: -4, to: -3) == [3, 4]
}

test slice_6() {
  slice([1, 2, 3, 4, 5, 6], from: -2, to: 1) == []
}

/// Cut a list in two, such that the first list contains the given number of /
/// elements and the second list contains the rest.
///
/// Fundamentally equivalent to (but more efficient):
///
/// ```aiken
/// // span(xs, n) == (take(xs, n), drop(xs, n))
/// span([1, 2, 3, 4, 5], 3) == ([1, 2, 3], [4, 5])
/// ```
pub fn span(self: List<a>, n: Int) -> (List<a>, List<a>) {
  when self is {
    [] -> ([], [])
    [x, ..xs] ->
      if n <= 0 {
        ([], self)
      } else {
        let (left, right) = span(xs, n - 1)
        ([x, ..left], right)
      }
  }
}

test span_1() {
  span([], 2) == ([], [])
}

test span_2() {
  span([1, 2, 3], 2) == ([1, 2], [3])
}

test span_3() {
  span([1, 2, 3], -1) == ([], [1, 2, 3])
}

test span_4() {
  span([1, 2, 3], 42) == ([1, 2, 3], [])
}

/// Get elements of a list after the first one, if any.
///
/// ```aiken
/// list.tail([]) == None
/// list.tail([1, 2, 3]) == Some([2, 3])
/// ```
pub fn tail(self: List<a>) -> Option<List<a>> {
  when self is {
    [] -> None
    [_, ..xs] -> Some(xs)
  }
}

test tail_1() {
  tail([1, 2, 3]) == Some([2, 3])
}

test tail_2() {
  tail([]) == None
}

/// Get the first `n` elements of a list.
///
/// ```aiken
/// list.take([1, 2, 3], 2) == [1, 2]
/// list.take([1, 2, 3], 14) == [1, 2, 3]
/// ```
pub fn take(self: List<a>, n: Int) -> List<a> {
  if n <= 0 {
    []
  } else {
    when self is {
      [] -> []
      [x, ..xs] -> [x, ..take(xs, n - 1)]
    }
  }
}

test take_1() {
  take([], 42) == []
}

test take_2() {
  take([1, 2, 3], 2) == [1, 2]
}

/// Returns the longest prefix of the given list where all elements satisfy the predicate.
///
/// ```aiken
/// list.take_while([1, 2, 3], fn(x) { x > 2 }) == []
/// list.take_while([1, 2, 3], fn(x) { x < 2 }) == [1]
/// ```
pub fn take_while(self: List<a>, predicate: fn(a) -> Bool) -> List<a> {
  when self is {
    [] -> []
    [x, ..xs] ->
      if predicate(x) {
        [x, ..take_while(xs, predicate)]
      } else {
        []
      }
  }
}

test take_while_1() {
  take_while([], fn(x) { x > 2 }) == []
}

test take_while_2() {
  let xs = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  take_while(xs, fn(x) { x > 5 }) == [10, 9, 8, 7, 6]
}

test take_while_3() {
  let xs = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  take_while(xs, fn(x) { x == 42 }) == []
}

test take_while_4() {
  let xs = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  take_while(xs, fn(x) { x < 42 }) == xs
}

/// Removes duplicate elements from a list.
///
/// ```aiken
/// list.unique([1, 2, 3, 1]) == [1, 2, 3]
/// ```
pub fn unique(self: List<a>) -> List<a> {
  when self is {
    [] -> []
    [x, ..xs] -> [x, ..unique(filter(xs, fn(y) { y != x }))]
  }
}

test unique_1() {
  unique([]) == []
}

test unique_2() {
  let xs = [1, 2, 3, 1, 1, 3, 4, 1, 2, 3, 2, 4, 5, 6, 7, 8, 9, 10, 9]
  unique(xs) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
}

// ### Mapping

/// Map elements of a list into a new list and flatten the result.
///
/// ```aiken
/// list.flat_map([1, 2, 3], fn(a) { [a, 2*a] }) == [1, 2, 2, 4, 3, 6]
/// ```
pub fn flat_map(self: List<a>, with: fn(a) -> List<b>) -> List<b> {
  foldr(self, [], fn(x, xs) { concat(with(x), xs) })
}

test flat_map_1() {
  flat_map([], fn(a) { [a] }) == []
}

test flat_map_2() {
  flat_map([1, 2, 3], fn(a) { [a, a] }) == [1, 1, 2, 2, 3, 3]
}

/// Perform an action for each element of a list.
///
/// ```aiken
/// list.for_each(labels, do: fn(lbl) { trace lbl Void })
/// ```
pub fn for_each(self: List<a>, do: fn(a) -> Void) -> Void {
  foldr(self, Void, fn(x, _) { do(x) })
}

test for_each_1() {
  for_each(
    [@"hello", @"world"],
    do: fn(lbl) {
      trace lbl
      Void
    },
  )
}

/// List [`map`](#map) but provides the position (0-based) of the elements while iterating.
///
/// ```aiken
/// list.indexed_map([1, 2, 3], fn(i, x) { i + x }) == [1, 3, 5]
/// ```
pub fn indexed_map(self: List<a>, with: fn(Int, a) -> result) -> List<result> {
  do_indexed_map(0, self, with)
}

fn do_indexed_map(
  n: Int,
  self: List<a>,
  with: fn(Int, a) -> result,
) -> List<result> {
  when self is {
    [] -> []
    [x, ..xs] -> [with(n, x), ..do_indexed_map(n + 1, xs, with)]
  }
}

test indexed_map_1() {
  indexed_map([], fn(i, _n) { i }) == []
}

test indexed_map_2() {
  indexed_map(
    [4, 8, 13, 2],
    fn(i, n) {
      if n == 8 {
        n
      } else {
        i
      }
    },
  ) == [0, 8, 2, 3]
}

/// Apply a function to each element of a list.
///
/// ```aiken
/// list.map([1, 2, 3, 4], fn(n) { n + 1 }) == [2, 3, 4, 5]
/// ```
pub fn map(self: List<a>, with: fn(a) -> result) -> List<result> {
  when self is {
    [] -> []
    [x, ..xs] -> [with(x), ..map(xs, with)]
  }
}

test map_1() {
  map([], fn(n) { n + 1 }) == []
}

test map_2() {
  map([1, 2, 3, 4], fn(n) { n + 1 }) == [2, 3, 4, 5]
}

/// Apply a function of two arguments, combining elements from two lists.
///
/// Note: if one list is longer, the extra elements are dropped.
///
/// ```aiken
/// list.map2([1, 2, 3], [1, 2], fn(a, b) { a + b }) == [2, 4]
/// ```
pub fn map2(
  self: List<a>,
  bs: List<b>,
  with: fn(a, b) -> result,
) -> List<result> {
  when self is {
    [] -> []
    [x, ..xs] ->
      when bs is {
        [] -> []
        [y, ..ys] -> [with(x, y), ..map2(xs, ys, with)]
      }
  }
}

test map2_1() {
  map2([], [1, 2, 3], fn(a, b) { a + b }) == []
}

test map2_2() {
  map2([1, 2, 3], [1, 2], fn(a, b) { a + b }) == [2, 4]
}

test map2_3() {
  map2([42], [1, 2, 3], fn(_a, b) { Some(b) }) == [Some(1)]
}

/// Apply a function of three arguments, combining elements from three lists.
///
/// Note: if one list is longer, the extra elements are dropped.
///
/// ```aiken
/// list.map3([1, 2, 3], [1, 2], [1, 2, 3], fn(a, b, c) { a + b + c }) == [3, 6]
/// ```
pub fn map3(
  self: List<a>,
  bs: List<b>,
  cs: List<c>,
  with: fn(a, b, c) -> result,
) -> List<result> {
  when self is {
    [] -> []
    [x, ..xs] ->
      when bs is {
        [] -> []
        [y, ..ys] ->
          when cs is {
            [] -> []
            [z, ..zs] -> [with(x, y, z), ..map3(xs, ys, zs, with)]
          }
      }
  }
}

test map3_1() {
  map3([], [], [1, 2, 3], fn(a, b, c) { a + b + c }) == []
}

test map3_2() {
  map3([1, 2, 3], [1, 2], [1, 2, 3], fn(a, b, c) { a + b + c }) == [3, 6]
}

/// Return the list with its elements in the reserve order.
///
/// ```aiken
/// list.reverse([1, 2, 3]) == [3, 2, 1]
/// ```
pub fn reverse(self: List<a>) -> List<a> {
  foldl(self, [], fn(x, xs) { [x, ..xs] })
}

test reverse_1() {
  reverse([]) == []
}

test reverse_2() {
  reverse([1, 2, 3]) == [3, 2, 1]
}

/// Sort a list in ascending order using the given comparison function.
///
/// ```aiken
/// use aiken/int
///
/// sort([3, 1, 4, 0, 2], int.compare) == [0, 1, 2, 3, 4]
/// sort([1, 2, 3], int.compare) == [1, 2, 3]
/// ```
pub fn sort(self: List<a>, compare: fn(a, a) -> Ordering) -> List<a> {
  when self is {
    [] -> []
    [x, ..xs] -> insert(sort(xs, compare), x, compare)
  }
}

fn insert(self: List<a>, e: a, compare: fn(a, a) -> Ordering) -> List<a> {
  when self is {
    [] -> [e]
    [x, ..xs] ->
      if compare(e, x) == Less {
        [e, ..self]
      } else {
        [x, ..insert(xs, e, compare)]
      }
  }
}

test sort_1() {
  let xs = [6, 7, 5, 4, 1, 3, 9, 8, 0, 2]
  sort(xs, int.compare) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
}

test sort_2() {
  let xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  sort(xs, int.compare) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
}

test sort_3() {
  let xs = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
  sort(xs, int.compare) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
}

test sort_4() {
  sort([], int.compare) == []
}

/// Decompose a list of tuples into a tuple of lists.
///
/// ```
/// list.unzip([(1, "a"), (2, "b")]) == ([1, 2], ["a", "b"])
/// ```
pub fn unzip(self: List<(a, b)>) -> (List<a>, List<b>) {
  when self is {
    [] -> ([], [])
    [(a, b), ..xs] -> {
      let (a_tail, b_tail) = unzip(xs)
      ([a, ..a_tail], [b, ..b_tail])
    }
  }
}

test unzip_1() {
  unzip([]) == ([], [])
}

test unzip_2() {
  unzip([(1, "a"), (2, "b")]) == ([1, 2], ["a", "b"])
}

// ## Combining

/// Merge two lists together.
///
/// ```aiken
/// list.concat([], []) == []
/// list.concat([], [1, 2, 3]) == [1, 2, 3]
/// list.concat([1, 2, 3], [4, 5, 6]) == [1, 2, 3, 4, 5, 6]
/// ```
pub fn concat(left: List<a>, right: List<a>) -> List<a> {
  when left is {
    [] -> right
    [x, ..xs] -> [x, ..concat(xs, right)]
  }
}

test concat_1() {
  concat([1, 2, 3], [4, 5, 6]) == [1, 2, 3, 4, 5, 6]
}

test concat_2() {
  concat([1, 2, 3], []) == [1, 2, 3]
}

test concat_3() {
  concat([], [1, 2, 3]) == [1, 2, 3]
}

/// Remove the first occurrence of each element of the second list from the first one.
///
/// ```
/// list.difference(["h", "e", "l", "l", "o"], ["l", "e", "l"]) == ["h", "o"]
/// list.difference([1, 2, 3, 4, 5], [1, 1, 2]) == [3, 4, 5]
/// list.difference([1, 2, 3], []) == [1, 2, 3]
/// ```
pub fn difference(self: List<a>, with: List<a>) -> List<a> {
  when with is {
    [] -> self
    [x, ..xs] -> difference(delete(self, x), xs)
  }
}

test difference_1() {
  difference(["h", "e", "l", "l", "o"], ["l", "e", "l"]) == ["h", "o"]
}

test difference_2() {
  difference([1, 2, 3, 4, 5], [1, 1, 2]) == [3, 4, 5]
}

test difference_3() {
  difference([1, 2, 3], []) == [1, 2, 3]
}

test difference_4() {
  difference([], [1, 2, 3]) == []
}

/// Combine two lists together.
///
/// Note: if one list is longer, the extra elements are dropped.
///
/// ```aiken
/// list.zip([1, 2], ["a", "b", "c"]) == [(1, "a"), (2, "b")]
/// ```
pub fn zip(self: List<a>, bs: List<b>) -> List<(a, b)> {
  when self is {
    [] -> []
    [x, ..xs] ->
      when bs is {
        [] -> []
        [y, ..ys] -> [(x, y), ..zip(xs, ys)]
      }
  }
}

test zip_1() {
  zip([], [1, 2, 3]) == []
}

test zip_2() {
  zip([1, 2, 3], []) == []
}

test zip_3() {
  zip([1, 2], ["a", "b", "c"]) == [(1, "a"), (2, "b")]
}

// ## Transforming

/// Reduce a list from left to right.
///
/// ```aiken
/// list.foldl([1, 2, 3], 0, fn(n, total) { n + total }) == 6
/// list.foldl([1, 2, 3], [], fn(x, xs) { [x, ..xs] }) == [3, 2, 1]
/// ```
pub fn foldl(self: List<a>, zero: b, with: fn(a, b) -> b) -> b {
  when self is {
    [] -> zero
    [x, ..xs] -> foldl(xs, with(x, zero), with)
  }
}

pub type Fold2<a, b, result> =
  fn(a, b) -> result

/// > [!CAUTION]
/// > This function was released by mistake as part of the v2 series of updates. However, unlike the name suggests, it isn't a left-fold but a right-fold.
/// >
/// > It cannot be removed without changing its semantic and possibly breaking existing code that relies on it. Sorry for the inconvenience.
/// >
/// > Be also aware that starting from `v3.0.0`, this has been fixed. So be careful if you've been using `foldl2` when upgrading to `v3.0.0`; you very likely want to use v3's `foldr2` instead.
pub fn foldl2(
  self: List<elem>,
  zero_a: a,
  zero_b: b,
  with: fn(elem, a, b, Fold2<a, b, result>) -> result,
  return: Fold2<a, b, result>,
) -> result {
  do_foldl2(self, with, return)(zero_a, zero_b)
}

fn do_foldl2(
  self: List<elem>,
  with: fn(elem, a, b, Fold2<a, b, result>) -> result,
  return: Fold2<a, b, result>,
) -> Fold2<a, b, result> {
  when self is {
    [] -> return
    [x, ..xs] -> do_foldl2(xs, with, fn(a, b) { with(x, a, b, return) })
  }
}

test foldl2_optimized() {
  let
    len,
    sum,
  <-
    foldl2(
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
      0,
      0,
      fn(n, len, sum, return) { return(len + 1, sum + n) },
    )

  and {
    len == 10,
    sum == 55,
  }
}

test foldl2_classic() {
  let (len, sum) =
    foldl(
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
      (0, 0),
      fn(n, (len, sum)) { (len + 1, sum + n) },
    )

  and {
    len == 10,
    sum == 55,
  }
}

type Foo {
  Foo(Int, Int)
}

test foldl2_pair() {
  let Pair(len, sum) =
    foldl(
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
      Pair(0, 0),
      fn(n, Pair(len, sum)) { Pair(len + 1, sum + n) },
    )

  and {
    len == 10,
    sum == 55,
  }
}

test foldl2_foo() {
  let Foo(len, sum) =
    foldl(
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
      Foo(0, 0),
      fn(n, Foo(len, sum)) { Foo(len + 1, sum + n) },
    )

  and {
    len == 10,
    sum == 55,
  }
}

test foldl_1() {
  foldl([], 0, fn(_, _) { 1 }) == 0
}

test foldl_2() {
  foldl([1, 2, 3, 4, 5], 0, fn(n, total) { n + total }) == 15
}

test foldl_3() {
  foldl([1, 2, 3, 4], [], fn(x, xs) { [x, ..xs] }) == [4, 3, 2, 1]
}

/// Reduce a list from right to left.
///
/// ```aiken
/// list.foldr([1, 2, 3], 0, fn(n, total) { n + total }) == 6
/// list.foldr([1, 2, 3], [], fn(x, xs) { [x, ..xs] }) == [1, 2, 3]
/// ```
pub fn foldr(self: List<a>, zero: b, with: fn(a, b) -> b) -> b {
  when self is {
    [] -> zero
    [x, ..xs] -> with(x, foldr(xs, zero, with))
  }
}

test foldr_1() {
  foldr([1, 2, 3, 4, 5], 0, fn(n, total) { n + total }) == 15
}

test foldr_2() {
  foldr(
    [1, 2, 3],
    "",
    fn(n, _str) {
      if builtin.mod_integer(n, 2) == 0 {
        "foo"
      } else {
        "bar"
      }
    },
  ) == "bar"
}

test foldr_3() {
  foldr([1, 2, 3, 4], [], fn(x, xs) { [x, ..xs] }) == [1, 2, 3, 4]
}

/// Like [`foldr`](#foldr), but also provides the position (0-based) of the elements when iterating.
///
/// ```aiken
/// let group = fn(i, x, xs) { [(i, x), ..xs] }
/// list.indexed_foldr(["a", "b", "c"], [], group) == [
///   (0, "a"),
///   (1, "b"),
///   (2, "c")
/// ]
/// ```
pub fn indexed_foldr(
  self: List<a>,
  zero: result,
  with: fn(Int, a, result) -> result,
) -> result {
  do_indexed_foldr(0, self, zero, with)
}

fn do_indexed_foldr(
  n: Int,
  self: List<a>,
  zero: result,
  with: fn(Int, a, result) -> result,
) -> result {
  when self is {
    [] -> zero
    [x, ..xs] -> with(n, x, do_indexed_foldr(n + 1, xs, zero, with))
  }
}

test indexed_foldr_1() {
  indexed_foldr([], 0, fn(i, x, xs) { i + x + xs }) == 0
}

test indexed_foldr_2() {
  let letters = ["a", "b", "c"]
  indexed_foldr(letters, [], fn(i, x, xs) { [(i, x), ..xs] }) == [
    (0, "a"), (1, "b"), (2, "c"),
  ]
}

/// Reduce a list from left to right using the accumulator as left operand.
/// Said differently, this is [`foldl`](#foldl) with callback arguments swapped.
///
/// ```aiken
/// list.reduce([#[1], #[2], #[3]], #[0], bytearray.concat) == #[0, 1, 2, 3]
/// list.reduce([True, False, True], False, fn(b, a) { or { b, a } }) == True
/// ```
pub fn reduce(self: List<a>, zero: b, with: fn(b, a) -> b) -> b {
  foldl(self, zero, flip(with))
}

test reduce_1() {
  reduce([], 0, fn(n, total) { n + total }) == 0
}

test reduce_2() {
  reduce([1, 2, 3], 0, fn(n, total) { n + total }) == 6
}

test reduce_3() {
  reduce([True, False, True], False, fn(left, right) { left || right }) == True
}

test reduce_4() {
  reduce(
    [#[1], #[2], #[3]],
    #[9],
    fn(left, right) { bytearray.concat(left, right) },
  ) == #[9, 1, 2, 3]
}
