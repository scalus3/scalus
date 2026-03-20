# Changelog

## v2.2.0 - 2025-07-13

### Added

- New Cardano-specific fuzzers covering
  - [Address](https://aiken-lang.github.io/fuzz/cardano/fuzz.html#Address)
  - [Assets](https://aiken-lang.github.io/fuzz/cardano/fuzz.html#Assets)
  - [Certificate](https://aiken-lang.github.io/fuzz/cardano/fuzz.html#Certificate)
  - [Transaction](https://aiken-lang.github.io/fuzz/cardano/fuzz.html#Transaction)

- New chaos-testing framework under [aiken/fuzz/scenario](https://aiken-lang.github.io/fuzz/aiken/fuzz/scenario.html)

- New [`data() -> Fuzzer<Data>`](https://aiken-lang.github.io/fuzz/aiken/fuzz.html#data)

## v2.1.1 - 2025-01-21

### Added

- More primitives for fuzzers:
    - `tuple`, `tuple3`, `tuple4`, `tuple5`, `tuple6`, `tuple7`, `tuple8`, `tuple9`

## v2.1.0 - 2024-09-24

### Added

- More primitives for combining fuzzers:
    - `either2`, `either3`, `either4`, `either5`, `either6`, `either7`, `either8`, `either9`
    - `pick(xs: List<a>) -> Fuzzer<(Int, a)>`
    - `label_if(str: String, predicate: Bool) -> Void`

### Changed

- Have bytearray primitives simplify towards the null bytestring (32 zeroes),
  so that it's easier to identify in simplified examples.

## v2.0.0 - 2024-09-01

### Changed

- Integrated with `aiken==1.1.0` and `aiken-lang/stdlib==v2`

## v1.0.0 - 2024-07-26

### Added

- Initial fuzz API covering various primitives:
  - **Labelling**
    - `label(str: String) -> Void`
    - `label_when(predicate: Bool, str: String, default: String) -> Void`

  - **Primitives**
    - `constant(a: a) -> Fuzzer<a>`
    - `option(fuzz_a: Fuzzer<a>) -> Fuzzer<Option<a>>`
    - `bool() -> Fuzzer<Bool>`

  - **Int**
    - `int() -> Fuzzer<Int>`
    - `int_at_least(min: Int) -> Fuzzer<Int>`
    - `int_at_most(max: Int) -> Fuzzer<Int>`
    - `int_between(min: Int, max: Int) -> Fuzzer<Int>`
    - `byte() -> Fuzzer<Int>`

  - **ByteArray**
    - `bytearray() -> Fuzzer<ByteArray>`
    - `bytearray_fixed(len: Int) -> Fuzzer<ByteArray>`
    - `bytearray_between(min: Int, max: Int) -> Fuzzer<ByteArray>`

  - **List**
    - `list(fuzzer: Fuzzer<a>) -> Fuzzer<List<a>>`
    - `list_at_least(fuzzer: Fuzzer<a>, min: Int) -> Fuzzer<List<a>>`
    - `list_at_most(fuzzer: Fuzzer<a>, max: Int) -> Fuzzer<List<a>>`
    - `list_between(fuzzer: Fuzzer<a>, min: Int, max: Int) -> Fuzzer<List<a>>`
    - `list_with_elem(fuzzer: Fuzzer<a>) -> Fuzzer<(List<a>, a)>`
    - `sublist(xs: List<a>) -> Fuzzer<List<a>>`

  - **Set**
    - `set(fuzzer: Fuzzer<a>) -> Fuzzer<List<a>>`
    - `set_at_least(fuzzer: Fuzzer<a>, min: Int) -> Fuzzer<List<a>>`
    - `set_at_most(fuzzer: Fuzzer<a>, max: Int) -> Fuzzer<List<a>>`
    - `set_between(fuzzer: Fuzzer<a>, min: Int, max: Int) -> Fuzzer<List<a>>`
    - `set_with_elem(fuzzer: Fuzzer<a>) -> Fuzzer<(List<a>, a)>`
    - `subset(xs: List<a>) -> Fuzzer<List<a>>`

  - **Combinators**
    - `one_of(xs: List<a>) -> Fuzzer<a>`
    - `either(left: Fuzzer<a>, right: Fuzzer<a>) -> Fuzzer<a>`
    - `both(left: Fuzzer<a>, right: Fuzzer<b>) -> Fuzzer<(a, b)>`
    - `such_that(fuzzer: Fuzzer<a>, predicate: fn(a) -> Bool) -> Fuzzer<a>`
    - `and_then(fuzz_a: Fuzzer<a>, f: fn(a) -> Fuzzer<b>) -> Fuzzer<b>`
    - `map(fuzz_a: Fuzzer<a>, f: fn(a) -> b) -> Fuzzer<b>`
    - `map2(fuzz_0: Fuzzer<t0>, fuzz_1: Fuzzer<t1>, f: fn(t0, t1) -> result) -> Fuzzer<result>`
    - `...`
    - `map9(..) -> Fuzzer<result>`
