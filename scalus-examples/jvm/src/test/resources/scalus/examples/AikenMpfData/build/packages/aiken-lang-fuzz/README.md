<div align="center">
  <hr />
    <h2 align="center" style="border-bottom: none"><img style="position: relative; top: 0.25rem;" src="https://raw.githubusercontent.com/aiken-lang/branding/main/assets/icon.png" alt="Aiken" height="30" /> aiken/fuzz</h2>

[![Licence](https://img.shields.io/github/license/aiken-lang/fuzz?style=for-the-badge)](https://github.com/aiken-lang/fuzz/blob/main/LICENSE)
[![Continuous Integration](https://img.shields.io/github/actions/workflow/status/aiken-lang/fuzz/continuous-integration.yml?style=for-the-badge)](https://github.com/aiken-lang/fuzz/actions/workflows/continuous-integration.yml)
  <hr/>
</div>

The official library for writing _fuzzers_ (a.k.a generators) for the [Aiken](https://aiken-lang.org) Cardano
smart-contract language.

It provides many useful primitives for writing and composing arbitrary generators in the context of [property-based testing](https://en.wikipedia.org/wiki/Property_testing).

## Installation

```
aiken add aiken-lang/fuzz --version v2
```

## Compatibility

aiken's version | fuzz's version(s)
---             | ---
$>=$ `v1.1.15`  | `2.2.0`
$>=$ `v1.1.13`  | `2.1.1`, `2.1.0`
$>=$ `v1.1.0`   | `1.0.0`

## Getting started

First, make sure you have the [Aiken's user manual about tests](https://aiken-lang.org/language-tour/tests#property-based-test); in particular the section about property-based test.

In many situations, you can use primitives from this library out-of-the-box, composing them inline when necessary. For example, if you need a non-empty list of values, you can simply write:

```
use aiken/fuzz

test my_prop(xs via fuzz.list_between(fuzz.int(), 1, 10)) {
  // some property
}
```

You can also write your own more complex fuzzer. Note that writing good fuzzers can be complicated, so here are a few guiding principles you should follow if you want them to be (a) effective and (b) easy to simplify for the test runner:

1. Ensure that _smaller values_ lead to _smaller fuzzers_. For example, if you're constructing a compound structure and you draw a value for the size, ensure that smaller values generate smaller structures. This is because the simplification simplifies towards 0.

2. Avoid fuzzers depending on far-away fuzzers. For example, you can write a fuzzer for generating list of values in mainly two ways: you can generate a random length, and then, a number of elements corresponding to that length. Or you can flip a coin, and each time choose to generate another element or to stop. The former is more intuitive, but the latter will produce lists that are easier to simplify.

3. Test your fuzzers! Use `label`, and ensure that the distribution of cases is what you expect it to be. Ensure that you go through specific scenarios. Writing fuzzers that omit crucial parts of the input domain is, unfortunately, quite easy.
