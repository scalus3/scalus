# Changelog

## v2.2.1 - 2025-12-16

### Fixed

- Add warning comment to `foldl2`.
- Expose `Foldl2` as a public type.

## v2.2.0 - 2024-12-13

### Added

- [`aiken/cbor.{deserialise}`](https://aiken-lang.github.io/stdlib/aiken/cbor.html#deserialise): to recover `Data` from CBOR bytes.
- [`aiken/collection/pairs.{insert_with_by_ascending_key}`](https://aiken-lang.github.io/stdlib/aiken/collection/pairs.html#insert_with_by_ascending_key): for inserting in pairs while specifying how to combine values on key conflict.

## v2.1.0 - 2024-09-14

### Added

- Various new helper functions:
  - [`aiken/collection/list.{for_each}`](https://aiken-lang.github.io/stdlib/aiken/collection/list.html#for_each): for performing many side-effects.
  - [`aiken/collection/dict.{pop}`](https://aiken-lang.github.io/stdlib/aiken/collection/dict.html#pop): for accessing and removing a value from a dictionnary in a single op.
  - [`aiken/primitive/bytearray.{starts_with}`](https://aiken-lang.github.io/stdlib/aiken/primitive/bytearray.html#starts_with): for matching bytearray prefixes.
  - [`aiken/primitive/math/rational.{pow}`](https://aiken-lang.github.io/stdlib/aiken/primitive/math/rational.html#pow): for computing (int) powers of rational numbers.
  - [`cardano/assets.{match}`](https://aiken-lang.github.io/stdlib/cardano/assets.html#match): efficiently compare two value-like.
  - [`cardano/assets.{restricted_to}`](https://aiken-lang.github.io/stdlib/cardano/assets.html#restricted_to): extracting value subsets from parent value.

- Comparison functions for various Cardano types:
  - [`cardano/address/credential.{compare}`](https://aiken-lang.github.io/stdlib/cardano/address/credential.html#compare): for ordering credentials.
  - [`cardano/governance/voter.{compare}`](https://aiken-lang.github.io/stdlib/cardano/governacen/voter.html#compare): for ordering voters.
  - [`cardano/transaction/output_reference.{compare}`](https://aiken-lang.github.io/stdlib/cardano/transaction/output_reference.html#compare): for ordering output references.
  - [`cardano/transaction/script_purpose.{compare}`](https://aiken-lang.github.io/stdlib/cardano/transaction/script_purpose.html#compare): for ordering script purpose.

- New BLS12-381 crypto modules:
  - [`aiken/crypto/bls12_381/g1`](https://aiken-lang.github.io/stdlib/aiken/crypto/bls12_381/g1.html)
  - [`aiken/crypto/bls12_381/g2`](https://aiken-lang.github.io/stdlib/aiken/crypto/bls12_381/g2.html)
  - [`aiken/crypto/bls12_381/scalar`](https://aiken-lang.github.io/stdlib/aiken/crypto/bls12_381/scalar.html)

### Changed

- N/A

### Removed

- N/A

## v2.0.0 - 2024-09-01

> [!NOTE]
> Significant performance improvements (mostly on CPU) across all boards mostly due to the integration of Plutus V3.
>
> <details><summary>see benchmarks</summary>
>
> test                                               | cpu     | mem
> ---                                                | ---     | ---
> aiken/cbor.{serialise_1}                           | -38.20% | ±0.00%
> aiken/cbor.{serialise_2}                           | -38.20% | ±0.00%
> aiken/cbor.{serialise_3}                           | -37.25% | ±0.00%
> aiken/cbor.{serialise_4}                           | -41.95% | ±0.00%
> aiken/cbor.{serialise_5}                           | -42.77% | ±0.00%
> aiken/cbor.{serialise_6}                           | -42.63% | ±0.00%
> aiken/cbor.{serialise_7}                           | -40.51% | ±0.00%
> aiken/cbor.{serialise_8}                           | -37.25% | ±0.00%
> aiken/cbor.{serialise_9}                           | -41.95% | ±0.00%
> aiken/cbor.{diagnostic_1}                          | -47.62% | -4.35%
> aiken/cbor.{diagnostic_2}                          | -45.16% | -2.87%
> aiken/cbor.{diagnostic_3}                          | -43.32% | -13.33%
> aiken/cbor.{diagnostic_4}                          | -38.28% | -8.03%
> aiken/cbor.{diagnostic_5}                          | -44.15% | -14.59%
> aiken/cbor.{diagnostic_6}                          | -42.77% | -12.21%
> aiken/cbor.{diagnostic_7}                          | -43.87% | -16.87%
> aiken/cbor.{diagnostic_7_alt}                      | -42.99% | -11.56%
> aiken/cbor.{diagnostic_8}                          | -46.00% | -10.23%
> aiken/cbor.{diagnostic_9}                          | -42.81% | -2.81%
> aiken/cbor.{diagnostic_10}                         | -38.28% | -8.03%
> aiken/cbor.{diagnostic_10_alt}                     | -38.43% | -8.03%
> aiken/cbor.{diagnostic_11}                         | -44.00% | -8.51%
> aiken/cbor.{diagnostic_12}                         | -45.65% | -11.56%
> aiken/cbor.{diagnostic_13}                         | -44.44% | -9.34%
> aiken/cbor.{diagnostic_14}                         | -43.59% | -19.77%
> aiken/cbor.{diagnostic_15}                         | -46.50% | -3.67%
> aiken/cbor.{diagnostic_16}                         | -41.89% | -13.41%
> aiken/collection/dict.{bench_from_ascending_pairs} | -20.48% | ±0.00%
> aiken/collection/dict.{from_list_1}                | -20.16% | ±0.00%
> aiken/collection/dict.{from_list_2}                | -18.28% | ±0.00%
> aiken/collection/dict.{from_list_3}                | -17.83% | ±0.00%
> aiken/collection/dict.{from_list_4}                | -18.97% | ±0.00%
> aiken/collection/dict.{bench_from_pairs}           | -25.28% | ±0.00%
> aiken/collection/dict.{find_1}                     | -20.63% | ±0.00%
> aiken/collection/dict.{find_2}                     | -20.43% | ±0.00%
> aiken/collection/dict.{find_3}                     | -22.03% | ±0.00%
> aiken/collection/dict.{find_4}                     | -22.53% | ±0.00%
> aiken/collection/dict.{get_1}                      | -20.63% | ±0.00%
> aiken/collection/dict.{get_2}                      | -22.72% | ±0.00%
> aiken/collection/dict.{get_3}                      | -23.26% | ±0.00%
> aiken/collection/dict.{get_4}                      | -26.91% | ±0.00%
> aiken/collection/dict.{get_5}                      | -26.30% | ±0.00%
> aiken/collection/dict.{has_key_1}                  | -28.07% | ±0.00%
> aiken/collection/dict.{has_key_2}                  | -30.77% | ±0.00%
> aiken/collection/dict.{has_key_3}                  | -30.22% | ±0.00%
> aiken/collection/dict.{has_key_4}                  | -27.25% | ±0.00%
> aiken/collection/dict.{is_empty_1}                 | -27.86% | ±0.00%
> aiken/collection/dict.{keys_1}                     | -20.30% | ±0.00%
> aiken/collection/dict.{keys_2}                     | -17.48% | ±0.00%
> aiken/collection/dict.{size_1}                     | -37.90% | ±0.00%
> aiken/collection/dict.{size_2}                     | -32.34% | ±0.00%
> aiken/collection/dict.{size_3}                     | -27.97% | ±0.00%
> aiken/collection/dict.{values_1}                   | -20.30% | ±0.00%
> aiken/collection/dict.{values_2}                   | -17.58% | ±0.00%
> aiken/collection/dict.{delete_1}                   | -20.16% | ±0.00%
> aiken/collection/dict.{delete_2}                   | -24.29% | ±0.00%
> aiken/collection/dict.{delete_3}                   | -21.03% | ±0.00%
> aiken/collection/dict.{delete_4}                   | -25.03% | ±0.00%
> aiken/collection/dict.{delete_5}                   | -27.22% | ±0.00%
> aiken/collection/dict.{delete_6}                   | -25.83% | ±0.00%
> aiken/collection/dict.{filter_1}                   | -20.16% | ±0.00%
> aiken/collection/dict.{filter_2}                   | -19.61% | ±0.00%
> aiken/collection/dict.{filter_3}                   | -20.15% | ±0.00%
> aiken/collection/dict.{insert_1}                   | -22.83% | ±0.00%
> aiken/collection/dict.{insert_2}                   | -21.77% | ±0.00%
> aiken/collection/dict.{insert_with_1}              | -17.21% | ±0.00%
> aiken/collection/dict.{insert_with_2}              | -22.66% | ±0.00%
> aiken/collection/dict.{insert_with_3}              | -25.81% | ±0.00%
> aiken/collection/dict.{map_1}                      | -19.56% | ±0.00%
> aiken/collection/dict.{map_2}                      | -23.66% | ±0.00%
> aiken/collection/dict.{union_1}                    | -17.91% | ±0.00%
> aiken/collection/dict.{union_2}                    | -8.67%  | ±0.00%
> aiken/collection/dict.{union_3}                    | -22.82% | ±0.00%
> aiken/collection/dict.{union_4}                    | -22.77% | ±0.00%
> aiken/collection/dict.{union_with_1}               | -22.90% | ±0.00%
> aiken/collection/dict.{fold_1}                     | -35.94% | ±0.00%
> aiken/collection/dict.{fold_2}                     | -22.31% | ±0.00%
> aiken/collection/dict.{foldr_1}                    | -36.21% | ±0.00%
> aiken/collection/dict.{foldr_2}                    | -21.93% | ±0.00%
> aiken/collection/dict.{to_list_1}                  | -98.69% | -66.72%
> aiken/collection/dict.{to_list_2}                  | -98.91% | -66.72%
> aiken/collection/list.{push_1}                     | -8.02%  | ±0.00%
> aiken/collection/list.{push_2}                     | 1.25%   | ±0.00%
> aiken/collection/list.{range_1}                    | -27.77% | ±0.00%
> aiken/collection/list.{range_2}                    | -27.39% | ±0.00%
> aiken/collection/list.{repeat_1}                   | -23.72% | ±0.00%
> aiken/collection/list.{repeat_2}                   | -27.96% | ±0.00%
> aiken/collection/list.{all_1}                      | -28.36% | ±0.00%
> aiken/collection/list.{all_2}                      | -27.59% | ±0.00%
> aiken/collection/list.{all_3}                      | -27.94% | ±0.00%
> aiken/collection/list.{any_1}                      | -28.23% | ±0.00%
> aiken/collection/list.{any_2}                      | -28.09% | ±0.00%
> aiken/collection/list.{any_3}                      | -26.95% | ±0.00%
> aiken/collection/list.{at_1}                       | -27.60% | ±0.00%
> aiken/collection/list.{at_2}                       | -19.96% | ±0.00%
> aiken/collection/list.{at_3}                       | -27.60% | ±0.00%
> aiken/collection/list.{at_4}                       | -20.77% | ±0.00%
> aiken/collection/list.{at_5}                       | -25.75% | ±0.00%
> aiken/collection/list.{count_empty}                | -36.83% | ±0.00%
> aiken/collection/list.{count_all}                  | -32.37% | ±0.00%
> aiken/collection/list.{count_some}                 | -31.73% | ±0.00%
> aiken/collection/list.{count_none}                 | -30.44% | ±0.00%
> aiken/collection/list.{find_1}                     | -20.59% | ±0.00%
> aiken/collection/list.{find_2}                     | -25.53% | ±0.00%
> aiken/collection/list.{find_3}                     | -19.64% | ±0.00%
> aiken/collection/list.{has_1}                      | -27.88% | ±0.00%
> aiken/collection/list.{has_2}                      | -27.69% | ±0.00%
> aiken/collection/list.{has_3}                      | -26.95% | ±0.00%
> aiken/collection/list.{head_1}                     | -14.03% | ±0.00%
> aiken/collection/list.{head_2}                     | -16.90% | ±0.00%
> aiken/collection/list.{is_empty_1}                 | -26.48% | ±0.00%
> aiken/collection/list.{is_empty_2}                 | -25.35% | ±0.00%
> aiken/collection/list.{index_of_1}                 | -25.62% | ±0.00%
> aiken/collection/list.{index_of_2}                 | -27.52% | ±0.00%
> aiken/collection/list.{index_of_3}                 | -26.65% | ±0.00%
> aiken/collection/list.{index_of_4}                 | -19.96% | ±0.00%
> aiken/collection/list.{last_1}                     | -19.18% | ±0.00%
> aiken/collection/list.{last_2}                     | -16.26% | ±0.00%
> aiken/collection/list.{last_3}                     | -17.13% | ±0.00%
> aiken/collection/list.{length_1}                   | -37.90% | ±0.00%
> aiken/collection/list.{length_2}                   | -30.89% | ±0.00%
> aiken/collection/list.{delete_1}                   | -20.20% | ±0.00%
> aiken/collection/list.{delete_2}                   | -15.02% | ±0.00%
> aiken/collection/list.{delete_3}                   | -20.55% | ±0.00%
> aiken/collection/list.{delete_4}                   | -22.46% | ±0.00%
> aiken/collection/list.{drop_1}                     | -24.62% | ±0.00%
> aiken/collection/list.{drop_2}                     | -28.08% | ±0.00%
> aiken/collection/list.{drop_while_1}               | -19.79% | ±0.00%
> aiken/collection/list.{drop_while_2}               | -22.25% | ±0.00%
> aiken/collection/list.{drop_while_3}               | 0.86%   | ±0.00%
> aiken/collection/list.{drop_while_4}               | -27.26% | ±0.00%
> aiken/collection/list.{filter_1}                   | -20.20% | ±0.00%
> aiken/collection/list.{filter_2}                   | -32.06% | ±0.00%
> aiken/collection/list.{filter_3}                   | -31.39% | ±0.00%
> aiken/collection/list.{filter_map_1}               | -21.10% | ±0.00%
> aiken/collection/list.{filter_map_2}               | -28.74% | ±0.00%
> aiken/collection/list.{init_1}                     | -19.64% | ±0.00%
> aiken/collection/list.{init_2}                     | -20.01% | ±0.00%
> aiken/collection/list.{init_3}                     | -13.72% | ±0.00%
> aiken/collection/list.{partition_1}                | -14.63% | ±0.00%
> aiken/collection/list.{partition_2}                | -16.85% | ±0.00%
> aiken/collection/list.{partition_3}                | -16.63% | ±0.00%
> aiken/collection/list.{partition_4}                | -16.87% | ±0.00%
> aiken/collection/list.{partition_5}                | -22.94% | ±0.00%
> aiken/collection/list.{slice_1}                    | -29.08% | -2.81%
> aiken/collection/list.{slice_2}                    | -30.11% | -2.25%
> aiken/collection/list.{slice_3}                    | -30.29% | -1.46%
> aiken/collection/list.{slice_4}                    | -28.53% | -1.48%
> aiken/collection/list.{slice_5}                    | -29.73% | -1.64%
> aiken/collection/list.{slice_6}                    | -32.01% | -1.80%
> aiken/collection/list.{span_1}                     | -15.05% | ±0.00%
> aiken/collection/list.{span_2}                     | -18.03% | ±0.00%
> aiken/collection/list.{span_3}                     | -12.49% | ±0.00%
> aiken/collection/list.{span_4}                     | -18.13% | ±0.00%
> aiken/collection/list.{tail_1}                     | -8.88%  | ±0.00%
> aiken/collection/list.{tail_2}                     | -16.90% | ±0.00%
> aiken/collection/list.{take_1}                     | -24.98% | ±0.00%
> aiken/collection/list.{take_2}                     | -24.35% | ±0.00%
> aiken/collection/list.{take_while_1}               | -20.20% | ±0.00%
> aiken/collection/list.{take_while_2}               | -21.56% | ±0.00%
> aiken/collection/list.{take_while_3}               | -22.46% | ±0.00%
> aiken/collection/list.{take_while_4}               | -21.02% | ±0.00%
> aiken/collection/list.{unique_1}                   | -20.20% | ±0.00%
> aiken/collection/list.{unique_2}                   | -24.34% | ±0.00%
> aiken/collection/list.{flat_map_1}                 | -19.79% | ±0.00%
> aiken/collection/list.{flat_map_2}                 | -13.36% | ±0.00%
> aiken/collection/list.{indexed_map_1}              | -20.10% | ±0.00%
> aiken/collection/list.{indexed_map_2}              | -23.36% | ±0.00%
> aiken/collection/list.{map_1}                      | -19.79% | ±0.00%
> aiken/collection/list.{map_2}                      | -16.75% | ±0.00%
> aiken/collection/list.{map2_1}                     | -20.10% | ±0.00%
> aiken/collection/list.{map2_2}                     | -17.46% | ±0.00%
> aiken/collection/list.{map2_3}                     | -15.92% | ±0.00%
> aiken/collection/list.{map3_1}                     | -20.39% | ±0.00%
> aiken/collection/list.{map3_2}                     | -19.22% | ±0.00%
> aiken/collection/list.{reverse_1}                  | -20.10% | ±0.00%
> aiken/collection/list.{reverse_2}                  | -12.26% | ±0.00%
> aiken/collection/list.{sort_1}                     | -22.31% | ±0.00%
> aiken/collection/list.{sort_2}                     | -17.93% | ±0.00%
> aiken/collection/list.{sort_3}                     | -23.09% | ±0.00%
> aiken/collection/list.{sort_4}                     | -20.20% | ±0.00%
> aiken/collection/list.{unzip_1}                    | -14.01% | ±0.00%
> aiken/collection/list.{unzip_2}                    | -5.48%  | ±0.00%
> aiken/collection/list.{concat_1}                   | -6.56%  | ±0.00%
> aiken/collection/list.{concat_2}                   | -11.25% | ±0.00%
> aiken/collection/list.{concat_3}                   | -9.35%  | ±0.00%
> aiken/collection/list.{difference_1}               | -24.23% | ±0.00%
> aiken/collection/list.{difference_2}               | -22.59% | ±0.00%
> aiken/collection/list.{difference_3}               | -10.64% | ±0.00%
> aiken/collection/list.{difference_4}               | -21.68% | ±0.00%
> aiken/collection/list.{zip_1}                      | -20.10% | ±0.00%
> aiken/collection/list.{zip_2}                      | -19.17% | ±0.00%
> aiken/collection/list.{zip_3}                      | -10.35% | ±0.00%
> aiken/collection/list.{foldl_1}                    | -36.95% | ±0.00%
> aiken/collection/list.{foldl_2}                    | -26.90% | ±0.00%
> aiken/collection/list.{foldl_3}                    | -11.27% | ±0.00%
> aiken/collection/list.{foldr_1}                    | -26.68% | ±0.00%
> aiken/collection/list.{foldr_2}                    | -38.04% | ±0.00%
> aiken/collection/list.{foldr_3}                    | -10.14% | ±0.00%
> aiken/collection/list.{indexed_foldr_1}            | -36.95% | ±0.00%
> aiken/collection/list.{indexed_foldr_2}            | -11.06% | ±0.00%
> aiken/collection/list.{reduce_1}                   | -36.95% | ±0.00%
> aiken/collection/list.{reduce_2}                   | -27.99% | ±0.00%
> aiken/collection/list.{reduce_3}                   | -23.54% | ±0.00%
> aiken/collection/list.{reduce_4}                   | -24.84% | ±0.00%
> aiken/collection/pairs.{get_all_1}                 | -21.10% | ±0.00%
> aiken/collection/pairs.{get_all_2}                 | -18.86% | ±0.00%
> aiken/collection/pairs.{get_all_3}                 | -19.53% | ±0.00%
> aiken/collection/pairs.{get_all_4}                 | -18.70% | ±0.00%
> aiken/collection/pairs.{get_all_5}                 | -21.19% | ±0.00%
> aiken/collection/pairs.{get_first_1}               | -20.63% | ±0.00%
> aiken/collection/pairs.{get_first_2}               | -18.86% | ±0.00%
> aiken/collection/pairs.{get_first_3}               | -18.86% | ±0.00%
> aiken/collection/pairs.{get_first_4}               | -18.86% | ±0.00%
> aiken/collection/pairs.{get_first_5}               | -21.05% | ±0.00%
> aiken/collection/pairs.{get_last_1}                | -20.63% | ±0.00%
> aiken/collection/pairs.{get_last_2}                | -21.13% | ±0.00%
> aiken/collection/pairs.{get_last_3}                | -21.16% | ±0.00%
> aiken/collection/pairs.{get_last_4}                | -21.79% | ±0.00%
> aiken/collection/pairs.{get_last_5}                | -21.05% | ±0.00%
> aiken/collection/pairs.{find_all_1}                | -21.10% | ±0.00%
> aiken/collection/pairs.{find_all_2}                | -18.33% | ±0.00%
> aiken/collection/pairs.{find_all_3}                | -20.51% | ±0.00%
> aiken/collection/pairs.{find_all_4}                | -17.79% | ±0.00%
> aiken/collection/pairs.{find_first_1}              | -20.63% | ±0.00%
> aiken/collection/pairs.{find_first_2}              | -18.28% | ±0.00%
> aiken/collection/pairs.{find_first_3}              | -20.22% | ±0.00%
> aiken/collection/pairs.{find_first_4}              | -18.28% | ±0.00%
> aiken/collection/pairs.{find_last_1}               | -20.63% | ±0.00%
> aiken/collection/pairs.{find_last_2}               | -20.70% | ±0.00%
> aiken/collection/pairs.{find_last_3}               | -20.22% | ±0.00%
> aiken/collection/pairs.{find_last_4}               | -20.98% | ±0.00%
> aiken/collection/pairs.{has_key_1}                 | -28.07% | ±0.00%
> aiken/collection/pairs.{has_key_2}                 | -25.70% | ±0.00%
> aiken/collection/pairs.{has_key_3}                 | -25.80% | ±0.00%
> aiken/collection/pairs.{has_key_4}                 | -24.93% | ±0.00%
> aiken/collection/pairs.{has_key_5}                 | -25.70% | ±0.00%
> aiken/collection/pairs.{keys_1}                    | -20.30% | ±0.00%
> aiken/collection/pairs.{keys_2}                    | -13.89% | ±0.00%
> aiken/collection/pairs.{keys_3}                    | -10.43% | ±0.00%
> aiken/collection/pairs.{values_1}                  | -20.30% | ±0.00%
> aiken/collection/pairs.{values_2}                  | -14.02% | ±0.00%
> aiken/collection/pairs.{values_3}                  | -10.65% | ±0.00%
> aiken/collection/pairs.{values_4}                  | -8.53%  | ±0.00%
> aiken/collection/pairs.{map_1}                     | -11.17% | ±0.00%
> aiken/collection/pairs.{map_2}                     | -12.89% | ±0.00%
> aiken/collection/pairs.{foldl_1}                   | -35.94% | ±0.00%
> aiken/collection/pairs.{foldl_2}                   | -22.31% | ±0.00%
> aiken/collection/pairs.{foldr_1}                   | -36.21% | ±0.00%
> aiken/collection/pairs.{foldr_2}                   | -21.93% | ±0.00%
> aiken/collection/pairs.{foldr_3}                   | -20.00% | ±0.00%
> aiken/interval.{contains_1}                        | -21.08% | -4.01%
> aiken/interval.{contains_2}                        | -31.22% | -13.95%
> aiken/interval.{contains_3}                        | -26.80% | -10.08%
> aiken/interval.{contains_4}                        | -31.02% | -13.67%
> aiken/interval.{contains_5}                        | -32.32% | -13.59%
> aiken/interval.{contains_6}                        | -28.15% | -9.81%
> aiken/interval.{contains_7}                        | -32.11% | -13.32%
> aiken/interval.{contains_8}                        | -29.56% | -12.59%
> aiken/interval.{contains_9}                        | -29.68% | -12.78%
> aiken/interval.{contains_10}                       | -29.68% | -12.78%
> aiken/interval.{contains_11}                       | -35.17% | -17.77%
> aiken/interval.{contains_12}                       | -21.09% | -3.86%
> aiken/interval.{is_entirely_after_1}               | -29.89% | -13.81%
> aiken/interval.{is_entirely_after_2}               | -29.63% | -13.39%
> aiken/interval.{is_entirely_after_3}               | -29.63% | -13.39%
> aiken/interval.{is_entirely_after_4}               | -29.48% | -11.81%
> aiken/interval.{is_entirely_after_5}               | -29.70% | -12.14%
> aiken/interval.{is_entirely_after_6}               | -36.09% | -19.77%
> aiken/interval.{is_entirely_after_7}               | -24.19% | -3.99%
> aiken/interval.{is_entirely_after_8}               | -24.19% | -3.99%
> aiken/interval.{is_entirely_after_9}               | -24.19% | -3.99%
> aiken/interval.{is_entirely_before_1}              | -28.44% | -13.48%
> aiken/interval.{is_entirely_before_2}              | -28.24% | -13.09%
> aiken/interval.{is_entirely_before_3}              | -28.24% | -13.09%
> aiken/interval.{is_entirely_before_4}              | -28.44% | -11.88%
> aiken/interval.{is_entirely_before_5}              | -28.26% | -11.57%
> aiken/interval.{is_entirely_before_6}              | -34.63% | -19.34%
> aiken/interval.{is_entirely_before_7}              | -22.97% | -4.02%
> aiken/interval.{is_entirely_before_8}              | -22.97% | -4.02%
> aiken/interval.{is_entirely_before_9}              | -22.97% | -4.02%
> aiken/interval.{hull_1}                            | -21.51% | -0.73%
> aiken/interval.{hull_2}                            | -23.06% | -0.80%
> aiken/interval.{hull_3}                            | -22.00% | -0.86%
> aiken/interval.{intersection_1}                    | -21.51% | -0.73%
> aiken/interval.{intersection_2}                    | -21.51% | -0.73%
> aiken/interval.{intersection_3}                    | -26.55% | -4.65%
> aiken/interval.{intersection_4}                    | -26.45% | -4.51%
> aiken/interval.{intersection_5}                    | -22.87% | -0.76%
> aiken/interval.{intersection_6}                    | -19.73% | -0.98%
> aiken/math.{abs_1}                                 | -61.39% | -21.07%
> aiken/math.{abs_2}                                 | -70.90% | -34.84%
> aiken/math.{clamp_1}                               | -60.95% | -23.55%
> aiken/math.{clamp_2}                               | -60.95% | -23.55%
> aiken/math.{clamp_3}                               | -59.22% | -18.20%
> aiken/math.{gcd_test1}                             | -47.20% | ±0.00%
> aiken/math.{gcd_test2}                             | -47.81% | ±0.00%
> aiken/math.{gcd_test3}                             | -46.10% | ±0.00%
> aiken/math.{is_sqrt1}                              | -87.41% | -68.64%
> aiken/math.{is_sqrt2}                              | -87.41% | -68.64%
> aiken/math.{log_10_2}                              | -51.35% | -8.40%
> aiken/math.{log_42_2}                              | -51.46% | -8.24%
> aiken/math.{log_42_3}                              | -51.05% | -7.81%
> aiken/math.{log_5_0}                               | -54.05% | -12.92%
> aiken/math.{log_4_4}                               | -50.59% | -9.31%
> aiken/math.{log_4_43}                              | -49.14% | -7.28%
> aiken/math.{max_1}                                 | -61.39% | -21.07%
> aiken/math.{max_2}                                 | -61.39% | -21.07%
> aiken/math.{max_3}                                 | -61.39% | -21.07%
> aiken/math.{min_1}                                 | -61.39% | -21.07%
> aiken/math.{min_2}                                 | -61.39% | -21.07%
> aiken/math.{min_3}                                 | -61.39% | -21.07%
> aiken/math.{pow_3_5}                               | -46.34% | ±0.00%
> aiken/math.{pow_7_2}                               | -46.38% | ±0.00%
> aiken/math.{pow_3__4}                              | -43.50% | ±0.00%
> aiken/math.{pow_0_0}                               | -43.95% | ±0.00%
> aiken/math.{pow_513_3}                             | -45.80% | ±0.00%
> aiken/math.{pow_2_4}                               | -46.79% | ±0.00%
> aiken/math.{pow_2_42}                              | -46.77% | ±0.00%
> aiken/math.{pow2_neg}                              | -44.71% | ±0.00%
> aiken/math.{pow2_0}                                | -45.00% | ±0.00%
> aiken/math.{pow2_1}                                | -45.00% | ±0.00%
> aiken/math.{pow2_4}                                | -45.00% | ±0.00%
> aiken/math.{pow2_42}                               | -42.01% | ±0.00%
> aiken/math.{pow2_256}                              | -41.40% | ±0.00%
> aiken/math.{sqrt1}                                 | -32.56% | -17.18%
> aiken/math.{sqrt2}                                 | -32.56% | -17.18%
> aiken/math.{sqrt3}                                 | -49.99% | -8.90%
> aiken/math.{sqrt4}                                 | -51.76% | -3.90%
> aiken/math.{sqrt5}                                 | -52.63% | -1.33%
> aiken/math.{sqrt6}                                 | -28.16% | -15.41%
> aiken/math/rational.{from_int_1}                   | -14.32% | ±0.00%
> aiken/math/rational.{new_1}                        | -22.98% | ±0.00%
> aiken/math/rational.{zero_1}                       | -8.08%  | ±0.00%
> aiken/math/rational.{denominator_1}                | -28.33% | ±0.00%
> aiken/math/rational.{numerator_1}                  | -29.34% | ±0.00%
> aiken/math/rational.{abs_examples}                 | -18.25% | ±0.00%
> aiken/math/rational.{negate_1}                     | -15.39% | ±0.00%
> aiken/math/rational.{reciprocal_1}                 | -23.28% | ±0.00%
> aiken/math/rational.{reduce_1}                     | -31.89% | ±0.00%
> aiken/math/rational.{add_1}                        | -15.11% | ±0.00%
> aiken/math/rational.{add_2}                        | -15.11% | ±0.00%
> aiken/math/rational.{div_1}                        | -22.31% | -2.75%
> aiken/math/rational.{div_2}                        | -22.37% | -2.79%
> aiken/math/rational.{mul_1}                        | -13.37% | ±0.00%
> aiken/math/rational.{mul_2}                        | -13.37% | ±0.00%
> aiken/math/rational.{mul_3}                        | -26.25% | ±0.00%
> aiken/math/rational.{sub_1}                        | -15.11% | ±0.00%
> aiken/math/rational.{sub_2}                        | -15.11% | ±0.00%
> aiken/math/rational.{sub_3}                        | -15.11% | ±0.00%
> aiken/math/rational.{compare_1}                    | -21.70% | ±0.00%
> aiken/math/rational.{compare_with_eq}              | -23.05% | ±0.00%
> aiken/math/rational.{compare_with_neq}             | -22.33% | ±0.00%
> aiken/math/rational.{compare_with_gte}             | -22.48% | ±0.00%
> aiken/math/rational.{compare_with_gt}              | -23.18% | ±0.00%
> aiken/math/rational.{compare_with_lte}             | -22.48% | ±0.00%
> aiken/math/rational.{compare_with_lt}              | -23.18% | ±0.00%
> aiken/math/rational.{arithmetic_mean_1}            | -23.31% | ±0.00%
> aiken/math/rational.{arithmetic_mean_2}            | -23.31% | ±0.00%
> aiken/math/rational.{arithmetic_mean_3}            | -20.58% | ±0.00%
> aiken/math/rational.{geometric_mean1}              | -29.87% | ±0.00%
> aiken/math/rational.{geometric_mean2}              | -24.52% | ±0.00%
> aiken/math/rational.{geometric_mean3}              | -24.52% | ±0.00%
> aiken/math/rational.{geometric_mean4}              | -33.55% | ±0.00%
> aiken/math/rational.{geometric_mean5}              | -45.34% | ±0.00%
> aiken/math/rational.{ceil_1}                       | -36.26% | ±0.00%
> aiken/math/rational.{floor_1}                      | -29.49% | ±0.00%
> aiken/math/rational.{proper_fraction_1}            | -18.44% | ±0.00%
> aiken/math/rational.{proper_fraction_2}            | -18.44% | ±0.00%
> aiken/math/rational.{proper_fraction_3}            | -18.44% | ±0.00%
> aiken/math/rational.{round_1}                      | -25.17% | ±0.00%
> aiken/math/rational.{round_even_1}                 | -25.91% | ±0.00%
> aiken/math/rational.{truncate_1}                   | -29.49% | ±0.00%
> aiken/option.{is_none_1}                           | -26.56% | ±0.00%
> aiken/option.{is_none_2}                           | -27.52% | ±0.00%
> aiken/option.{is_some_1}                           | -27.52% | ±0.00%
> aiken/option.{is_some_2}                           | -26.56% | ±0.00%
> aiken/option.{and_then_1}                          | -20.19% | ±0.00%
> aiken/option.{and_then_2}                          | -22.15% | ±0.00%
> aiken/option.{and_then_3}                          | -21.85% | ±0.00%
> aiken/option.{choice_1}                            | -17.11% | ±0.00%
> aiken/option.{choice_2}                            | -19.75% | ±0.00%
> aiken/option.{choice_3}                            | -18.68% | ±0.00%
> aiken/option.{flatten_1}                           | -12.25% | ±0.00%
> aiken/option.{flatten_2}                           | -15.41% | ±0.00%
> aiken/option.{flatten_3}                           | -19.46% | ±0.00%
> aiken/option.{flatten_4}                           | -14.31% | ±0.00%
> aiken/option.{map_1}                               | -19.89% | ±0.00%
> aiken/option.{map_2}                               | -18.18% | ±0.00%
> aiken/option.{map2_1}                              | -20.47% | ±0.00%
> aiken/option.{map2_2}                              | -19.93% | ±0.00%
> aiken/option.{map2_3}                              | -13.64% | ±0.00%
> aiken/option.{map3_1}                              | -20.74% | ±0.00%
> aiken/option.{map3_2}                              | -20.00% | ±0.00%
> aiken/option.{map3_3}                              | -19.90% | ±0.00%
> aiken/option.{or_try_1}                            | -14.36% | ±0.00%
> aiken/option.{or_try_2}                            | -14.36% | ±0.00%
> aiken/option.{or_else_1}                           | -38.16% | ±0.00%
> aiken/option.{or_else_2}                           | -27.62% | ±0.00%
> aiken/primitive/bytearray.{from_string_1}          | -62.36% | ±0.00%
> aiken/primitive/bytearray.{from_string_2}          | -41.62% | ±0.00%
> aiken/primitive/bytearray.{push_1}                 | -97.51% | -80.06%
> aiken/primitive/bytearray.{push_2}                 | -97.51% | -80.06%
> aiken/primitive/bytearray.{push_3}                 | -88.82% | -89.83%
> aiken/primitive/bytearray.{index_of_1}             | -39.75% | ±0.00%
> aiken/primitive/bytearray.{index_of_2}             | -43.19% | ±0.00%
> aiken/primitive/bytearray.{index_of_3}             | -41.70% | ±0.00%
> aiken/primitive/bytearray.{index_of_4}             | -37.24% | ±0.00%
> aiken/primitive/bytearray.{index_of_5}             | -26.02% | ±0.00%
> aiken/primitive/bytearray.{is_empty_1}             | -37.52% | ±0.00%
> aiken/primitive/bytearray.{is_empty_2}             | -33.77% | ±0.00%
> aiken/primitive/bytearray.{length_1}               | -49.73% | ±0.00%
> aiken/primitive/bytearray.{length_2}               | -49.73% | ±0.00%
> aiken/primitive/bytearray.{test_bit_0}             | -45.48% | 5.88%
> aiken/primitive/bytearray.{test_bit_1}             | -56.22% | -10.85%
> aiken/primitive/bytearray.{test_bit_2}             | -56.22% | -10.85%
> aiken/primitive/bytearray.{test_bit_3}             | -56.22% | -10.85%
> aiken/primitive/bytearray.{test_bit_7}             | -58.31% | -11.81%
> aiken/primitive/bytearray.{test_bit_8}             | -56.22% | -10.85%
> aiken/primitive/bytearray.{test_bit_20_21_22_23}   | -44.38% | 5.52%
> aiken/primitive/bytearray.{drop_1}                 | -58.79% | ±0.00%
> aiken/primitive/bytearray.{drop_2}                 | -58.79% | ±0.00%
> aiken/primitive/bytearray.{drop_3}                 | -58.79% | ±0.00%
> aiken/primitive/bytearray.{drop_4}                 | -58.79% | ±0.00%
> aiken/primitive/bytearray.{slice_1}                | -98.79% | -90.04%
> aiken/primitive/bytearray.{slice_2}                | -98.79% | -90.04%
> aiken/primitive/bytearray.{slice_3}                | -98.79% | -90.04%
> aiken/primitive/bytearray.{slice_4}                | -98.79% | -90.04%
> aiken/primitive/bytearray.{slice_5}                | -98.79% | -90.04%
> aiken/primitive/bytearray.{take_1}                 | -97.81% | -83.40%
> aiken/primitive/bytearray.{take_2}                 | -97.81% | -83.40%
> aiken/primitive/bytearray.{take_3}                 | -97.81% | -83.40%
> aiken/primitive/bytearray.{take_4}                 | -97.81% | -83.40%
> aiken/primitive/bytearray.{concat_1}               | -96.22% | -80.06%
> aiken/primitive/bytearray.{concat_2}               | -96.22% | -80.06%
> aiken/primitive/bytearray.{concat_3}               | -96.22% | -80.06%
> aiken/primitive/bytearray.{concat_4}               | -96.22% | -80.06%
> aiken/primitive/bytearray.{foldl_1}                | -40.96% | ±0.00%
> aiken/primitive/bytearray.{foldl_2}                | -40.09% | ±0.00%
> aiken/primitive/bytearray.{foldl_3}                | -40.29% | ±0.00%
> aiken/primitive/bytearray.{foldl_4}                | -44.76% | ±0.00%
> aiken/primitive/bytearray.{foldr_1}                | -42.56% | ±0.00%
> aiken/primitive/bytearray.{foldr_2}                | -40.93% | ±0.00%
> aiken/primitive/bytearray.{foldr_3}                | -45.34% | ±0.00%
> aiken/primitive/bytearray.{reduce_1}               | -42.95% | ±0.00%
> aiken/primitive/bytearray.{reduce_2}               | -44.60% | ±0.00%
> aiken/primitive/bytearray.{to_string_1}            | -69.56% | ±0.00%
> aiken/primitive/bytearray.{to_string_2}            | -53.54% | ±0.00%
> aiken/primitive/bytearray.{to_hex_1}               | -48.15% | ±0.00%
> aiken/primitive/bytearray.{to_hex_2}               | -48.15% | ±0.00%
> aiken/primitive/int.{from_utf8_1}                  | -37.06% | ±0.00%
> aiken/primitive/int.{from_utf8_2}                  | -33.40% | ±0.00%
> aiken/primitive/int.{from_utf8_3}                  | -37.06% | ±0.00%
> aiken/primitive/int.{from_utf8_4}                  | -32.78% | ±0.00%
> aiken/primitive/int.{from_utf8_5}                  | -32.05% | ±0.00%
> aiken/primitive/int.{from_utf8_6}                  | -31.36% | ±0.00%
> aiken/primitive/string.{from_bytearray_1}          | -69.56% | ±0.00%
> aiken/primitive/string.{from_bytearray_2}          | -53.54% | ±0.00%
> aiken/primitive/string.{from_bytearray_3}          | -53.54% | ±0.00%
> aiken/primitive/string.{from_int_1}                | -40.54% | -7.05%
> aiken/primitive/string.{from_int_2}                | -45.93% | -5.30%
> aiken/primitive/string.{from_int_3}                | -47.62% | -4.35%
> aiken/primitive/string.{from_int_4}                | -48.58% | -3.69%
> aiken/primitive/string.{concat_1}                  | -92.30% | -80.10%
> aiken/primitive/string.{concat_2}                  | -97.34% | -85.87%
> aiken/primitive/string.{concat_3}                  | -98.67% | -80.35%
> aiken/primitive/string.{join_1}                    | -42.87% | ±0.00%
> aiken/primitive/string.{join_2}                    | -37.65% | ±0.00%
> aiken/primitive/string.{to_bytearray_1}            | -62.36% | ±0.00%
> aiken/primitive/string.{to_bytearray_2}            | -41.62% | ±0.00%
> aiken/primitive/string.{to_bytearray_3}            | -41.62% | ±0.00%
> cardano/assets.{from_asset_list_1}                 | -20.51% | ±0.00%
> cardano/assets.{from_asset_list_2}                 | -10.09% | ±0.00%
> cardano/assets.{from_asset_list_3}                 | -12.21% | ±0.00%
> cardano/assets.{from_asset_list_4}                 | -16.22% | ±0.00%
> cardano/assets.{from_asset_list_5}                 | -14.60% | ±0.00%
> cardano/assets.{from_asset_list_6}                 | -20.97% | ±0.00%
> cardano/assets.{from_asset_list_7}                 | -20.25% | ±0.00%
> cardano/assets.{from_asset_list_8}                 | -14.51% | ±0.00%
> cardano/assets.{from_asset_list_9}                 | -16.07% | ±0.00%
> cardano/assets.{add_1}                             | -27.84% | ±0.00%
> cardano/assets.{add_2}                             | -27.56% | -0.54%
> cardano/assets.{add_3}                             | -26.39% | ±0.00%
> cardano/assets.{add_4}                             | -29.75% | -10.41%
> cardano/assets.{add_5}                             | -27.80% | ±0.00%
> cardano/assets.{merge_1}                           | -26.02% | ±0.00%
> cardano/assets.{merge_2}                           | -19.60% | ±0.00%
> cardano/assets.{merge_3}                           | -23.80% | ±0.00%
> cardano/assets.{merge_4}                           | -25.92% | ±0.00%
> cardano/assets.{merge_5}                           | -27.61% | -1.98%
> cardano/assets.{without_lovelace_1}                | -28.00% | -2.24%
> cardano/assets.{without_lovelace_2}                | -27.49% | ±0.00%
> cardano/assets.{without_lovelace_3}                | -23.40% | -0.34%
> cardano/assets.{flatten_with_1}                    | -21.10% | ±0.00%
> cardano/assets.{flatten_with_2}                    | -22.77% | ±0.00%
> cardano/assets.{reduce_1}                          | -24.31% | ±0.00%
> cardano/assets.{reduce_2}                          | -20.89% | ±0.00%
> cardano/assets.{reduce_3}                          | -36.21% | ±0.00%
> </details>

### Added

- New modules covering Conway-related features (i.e. governance)
  - [`cardano/governance`](https://aiken-lang.github.io/stdlib/cardano/governance.html)
  - [`cardano/governance/protocol_parameters`](https://aiken-lang.github.io/stdlib/cardano/governance/protocol_parameters.html)

- New primitives in `aiken/collection/pairs`:
  - [`insert_by_ascending_key`](https://aiken-lang.github.io/stdlib/aiken/collection/pairs.html#insert_by_ascending_key)
  - [`repsert_by_ascending_key`](https://aiken-lang.github.io/stdlib/aiken/collection/pairs.html#repsert_by_ascending_key)

- New primitives in `aiken/crypto`:
  - [`blake2b_224`](https://aiken-lang.github.io/stdlib/aiken/crypto.html#blake2b_224)
  - [`keccak_256`](https://aiken-lang.github.io/stdlib/aiken/crypto.html#keccak_256)

- New primitives in `aiken/math`:
  - [`log2`](https://aiken-lang.github.io/stdlib/aiken/math.html#log2)

- New primitives in `aiken/primitive/bytearray`:
  - [`at`](https://aiken-lang.github.io/stdlib/aiken/primitive/bytearray.html#at)
  - [`from_int_big_endian`](https://aiken-lang.github.io/stdlib/aiken/primitive/bytearray.html#from_int_big_endian)
  - [`from_int_little_endian`](https://aiken-lang.github.io/stdlib/aiken/primitive/bytearray.html#from_int_little_endian)
  - [`to_int_big_endian`](https://aiken-lang.github.io/stdlib/aiken/primitive/bytearray.html#to_int_big_endian)
  - [`to_int_little_endian`](https://aiken-lang.github.io/stdlib/aiken/primitive/bytearray.html#to_int_little_endian)

- New primitives in `aiken/primitive/int`:
  - [`from_bytearray_big_endian`](https://aiken-lang.github.io/stdlib/aiken/primitive/int.html#from_bytearray_big_endian)
  - [`from_bytearray_little_endian`](https://aiken-lang.github.io/stdlib/aiken/primitive/int.html#from_bytearray_little_endian)

- New primitives in `aiken/crypto`:
  - [`verify_ecdsa_signature`](https://aiken-lang.github.io/stdlib/cardano/credential.html#verify_ecdsa_signature)
  - [`verify_schnorr_signature`](https://aiken-lang.github.io/stdlib/cardano/credential.html#verify_schnorr_signature)

### Changed

- Few modules have been relocated and better organized:
  - `aiken/hash` -> [`aiken/crypto`](https://aiken-lang.github.io/stdlib/aiken/crypto.html)
  - **collections**
    - `aiken/dict` -> [`aiken/collection/dict`](https://aiken-lang.github.io/stdlib/aiken/collection/dict.html)
    - `aiken/list` -> [`aiken/collection/list`](https://aiken-lang.github.io/stdlib/aiken/collection/list.html)
    - `aiken/pairs` -> [`aiken/collection/pairs`](https://aiken-lang.github.io/stdlib/aiken/collection/pairs.html)
  - **primitive**
    - `aiken/bytearray` -> [`aiken/primitive/bytearray`](https://aiken-lang.github.io/stdlib/aiken/primitive/bytearray.html)
    - `aiken/int` -> [`aiken/primitive/int`](https://aiken-lang.github.io/stdlib/aiken/primitive/int.html)
    - `aiken/string` -> [`aiken/primitive/string`](https://aiken-lang.github.io/stdlib/aiken/primitive/string.html)
  - **cardano**
    - `aiken/transaction` -> [`cardano/transaction`](https://aiken-lang.github.io/stdlib/cardano/transaction.html)
    - `aiken/transaction/certificate` -> [`cardano/certificate`](https://aiken-lang.github.io/stdlib/cardano/certificate.html)
    - `aiken/transaction/credential` -> [`cardano/address`](https://aiken-lang.github.io/stdlib/cardano/address.html) & `aiken/crypto`
    - `aiken/transaction/value` -> [`cardano/assets`](https://aiken-lang.github.io/stdlib/cardano/assets.html)

- Several zero-argument functions have been turned into top-level constants
  - `aiken/dict.new()` -> [`aiken/collection/dict.empty`](https://aiken-lang.github.io/stdlib/aiken/collection/dict.html#empty)
  - `aiken/interval.empty()` -> [`aiken/interval.empty`](https://aiken-lang.github.io/stdlib/aiken/interval.html#empty)
  - `aiken/interval.everything()` -> [`aiken/interval.everything`](https://aiken-lang.github.io/stdlib/aiken/interval.html#everything)
  - `aiken/math/rational.zero()` -> [`aiken/math/rational.zero`](https://aiken-lang.github.io/stdlib/aiken/math/rational.html#zero)
  - `aiken/transaction/value.zero()` -> [`cardano/assets.zero`](https://aiken-lang.github.io/stdlib/cardano/assets.html#zero)

- The `Transaction` type from [`cardano/transaction`](https://aiken-lang.github.io/stdlib/cardano/transaction.html) (originally `aiken/transaction`) has been greatly reworked to match the new transaction format in Plutus V3.

- The `ScriptContext` type has split from `cardano/transaction` (originally `aiken/transaction`) and moved into its own module [`cardano/script_context`](https://aiken-lang.github.io/stdlib/cardano/script_context.html) and adjusted to its new form as per Plutus V3.

- The constructors of [`Credential`](https://aiken-lang.github.io/stdlib/cardano/address.html#credential) have been renamed from `VerificationKeyCredential` and `ScriptCredential` into `VerificationKey` and `Script` respectively.

- The function `remove_all`, `remove_first` and `remove_last` from [`aiken/collection/pairs`](https://aiken-lang.github.io/stdlib/aiken/collection/pairs.html) (originally `aiken/pairs`) have been renamed to `delete_all`, `delete_first` and `delete_last` respectively.

- The function `verify_signature` from [`aiken/crypto`](https://aiken-lang.github.io/stdlib/aiken/crypto.html) (originally `aiken/credential`) has been renamed to `verify_ed25519_signature`.

### Removed

- The module `aiken/time`. The `PosixTime` alias is no longer used anywhere.

- `MintedValue` (from `aiken/transaction/value` originally) and its associated functions are no longer needed and, therefore, gone.

## v1.9.0 - 2024-05-24

### Added

- A new module [`aiken/pairs`](https://aiken-lang.github.io/stdlib/aiken/pairs.html) to work with associative lists (a.k.a. `Pairs`).

### Changed

- **BREAKING-CHANGE**<br/>
  Specialized all `Dict`'s key to `ByteArray`, and thus remove the need for passing an extra comparison function in many functions. `Dict` are however still specialized with a phantom type for keys.

- **BREAKING-CHANGE**<br/>
  Few functions from `Dict` have been renamed for consistency:
  - `from_list` -> `from_pairs`
  - `from_ascending_list` -> `from_ascending_pairs`
  - `to_list` -> `to_pairs`

### Removed

N/A

## v1.8.0 - 2024-03-28

### Added

- [`value.reduce`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#reduce) to efficiently fold over a value and its elements.

- [`value.from_asset_list`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#from_asset_list) to turn an asset list into a Value while enforcing invariants expected of `Value`.

- [`math.is_sqrt`](https://aiken-lang.github.io/stdlib/aiken/math.html#is_sqrt) as a more efficient alternative to `sqrt`.

### Changed

- Disclaimers in documentation to [`bytearray.to_string`](https://aiken-lang.github.io/stdlib/aiken/bytearray.html#to_string) and [`string.from_bytearray`](https://aiken-lang.github.io/stdlib/aiken/string.html#from_bytearray) regarding UTF-8 encoding.

### Removed

N/A

## v1.7.0 - 2023-11-07

### Added

- [`list.index_of`](https://aiken-lang.github.io/stdlib/aiken/list.html#index_of): For getting a values index in a list.
- [`transaction.placeholder`](https://aiken-lang.github.io/stdlib/aiken/transaction.html#placeholder): For constructing test transactions.
- [`transaction.value.is_zero`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#is_zero): For checking whether a value is null.

### Changed

- [`value.to_minted_value`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#to_minted_value) now correctly preserves the invariant of `MintedValue`: it always contain a null quantity of Ada.

### Removed

N/A

## v1.6.0 - 2023-09-08

### Added

- [`math.pow2`](https://aiken-lang.github.io/stdlib/aiken/math.html#pow2): For faster exponentions for powers of two.
- [`bytearray.test_bit`](https://aiken-lang.github.io/stdlib/aiken/bytearray.html#test_bit): For testing if a bit is set in a bytearray (MSB).

## v1.5.0 - 2023-08-16

### Removed

- retired `list.and` and `list.or` because of the new keywords for logical op chaining.

## v1.4.0 - 2023-07-21

### Changed

- Fixed missing null-check on `value.add`. Adding a null quantity of token is now correctly a no-op.

## v1.3.0 - 2023-06-30

### Added

- [`math.sqrt`](https://aiken-lang.github.io/stdlib/aiken/math.html#sqrt): For calculating integer square roots using a quadratically convergent method.
- [`math/rational.numerator`](https://aiken-lang.github.io/stdlib/aiken/math/rational.html#numerator) & [`math/rational.denominator`](https://aiken-lang.github.io/stdlib/aiken/math/rational.html#numerator): For accessing parts of a rational value.
- [`math/rational.arithmetic_mean`](https://aiken-lang.github.io/stdlib/aiken/math/rational.html#arithmetic_mean): For computing [arithmetic mean](https://en.wikipedia.org/wiki/Arithmetic_mean) of rational values.
- [`math/rational.geometric_mean`](https://aiken-lang.github.io/stdlib/aiken/math/rational.html#geometric_mean): For computing [geometric mean](https://en.wikipedia.org/wiki/Geometric_mean) of two rational values.

### Changed

- Clear empty asset lists in [`Value`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#Value) on various operations. Before that fix, it could happen that removing all assets from a given policy would lead to an empty dictionnary of assets still be present in the `Value`.

## v1.2.0 - 2023-06-17

### Added

- [`transaction/value.MintedValue`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#MintedValue)
- [`transaction/value.from_minted_value`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#from_minted_value): Convert from `MintedValue` to `Value`
- [`transaction/value.to_minted_value`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#to_minted_value): Convert from `Value` to `MintedValue`
- [`transaction/bytearray.to_hex`](https://aiken-lang.github.io/stdlib/aiken/bytearray.html#to_hex): Convert a `ByteArray` to a hex encoded `String`
- [`math/rational`](https://aiken-lang.github.io/stdlib/aiken/math/rational.html): Working with rational numbers.
  - [x] `abs`
  - [x] `add`
  - [x] `ceil`
  - [x] `compare`
  - [x] `compare_with`
  - [x] `div`
  - [x] `floor`
  - [x] `from_int`
  - [x] `mul`
  - [x] `negate`
  - [x] `new`
  - [x] `proper_fraction`
  - [x] `reciprocal`
  - [x] `reduce`
  - [x] `round`
  - [x] `round_even`
  - [x] `sub`
  - [x] `truncate`
  - [x] `zero`

### Removed

- module `MintedValue` was merged with `Value`

## v1.1.0 - 2023-06-06

### Added

- [`list.count`](https://aiken-lang.github.io/stdlib/aiken/list.html#count): Count how many items in the list satisfy the given predicate.

- [`int.from_utf8`](https://aiken-lang.github.io/stdlib/aiken/int.html#from_utf8): Parse an integer from a utf-8 encoded `ByteArray`, when possible.

- [`dict.foldl`](https://aiken-lang.github.io/stdlib/aiken/dict.html#foldl) & [`dict.foldr`](https://aiken-lang.github.io/stdlib/aiken/dict.html#foldr): for left and right folds over dictionnary elements in ascending key order.

- [`dict.insert_with`](https://aiken-lang.github.io/stdlib/aiken/dict.html#insert_with): Insert a value in the dictionary at a given key. When the key already exist, the provided merge function is called.

- [`transaction/value.add`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#add): Add a (positive or negative) quantity of a single token to a value. This is more efficient than `merge` for a single asset.

- [`transaction/value.to_dict`](https://aiken-lang.github.io/stdlib/aiken/transaction/value.html#to_dict): Convert a `Value` into a dictionnary of dictionnaries.

- A new module [`transaction/minted_value`](https://aiken-lang.github.io/stdlib/aiken/transaction/minted_value.html): This is used exclusively for representing values present in the `mint` field of transactions. This allows to simplify some of the implementation for `Value` which no longer needs to handle the special case where null-quantity tokens would be present. It isn't possible to construct `MintedValue` by hand, they come from the script context entirely and are 'read-only'.

- More documentation for `dict` and `interval` modules.

### Changed

> **Warning**
>
> Most of those changes are breaking-changes. Though, given we're still in an
> alpha state, only the `minor` component is bumped from the version number.
> Please forgive us.

- Rework `list.{foldl, foldr, reduce, indexed_foldr}`, `dict.{fold}`, `bytearray.{foldl, foldr, reduce}` to take the iterator as last argument. For example:

  ```
  fn foldl(self: List<a>, with: fn(a, b) -> b, zero: b) -> b

  ↓ becomes

  fn foldl(self: List<a>, zero: b, with: fn(a, b) -> b) -> b
  ```

- Fixed implementation of `bytearray.slice`; `slice` would otherwise behave as if the second argument were an offset.

- Rename `transaction/value.add` into `transaction/value.merge`.

- Swap arguments of the merge function in `dict.union_with`; the first value received now corresponds to the value already present in the dictionnary.

- Fixed various examples from the documentation

### Removed

- Removed `dict.fold`; replaced with `dict.foldl` and `dict.foldr` to remove ambiguity.

## v1.0.0 - 2023-04-13

### Added

N/A

### Changed

N/A

### Removed

N/A
