package scalus.cardano.ledger.value

/** A MultiAsset is, semantically, a collection of associations [[PolicyId]]/[[AssetName]]/Quantity.
  *
  * The interpretation of this type is as follows:
  *   - It can be thought of loosely as a `Map[PolicyId, Map[AssetName, quantity]]` with additional
  *     invariants.
  *     - We term the `Map` keyed by `AssetName` the "Inner Map".
  *   - The `quantity` can be a bounded integer, unbounded integer, or rational number (see package
  *     docs for [[scalus.cardano.ledger.coin]].
  *   - In both the inner and outer maps (the maps keyed by `AssetName` and `PolicyId`,
  *     respectively), "empty" values are removed from the map. This means that an `AssetName` with
  *     a quantity of zero will never appear in the inner map, and a `PolicyId` corresponding to an
  *     empty `Map[AssetName, quantity]` will never appear in the outer map.
  *     - Thus, if a lookup for a specific `(PolicyId, AssetName)` fails, it can be semantically
  *       interpreted as "zero".
  *     - This allows the underlying map to have an algebraic interpretation, depending on the
  *       quantity. Specifically, [[MultiAsset.Unbounded]] is a [[CModule]], and
  *       [[MultiAsset.Fractional]] is a [[VectorSpace]]
  *   - Both maps are sorted lexicographically. This is in accordance with the plutus +
  *     `cardano-ledger` representation.
  *     - This means that both the inner and outer maps have a [[PartialOrder]] defined
  */
package object multiasset {}
