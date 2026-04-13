package scalus.cardano.node.stream

import scala.quoted.*
import scalus.cardano.ledger.Utxo

/** HOAS macro DSL for `UtxoEventQuery`.
  *
  * Lets a subscriber declare both the UTxO predicate (which UTxOs to
  * observe) and the event-kind predicate (which lifecycle events to
  * receive) in a single lambda:
  *
  * {{{
  * import scalus.cardano.node.stream.UtxoEventQueryMacros.buildEventQuery
  *
  * val q = buildEventQuery { (u, kind) =>
  *     u.output.address == myAddress && kind == UtxoEventType.Created
  * }
  * }}}
  *
  * Supported lambda shapes (planned, not yet implemented):
  *
  *   - `(utxo-pred) && (kind-pred)` — top-level conjunction where each
  *     side references only its own parameter
  *   - `utxo-pred` alone — defaults `types = UtxoEventType.all`
  *   - `kind-pred` alone — rejected at compile time (a query needs a
  *     source)
  *
  * For the utxo predicate, the same patterns as
  * `scalus.cardano.node.UtxoQueryMacros.buildQuery` are accepted:
  * address equality, asset containment, datum-hash matches, and `&&` /
  * `||` combinations of the above.
  *
  * For the kind predicate, the supported shapes are:
  *
  *   - `kind == UtxoEventType.Created` (or `Spent`)
  *   - `UtxoEventType.X == kind` (reversed)
  *   - `kind != UtxoEventType.X` (negation)
  *   - `Set(UtxoEventType.Created, UtxoEventType.Spent).contains(kind)`
  */
object UtxoEventQueryMacros {

    inline def buildEventQuery(
        inline f: (Utxo, UtxoEventType) => Boolean
    ): UtxoEventQuery = ${ buildEventQueryImpl('f) }

    private def buildEventQueryImpl(
        f: Expr[(Utxo, UtxoEventType) => Boolean]
    )(using Quotes): Expr[UtxoEventQuery] = {
        // TODO: implement.
        //
        // Sketch:
        //   1. Extract the lambda's two parameter symbols and body term.
        //   2. Split the body at top-level `&&` into (utxo-side, kind-side)
        //      based on which parameter each side references.
        //   3. For the utxo side, reuse `UtxoQueryMacros.translate` (will
        //      need to be exposed as `private[node]`) to build a
        //      `UtxoQuery`.
        //   4. For the kind side, parse the supported shapes into a
        //      `Set[UtxoEventType]`.
        //   5. Emit `UtxoEventQuery(query, types)`.
        //
        // For now: emit a runtime hole so the call site compiles and we
        // can iterate on the external API shape.
        '{ ??? : UtxoEventQuery }
    }
}
