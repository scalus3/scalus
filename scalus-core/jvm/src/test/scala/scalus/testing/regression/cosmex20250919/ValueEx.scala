package scalus.testing.regression.cosmex20250919

import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.*

@Compile
object ValueEx {

    def unionVal(
        l: Value,
        r: Value
    ): SortedMap[PolicyId, SortedMap[TokenName, These[BigInt, BigInt]]] = {
        val combined: SortedMap[
          PolicyId,
          These[SortedMap[TokenName, BigInt], SortedMap[TokenName, BigInt]]
        ] = SortedMap.union(l.toSortedMap, r.toSortedMap)
        combined.mapValues {
            case These.These(v1, v2) => SortedMap.union(v1, v2)
            case These.This(v1) =>
                v1.mapValues {
                    These.This(_)
                }
            case These.That(v2) =>
                v2.mapValues {
                    These.That(_)
                }
        }
    }

    def unionWith(op: (BigInt, BigInt) => BigInt)(a: Value, b: Value): Value = {
        val combined = unionVal(a, b)
        val unThese: These[BigInt, BigInt] => BigInt = {
            case These.These(v1, v2) => op(v1, v2)
            case These.This(v1)      => op(v1, 0)
            case These.That(v2)      => op(0, v2)
        }

        Value.fromList(
          // SortedMap.unsafeFromList(
          combined.toList.filterMap { pair =>
              val tokens = pair._2.toList.filterMap { case (tn, v) =>
                  val value = unThese(v)
                  if value !== BigInt(0) then Option.Some((tn, value))
                  else Option.None
              }

              // if tokens.nonEmpty then Option.Some((pair._1, SortedMap.unsafeFromList(tokens)))
              if tokens.nonEmpty then Option.Some((pair._1, tokens))
              else Option.None
          }
          // )
        )
    }

}
