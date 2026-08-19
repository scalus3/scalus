package scalus.compiler.intrinsics

import scalus.cardano.onchain.plutus.v1.Value
import scalus.compiler.Compile
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.compiler.sir.lowering.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.internal.UniversalDataConversion
import scalus.uplc.builtin.{ByteString, Data}

/** PV11 (vanRossem) intrinsic lowerings for `plutus.v1.Value` operations via the CIP-153
  * MaryEraValue builtins. Registered in `IntrinsicResolver.registry` with minPV = 11; at PV10 the
  * linked SIR bodies are used unchanged.
  *
  * Semantics note: `unValueData` requires canonical form (strictly ascending keys, no zero amounts,
  * no empty inner maps, keys <= 32 bytes, amounts within +-(2^127)) and fails otherwise;
  * `unionValue`/`scaleValue` fail on 128-bit overflow. See the design doc
  * `docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md`.
  */
@Compile
object ValueIntrinsicsV11 {

    // Why `typeProxy[Data](v)` is a sound, zero-cost cast: `Value` is
    // `@UplcRepr(ProductCaseOneElement)` around a single `SortedMap` field that is itself
    // `@UplcRepr(PackedDataMap)`, so a `Value`'s runtime representation is
    // `OneElementWrapper(PackedDataMap)` - the very bytes of the Data map the CIP-153
    // `unValueData` builtin expects. The proxy only relabels the SIR type; no conversion code
    // is emitted.
    def quantityOf(v: Value, cs: ByteString, tn: ByteString): BigInt =
        lookupCoin(cs, tn, unValueData(typeProxy[Data](v)))

    // The `Value`-returning bodies re-enter `Value` through `UniversalDataConversion.fromData`
    // (the same marker `MapIntrinsics` uses for `toData`), NOT through
    // `typeProxyRepr[Value](..., ProductCaseOneElement)` - `interpretReprSIR`'s
    // `resolveReprTagName` only knows leaf / `SumBuiltinList` tags and throws `unknown ReprTag`
    // on that one - and not through a plain `typeProxy[Value]` either: that keeps the builtin's
    // `DataData` label, and `DataData -> OneElementWrapper(PackedDataMap)` is not a supported
    // conversion. `Lowering.lowerFromData` turns this marker into a pure relabel at
    // `defaultDataRepresentation(Value)` = `OneElementWrapper(PackedDataMap)`, which is exactly
    // the shape of the Data map `valueData` returns - so it emits no code and already matches
    // `ValueReprRules.valueOut` below. The surface `Data.fromData[Value]` cannot be used here:
    // it resolves to the `Value$.valueFromData` given, an external binding that is not in scope
    // when the provider body is substituted.
    def plus(a: Value, b: Value): Value =
        UniversalDataConversion.fromData[Value](
          valueData(
            unionValue(unValueData(typeProxy[Data](a)), unValueData(typeProxy[Data](b)))
          )
        )

    def minus(a: Value, b: Value): Value =
        UniversalDataConversion.fromData[Value](
          valueData(
            unionValue(
              unValueData(typeProxy[Data](a)),
              scaleValue(BigInt(-1), unValueData(typeProxy[Data](b)))
            )
          )
        )

    def multiply(v: Value, factor: BigInt): Value =
        UniversalDataConversion.fromData[Value](
          valueData(scaleValue(factor, unValueData(typeProxy[Data](v))))
        )

    def negate(v: Value): Value =
        UniversalDataConversion.fromData[Value](
          valueData(scaleValue(BigInt(-1), unValueData(typeProxy[Data](v))))
        )

    def containsAtLeast(v: Value, other: Value): Boolean =
        valueContains(unValueData(typeProxy[Data](v)), unValueData(typeProxy[Data](other)))
}

object ValueReprRules {

    private val defaultOut: ReprRule = (outTp, _, lctx) =>
        typegens.SirTypeUplcGenerator.defaultRepresentation(outTp)(using lctx)

    /** Output representation of the `Value`-returning intrinsics: `Value`'s own default,
      * `OneElementWrapper(PackedDataMap)`, which is exactly the Data map the `valueData` builtin
      * produces.
      */
    private val valueOut: ReprRule = (_, _, _) =>
        ProductCaseClassRepresentation.OneElementWrapper(
          ProductCaseClassRepresentation.PackedDataMap
        )

    val rules: Map[String, ReprRule] = Map(
      "quantityOf" -> defaultOut,
      "plus" -> valueOut,
      "minus" -> valueOut,
      "multiply" -> valueOut,
      "negate" -> valueOut,
      "containsAtLeast" -> defaultOut
    )
}
