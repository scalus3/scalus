package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.{AssocMap, SortedMap}
import scalus.compiler.UplcRepresentation
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.compiler.sir.SIRType
import scalus.compiler.sir.lowering.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.internal.UniversalDataConversion

// ---------------------------------------------------------------------------
//  @Compile provider objects for Map factory/zero-arg intrinsics
// ---------------------------------------------------------------------------

@Compile
object SortedMapIntrinsics {

    /** Factory: singleton — builtins only, no scope dependencies */
    def singleton[K, V](key: K, value: V): SortedMap[K, V] =
        typeProxyRepr[SortedMap[K, V]](
          mapData(
            mkCons(
              mkPairData(
                UniversalDataConversion.toData(key),
                UniversalDataConversion.toData(value)
              ),
              mkNilPairData()
            )
          ),
          UplcRepresentation.PackedDataMap
        )

    /** Zero-arg: empty — directly produces mapData(mkNilPairData()) */
    def empty[K, V]: SortedMap[K, V] =
        typeProxyRepr[SortedMap[K, V]](mapData(mkNilPairData()), UplcRepresentation.PackedDataMap)
}

@Compile
object AssocMapIntrinsics {

    /** Factory: singleton — builtins only, no scope dependencies */
    def singleton[K, V](key: K, value: V): AssocMap[K, V] =
        typeProxyRepr[AssocMap[K, V]](
          mapData(
            mkCons(
              mkPairData(
                UniversalDataConversion.toData(key),
                UniversalDataConversion.toData(value)
              ),
              mkNilPairData()
            )
          ),
          UplcRepresentation.PackedDataMap
        )

    /** Zero-arg: empty — directly produces mapData(mkNilPairData()) */
    def empty[K, V]: AssocMap[K, V] =
        typeProxyRepr[AssocMap[K, V]](mapData(mkNilPairData()), UplcRepresentation.PackedDataMap)
}

// ---------------------------------------------------------------------------
//  Repr rules for Map intrinsics
// ---------------------------------------------------------------------------

object MapReprRules {

    /** singleton: (K, V) -> Map[K, V] — curried, so inner apply returns Fun(V, Map[K,V]) */
    val singletonRule: ReprRule = (outTp, _, lctx) =>
        given LoweringContext = lctx
        outTp match
            case SIRType.Fun(argTp, _) =>
                val tg = lctx.typeGenerator(argTp)
                val inRepr =
                    if tg.canBeConvertedToData(argTp) then tg.defaultDataRepresentation(argTp)
                    else tg.defaultRepresentation(argTp)
                LambdaRepresentation(
                  outTp,
                  InOutRepresentationPair(inRepr, ProductCaseClassRepresentation.PackedDataMap)
                )
            case _ => ProductCaseClassRepresentation.PackedDataMap

    /** Convert first arg (key) to default data representation — map keys are always Data. */
    val singletonArgConvert: ArgReprConvertRule = (argTp, _, lctx) =>
        given LoweringContext = lctx
        val tg = lctx.typeGenerator(argTp)
        if tg.canBeConvertedToData(argTp) then Some(tg.defaultDataRepresentation(argTp))
        else None

    /** empty: Unit -> Map[K, V] — handled through Apply-based factory intrinsic */
    val emptyReprRule: ReprRule = (_, _, _) => ProductCaseClassRepresentation.PackedDataMap

    val factoryRules: Map[String, ReprRule] = Map(
      "singleton" -> singletonRule,
      "empty" -> emptyReprRule
    )

    val factoryArgConvertRules: Map[String, ArgReprConvertRule] = Map(
      "singleton" -> singletonArgConvert
    )
}
