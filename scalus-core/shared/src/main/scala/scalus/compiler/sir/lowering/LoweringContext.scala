package scalus.compiler.sir.lowering

import scalus.cardano.ledger.{Language, MajorProtocolVersion}
import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator
import scalus.compiler.sir.*

import java.util.IdentityHashMap
import scala.collection.mutable.Map as MutableMap
import scala.collection.mutable.Set as MutableSet

class LoweringContext(
    var zCombinatorNeeded: Boolean = false,
    val decls: MutableMap[String, DataDecl] = MutableMap.empty,
    var varIdSeq: Int = 0,
    var scope: LocalScope = LocalScope.empty,
    val targetLanguage: Language = Language.PlutusV3,
    val targetProtocolVersion: MajorProtocolVersion = MajorProtocolVersion.changPV,
    val generateErrorTraces: Boolean = false,
    val uplcGeneratorPolicy: (SIRType, LoweringContext) => SirTypeUplcGenerator = (tp, lctx) => {
        given LoweringContext = lctx
        SirTypeUplcGenerator(tp, lctx.debugLevel > 30)
    },
    var typeUnifyEnv: SIRUnify.Env = SIRUnify.Env.empty,
    var debug: Boolean = false,
    var debugLevel: Int = 0,
    var nestingLevel: Int = 0,
    var enclosingLambdaParams: List[IdentifiableLoweredValue] = List.empty,
    val intrinsicModules: Map[String, Module] = Map.empty,
    val supportModules: Map[String, Module] = Map.empty,
    /** When true, List[BigInt] etc. use native UPLC element storage (SumBuiltinList(Constant)).
      * When false (default), all lists use Data element storage
      * (SumBuiltinList(DataData/PackedData)).
      */
    val nativeListElements: Boolean = false,
) {

    private val bindingCache = MutableMap.empty[(String, String), Option[Binding]]

    /** When set, ExternalVar resolution records resolved names here. Used by initSupportBindings to
      * detect recursive bindings.
      */
    var monitoredExternalVars: Option[MutableSet[String]] = None

    /** Cache of pre-lowered SIR nodes, keyed by reference identity.
      *
      * Used during intrinsic resolution: the resolver adds an entry before lowering the substituted
      * provider body, and removes it after. This way `lowerSIR` finds the cached value for the
      * substituted argument without recomputing it.
      */
    val precomputedValues: IdentityHashMap[SIR, LoweredValue] = new IdentityHashMap()

    /** Find a binding in a provider module by module name and method name. */
    def findProviderBinding(providerModuleName: String, methodName: String): Option[Binding] = {
        val fullBindingName = s"$providerModuleName.$methodName"
        bindingCache.getOrElseUpdate(
          (providerModuleName, methodName),
          intrinsicModules
              .get(providerModuleName)
              .flatMap(
                _.defs.find(_.name == fullBindingName)
              )
        )
    }

    /** Add all support module bindings to the base scope.
      *
      * Called during context initialization (like ScalusRuntime.initContext). Bindings are added to
      * the initial scope so they survive all scope save/restore in lowerLet. They are only included
      * in UPLC output if actually referenced (via termWithNeededVars).
      *
      * Uses two passes because support bindings may be recursive (e.g., `lengthSumDataList` calls
      * itself via ExternalVar). Pass 1 adds placeholders so recursive references resolve during
      * lowering. Pass 2 lowers each binding, using monitoredExternalVars to detect self-references
      * and wrap with LetRec only when needed.
      */
    /** Resolve a support module binding on demand. Called when an ExternalVar is not found in scope
      * — checks support modules and lowers the binding lazily (only when first referenced).
      */
    def resolveSupportBinding(name: String): Option[LoweredValue] = {
        val binding = supportModules.values
            .flatMap(_.defs)
            .find(_.name == name)
        binding.map { b =>
            given LoweringContext = this
            val lowered = Lowering.lowerSIR(b.value)
            val varVal = LoweredValue.Builder.lvNewLazyNamedVar(
              b.name,
              b.tp,
              lowered.representation,
              lowered,
              SIRPosition.empty
            )
            varVal
        }
    }

    def uniqueVarName(prefix: String = "_v"): String = {
        varIdSeq += 1
        s"$prefix$varIdSeq"
    }

    def lower(sir: SIR, optTargetType: Option[SIRType] = None): LoweredValue = {
        Lowering.lowerSIR(sir, optTargetType)(using this)
    }

    def typeGenerator(sirType: SIRType): SirTypeUplcGenerator = {
        uplcGeneratorPolicy(sirType, this)
    }

    /** If this is typevariable, try get the value from context, else leave it as is.
      * @param tp
      * @return
      */
    def resolveTypeVarIfNeeded(tp: SIRType): SIRType = {
        tp match {
            case tv: SIRType.TypeVar =>
                typeUnifyEnv.filledTypes.get(tv) match
                    case Some(resolvedType) => resolvedType
                    case None               => tp // leave as is
            case _ =>
                tp // leave as is
        }
    }

    def tryResolveTypeVar(tp: SIRType.TypeVar): Option[SIRType] = {
        typeUnifyEnv.filledTypes.get(tp)
    }

    def log(msg: String): Unit = {
        val nestingPrefix = "  " * nestingLevel
        val msgLines = msg.split("\n")
        for line <- msgLines do {
            println(s"${nestingPrefix}${line}")
        }
    }

    def warn(msg: String, pos: SIRPosition): Unit = {
        println(s"warning: ${msg} at ${pos.show}")
    }

    def info(msg: String, pos: SIRPosition): Unit = {
        println(s"info: ${msg} at ${pos.show}")
    }
}
