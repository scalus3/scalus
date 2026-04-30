package scalus.compiler.sir.lowering

import scalus.cardano.ledger.{Language, MajorProtocolVersion}
import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator
import scalus.compiler.sir.*

import scala.collection.mutable.Map as MutableMap

class LoweringContext(
    var zCombinatorNeeded: Boolean = false,
    val decls: MutableMap[String, DataDecl] = MutableMap.empty,
    var varIdSeq: Int = 0,
    var scope: LocalScope = LocalScope.empty,
    val targetLanguage: Language = Language.PlutusV3,
    val targetProtocolVersion: MajorProtocolVersion = MajorProtocolVersion.changPV,
    val generateErrorTraces: Boolean = false,
    val warnListConversions: Boolean = false,
    val noWarn: Boolean = false,
    /** When true, unannotated `List[_]` / `Option[_]` types dispatch to
      * `SumCaseUplcConstrSirTypeGenerator` (native-Constr form) instead of the default
      * `SumBuiltinList` / `DataConstr` generator. Set by `IntrinsicResolver` for the duration of
      * lowering a dispatcher/support-op body that operates on native-Constr lists/options. Saved
      * and restored around each swap, mirroring `typeUnifyEnv` / `typeVarReprEnv`.
      */
    var inUplcConstrListScope: Boolean = false,
    var typeUnifyEnv: SIRUnify.Env = SIRUnify.Env.empty,
    /** Parallel to `typeUnifyEnv.filledTypes`, but tracks the concrete UPLC representation for each
      * abstract TypeVar at its call site. Populated from actual argument representations at
      * intrinsic resolution time (see `IntrinsicResolver.bindElementTypeVars`). Queried by
      * `TypeVarSirTypeGenerator.toRepresentation` and `makeResolvedProxy` so conversions of
      * TypeVar-typed values use the caller's real representation instead of the abstract default.
      * Same save/restore discipline as `typeUnifyEnv`.
      */
    var typeVarReprEnv: Map[SIRType.TypeVar, LoweredValueRepresentation] = Map.empty,
    var debug: Boolean = false,
    var debugLevel: Int = 0,
    var nestingLevel: Int = 0,
    var enclosingLambdaParams: List[IdentifiableLoweredValue] = List.empty,
    val intrinsicModules: Map[String, Module] = Map.empty,
    val supportModules: Map[String, Module] = Map.empty,
) {

    private val bindingCache = MutableMap.empty[(String, String), Option[Binding]]

    /** Annotation-keyed cache of pre-lowered values. Indexed by Int.
      *
      * Used by intrinsic resolution as an alternative to identity-based `precomputedValues`: the
      * resolver substitutes references to lambda parameters with `SIR.Var(name, type, anns +
      * "argCache" → Const(Integer(idx)))`. `lowerSIR` checks the annotation and returns
      * `argCache(idx)` directly. Survives `substituteVarAndTypes` walking that creates fresh
      * SIR.Var instances (annotations are preserved in the copy).
      */
    val argCache: MutableMap[Int, LoweredValue] = MutableMap.empty
    private var argCacheNextId: Int = 0
    def newArgCacheKey(): Int = {
        val id = argCacheNextId
        argCacheNextId += 1
        id
    }

    /** Cache for top-level recursive helpers (e.g. per-type `sumEq` functions emitted by
      * [[LoweringEq.generateSumUplcConstrEquals]]). Keyed by a stable type-fingerprint string. Each
      * entry is the recursive `IdentifiableLoweredValue` that subsequent uses can reference; the
      * corresponding rhs is registered in [[pendingTopLevelLetRecs]] and emitted as a let-rec
      * wrapping the lowered SIR root.
      */
    val cachedTopLevelHelpers
        : scala.collection.mutable.LinkedHashMap[String, IdentifiableLoweredValue] =
        scala.collection.mutable.LinkedHashMap.empty

    /** Diagnostic toggle: when true, [[lookupCachedHelper]] always returns `None`, forcing every
      * call site to materialize a fresh helper RHS instead of reusing a cached one. Used to bisect
      * Heisenbug failures whose root cause is a stale `cachedTopLevelHelpers` entry shared across
      * dispatches with subtly-different lowering env. Set via `-Dscalus.disable.helper.cache=1`.
      */
    private val disableHelperCache: Boolean =
        System.getProperty("scalus.disable.helper.cache") != null

    /** Cache lookup with a diagnostic disable knob. Call sites should use this instead of
      * `cachedTopLevelHelpers.get(...)` directly so the disable flag covers them uniformly.
      */
    def lookupCachedHelper(key: String): Option[IdentifiableLoweredValue] =
        if disableHelperCache then None
        else cachedTopLevelHelpers.get(key)

    /** Pending top-level let-rec bindings collected during lowering. After [[Lowering.lowerSIR]]
      * returns the lowered SIR root, the lowering driver wraps it with a chain of let-recs for each
      * entry. Entries are appended by innermost-completing helpers first (helper `+=`s AFTER any
      * transitively-triggered sub-helpers have completed their own `+=`), so with `foldRight`
      * wrapping, the first entry becomes outermost — inner helpers can see their outer
      * dependencies. NOT sound for mutually recursive sums; detect and reject those at
      * helper-construction time.
      */
    val pendingTopLevelLetRecs: scala.collection.mutable.ArrayBuffer[
      (IdentifiableLoweredValue, LoweredValue)
    ] = scala.collection.mutable.ArrayBuffer.empty

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

    def uniqueVarName(prefix: String = "_v"): String = {
        varIdSeq += 1
        s"$prefix$varIdSeq"
    }

    def lower(sir: SIR, optTargetType: Option[SIRType] = None): LoweredValue = {
        Lowering.lowerSIR(sir, optTargetType)(using this)
    }

    /** Return the UPLC type generator for `sirType`, based purely on the type (and its
      * annotations).
      *
      * Previously this consulted `inUplcConstrListScope` to route unannotated `List[_]` /
      * `Option[_]` into `SumCaseUplcConstrSirTypeGenerator` inside dispatcher/support-op bodies.
      * Now that every native-Constr intrinsic signature carries a type-level
      * `@UplcRepr(UplcConstr)` annotation (which `SIRTyper.sirTypeInEnv` propagates into the SIR
      * type), the routing is entirely annotation-driven and this function does not need the flag.
      *
      * `inUplcConstrListScope` survives only in `IntrinsicResolver.tryResolve` to select the
      * native-Constr provider binding when dispatching through a support module.
      */
    def typeGenerator(sirType: SIRType): SirTypeUplcGenerator = {
        given LoweringContext = this
        SirTypeUplcGenerator(sirType, debugLevel > 30)
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

    /** Produce a stable fingerprint of the parts of the enclosing call-site state that a cached
      * helper's RHS captures from `lctx`, restricted to TypeVars occurring in `tps`.
      *
      * Helpers in `cachedTopLevelHelpers` are built once per cache key and reused on subsequent
      * hits — but their RHS bodies often consult `typeVarReprEnv` and the `inUplcConstrListScope`
      * flag during construction (e.g. element-repr resolution inside an `lvMatchList` body). If the
      * cache key omits this state, the FIRST caller's env wins, and later callers under a different
      * env get a helper RHS that doesn't match their context. This is the alone-vs-combined
      * Heisenbug pattern documented in sessions 11-15: the same `(type, fromRepr, toRepr)` cache
      * key produces functionally-different helpers depending on first-miss timing.
      *
      * Including this fingerprint in the cache key forces helpers with different captured env state
      * to live as separate entries.
      */
    def captureFingerprint(tps: SIRType*): String = {
        val seen = new java.util.IdentityHashMap[SIRType.TypeProxy, Boolean]()
        val acc = scala.collection.mutable.LinkedHashSet[SIRType.TypeVar]()
        def collect(t: SIRType, bound: Set[SIRType.TypeVar]): Unit = t match
            case tv: SIRType.TypeVar =>
                if !bound.contains(tv) then acc += tv
            case SIRType.TypeLambda(ps, body) => collect(body, bound ++ ps)
            case SIRType.Fun(in, out)         => collect(in, bound); collect(out, bound)
            case SIRType.CaseClass(_, args, parent) =>
                args.foreach(collect(_, bound)); parent.foreach(collect(_, bound))
            case SIRType.SumCaseClass(_, args) =>
                args.foreach(collect(_, bound))
            case SIRType.Annotated(t1, _) => collect(t1, bound)
            case SIRType.TypeProxy(ref) =>
                if ref != null && !seen.containsKey(t) then
                    seen.put(t.asInstanceOf[SIRType.TypeProxy], true)
                    collect(ref, bound)
            case _ => // primitives, FreeUnificator, TypeNothing
        tps.foreach(collect(_, Set.empty))
        val freeTvs = acc.toList
        def tvId(tv: SIRType.TypeVar): String =
            s"${tv.name}#${tv.optId.getOrElse(0L)}"
        // For each free TypeVar in `tps`, capture two pieces of state:
        //   - resolution: `typeUnifyEnv.filledTypes.get(tv)` rendered with `showDebug` so
        //     resolved TypeVars include their IDs (avoids name collisions across distinct
        //     same-named TypeVars). The caller-provided `tps.show` does NOT capture this:
        //     `List[B]` rendering is the same whether B is resolved or not, so callers with
        //     `listType=List[B]` but different B-resolutions would otherwise share a helper.
        //   - abstract repr: `typeVarReprEnv.get(tv)` for unresolved TypeVars whose abstract
        //     representation affects helper RHS construction.
        // Plus `inUplcConstrListScope` (binary flag).
        //
        // Restricted to free TypeVars in `tps` to avoid over-specializing on irrelevant
        // env bindings — including the entire `filledTypes` over-keyed and broke valid
        // helper sharing across structurally-equivalent calls.
        val bindings = freeTvs.flatMap { tv =>
            val resolved = typeUnifyEnv.filledTypes.get(tv).map(_.showDebug)
            val repr =
                if resolved.isEmpty then typeVarReprEnv.get(tv).map(_.stableKey) else None
            if resolved.isEmpty && repr.isEmpty then None
            else
                Some(
                  s"${tvId(tv)}=${resolved.map("u:" + _).getOrElse("")}${repr.map("r:" + _).getOrElse("")}"
                )
        }
        s"scope=$inUplcConstrListScope|env=[${bindings.mkString(",")}]"
    }

    def log(msg: String): Unit = {
        val nestingPrefix = "  " * nestingLevel
        val msgLines = msg.split("\n")
        for line <- msgLines do {
            println(s"${nestingPrefix}${line}")
        }
    }

    def warn(msg: String, pos: SIRPosition): Unit = {
        if !noWarn then println(s"warning: ${msg} at ${pos.show}")
    }

    def info(msg: String, pos: SIRPosition): Unit = {
        println(s"info: ${msg} at ${pos.show}")
    }
}

object LoweringContext {

    /** Process-wide trace facility for `pendingTopLevelLetRecs` add/hit events. Gated by
      * `SCALUS_TRACE_LETREC` env var (or `-Dscalus.trace.letrec=true` JVM prop). Emits a monotonic
      * counter so events across multiple `compile { }` calls in the same JVM can be interleaved and
      * diffed (run-alone vs. run-combined).
      *
      * Intended usage sites: `ScalusRuntime.builtinListToUplcConstr`,
      * `ScalusRuntime.uplcConstrToBuiltinList`, `LoweringEq.createSumEqHelper`.
      */
    private val letRecTraceEnabled: Boolean =
        sys.env.contains("SCALUS_TRACE_LETREC") ||
            sys.props.get("scalus.trace.letrec").exists(_ != "false")
    private val letRecCounter: java.util.concurrent.atomic.AtomicInteger =
        new java.util.concurrent.atomic.AtomicInteger(0)

    def traceLetRec(event: String, site: String, key: String): Unit =
        if letRecTraceEnabled then
            val n = letRecCounter.incrementAndGet()
            System.err.println(s"LETREC_$event #$n [$site] key=$key")

    def traceLetRecBoundary(tag: String): Unit =
        if letRecTraceEnabled then
            val n = letRecCounter.incrementAndGet()
            System.err.println(s"LETREC_BOUND #$n $tag")
}
