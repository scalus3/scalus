package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.compiler.sir.*
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.pretty
import scalus.uplc.*
import scalus.uplc.UplcAnnotation

import scala.util.control.NonFatal

object Lowering {

    extension (fun: DefaultFun) def tpf: Term = builtinTerms(fun)

    def forcedBuiltin(fun: DefaultFun): Term = builtinTerms(fun)

    def genError(
        msg: String
    )(using lctx: LoweringContext): Term =
        if lctx.generateErrorTraces then
            !(DefaultFun.Trace.tpf $ Term.Const(
              Constant.String(msg)
            ) $ ~Term.Error())
        else Term.Error()

    private lazy val builtinTerms = Meaning.allBuiltins.forcedBuiltins

    /** Patch missing return-position `@UplcRepr` annotation onto `base`'s return type, using
      * `source` (typically the binding's declared DefDef type) as the annotation donor.
      *
      * Walks matching `Fun` chains in parallel. If the final return position in `base` is
      * unannotated but the corresponding position in `source` is `Annotated(..., uplcRepr→…)`,
      * wrap `base`'s return with that annotation. This works around the LamAbs body type losing
      * the DefDef's declared return annotation.
      */
    private def mergeReturnAnnotation(base: SIRType, source: SIRType): SIRType =
        (base, source) match
            case (SIRType.Fun(baseArg, baseRet), SIRType.Fun(_, sourceRet)) =>
                SIRType.Fun(baseArg, mergeReturnAnnotation(baseRet, sourceRet))
            case (SIRType.TypeLambda(params, bodyBase), SIRType.TypeLambda(_, bodySrc)) =>
                SIRType.TypeLambda(params, mergeReturnAnnotation(bodyBase, bodySrc))
            case (b, SIRType.Annotated(_, anns)) if anns.data.contains("uplcRepr") =>
                b match
                    case SIRType.Annotated(_, bAnns) if bAnns.data.contains("uplcRepr") => b
                    case _                                                              =>
                        SIRType.Annotated(b, anns)
            case _ => base

    def lowerSIR(
        sir: SIR,
        optTargetType: Option[SIRType] = None
    )(using lctx: LoweringContext): LoweredValue = {
        lctx.nestingLevel += 1
        val retval = sir match
            case SIR.Decl(data, term) =>
                lctx.decls.put(data.name, data).foreach { _ =>
                    // TODO: pass logger.
                    println(s"Data declaration ${data.name} already exists")
                }
                lowerSIR(term, optTargetType)
            case constr @ SIR.Constr(name, decl, args, tp, anns) =>
                val resolvedType = lctx.resolveTypeVarIfNeeded(tp)
                // For List.Cons we pre-lower the tail FIRST. The previous identity-
                // cache-based design lowered the tail during a repr peek and then the
                // head later inside genConstr's args.map; tail-first ordering affects
                // lctx state evolution and must be preserved.
                val loweredArgs = resolvedType match
                    case SIRType.CaseClass(cd, _, Some(_))
                        if cd.name.endsWith("List$.Cons") && constr.args.length >= 2 =>
                        val loweredTail = lctx.lower(constr.args.last)
                        val loweredInit = constr.args.init.map(arg => lctx.lower(arg))
                        loweredInit :+ loweredTail
                    case _ => constr.args.map(arg => lctx.lower(arg))
                // Nil with target type carries special dispatch (the constr's declared
                // tp is `List[Nothing]` — we need to honor the caller's target type for
                // type-correct Nil emission). Other Constr forms dispatch by their
                // resolved type; the chosen generator's `genConstrLowered` then handles
                // any context-driven delegation (see `ConstrDispatcher`).
                val (typeGenerator, effectiveConstr) =
                    if name == "scalus.cardano.onchain.plutus.prelude.List$.Nil"
                        || name == typegens.SumListCommonSirTypeGenerator.PairNilName
                    then typegens.ConstrDispatcher.dispatchNil(constr, resolvedType, optTargetType)
                    else (lctx.typeGenerator(resolvedType), constr)
                typeGenerator.genConstrLowered(effectiveConstr, loweredArgs, optTargetType)
            case sirMatch @ SIR.Match(scrutinee, cases, rhsType, anns) =>
                if lctx.debug then
                    lctx.log(
                      s"Lowering match: ${sir.pretty.render(100)}\n" +
                          s"  scrutinee.tp = ${scrutinee.tp.show}\n"
                    )
                val loweredScrutinee = lowerSIR(scrutinee)
                // Match-result type is the Scala-level LUB of branches, which strips
                // `@UplcRepr` annotations. If we're in a native-Constr dispatcher scope
                // OR any branch's body type carries `@UplcRepr(UplcConstr)`, propagate
                // that annotation to the target passed to branch lowering, so inner
                // Constr emissions / repr-conversions see the UplcConstr shape.
                def hasUplcReprAnn(t: SIRType): Boolean = t match
                    case SIRType.Annotated(_, anns) => anns.data.contains("uplcRepr")
                    case _                          => false
                val anyBranchUplcRepr = cases.exists(c => hasUplcReprAnn(c.body.tp))
                val shouldAnnotate =
                    (lctx.inUplcConstrListScope || anyBranchUplcRepr)
                        && IntrinsicResolver.isUplcConstrListOrOption(rhsType)
                        && !hasUplcReprAnn(rhsType)
                val effectiveTarget =
                    if shouldAnnotate then
                        val uplcConstrAnns = scalus.compiler.sir.AnnotationsDecl(
                          anns.pos,
                          data = Map(
                            "uplcRepr" -> SIR.Const(
                              scalus.uplc.Constant.String("UplcConstr"),
                              SIRType.String,
                              scalus.compiler.sir.AnnotationsDecl(anns.pos)
                            )
                          )
                        )
                        Some(SIRType.Annotated(rhsType, uplcConstrAnns))
                    else optTargetType
                // Use representation-aware dispatch: if scrutinee has SumUplcConstr repr,
                // use Case-based genMatch instead of the default type generator's match.
                val retval = loweredScrutinee.representation match
                    case _: SumCaseClassRepresentation.SumUplcConstr =>
                        typegens.SumUplcConstrSirTypeGenerator
                            .genMatchUplcConstr(sirMatch, loweredScrutinee, effectiveTarget)
                    case _ =>
                        lctx.typeGenerator(loweredScrutinee.sirType)
                            .genMatch(sirMatch, loweredScrutinee, effectiveTarget)
                if lctx.debug then
                    lctx.log(
                      s"Lowered match: ${sir.pretty.render(100)}\n" +
                          s"  retval = ${retval.pretty.render(100)}\n"
                    )
                retval
            case SIR.Var(name, tp, anns) =>
                // Annotation-based argCache lookup. Set by `IntrinsicResolver.substituteSelf`
                // when substituting lambda params with the arg's lowered value. Survives
                // `substituteVarAndTypes` walking that creates fresh SIR.Var instances.
                anns.data.get(IntrinsicResolver.ArgCacheAnnotKey) match
                    case Some(SIR.Const(scalus.uplc.Constant.Integer(idx), _, _)) =>
                        val key = idx.toInt
                        lctx.argCache.get(key) match
                            case Some(lv) => return lv
                            case None =>
                                throw LoweringException(
                                  s"argCache miss for key $key (var $name)",
                                  anns.pos
                                )
                    case _ => // fall through to normal Var lowering
                lctx.scope.getByName(name) match
                    case Some(value) =>
                        // TODO: check types are correct
                        value.sirType match {
                            case tv: SIRType.TypeVar =>
                                // if this is type variable, try to resolve it
                                lctx.tryResolveTypeVar(tv) match
                                    case Some(resolvedType) =>
                                        val gen = lctx.typeGenerator(resolvedType)
                                        val representation = value.representation match
                                            case tvr: TypeVarRepresentation if tvr.isBuiltin =>
                                                tvr
                                            case _: TypeVarRepresentation =>
                                                gen.defaultTypeVarReperesentation(resolvedType)
                                            case concrete =>
                                                concrete
                                        TypeRepresentationProxyLoweredValue(
                                          value,
                                          resolvedType,
                                          representation,
                                          anns.pos
                                        )
                                    case None =>
                                        // Preserve concrete repr from scope binding
                                        // (e.g., variables bound in UplcConstr match context
                                        // have ProdUplcConstr repr from variant fields).
                                        // For TypeVar reprs, preserve the binding's kind rather
                                        // than rewriting to Fixed: rewriting an Unwrapped-bound
                                        // variable to Fixed at reference time produces a
                                        // byte-shape lie (bytes were in concrete-default form,
                                        // but the new label says Data-wrapped).
                                        val repr = value.representation match
                                            case tvr: TypeVarRepresentation => tvr
                                            case concrete                   => concrete
                                        TypeRepresentationProxyLoweredValue(
                                          value,
                                          tp,
                                          repr,
                                          anns.pos
                                        )
                            case _ => value
                        }
                    case None =>
                        throw LoweringException(
                          s"Variable $name not found in the scope at ${anns.pos.file}:${anns.pos.startLine}",
                          anns.pos
                        )
            case ev0 @ SIR.ExternalVar(moduleName, name, tp0, _) =>
                // If this ExternalVar targets a module dispatched by the intrinsic resolver,
                // rewrite its type's TypeVars to `Transparent`. The Scala plugin defaults to
                // `Fixed`; without this rewrite, `Fixed` leaks into enclosing intrinsic-body
                // substitutions and corrupts output representation inference. The provider's
                // body already runs with Transparent TypeVars (per `defaultIntrinsicModules`
                // post-processing) and wraps HO arguments with `toDefaultTypeVarRepr` where
                // the arg expects Data-encoded values.
                val tp =
                    if IntrinsicResolver.isIntrinsicDispatchedModule(moduleName) then
                        SIRType.mapTypeVars(
                          tp0,
                          _.copy(kind = SIRType.TypeVarKind.Transparent)
                        )
                    else tp0
                val ev = if tp eq tp0 then ev0 else ev0.copy(tp = tp)
                // SIRLinker made usual variable from names.
                if lctx.debug then
                    lctx.log(
                      s"Lowering external variable: ${ev.pretty.render(100)}\n" +
                          s"  name = $name\n" +
                          s"  tp = ${tp.show}\n"
                    )
                val myVar = lctx.scope.getByName(name) match
                    case Some(value) =>
                        if lctx.debug then {
                            lctx.log(
                              s"Found external variable $name in the scope at ${ev.anns.pos.file}:${ev.anns.pos.startLine}"
                            )
                        }
                        value
                    case None =>
                        // Check if this is a UniversalDataConversion function used outside Apply
                        if moduleName == "scalus.uplc.builtin.internal.UniversalDataConversion$"
                        then
                            throw LoweringException(
                              s"UniversalDataConversion function '$name' appears as a standalone variable at ${ev.anns.pos.file}:${ev.anns.pos.startLine + 1}. " +
                                  s"These functions must be applied to an argument and cannot be used as values " +
                                  s"(e.g., passed as argument, returned, or stored). " +
                                  s"This usually indicates the function is being used incorrectly in the code.",
                              ev.anns.pos
                            )
                        // Support bindings are eagerly materialized in `ScalusRuntime.initContext`
                        // (`initSupportBindings`) — every support-module def is registered in
                        // `lctx.scope` before user lowering starts. If `getByName` missed here,
                        // the name either targets a not-yet-registered intrinsic/support module
                        // or is genuinely unknown. Treat as a hard error rather than lazy-resolve.
                        throw LoweringException(
                          s"External variable $name not found in the scope at ${ev.anns.pos.file}:${ev.anns.pos.startLine}",
                          ev.anns.pos
                        )
                myVar
            case sirLet @ SIR.Let(bindings, body, flags, anns) =>
                // don;t generate FromData/ToData (now handled by Data Representation)
                val nBindings =
                    bindings.filterNot(b => isFromDataName(b.name) || (isToDataName(b.name)))
                if nBindings.isEmpty then lowerSIR(body)
                else lowerLet(sirLet.copy(bindings = nBindings))
            case SIR.LamAbs(param, term, typeParams, anns) =>
                // TODO: add type params to context.  Now we do all computations in
                // new context, so type params are assumed implicitly
                if lctx.debug then
                    lctx.log(
                      s"Lowering lamAbs: ${sir.pretty.render(100)}\n" +
                          s"  param.tp = ${param.tp.show}\n" +
                          s"  term.tp = ${term.tp.show}\n"
                    )
                val optTermTargetType = optTargetType.flatMap { tp =>
                    SIRType.collectPolyOrFun(tp).flatMap { case (typeParams, in, out) =>
                        Some(out)
                    }
                }
                val retval = lvLamAbs(
                  param,
                  lctx.typeGenerator(param.tp).defaultRepresentation(param.tp),
                  _id => {
                      val loweredBody = summon[LoweringContext].lower(term, optTermTargetType)
                      // If there's a target return type, try to upcast the body
                      // This handles cases where body returns a subtype of the declared return type
                      optTermTargetType match
                          case Some(targetType) =>
                              if lctx.debug then
                                  lctx.log(
                                    s"[Lambda body upcast] Upcasting body from ${loweredBody.sirType.show} to ${targetType.show}, representation: ${loweredBody.representation}"
                                  )
                              val upcasted = loweredBody.maybeUpcast(targetType, anns.pos)
                              // If return type has a binding @UplcRepr annotation,
                              // convert body to that pinned repr. Two sources count:
                              //   (a) `SIRType.Annotated(_, retAnns)` — explicit
                              //       per-occurrence annotation on the return position,
                              //   (b) class-level `@UplcRepr(UplcConstr)` on the
                              //       return type's case class declaration (e.g.,
                              //       `ChessSet` is `@UplcRepr(UplcConstr)` so any
                              //       lambda returning `ChessSet` should yield UC
                              //       bytes — without this, a body like `_.board`
                              //       projecting from a Data SE leaks Data ChessSet
                              //       to a UC slot, surfacing as native-UC selectors
                              //       applied to Data.Constr scrutinees at runtime).
                              val result =
                                  if hasUplcReprPin(targetType) then
                                      val targetGen =
                                          summon[LoweringContext].typeGenerator(targetType)
                                      val targetRepr = targetGen.defaultRepresentation(targetType)
                                      upcasted.toRepresentation(targetRepr, anns.pos)
                                  else upcasted
                              if lctx.debug then
                                  lctx.log(
                                    s"[Lambda body upcast] After upcast: ${result.sirType.show}, representation: ${result.representation}"
                                  )
                              result
                          case None =>
                              if lctx.debug then
                                  lctx.log(
                                    s"[Lambda body upcast] No target type, body type: ${loweredBody.sirType.show}, representation: ${loweredBody.representation}"
                                  )
                              loweredBody
                  },
                  anns.pos
                )
                if lctx.debug then
                    lctx.log(
                      s"Lowered lamAbs: ${sir.pretty.render(100)}\n" +
                          s"  retval = ${retval.pretty.render(100)}\n"
                    )
                retval
            case app: SIR.Apply =>
                lowerApp(app, optTargetType)
            case sel @ SIR.Select(scrutinee, field, tp, anns) =>
                val loweredScrutinee = lowerSIR(scrutinee)
                loweredScrutinee.sirType match {
                    case tv: SIRType.TypeVar =>
                        scrutinee.tp match
                            case _: SIRType.TypeVar =>
                            case _ =>
                                throw LoweringException(
                                  s"Lowered scrutinee is typed as type variable but scrutinee is not.\n" +
                                      s"  scrutinee.tp: ${scrutinee.tp.show}\n" +
                                      s"  resolved typevar: ${lctx.typeUnifyEnv.filledTypes
                                              .get(tv)}, typeVar = $tv\n" +
                                      s"  loweredScrutinee.sirType: ${loweredScrutinee.sirType.show}, representation: ${loweredScrutinee.representation}",
                                  anns.pos
                                )
                    case _ =>
                }
                val generator = lctx.typeGenerator(loweredScrutinee.sirType)
                val retval = generator.genSelect(sel, loweredScrutinee)
                if lctx.debug then {
                    lctx.log(
                      s"Lowered SIR.Select: ${sir.pretty.render(100)}\n" +
                          s"  retval = ${retval.pretty.render(100)}\n"
                    )
                }
                retval
            case sirConst @ SIR.Const(const, tp, anns) =>
                // Handle BuiltinList and BuiltinArray constants - transform elements to Data if needed
                // The constant value is transformed to use Data elements at runtime,
                // but the semantic type is preserved to maintain correct type checking.
                // For primitive element types (Integer, ByteString, String, Boolean),
                // keep native UPLC list types to avoid per-element wrapping/unwrapping.
                val (transformedConst, repr) = tp match {
                    case SIRType.BuiltinList(elemType)
                        if !SIRType.Data.unapply(elemType) && isPrimitiveElementType(elemType) =>
                        // Preserve native UPLC list — no conversion needed
                        (
                          const,
                          SumCaseClassRepresentation.SumBuiltinList(
                            PrimitiveRepresentation.Constant
                          )
                        )
                    case SIRType.BuiltinList(elemType) if !SIRType.Data.unapply(elemType) =>
                        const match {
                            case Constant.List(_, elements) =>
                                val dataElements = elements.map(constantToData)
                                val newConst = Constant.List(DefaultUni.Data, dataElements)
                                (
                                  newConst,
                                  SumCaseClassRepresentation.SumBuiltinList(
                                    SumCaseClassRepresentation.DataData
                                  )
                                )
                            case _ =>
                                (const, LoweredValueRepresentation.constRepresentation(tp))
                        }
                    case SIRType.BuiltinArray(elemType) if !SIRType.Data.unapply(elemType) =>
                        const match {
                            case Constant.Array(_, elements) =>
                                val dataElements = elements.map(constantToData)
                                val newConst = Constant.Array(DefaultUni.Data, dataElements)
                                (
                                  newConst,
                                  LoweredValueRepresentation.constRepresentation(
                                    SIRType.BuiltinArray(SIRType.Data.tp)
                                  )
                                )
                            case _ =>
                                (const, LoweredValueRepresentation.constRepresentation(tp))
                        }
                    case _ =>
                        (const, LoweredValueRepresentation.constRepresentation(tp))
                }
                ConstantLoweredValue(SIR.Const(transformedConst, tp, anns), repr)
            case SIR.And(lhs, rhs, anns) =>
                lowerSIR(
                  SIR.IfThenElse(
                    lhs,
                    rhs,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean, anns),
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.Or(lhs, rhs, anns) =>
                lowerSIR(
                  SIR.IfThenElse(
                    lhs,
                    SIR.Const(Constant.Bool(true), SIRType.Boolean, anns),
                    rhs,
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.Not(term, anns) =>
                lowerSIR(
                  SIR.IfThenElse(
                    term,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean, anns),
                    SIR.Const(Constant.Bool(true), SIRType.Boolean, anns),
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                val loweredCond =
                    lowerSIR(cond, Some(SIRType.Boolean))
                        .toRepresentation(PrimitiveRepresentation.Constant, cond.anns.pos)
                // Prefer the caller's target type when provided so that any @UplcRepr
                // annotation on the surrounding context (e.g. an annotated function return
                // type) propagates into the branches' lowering. Falling back to the if's own
                // static `tp` ignores those annotations and forces both branches to the
                // unwrapped default repr; the outer wrapper then needs a structural
                // conversion that, for `List[ChessSet]` and similar, can fail to thread the
                // element representations correctly.
                val branchTarget = optTargetType.orElse(Some(tp))
                val loweredT = lowerSIR(t, branchTarget)
                val loweredF = lowerSIR(f, branchTarget)
                lvIfThenElse(loweredCond, loweredT, loweredF, anns.pos, branchTarget)
            case SIR.Cast(expr, tp, anns) =>
                val loweredExpr = lowerSIR(expr, Some(tp))
                val isTypeProxy = anns.data.contains("typeProxy")
                try lvCast(loweredExpr, tp, anns.pos, isTypeProxy)
                catch
                    case NonFatal(ex) =>
                        println(
                          s"Error lowering cast: ${sir.pretty.render(100)} at ${anns.pos.file}:${anns.pos.startLine + 1}"
                        )
                        throw ex
            case sirBuiltin @ SIR.Builtin(bn, tp, anns) =>
                BuiltinRefLoweredValue(
                  sirBuiltin,
                  builtinTerms(bn),
                  SirTypeUplcGenerator(tp).defaultRepresentation(tp)
                )
            case sirError @ SIR.Error(msg, anns, cause) =>
                if lctx.generateErrorTraces then
                    if msg.tp != SIRType.String then
                        throw LoweringException(
                          s"Error message should be String, but got ${msg.tp.show}",
                          msg.anns.pos
                        )
                    val loweredMsg = lowerSIR(msg, Some(SIRType.String))
                    val errorTerm =
                        ErrorLoweredValue(sirError, ~Term.Error(UplcAnnotation(anns.pos)))
                    lvForce(
                      lvBuiltinApply2(
                        SIRBuiltins.trace,
                        loweredMsg,
                        errorTerm,
                        sirError.tp,
                        ErrorRepresentation,
                        anns.pos
                      ),
                      anns.pos
                    )
                else ErrorLoweredValue(sirError, Term.Error(UplcAnnotation(anns.pos)))
        lctx.nestingLevel -= 1
        retval
    }

    private def lowerLet(sirLet: SIR.Let)(using lctx: LoweringContext): LoweredValue = {
        val retval = sirLet match
            case SIR.Let(bindings, body, flags, anns) =>
                val prevScope = lctx.scope
                if !flags.isRec then
                    val bindingValues = bindings.map { b =>
                        var prevDebug = lctx.debug
                        val loweredRhs = lowerSIR(b.value, Some(b.tp))
                        if lctx.debug then
                            lctx.log(
                              s"[LET binding] ${b.name}: lowered RHS type ${loweredRhs.sirType.show}, repr ${loweredRhs.representation}, target type ${b.tp.show}"
                            )
                        val rhs = loweredRhs.maybeUpcast(b.tp, anns.pos)
                        if lctx.debug then
                            lctx.log(
                              s"[LET binding] ${b.name}: after upcast type ${rhs.sirType.show}, repr ${rhs.representation}"
                            )
                        val varId = lctx.uniqueVarName(b.name)
                        lctx.debug = prevDebug
                        val varVal = VariableLoweredValue(
                          id = varId,
                          name = b.name,
                          sir = SIR.Var(b.name, b.tp, anns),
                          representation = rhs.representation,
                          optRhs = if flags.isLazy then Some(rhs) else None
                        )
                        lctx.scope = lctx.scope.add(varVal)
                        (varVal: IdentifiableLoweredValue, rhs)
                    }.toSeq
                    val bodyValue = lowerSIR(body)
                    lctx.scope = prevScope
                    if flags.isLazy then
                        ScopeBracketsLoweredValue(bindingValues.map(_._1).toSet, bodyValue)
                    else LetNonRecLoweredValue(bindingValues, bodyValue, sirLet.anns.pos)
                else {
                    // in rec case, Lazy doesn't make sense, so we ignore it
                    bindings match
                        case List(Binding(name, tp, rhs)) =>
                            lctx.zCombinatorNeeded = true
                            // Use `rhs.tp` (from LamAbs) as the base — it preserves the
                            // method's TypeVar kinds (e.g. `@UplcRepr(TypeVar(Unwrapped)) B`
                            // becomes `TypeVar(B, id, Unwrapped)` in LamAbs.param.tp/body.tp).
                            // But `body.tp` is the body's *inferred* type and loses the
                            // declared return `@UplcRepr` annotation. Patch it back in from
                            // the binding's `tp` (DefDef signature) which preserves return
                            // annotations. This is a workaround until the compiler plugin
                            // annotates `LamAbs.term.tp` with the DefDef's declared return
                            // annotation directly.
                            val rhsRepTp = mergeReturnAnnotation(rhs.tp, tp)
                            val rhsRepr =
                                lctx.typeGenerator(rhsRepTp).defaultRepresentation(rhsRepTp)
                            val newVar = VariableLoweredValue(
                              id = lctx.uniqueVarName(name),
                              name = name,
                              sir = SIR.Var(name, tp, anns),
                              representation = rhsRepr
                            )
                            val prevScope = lctx.scope
                            lctx.scope = lctx.scope.add(newVar)

                            val loweredRhs =
                                lowerSIR(rhs)
                                    .maybeUpcast(tp, anns.pos)
                                    .toRepresentation(rhsRepr, anns.pos)
                            val loweredBody = lowerSIR(body)

                            lctx.scope = prevScope
                            LetRecLoweredValue(newVar, loweredRhs, loweredBody, sirLet.anns.pos)
                        case Nil =>
                            sys.error(
                              s"Empty let binding at ${sirLet.anns.pos.file}:${sirLet.anns.pos.startLine}"
                            )
                        case _ =>
                            sys.error(
                              s"Mutually recursive bindings are not supported: $bindings at ${sirLet.anns.pos.file}:${sirLet.anns.pos.startLine}"
                            )
                }
        retval

    }

    private def lowerApp(app: SIR.Apply, optTargetType: Option[SIRType])(using
        lctx: LoweringContext
    ): LoweredValue = {
        if isFromDataApp(app) then lowerFromData(app)
        else if isToDataApp(app) then lowerToData(app)
        else if isPairListConversion(app) then lowerPairListConversion(app)
        else if isTypeProxyApp(app) then lowerTypeProxy(app)
        else if isTypeProxyReprApp(app) then lowerTypeProxyRepr(app)
        else if isToDefaultTypeVarReprApp(app) then lowerToDefaultTypeVarRepr(app)
        else if isFromDefaultTypeVarReprApp(app) then lowerFromDefaultTypeVarRepr(app)
        else if isUnboxedNilApp(app) then lowerUnboxedNil(app, optTargetType)
        else if LoweringEq.isEqualsReprApp(app) then LoweringEq.lowerEqualsRepr(app)
        // Enabled globally: generateEqualsForRepr falls back to generateDataEquals (== equalsData)
        // for non-UplcConstr types, so semantically matches the Eq.derived expansion.
        else if LoweringEq.isEqIntrinsicApp(app) then
            LoweringEq.lowerEqIntrinsic(app, a => lowerNormalApp(a, optTargetType))
        else lowerNormalApp(app, optTargetType)
    }

    private val TypeProxyName = "scalus.compiler.intrinsics.IntrinsicHelpers$.typeProxy"
    private val TypeProxyReprName = "scalus.compiler.intrinsics.IntrinsicHelpers$.typeProxyRepr"
    private val ToDefaultTypeVarReprName =
        "scalus.compiler.intrinsics.IntrinsicHelpers$.toDefaultTypeVarRepr"
    private val FromDefaultTypeVarReprName =
        "scalus.compiler.intrinsics.IntrinsicHelpers$.fromDefaultTypeVarRepr"
    private val UnboxedNilName =
        "scalus.cardano.onchain.plutus.prelude.List$.unboxedNil"

    private def isTypeProxyApp(app: SIR.Apply): Boolean = app.f match
        case SIR.ExternalVar(_, name, _, _) => name == TypeProxyName
        case _                              => false

    private def isTypeProxyReprApp(app: SIR.Apply): Boolean = app.f match
        case SIR.ExternalVar(_, name, _, _) => name == TypeProxyReprName
        case _                              => false

    private def isToDefaultTypeVarReprApp(app: SIR.Apply): Boolean = app.f match
        case SIR.ExternalVar(_, name, _, _) => name == ToDefaultTypeVarReprName
        case _                              => false

    private def isFromDefaultTypeVarReprApp(app: SIR.Apply): Boolean = app.f match
        case SIR.ExternalVar(_, name, _, _) => name == FromDefaultTypeVarReprName
        case _                              => false

    private def isUnboxedNilApp(app: SIR.Apply): Boolean = app.f match
        case SIR.ExternalVar(_, name, _, _) => name == UnboxedNilName
        case SIR.Var(name, _, _)            => name == UnboxedNilName
        case _                              => false

    /** Direct lowering of `List.unboxedNil[A]()`. The plugin's SIR for a `def f[A]: T` is
      * polymorphic and typed as `List[Nothing]` inside the body (accurate for Scala's covariant
      * List, but not what UPLC needs — UPLC list constants are typed). Intercepting at the call
      * site lets us read the concrete `List[A]` from `app.tp` and build the right `Constant.List`
      * directly, so the API actually delivers on its "unboxed" promise.
      */
    private def lowerUnboxedNil(
        app: SIR.Apply,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        import SumCaseClassRepresentation.SumBuiltinList
        val pos = app.anns.pos
        // Prefer app.tp (Scala-plugin-instantiated result type); fall back to optTargetType.
        // `retrieveListElementType` unwraps Annotated / TypeLambda / TypeProxy.
        val (listType, elemType) =
            SumBuiltinList.retrieveListElementType(app.tp).map(app.tp -> _) match
                case Some(r) => r
                case None =>
                    optTargetType
                        .flatMap(t => SumBuiltinList.retrieveListElementType(t).map(t -> _))
                        .getOrElse(
                          throw LoweringException(
                            s"lowerUnboxedNil: expected a concrete List[A] target; got app.tp=${app.tp.show}",
                            pos
                          )
                        )
        val elemRepr =
            lctx.typeGenerator(elemType).defaultRepresentation(elemType)
        elemRepr match
            case _: SumCaseClassRepresentation.SumUplcConstr |
                _: ProductCaseClassRepresentation.ProdUplcConstr =>
                // Constr-encoded element → re-enter the normal Nil path, which the
                // SumCaseUplcConstr generator handles via its genConstr. The Nil-special-case
                // at the top of lowerSIR swaps in `listType` via `optTargetType`.
                val nilConstr = SIR.Constr(
                  SIRType.List.NilConstr.name,
                  SIRType.List.dataDecl,
                  scala.Nil,
                  SIRType.List.Nil,
                  app.anns
                )
                lowerSIR(nilConstr, Some(listType))
            case _ if typegens.SirTypeUplcGenerator.isPrimitiveElementType(elemType) =>
                lvTypedNil(
                  pos,
                  elemType,
                  listType,
                  SumBuiltinList(PrimitiveRepresentation.Constant)
                )
            case _ =>
                // Data-element fallback — same result as `List.empty[A]`.
                lvDataNil(pos, listType, SumBuiltinList(SumCaseClassRepresentation.DataData))
    }

    /** Type-only cast — changes SIR type, keeps representation from the lowered argument. */
    private def lowerTypeProxy(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {
        val loweredArg = lowerSIR(app.arg)
        TypeRepresentationProxyLoweredValue(
          loweredArg,
          app.tp,
          loweredArg.representation,
          app.anns.pos
        )
    }

    private def lowerTypeProxyRepr(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {
        val loweredArg = lowerSIR(app.arg)
        val baseRepr = app.anns.data.get("repr") match
            case Some(reprSir) => interpretReprSIR(reprSir, app.tp, app.anns.pos)
            case _ =>
                throw LoweringException(
                  "typeProxyRepr: missing 'repr' annotation",
                  app.anns.pos
                )
        // Refine SumBuiltinList repr based on concrete element type
        val repr = baseRepr match
            case SumCaseClassRepresentation.SumBuiltinList(_) =>
                SumCaseClassRepresentation.SumBuiltinList.retrieveListElementType(app.tp) match
                    case Some(elemType)
                        if elemType != SIRType.FreeUnificator
                            && !elemType.isInstanceOf[SIRType.TypeVar] =>
                        SumCaseClassRepresentation.SumBuiltinList(
                          typegens.SirTypeUplcGenerator.elementReprFor(elemType)
                        )
                    case _ => baseRepr // unresolved type var — keep sentinel
            case other => other
        TypeRepresentationProxyLoweredValue(
          loweredArg,
          app.tp,
          repr,
          app.anns.pos
        )
    }

    /** Convert a value to its defaultTypeVarRepresentation. Used in intrinsic bodies to convert
      * native-repr values for pre-compiled HO functions.
      */
    private def lowerToDefaultTypeVarRepr(
        app: SIR.Apply
    )(using lctx: LoweringContext): LoweredValue = {
        val loweredArg = lowerSIR(app.arg)
        val tp = app.tp
        val gen = lctx.typeGenerator(tp)
        val targetRepr = gen.defaultTypeVarReperesentation(tp)
        // Force the conversion into a named variable so it's included in UPLC output.
        // Without this, DependendVariableLoweredValue may not emit conversion code.
        val converted = loweredArg.toRepresentation(targetRepr, app.anns.pos)
        if converted eq loweredArg then converted
        else
            LoweredValue.Builder.lvNewLazyIdVar(
              lctx.uniqueVarName("_tvRepr"),
              tp,
              targetRepr,
              converted,
              app.anns.pos
            )
    }

    private def lowerFromDefaultTypeVarRepr(
        app: SIR.Apply
    )(using lctx: LoweringContext): LoweredValue = {
        val loweredArg = lowerSIR(app.arg)
        // TypeVars are bound in typeUnifyEnv but not substituted in the SIR AST, so resolve
        // before looking up the generator. Without this, TypeVar_A maps to TypeVarSirTypeGenerator
        // (a no-op), while we need the concrete type's generator (e.g., ProdUplcConstr for
        // @UplcRepr(UplcConstr) types).
        val tp = lctx.resolveTypeVarIfNeeded(app.tp)
        val gen = lctx.typeGenerator(tp)
        val targetRepr = gen.defaultRepresentation(tp)
        if lctx.debug then
            lctx.log(
              s"[fromDefaultTypeVarRepr] app.tp=${app.tp.show}, resolved tp=${tp.show}, loweredArg.repr=${loweredArg.representation}, targetRepr=$targetRepr"
            )
        val converted = loweredArg.toRepresentation(targetRepr, app.anns.pos)
        if converted eq loweredArg then converted
        else
            LoweredValue.Builder.lvNewLazyIdVar(
              lctx.uniqueVarName("_tvFromRepr"),
              tp,
              targetRepr,
              converted,
              app.anns.pos
            )
    }

    /** Interpret a SIR expression produced by the plugin for a `ReprTag` value.
      *
      * The plugin compiles `ReprTag` case objects as `SIR.Constr` with 0 args, and
      * `ReprTag.SumBuiltinList(elem)` as `SIR.Constr` with 1 arg.
      */
    private def interpretReprSIR(
        sir: SIR,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValueRepresentation = {
        sir match
            // Case object: SIR.Constr("ReprTag$.DataData", _, Nil, _, _)
            case SIR.Constr(name, _, Nil, _, _) =>
                resolveReprTagName(name, pos)
            // Case class with 1 arg: SIR.Constr("ReprTag$.SumBuiltinList", _, List(elemReprSir), _, _)
            case SIR.Constr(name, _, List(elemReprSir), _, _) =>
                val shortName = name.split("\\$?\\.").last
                if shortName == "SumBuiltinList" then
                    val elemRepr = interpretReprSIR(elemReprSir, targetType, pos)
                    SumCaseClassRepresentation.SumBuiltinList(elemRepr)
                else
                    throw LoweringException(
                      s"typeProxyRepr: unsupported ReprTag constructor '$name'",
                      pos
                    )
            // Case class with 2 args: SIR.Constr("ReprTag$.ProdBuiltinPair", _, List(fst, snd), _, _)
            case SIR.Constr(name, _, List(fstReprSir, sndReprSir), _, _) =>
                val shortName = name.split("\\$?\\.").last
                if shortName == "ProdBuiltinPair" then
                    val fstRepr = interpretReprSIR(fstReprSir, targetType, pos)
                    val sndRepr = interpretReprSIR(sndReprSir, targetType, pos)
                    ProductCaseClassRepresentation.ProdBuiltinPair(fstRepr, sndRepr)
                else
                    throw LoweringException(
                      s"typeProxyRepr: unsupported ReprTag constructor '$name'",
                      pos
                    )
            // ExternalVar reference to a case object
            case SIR.ExternalVar(_, name, _, _) =>
                resolveReprTagName(name, pos)
            // Cast wrapper (from inline def expansion)
            case SIR.Cast(inner, _, _) =>
                interpretReprSIR(inner, targetType, pos)
            // String constant (backward compat)
            case SIR.Const(scalus.uplc.Constant.String(reprName), _, _) =>
                resolveReprTagName(reprName, pos)
            case _ =>
                throw LoweringException(
                  s"typeProxyRepr: cannot interpret repr SIR: ${sir.pretty.render(80)}",
                  pos
                )
    }

    private def resolveReprTagName(
        name: String,
        pos: SIRPosition
    ): LoweredValueRepresentation = {
        val shortName = name.split("\\$?\\.").last
        shortName match
            case "DataData"          => SumCaseClassRepresentation.DataData
            case "DataConstr"        => SumCaseClassRepresentation.DataConstr
            case "Constant"          => PrimitiveRepresentation.Constant
            case "PackedData"        => PrimitiveRepresentation.PackedData
            case "PackedSumDataList" => SumCaseClassRepresentation.PackedSumDataList
            case "PackedDataMap"     => ProductCaseClassRepresentation.PackedDataMap
            // Compound list reprs for test backward compat
            case "SumBuiltinList(DataData)" =>
                SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData)
            case "SumPairBuiltinList" =>
                throw LoweringException(
                  s"typeProxyRepr: SumPairBuiltinList requires type context, use typeProxyRepr with SumDataAssocMap instead",
                  pos
                )
            case "SumBuiltinList(Constant)" =>
                SumCaseClassRepresentation.SumBuiltinList(PrimitiveRepresentation.Constant)
            case _ =>
                throw LoweringException(s"unknown ReprTag '$name'", pos)
    }

    @scala.annotation.tailrec
    private def isFunOrTypeLambdaOverFun(tp: SIRType): Boolean = tp match {
        case _: SIRType.Fun                        => true
        case SIRType.TypeLambda(_, body)           => isFunOrTypeLambdaOverFun(body)
        case SIRType.TypeProxy(ref) if ref != null => isFunOrTypeLambdaOverFun(ref)
        case SIRType.Annotated(t, _)               => isFunOrTypeLambdaOverFun(t)
        case _                                     => false
    }

    /** True iff `tp` is a TypeVar at its root, or a single-arg TypeRefApply whose argument
      * is a TypeVar (e.g. `List[B]`, `Option[A]`). Walks past `TypeProxy`, `TypeLambda`,
      * `Annotated`. The narrow shape covered here matches the SIR-plugin output-type leak
      * observed in `IntrinsicsUplcConstrList.map[A,B]` where `app.tp` carries `List[B]`
      * with `B` not yet bound to the lambda's body type.
      */
    @scala.annotation.tailrec
    /** True if `tp` has a binding `@UplcRepr` annotation — either as a
      * `SIRType.Annotated(_, anns)` wrapper or as a class-level annotation in
      * its constrDecl. Used by `SIR.LamAbs` lowering to decide whether the
      * return type pins a specific repr that the body's natural repr must
      * conform to.
      */
    private def hasUplcReprPin(tp: SIRType): Boolean = tp match {
        case SIRType.Annotated(_, anns) if anns.data.contains("uplcRepr") => true
        case SIRType.Annotated(inner, _) => hasUplcReprPin(inner)
        case SIRType.CaseClass(decl, _, _) => decl.annotations.data.contains("uplcRepr")
        case SIRType.SumCaseClass(decl, _) => decl.annotations.data.contains("uplcRepr")
        case SIRType.TypeLambda(_, body) => hasUplcReprPin(body)
        case SIRType.TypeProxy(ref) if ref != null => hasUplcReprPin(ref)
        case _ => false
    }

    private def hasFreeTypeVarRoot(tp: SIRType): Boolean = tp match {
        case _: SIRType.TypeVar                    => true
        case SIRType.TypeProxy(ref) if ref != null => hasFreeTypeVarRoot(ref)
        case SIRType.TypeLambda(_, body)           => hasFreeTypeVarRoot(body)
        case SIRType.Annotated(t, _)               => hasFreeTypeVarRoot(t)
        case SIRType.SumCaseClass(_, args) =>
            args.exists {
                case _: SIRType.TypeVar => true
                case _                  => false
            }
        case SIRType.CaseClass(_, args, _) =>
            args.exists {
                case _: SIRType.TypeVar => true
                case _                  => false
            }
        case _ => false
    }

    private def lowerNormalApp(app: SIR.Apply, optTargetType: Option[SIRType])(using
        lctx: LoweringContext
    ): LoweredValue = {
        if lctx.debug then
            lctx.log(
              s"Lowering app: ${app.pretty.render(100)}\n" +
                  s"  app.tp = ${app.tp.show}\n" +
                  s"  f.tp = ${app.f.tp.show}\n" +
                  s"  arg.tp = ${app.arg.tp.show}\n" +
                  s"  f = ${app.f.pretty.render(100)}\n"
            )
        // Refine the resolver's appType using optTargetType when app.tp contains
        // unresolved TypeVars that the outer target can fill in. The Scala SIR plugin
        // doesn't always substitute output type parameters into Apply.tp (e.g.
        // `descAndNo.quicksort.map { _.board }` may emit `Apply(...): List[B]` instead
        // of `List[ChessSet]`), which leaves the intrinsic resolver unable to unify
        // and bind the output TypeVar — downstream conversions then trip on the
        // unresolved element. When the outer match-branch / let body / annotated
        // return type is known, prefer it for resolver dispatch.
        val refinedAppTp: SIRType = optTargetType match {
            case Some(target) if hasFreeTypeVarRoot(app.tp) =>
                target
            case _ => app.tp
        }
        if lctx.intrinsicModules.nonEmpty && !isFunOrTypeLambdaOverFun(refinedAppTp) then
            IntrinsicResolver.gatherApplyChain(app) match
                case Some((head, argSirs)) if argSirs.length > 1 =>
                    val firstLowered = lowerSIR(argSirs.head)
                    IntrinsicResolver.tryResolveFull(
                      head,
                      argSirs,
                      firstLowered :: scala.List.empty,
                      refinedAppTp,
                      app.anns.pos
                    )(using lctx) match
                        case Some(result) => return result
                        case None         => // fall through
                case _ => // fall through
        val fun = lowerSIR(app.f)
        val arg = lowerSIR(app.arg)

        // Try intrinsic resolution
        if lctx.intrinsicModules.nonEmpty then
            IntrinsicResolver.tryResolve(app.f, app.arg, arg, refinedAppTp, app.anns.pos)(using
              lctx
            ) match
                case Some(result) =>
                    if lctx.debug then
                        lctx.log(
                          s"Intrinsic resolved: ${app.f.pretty.render(60)} -> ${result.pretty.render(100)}"
                        )
                    return result
                case None => // fall through to normal apply
        val result =
            try
                lvApply(
                  fun,
                  arg,
                  app.anns.pos,
                  Some(app.tp),
                  None // representation can depend from fun, so should be calculated.
                )
            catch
                case ex: SIRType.CaclulateApplyTypeException =>
                    val location =
                        s"${app.anns.pos.file}:${app.anns.pos.startLine + 1}:${app.anns.pos.startColumn + 1}"
                    throw SIRType.CaclulateApplyTypeException(
                      s"${ex.msg}\nLocation: $location",
                      ex.cause
                    )
                case NonFatal(ex) =>
                    if lctx.debug then
                        lctx.log(
                          s"Error lowering app: ${app.pretty.render(100)} at ${app.anns.pos.file}:${app.anns.pos.startLine + 1}\n" +
                              s"  ${ex.getMessage}\n" +
                              s"  app.tp=${app.tp.show} (${app.tp.getClass.getName})\n" +
                              s"  app.f=${app.f.pretty.render(100)} (${app.f.getClass.getSimpleName})\n" +
                              s"  f.tp=${app.f.tp.show}, lowered f.tp=${fun.sirType.show}\n" +
                              s"  arg.tp=${app.arg.tp.show} (unrolled=${SIRType.unrollTypeProxy(app.arg.tp).show}), lowered arg.tp=${arg.sirType.show}"
                        )
                    throw ex
        result
    }

    private def isFromDataType(tp: SIRType): Boolean = tp match {
        case SIRType.Fun(SIRType.Data(), _)  => true
        case SIRType.TypeLambda(params, tp1) => isFromDataType(tp1)
        case _                               => false
    }

    private def isFromDataName(name: String): Boolean = {
        name == "scalus.uplc.builtin.internal.UniversalDataConversion$.fromData"
    }

    private def isToDataName(name: String): Boolean = {
        name == "scalus.uplc.builtin.internal.UniversalDataConversion$.toData"
    }

    private def isFromDataApp(app: SIR.Apply): Boolean = {
        app.f match
            case SIR.ExternalVar(moduleName, name, tp, _) =>
                // extrapolation.  TODO: write annotation when compiling FromData tp and extract it here
                isFromDataName(name) && isFromDataType(tp)
            case SIR.Var(name, tp, _) =>
                // Also check for Var nodes with fully qualified name
                isFromDataName(name) && isFromDataType(tp)
            case _ => false
    }

    private def isToDataApp(app: SIR.Apply): Boolean = {

        app.f match
            case SIR.ExternalVar(moduleName, name, tp, _) =>
                // extrapolation.  TODO: write annotation when compiling ToData tp and extract it here
                name == "scalus.uplc.builtin.internal.UniversalDataConversion$.toData"
            case SIR.Var(name, tp, _) =>
                // Also check for Var nodes with fully qualified name
                isToDataName(name)
            case _ => false

    }

    private def lowerFromData(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {
        val data = lctx.lower(app.arg)
        new ProxyLoweredValue(data) {
            override def sirType: SIRType = app.tp
            override def pos: SIRPosition = app.anns.pos
            override def representation: LoweredValueRepresentation =
                lctx.typeGenerator(app.tp).defaultDataRepresentation(app.tp)
            override def termInternal(gctx: TermGenerationContext): Term =
                data.termInternal(gctx)

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val left = Doc.text("FromData(")
                val right = Doc.text(s", ${app.tp.show})")
                data.docRef(ctx).bracketBy(left, right)
            }

            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val left = Doc.text("FromData(")
                val right = Doc.text(")")
                data.docRef(ctx).bracketBy(left, right)
            }

        }
    }

    private def isPairListConversionName(name: String): Boolean =
        SIRType.PairList.ConversionNames.contains(name)

    private def isPairListConversion(app: SIR.Apply): Boolean = {
        app.f match
            case SIR.ExternalVar(_, name, _, _) => isPairListConversionName(name)
            case SIR.Var(name, _, _)            => isPairListConversionName(name)
            case _                              => false
    }

    private def isToMapConversionName(name: String): Boolean =
        SIRType.PairList.ToMapNames.contains(name)

    private def isToMapConversion(app: SIR.Apply): Boolean = {
        app.f match
            case SIR.ExternalVar(_, name, _, _) => isToMapConversionName(name)
            case SIR.Var(name, _, _)            => isToMapConversionName(name)
            case _                              => false
    }

    private def lowerPairListConversion(
        app: SIR.Apply
    )(using lctx: LoweringContext): LoweredValue = {
        val loweredArg = lctx.lower(app.arg)
        if isToMapConversion(app) then
            // toSortedMap / toAssocMap: convert to SumPairBuiltinList then apply mapData
            val argType = app.arg.tp
            val elemType = SumCaseClassRepresentation.SumBuiltinList
                .retrieveListElementType(argType)
                .getOrElse(SIRType.Data.tp)
            val pairListRepr = SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(
              elemType,
              app.anns.pos
            )
            val asPairList = loweredArg.toRepresentation(pairListRepr, app.anns.pos)
            lvBuiltinApply(
              SIRBuiltins.mapData,
              asPairList,
              app.tp,
              ProductCaseClassRepresentation.PackedDataMap,
              app.anns.pos
            )
        else
            // toList / toPairList: repr conversion only
            val elemType = SumCaseClassRepresentation.SumBuiltinList
                .retrieveListElementType(app.tp)
                .getOrElse(SIRType.Data.tp)
            val pairListRepr = SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(
              elemType,
              app.anns.pos
            )
            val convertedArg = loweredArg.toRepresentation(pairListRepr, app.anns.pos)
            TypeRepresentationProxyLoweredValue(convertedArg, app.tp, pairListRepr, app.anns.pos)
    }

    private def lowerToData(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {
        if SIRType.isPolyFunOrFun(app.arg.tp) then {
            throw LoweringException(
              s"Argument of toData should be a data type, but got ${app.arg.tp.show}.\n" +
                  s"  app.arg = ${app.arg.pretty.render(100)}\n" +
                  s"  app.f = ${app.f.pretty.render(100)}\n" +
                  s"  app.tp = ${app.tp.show}",
              app.anns.pos
            )
        }
        val value = lctx
            .lower(app.arg)
            .toRepresentation(
              lctx.typeGenerator(app.arg.tp).defaultDataRepresentation(app.arg.tp),
              app.anns.pos
            )
        new ProxyLoweredValue(value) {
            override def sirType: SIRType = app.tp
            override def pos: SIRPosition = app.anns.pos
            override def representation: LoweredValueRepresentation =
                // Data is now a sum type with DataData representation
                SumCaseClassRepresentation.DataData
            override def termInternal(gctx: TermGenerationContext): Term =
                value.termInternal(gctx)

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val left = Doc.text("ToData(")
                val right = Doc.text(s", ${app.tp.show})")
                value.docRef(ctx).bracketBy(left, right)
            }

            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val left = Doc.text("ToData(")
                val right = Doc.text(")")
                value.docRef(ctx).bracketBy(left, right)
            }

        }
    }

    def generateDominatedUplevelVarsAccess(
        value: LoweredValue,
    )(using gctx: TermGenerationContext): Term = {

        val newVars = value.dominatingUplevelVars.filterNot(x => gctx.generatedVars.contains(x.id))
        val nGeneratedVars = gctx.generatedVars ++ newVars.map(_.id)
        val internalTerm =
            try value.termInternal(gctx.copy(generatedVars = nGeneratedVars))
            catch
                case NonFatal(ex) =>
                    println(
                      s"Error generating term of type ${value.sirType.show} at ${value.pos.file}:${value.pos.startLine + 1}\n" +
                          s"value:\n${value.shortShow}\n" +
                          s"generatedVars: ${gctx.generatedVars.mkString(", ")}\n" +
                          s"newVars: ${newVars.map(_.id).mkString(", ")}\n" +
                          s"dominatingUplevelVars: ${value.dominatingUplevelVars.map(_.id).mkString(", ")}"
                    )
                    throw ex

        val topSortedNewVars = topologicalSort(newVars)

        val retval = topSortedNewVars.foldLeft(internalTerm) { case (term, v) =>
            val nGctx = gctx.copy(
              generatedVars = gctx.generatedVars + v.id
            )
            v match
                case dv: DependendVariableLoweredValue =>
                    Term.Apply(
                      Term.LamAbs(dv.id, term, UplcAnnotation(dv.pos)),
                      dv.rhs.termWithNeededVars(nGctx),
                      UplcAnnotation(dv.pos)
                    )
                case v: VariableLoweredValue =>
                    v.optRhs match
                        case Some(rhs) =>
                            Term.Apply(
                              Term.LamAbs(v.id, term, UplcAnnotation(v.pos)),
                              rhs.termWithNeededVars(nGctx),
                              UplcAnnotation(v.pos)
                            )
                        case None =>
                            throw LoweringException(
                              s"Unexpected variable $v (id=${v.id}, " +
                                  s"name=${v.name}, pos=${v.pos.show}) is not in scope",
                              value.pos
                            )
        }

        retval
    }

    /** Generate uplevel vars access for non-effortless variables only (lambda barrier).
      *
      * This is used in lambda bodies to prevent non-effortless computations from being captured
      * inside the lambda, which would cause them to be re-evaluated on each lambda call. Effortless
      * variables (constants, vars, lambdas) can be safely captured.
      */
    def generateDominatedNonEffortlessUplevelVarsAccess(
        value: LoweredValue,
        innerTerm: Term
    )(using gctx: TermGenerationContext): Term = {
        // Filter to only non-effortless dominating uplevel vars
        val nonEffortlessVars = value.dominatingUplevelVars.filter(!_.isEffortLess)
        val newVars = nonEffortlessVars.filterNot(x => gctx.generatedVars.contains(x.id))

        if newVars.isEmpty then innerTerm
        else
            val topSortedNewVars = topologicalSort(newVars)

            topSortedNewVars.foldLeft(innerTerm) { case (term, v) =>
                val nGctx = gctx.copy(
                  generatedVars = gctx.generatedVars + v.id
                )
                v match
                    case dv: DependendVariableLoweredValue =>
                        Term.Apply(
                          Term.LamAbs(dv.id, term, UplcAnnotation(dv.pos)),
                          dv.rhs.termWithNeededVars(nGctx),
                          UplcAnnotation(dv.pos)
                        )
                    case v: VariableLoweredValue =>
                        v.optRhs match
                            case Some(rhs) =>
                                Term.Apply(
                                  Term.LamAbs(v.id, term, UplcAnnotation(v.pos)),
                                  rhs.termWithNeededVars(nGctx),
                                  UplcAnnotation(v.pos)
                                )
                            case None =>
                                throw LoweringException(
                                  s"Unexpected variable $v is not in scope",
                                  value.pos
                                )
            }
    }

    def addUsedVarsToCounts(
        vars: Set[IdentifiableLoweredValue],
        counts: Map[IdentifiableLoweredValue, Int]
    ): Map[IdentifiableLoweredValue, Int] = {
        vars.foldLeft(counts) { case (acc, v) =>
            acc.updated(v, acc.getOrElse(v, 0) + 1)
        }
    }

    def filterAndCountVars(
        p: IdentifiableLoweredValue => Boolean,
        subvalues: LoweredValue*
    ): Map[IdentifiableLoweredValue, Int] = {
        subvalues.foldLeft(Map.empty[IdentifiableLoweredValue, Int]) { case (acc, leaf) =>
            val usedVars = leaf.usedUplevelVars.filter(p(_))
            addUsedVarsToCounts(usedVars, acc)
        }
    }

    def topologicalSort(values: Set[IdentifiableLoweredValue]): List[IdentifiableLoweredValue] = {
        val visited = scala.collection.mutable.Set.empty[IdentifiableLoweredValue]
        val sorted = scala.collection.mutable.ListBuffer.empty[IdentifiableLoweredValue]

        // A depends from (B,C)
        // B depends from (D)
        // C depends from (D)
        // D depends from ()
        // E deonds from (A,B,C)
        //  A, B, C, D, E
        //  visit
        //

        def visit(value: IdentifiableLoweredValue): Unit = {
            if !visited.contains(value) then
                visited.add(value)
                value.usedUplevelVars.toList.sortBy(_.id).foreach(visit)
                if values.contains(value) then sorted.append(value)
        }

        values.toList.sortBy(_.id).foreach(visit)
        sorted.toList
    }

    private def isPrimitiveElementType(tp: SIRType): Boolean = tp match
        case SIRType.Integer | SIRType.ByteString | SIRType.String | SIRType.Boolean => true
        case _                                                                       => false

    /** Convert a UPLC Constant to a Constant.Data. Used to transform BuiltinList elements to Data
      * representation.
      */
    private def constantToData(c: Constant): Constant = c match {
        case Constant.Integer(v)    => Constant.Data(scalus.uplc.builtin.Data.I(v))
        case Constant.ByteString(v) => Constant.Data(scalus.uplc.builtin.Data.B(v))
        case Constant.String(v)     =>
            // String doesn't have direct Data representation, encode as ByteString
            Constant.Data(scalus.uplc.builtin.Data.B(scalus.uplc.builtin.ByteString.fromString(v)))
        case Constant.Bool(v) =>
            // Bool encoded as Constr(0/1, [])
            Constant.Data(scalus.uplc.builtin.Data.Constr(if v then 1 else 0, PList.Nil))
        case Constant.Unit =>
            // Unit encoded as Constr(0, [])
            Constant.Data(scalus.uplc.builtin.Data.Constr(0, PList.Nil))
        case Constant.Data(d) => c // already Data
        case Constant.List(elemTpe, elements) =>
            val dataElements = elements.map { elem =>
                constantToData(elem) match {
                    case Constant.Data(d) => d
                    case other => throw new RuntimeException(s"Expected Data constant, got $other")
                }
            }
            Constant.Data(scalus.uplc.builtin.Data.List(PList.from(dataElements)))
        case Constant.Pair(a, b) =>
            val aData = constantToData(a) match {
                case Constant.Data(d) => d
                case other => throw new RuntimeException(s"Expected Data constant, got $other")
            }
            val bData = constantToData(b) match {
                case Constant.Data(d) => d
                case other => throw new RuntimeException(s"Expected Data constant, got $other")
            }
            Constant.Data(scalus.uplc.builtin.Data.List(PList.from(List(aData, bData))))
        case other =>
            throw new RuntimeException(s"Cannot convert constant $other to Data")
    }

    /** Check if a repr conversion is needed between source and target.
      *
      * Only triggers for actual representation family changes (e.g., SumBuiltinList ↔
      * SumUplcConstr, ProdDataConstr ↔ ProdUplcConstr), not for structural differences in
      * LambdaRepresentation or SumReprProxy identity.
      */
    def needsReprConversion(
        src: LoweredValueRepresentation,
        tgt: LoweredValueRepresentation,
        typeShow: => String,
        pos: SIRPosition
    ): Boolean = {
        import SumCaseClassRepresentation.*
        import ProductCaseClassRepresentation.*
        (src, tgt) match
            // TypeVar rules: Transparent and Unwrapped are passthrough native, Fixed is Data.
            // Only error on passthrough→Fixed (unknown toData).
            // All other TypeVar combinations: no conversion.
            case (
                  src: TypeVarRepresentation,
                  TypeVarRepresentation(SIRType.TypeVarKind.Fixed)
                ) if src.isBuiltin =>
                throw LoweringException(
                  s"Cannot convert ${src.kind} TypeVar to Fixed: unknown toData for type ${typeShow}",
                  pos
                )
            case (_: TypeVarRepresentation, _) | (_, _: TypeVarRepresentation) => false
            // Sum list family changes
            case (_: SumBuiltinList, _: SumUplcConstr)  => true
            case (_: SumUplcConstr, _: SumBuiltinList)  => true
            case (PackedSumDataList, _: SumUplcConstr)  => true
            case (_: SumUplcConstr, PackedSumDataList)  => true
            case (DataConstr, _: SumUplcConstr)         => true
            case (_: SumUplcConstr, DataConstr)         => true
            case (PackedSumDataList, _: SumBuiltinList) => true
            case (_: SumBuiltinList, PackedSumDataList) => true
            // Product family changes
            case (ProdDataConstr, _: ProdUplcConstr) => true
            case (_: ProdUplcConstr, ProdDataConstr) => true
            // Everything else: no conversion
            case _ => false
    }

}
