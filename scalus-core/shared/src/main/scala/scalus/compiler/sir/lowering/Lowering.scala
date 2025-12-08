package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.cardano.ledger.Language
import scalus.compiler.sir.*
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator
import scalus.pretty
import scalus.uplc.*

import scala.annotation.unused
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
            ) $ ~Term.Error)
        else Term.Error

    private lazy val builtinTerms = Meaning.allBuiltins.forcedBuiltins

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
                val typeGenerator =
                    if name == "scalus.prelude.List$.Nil" then
                        optTargetType match
                            case Some(targetType) =>
                                lctx.typeGenerator(targetType)
                            case None =>
                                lctx.typeGenerator(resolvedType)
                    else lctx.typeGenerator(resolvedType)
                typeGenerator.genConstr(constr)
            case sirMatch @ SIR.Match(scrutinee, cases, rhsType, anns) =>
                if lctx.debug then
                    lctx.log(
                      s"Lowering match: ${sir.pretty.render(100)}\n" +
                          s"  scrutinee.tp = ${scrutinee.tp.show}\n"
                    )
                val loweredScrutinee = lowerSIR(scrutinee)
                val retval = lctx
                    .typeGenerator(loweredScrutinee.sirType)
                    .genMatch(sirMatch, loweredScrutinee, optTargetType)
                if lctx.debug then
                    lctx.log(
                      s"Lowered match: ${sir.pretty.render(100)}\n" +
                          s"  retval = ${retval.pretty.render(100)}\n"
                    )
                retval
            case SIR.Var(name, tp, anns) =>
                lctx.scope.getByName(name) match
                    case Some(value) =>
                        // TODO: check types are correct
                        value.sirType match {
                            case tv: SIRType.TypeVar =>
                                // if this is type variable, try to resolve it
                                lctx.tryResolveTypeVar(tv) match
                                    case Some(resolvedType) =>
                                        val gen = lctx.typeGenerator(resolvedType)
                                        val representation =
                                            if tv.isBuiltin then
                                                gen.defaultRepresentation(resolvedType)
                                            else gen.defaultTypeVarReperesentation(resolvedType)
                                        TypeRepresentationProxyLoweredValue(
                                          value,
                                          resolvedType,
                                          representation,
                                          anns.pos
                                        )
                                    case None =>
                                        // TODO!!!: find, how this can be possible and insert checks there
                                        //  [throw]
                                        val gen = lctx.typeGenerator(tp)
                                        val repr =
                                            if tv.isBuiltin then gen.defaultRepresentation(tp)
                                            else gen.defaultTypeVarReperesentation(tp)
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
            case ev @ SIR.ExternalVar(moduleName, name, tp, _) =>
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
                        if moduleName == "scalus.builtin.internal.UniversalDataConversion$" then
                            throw LoweringException(
                              s"UniversalDataConversion function '$name' appears as a standalone variable at ${ev.anns.pos.file}:${ev.anns.pos.startLine + 1}. " +
                                  s"These functions must be applied to an argument and cannot be used as values " +
                                  s"(e.g., passed as argument, returned, or stored). " +
                                  s"This usually indicates the function is being used incorrectly in the code.",
                              ev.anns.pos
                            )
                        throw LoweringException(
                          s"External variable $name not found in the scope at ${ev.anns.pos.file}:${ev.anns.pos.startLine}",
                          ev.anns.pos
                        )
                myVar
                // StaticLoweredValue(
                //  ev,
                //  Term.Var(NamedDeBruijn(name)),
                //  SirTypeUplcGenerator(tp).defaultRepresentation
                // )
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
                              val result = loweredBody.maybeUpcast(targetType, anns.pos)
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
                            case tp1: SIRType.TypeVar =>
                            //
                            case other =>
                                println(
                                  "lowered scrutinee is typed as type variable, but scrutinee is not"
                                )
                                println(s"scrutinee: $scrutinee")
                                println(s"loweredScrutinee: $loweredScrutinee")
                                println(s"scrutinee.tp: ${scrutinee.tp.show}")
                                println(
                                  s"resolved typevar: ${lctx.typeUnifyEnv.filledTypes.get(tv)}, typeVae = ${tv}"
                                )
                                println(
                                  s"loweredScrutinee.sirType: ${loweredScrutinee.sirType.show}, representation: ${loweredScrutinee.representation}"
                                )
                                println(s"lowered scrutinee crerated at:")
                                loweredScrutinee.createdEx.printStackTrace()
                                ???
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
                // Handle BuiltinList constants - transform elements to Data if needed
                // TODO: In the future, we may want to add SumConstBuiltinList representation
                // to avoid this transformation and keep native UPLC list types
                val (transformedConst, transformedTp, repr) = tp match {
                    case SIRType.BuiltinList(elemType) if elemType != SIRType.Data =>
                        const match {
                            case Constant.List(_, elements) =>
                                val dataElements = elements.map(constantToData)
                                val newConst = Constant.List(DefaultUni.Data, dataElements)
                                val newTp = SIRType.BuiltinList(SIRType.Data)
                                (newConst, newTp, SumCaseClassRepresentation.SumDataList)
                            case _ =>
                                (const, tp, LoweredValueRepresentation.constRepresentation(tp))
                        }
                    case _ =>
                        (const, tp, LoweredValueRepresentation.constRepresentation(tp))
                }
                StaticLoweredValue(
                  SIR.Const(transformedConst, transformedTp, anns),
                  Term.Const(transformedConst),
                  repr,
                  true
                )
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
                val loweredT = lowerSIR(t, Some(tp))
                val loweredF = lowerSIR(f, Some(tp))
                lvIfThenElse(loweredCond, loweredT, loweredF, anns.pos)
            case SIR.Cast(expr, tp, anns) =>
                val loweredExpr = lowerSIR(expr, Some(tp))
                try lvCast(loweredExpr, tp, anns.pos)
                catch
                    case NonFatal(ex) =>
                        println(
                          s"Error lowering cast: ${sir.pretty.render(100)} at ${anns.pos.file}:${anns.pos.startLine + 1}"
                        )
                        // lctx.debug = true
                        // lvCast(loweredExpr, tp, anns.pos)
                        throw ex
            case sirBuiltin @ SIR.Builtin(bn, tp, anns) =>
                StaticLoweredValue(
                  sirBuiltin,
                  builtinTerms(bn),
                  SirTypeUplcGenerator(tp).defaultRepresentation(tp),
                  true
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
                        StaticLoweredValue(sirError, ~Term.Error, ErrorRepresentation, false)
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
                else StaticLoweredValue(sirError, Term.Error, ErrorRepresentation, false)
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
                        val loweredRhs = lowerSIR(b.value)
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
                            val rhsRepr =
                                SirTypeUplcGenerator(rhs.tp).defaultRepresentation(tp)
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
        else lowerNormalApp(app, optTargetType)
    }

    private def lowerNormalApp(app: SIR.Apply, @unused optTargetType: Option[SIRType])(using
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
        val prevDebug = lctx.debug
        // lctx.debug = false
        val fun = lowerSIR(app.f)
        val arg = lowerSIR(app.arg)
        // lctx.debug = prevDebug
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
                    println(
                      s"Error lowering app: ${app.pretty.render(100)} at ${app.anns.pos.file}:${app.anns.pos.startLine + 1}"
                    )
                    println(ex.getMessage)
                    println(s"=== Problematic Apply Node ===")
                    println(s"app.tp=${app.tp.show}")
                    println(s"app.tp class=${app.tp.getClass.getName}")
                    println(s"app.f class=${app.f.getClass.getSimpleName}")
                    println(s"app.f=${app.f}")
                    println(s"f.tp=${app.f.tp.show}")
                    println(s"f=${app.f.pretty.render(100)}")
                    println(s"lowered f.tp: ${fun.sirType.show}")
                    println(s"arg.tp=${app.arg.tp.show}")
                    println(s"unrolled arg.tp=${SIRType.unrollTypeProxy(app.arg.tp).show}")
                    println(s"lowered arg.tp: ${arg.sirType.show}")
                    println(s"=== End Problematic Apply Node ===")
                    lctx.debug = true
                    // redu with debug mode to see the error
                    lvApply(
                      fun,
                      arg,
                      app.anns.pos,
                      Some(app.tp),
                      None // representation can depend from fun, so should be calculated.
                    )
                    throw ex;
        result
    }

    private def isFromDataType(tp: SIRType): Boolean = tp match {
        case SIRType.Fun(SIRType.Data, _)    => true
        case SIRType.TypeLambda(params, tp1) => isFromDataType(tp1)
        case _                               => false
    }

    private def isToDataType(tp: SIRType): Boolean = tp match {
        case SIRType.Fun(_, SIRType.Data)    => true
        case SIRType.TypeLambda(params, tp1) => isToDataType(tp1)
        case _                               => false
    }

    private def isFromDataName(name: String): Boolean = {
        name == "scalus.builtin.internal.UniversalDataConversion$.fromData"
    }

    private def isToDataName(name: String): Boolean = {
        name == "scalus.builtin.internal.UniversalDataConversion$.toData"
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
                name == "scalus.builtin.internal.UniversalDataConversion$.toData"
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

    private def lowerToData(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {
        if SIRType.isPolyFunOrFun(app.arg.tp) then {
            println(
              s"Warning: lowering ToData for poly function ${app.arg.pretty.render(100)} at ${app.anns.pos.file}:${app.anns.pos.startLine + 1}"
            )
            println(
              s" app= ${app.pretty.render(100)}"
            )
            println(
              s"  app.arg.tp = ${app.arg.tp.show}, app.tp = ${app.tp.show}, app.f = ${app.f.pretty.render(100)}"
            )
            throw LoweringException(
              s"Argument of toData should be a data type, but got ${app.arg.tp.show}",
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
                PrimitiveRepresentation.PackedData
            override def termInternal(gctx: TermGenerationContext): Term =
                value.termInternal(gctx)

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val left = Doc.text("FromData(")
                val right = Doc.text(s", ${app.tp.show})")
                value.docRef(ctx).bracketBy(left, right)
            }

            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val left = Doc.text("FromData(")
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
                    Term.Apply(Term.LamAbs(dv.id, term), dv.rhs.termWithNeededVars(nGctx))
                case v: VariableLoweredValue =>
                    v.optRhs match
                        case Some(rhs) =>
                            Term.Apply(Term.LamAbs(v.id, term), rhs.termWithNeededVars(nGctx))
                        case None =>
                            throw LoweringException(
                              s"Unexpected variable $v is not in scope",
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
                        Term.Apply(Term.LamAbs(dv.id, term), dv.rhs.termWithNeededVars(nGctx))
                    case v: VariableLoweredValue =>
                        v.optRhs match
                            case Some(rhs) =>
                                Term.Apply(Term.LamAbs(v.id, term), rhs.termWithNeededVars(nGctx))
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

    /** Convert a UPLC Constant to a Constant.Data. Used to transform BuiltinList elements to Data
      * representation. TODO: In the future, we may want to add SumConstBuiltinList representation
      * to avoid this transformation and keep native UPLC list types.
      */
    private def constantToData(c: Constant): Constant = c match {
        case Constant.Integer(v)    => Constant.Data(scalus.builtin.Data.I(v))
        case Constant.ByteString(v) => Constant.Data(scalus.builtin.Data.B(v))
        case Constant.String(v)     =>
            // String doesn't have direct Data representation, encode as ByteString
            Constant.Data(scalus.builtin.Data.B(scalus.builtin.ByteString.fromString(v)))
        case Constant.Bool(v) =>
            // Bool encoded as Constr(0/1, [])
            Constant.Data(scalus.builtin.Data.Constr(if v then 1 else 0, Nil))
        case Constant.Unit =>
            // Unit encoded as Constr(0, [])
            Constant.Data(scalus.builtin.Data.Constr(0, Nil))
        case Constant.Data(d) => c // already Data
        case Constant.List(elemTpe, elements) =>
            val dataElements = elements.map { elem =>
                constantToData(elem) match {
                    case Constant.Data(d) => d
                    case other => throw new RuntimeException(s"Expected Data constant, got $other")
                }
            }
            Constant.Data(scalus.builtin.Data.List(dataElements))
        case Constant.Pair(a, b) =>
            val aData = constantToData(a) match {
                case Constant.Data(d) => d
                case other => throw new RuntimeException(s"Expected Data constant, got $other")
            }
            val bData = constantToData(b) match {
                case Constant.Data(d) => d
                case other => throw new RuntimeException(s"Expected Data constant, got $other")
            }
            Constant.Data(scalus.builtin.Data.List(List(aData, bData)))
        case other =>
            throw new RuntimeException(s"Cannot convert constant $other to Data")
    }

}
