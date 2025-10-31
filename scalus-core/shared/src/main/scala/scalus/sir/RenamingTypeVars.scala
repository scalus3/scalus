package scalus.sir

import scalus.sir.SIR.*

object RenamingTypeVars {

    class RenamingContext(
        var renames: Map[SIRType.TypeVar, SIRType.TypeVar],
        val tvGen: SIRType.TypeVarGenerationContext,
        val proxyMap: java.util.IdentityHashMap[SIRType.TypeProxy, SIRType.TypeProxy],
        var changed: Boolean = false,
        val deepRenaming: Boolean = false,
        val renameDataDecls: Boolean = false,
        var renamedDataDecls: Map[String, DataDecl] = Map.empty,
        var renamedConstrDecls: Map[String, ConstrDecl] = Map.empty
    ) {
        def copy(
            renames: Map[SIRType.TypeVar, SIRType.TypeVar] = this.renames,
            tvGen: SIRType.TypeVarGenerationContext = this.tvGen,
            proxyMap: java.util.IdentityHashMap[SIRType.TypeProxy, SIRType.TypeProxy] =
                this.proxyMap,
            changed: Boolean = this.changed,
            deepRenaming: Boolean = this.deepRenaming,
            renameDateDecls: Boolean = this.renameDataDecls,
            renamedDataDecls: Map[String, DataDecl] = this.renamedDataDecls,
            renamedConstrDecls: Map[String, ConstrDecl] = this.renamedConstrDecls
        ): RenamingContext =
            new RenamingContext(
              renames,
              tvGen,
              proxyMap,
              changed,
              deepRenaming,
              renameDateDecls,
              renamedDataDecls,
              renamedConstrDecls
            )
    }

    def makeContext(
        renames: Map[SIRType.TypeVar, SIRType.TypeVar],
        tvGen: SIRType.TypeVarGenerationContext
    ): RenamingContext = {
        new RenamingContext(
          renames = renames,
          tvGen = tvGen,
          proxyMap = new java.util.IdentityHashMap[SIRType.TypeProxy, SIRType.TypeProxy]()
        )
    }

    def inSir(sir: SIR, ctx: RenamingContext): SIR =
        sir match {
            case SIR.Decl(data, term) =>
                val newData =
                    if ctx.renameDataDecls then inDataDecl(data, ctx)
                    else data
                val renamedTerm = inSir(term, ctx)
                // we assume that DataDecls in SIR are global
                SIR.Decl(newData, renamedTerm)
            case other: AnnotatedSIR =>
                inSirExpr(other, ctx)
        }

    def inSirExpr(
        expr: AnnotatedSIR,
        ctx: RenamingContext
    ): AnnotatedSIR = {
        expr match {
            case SIR.Apply(f, arg, tp, anns) =>
                SIR.Apply(
                  inSirExpr(f, ctx),
                  inSirExpr(arg, ctx),
                  inType(tp, ctx),
                  anns
                )
            case b @ SIR.Builtin(name, tp, anns) =>
                b
            case SIR.Const(value, tp, anns) =>
                SIR.Const(value, inType(tp, ctx), anns)
            case SIR.Constr(name, dataDecl, args, tp, anns) =>
                SIR.Constr(
                  name,
                  inDataDecl(dataDecl, ctx),
                  args.map(inSir(_, ctx)),
                  inType(tp, ctx),
                  anns
                )
            case SIR.LamAbs(name, term, typeParams, anns) =>
                val intersected = typeParams.filter(ctx.renames.contains)
                if intersected.isEmpty then
                    SIR.LamAbs(
                      name,
                      inSir(term, ctx),
                      typeParams,
                      anns
                    )
                else
                    // we need to rename type parameters in the new context where intersection is excluded.
                    val newContext = ctx.copy(renames = ctx.renames.removedAll(intersected))
                    SIR.LamAbs(
                      name,
                      inSir(term, newContext),
                      typeParams.map(tv => newContext.renames.getOrElse(tv, tv)),
                      anns
                    )
            case SIR.Let(bindings, body, flags, anns) =>
                SIR.Let(
                  bindings.map { binding =>
                      Binding(
                        binding.name,
                        inType(binding.tp, ctx),
                        inSir(binding.value, ctx)
                      )
                  },
                  inSir(body, ctx),
                  flags,
                  anns
                )
            case SIR.Select(s, field, tp, anns) =>
                SIR.Select(
                  inSir(s, ctx),
                  field,
                  inType(tp, ctx),
                  anns
                )
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                SIR.IfThenElse(
                  inSirExpr(cond, ctx),
                  inSirExpr(t, ctx),
                  inSirExpr(f, ctx),
                  inType(tp, ctx),
                  anns
                )
            case SIR.Match(scrutinee, cases, tp, anns) =>
                SIR.Match(
                  inSirExpr(scrutinee, ctx),
                  cases.map { case SIR.Case(pattern, body, anns) =>
                      SIR.Case(
                        inSirCasePattern(pattern, ctx),
                        inSir(body, ctx),
                        anns
                      )
                  },
                  inType(tp, ctx),
                  anns
                )
            case SIR.And(lhs, rhs, anns) =>
                SIR.And(
                  inSirExpr(lhs, ctx),
                  inSirExpr(rhs, ctx),
                  anns
                )
            case SIR.Or(lhs, rhs, anns) =>
                SIR.Or(
                  inSirExpr(lhs, ctx),
                  inSirExpr(rhs, ctx),
                  anns
                )
            case SIR.Not(term, anns) =>
                SIR.Not(
                  inSirExpr(term, ctx),
                  anns
                )
            case SIR.Var(name, tp, anns) =>
                SIR.Var(
                  name,
                  inType(tp, ctx),
                  anns
                )
            case SIR.ExternalVar(module, name, tp, anns) =>
                SIR.ExternalVar(
                  module,
                  name,
                  inType(tp, ctx),
                  anns
                )
            case err @ SIR.Error(msg, anns, cause) => err
            case SIR.Cast(expr, tp, anns)          =>
                // we should check
                tp match {
                    case SIRType.TypeLambda(tpTypeVars, body) =>
                        val intersected = tpTypeVars.filter(ctx.renames.contains)
                        if intersected.isEmpty then
                            SIR.Cast(
                              inSirExpr(expr, ctx),
                              inType(tp, ctx),
                              anns
                            )
                        else
                            // we need to rename type parameters in the new context where intersection is excluded.
                            val newContext = ctx.copy(renames = ctx.renames.removedAll(intersected))
                            SIR.Cast(
                              inSirExpr(expr, newContext),
                              inType(tp, newContext),
                              anns
                            )
                    case _ =>
                        SIR.Cast(
                          inSirExpr(expr, ctx),
                          inType(tp, ctx),
                          anns
                        )
                }
        }
    }

    def inSirCasePattern(pattern: Pattern, context: RenamingContext): Pattern = {
        pattern match {
            case Pattern.Constr(name, bindings, typeBindings) =>
                Pattern.Constr(
                  name,
                  bindings,
                  typeBindings.map(inType(_, context))
                )
            case Pattern.Const(value) =>
                Pattern.Const(inSirExpr(value, context).asInstanceOf[SIR.Const])
            case Pattern.Wildcard => Pattern.Wildcard
        }
    }

    def inType(tp: SIRType, ctx: RenamingContext): SIRType = {
        tp match {
            case p: SIRType.Primitive =>
                p
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                SIRType.CaseClass(
                  inConstrDecl(constrDecl, ctx),
                  typeArgs.map(inType(_, ctx)),
                  optParent.map(inType(_, ctx))
                )
            case SIRType.SumCaseClass(dataDecl, typeArgs) =>
                SIRType.SumCaseClass(
                  inDataDecl(dataDecl, ctx),
                  typeArgs.map(inType(_, ctx))
                )
            case SIRType.Fun(argType, returnType) =>
                SIRType.Fun(
                  inType(argType, ctx),
                  inType(returnType, ctx)
                )
            case SIRType.TypeLambda(typeVars, body) =>
                // if no intersection, we can just rename type variables
                // if there is an intersection, we renaming in the new context where intersection is excluded.
                val (intersection, nonIntersection) = typeVars.partition(ctx.renames.contains)
                if intersection.isEmpty then SIRType.TypeLambda(typeVars, inType(body, ctx))
                else
                    val newContext = ctx.copy(
                      renames = ctx.renames.removedAll(intersection)
                    )
                    val newBody = inType(body, newContext)
                    SIRType.TypeLambda(
                      typeVars.map(tv => ctx.renames.getOrElse(tv, tv)),
                      newBody
                    )
            case SIRType.FreeUnificator          => SIRType.FreeUnificator
            case SIRType.TypeNothing             => SIRType.TypeNothing
            case SIRType.TypeNonCaseModule(name) => SIRType.TypeNonCaseModule(name)
            case tv: SIRType.TypeVar             =>
                if ctx.renames.contains(tv) then
                    ctx.changed = true
                    ctx.renames(tv)
                else tv
            case proxy: SIRType.TypeProxy =>
                Option(ctx.proxyMap.get(proxy)) match {
                    case Some(renamed) =>
                        if renamed.ref != null then
                            if renamed.ref != proxy.ref then ctx.changed = true
                        renamed
                    case None =>
                        val newProxy = SIRType.TypeProxy(null)
                        ctx.proxyMap.put(proxy, newProxy)
                        val nCtx = ctx.copy(changed = false)
                        val nRef = inType(proxy.ref, nCtx)
                        if nCtx.changed then
                            ctx.changed = true
                            newProxy.ref = nRef
                        else newProxy.ref = proxy.ref
                        newProxy
                }
        }
    }

    def inDataDecl(dataDecl: DataDecl, ctx: RenamingContext): DataDecl = {
        if ctx.renameDataDecls then
            val renamedConstrs = dataDecl.constructors.map { constr =>
                inConstrDecl(constr, ctx)
            }
            val renamedTypeParams = dataDecl.typeParams.map(tv =>
                if ctx.tvGen.contains(tv) then ctx.tvGen.freshCopy(tv) else tv
            )
            // val localRenames =
            //    dataDecl.typeParams.zip(renamedTypeParams).filter(x => x._1 != x._2).toMap
            val renamed = DataDecl(
              dataDecl.name,
              renamedConstrs,
              renamedTypeParams,
              dataDecl.annotations
            )
            ctx.renamedDataDecls = ctx.renamedDataDecls.updated(
              dataDecl.name,
              renamed
            )
            renamed
        else dataDecl
    }

    def inConstrDecl(constrDecl: ConstrDecl, ctx: RenamingContext): ConstrDecl = {
        if ctx.renameDataDecls then
            ctx.renamedConstrDecls.get(constrDecl.name) match
                case Some(renamed) =>
                    renamed
                case None =>
                    val renamedTypeParams = constrDecl.typeParams.map(tv =>
                        if ctx.tvGen.contains(tv) then ctx.tvGen.freshCopy(tv) else tv
                    )
                    val localRenames =
                        constrDecl.typeParams.zip(renamedTypeParams).filter(x => x._1 != x._2).toMap
                    val locaCtx = ctx.copy(
                      renames = ctx.renames ++ localRenames,
                      changed = false
                    )
                    val renamed = ConstrDecl(
                      constrDecl.name,
                      constrDecl.params.map(b => TypeBinding(b.name, inType(b.tp, locaCtx))),
                      renamedTypeParams,
                      constrDecl.parentTypeArgs
                          .map(pt => inType(pt, locaCtx)),
                      constrDecl.annotations
                    )
                    ctx.renamedConstrDecls = ctx.renamedConstrDecls.updated(
                      constrDecl.name,
                      renamed
                    )
                    renamed
        else constrDecl
    }

}
