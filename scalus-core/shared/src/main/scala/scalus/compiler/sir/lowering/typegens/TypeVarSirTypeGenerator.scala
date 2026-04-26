package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.ProductCaseClassRepresentation.*
import scalus.compiler.sir.*

/** We assume that type variable can be converted to data, and it is impossible to do something
  * meaningful with it in the UPLC, except pass the value as an argument in unchanged form.
  */
object TypeVarSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation = {
        tp match
            case tv: SIRType.TypeVar =>
                TypeVarRepresentation(tv.kind)
            case SIRType.FreeUnificator =>
                TypeVarRepresentation(SIRType.TypeVarKind.Fixed)
            case SIRType.TypeLambda(params, body) =>
                defaultRepresentation(body)
            case _ =>
                throw IllegalStateException(
                  s"TypeVarSirTypeGenerator can't be used for type ${tp.show}",
                )
    }

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        // Requesting the Data representation of a Transparent or Unwrapped TypeVar is a bug:
        // both mean "no Data wrapping". Returning TypeVarRepresentation(Fixed) silently labels
        // a passthrough value as Data, which downstream code takes as permission to call
        // Data-only builtins on what may be a native Constr. Fail fast so the call site can
        // route through a passthrough-safe path or resolve the TypeVar to a concrete type first.
        tp match
            case tv: SIRType.TypeVar if tv.isBuiltin =>
                throw LoweringException(
                  s"defaultDataRepresentation requested on ${tv.kind} TypeVar ${tv.name}" +
                      s"(${tv.optId.getOrElse("-")}). Passthrough TypeVars must not be coerced " +
                      s"to Data repr — the caller should stay in the TypeVar's native passthrough " +
                      s"repr or resolve the TypeVar to a concrete type first.",
                  SIRPosition.empty
                )
            case _ =>
                TypeVarRepresentation(SIRType.TypeVarKind.Fixed)
    }

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        // For a passthrough TypeVar (Transparent or Unwrapped), the "default TypeVar repr" is
        // the same kind itself — the value passes through in whatever native form its enclosing
        // scope uses. Only Fixed kinds fall back to the Data-equivalent Fixed repr.
        tp match
            case tv: SIRType.TypeVar if tv.isBuiltin =>
                TypeVarRepresentation(tv.kind)
            case _ => defaultDataRepresentation(tp)

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = {
        tp match {
            case tv: SIRType.TypeVar =>
                lctx.typeUnifyEnv.filledTypes.get(tv) match
                    case Some(resolvedType) =>
                        lctx.typeGenerator(resolvedType).canBeConvertedToData(resolvedType)
                    case None =>
                        // for now we assume that type variable can be converted to data
                        // TODO: change when we will implement all representations.
                        true
            case SIRType.FreeUnificator => true
            case other                  =>
                // strange, in theory
                throw IllegalStateException(
                  s"TypeVarSirTypeGenerator can't be used for type ${other.show}",
                )
        }
    }

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        if input.representation == representation then return input

        // First try to resolve TypeVar via filledTypes — if concrete, dispatch to that
        // type's generator which knows the actual shape.
        val resolvedOpt = makeResolvedProxy(input, pos)
        if resolvedOpt.isDefined then
            val resolved = resolvedOpt.get
            return lctx
                .typeGenerator(resolved.sirType)
                .toRepresentation(resolved, representation, pos)

        // Source is an unresolved TypeVar. Dispatch by its kind.
        val srcKind = input.representation match
            case TypeVarRepresentation(k) => k
            case other =>
                throw LoweringException(
                  s"TypeVarSirTypeGenerator: expected TypeVarRepresentation for input " +
                      s"of TypeVar-typed value, got $other",
                  pos
                )

        import SIRType.TypeVarKind.*
        srcKind match
            case Transparent =>
                // Wildcard source — bytes are unknown.
                representation match
                    case TypeVarRepresentation(Unwrapped) =>
                        // Unwrapped requires bytes in defaultRepresentation form. We can
                        // only honor that if the sirType is concrete enough to compute
                        // defaultRepresentation. For an unresolved TypeVar sirType: error.
                        input.sirType match
                            case _: SIRType.TypeVar =>
                                throw LoweringException(
                                  s"TypeVarSirTypeGenerator: cannot convert Transparent TypeVar " +
                                      s"to Unwrapped on an unresolved TypeVar sirType — " +
                                      s"Unwrapped needs concrete-default form, but type is unknown. " +
                                      s"Type: ${input.sirType.show}",
                                  pos
                                )
                            case concrete =>
                                // sirType is concrete — convert via the type generator and
                                // relabel as Unwrapped.
                                val targetRepr =
                                    lctx.typeGenerator(concrete).defaultRepresentation(concrete)
                                val converted = input.toRepresentation(targetRepr, pos)
                                if converted.representation == representation then converted
                                else
                                    new RepresentationProxyLoweredValue(
                                      converted,
                                      representation,
                                      pos
                                    )
                    case _ =>
                        new RepresentationProxyLoweredValue(input, representation, pos)

            case Unwrapped =>
                // Bytes are in concrete-default form. Without knowing the concrete type
                // we can only relabel to another Unwrapped TypeVar repr. Transparent is
                // NOT interchangeable (semantics differ).
                representation match
                    case TypeVarRepresentation(Unwrapped) =>
                        new RepresentationProxyLoweredValue(input, representation, pos)
                    case TypeVarRepresentation(Transparent) =>
                        throw LoweringException(
                          s"TypeVarSirTypeGenerator: cannot convert Unwrapped TypeVar to " +
                              s"Transparent — kinds have distinct semantics " +
                              s"(Unwrapped = concrete-default form; Transparent = unknown). " +
                              s"Type: ${input.sirType.show}",
                          pos
                        )
                    case _ =>
                        throw LoweringException(
                          s"TypeVarSirTypeGenerator: cannot convert Unwrapped TypeVar to " +
                              s"$representation without resolving the concrete type. " +
                              s"Type: ${input.sirType.show}",
                          pos
                        )

            case Fixed =>
                // Bytes are in the TypeVar default (Data-wrapped) form.
                representation match
                    case TypeVarRepresentation(Fixed) | TypeVarRepresentation(Transparent) =>
                        new RepresentationProxyLoweredValue(input, representation, pos)
                    case TypeVarRepresentation(Unwrapped) =>
                        // Look up the caller's concrete repr for this TypeVar. If present,
                        // route through the concrete repr's Fixed→concrete conversion and
                        // relabel as Unwrapped (Unwrapped means "bytes are in concrete-default
                        // form", which is what we just produced).
                        val tvOpt = input.sirType match
                            case t: SIRType.TypeVar => Some(t)
                            case _                  => None
                        val reprOpt = tvOpt.flatMap(lctx.typeVarReprEnv.get)
                        reprOpt match
                            case Some(concreteRepr) =>
                                val converted = input.toRepresentation(concreteRepr, pos)
                                if converted.representation == representation then converted
                                else
                                    new RepresentationProxyLoweredValue(
                                      converted,
                                      representation,
                                      pos
                                    )
                            case None =>
                                val tvStr = tvOpt
                                    .map(t => s"${t.name}#${t.optId.getOrElse("-")}")
                                    .getOrElse(input.sirType.show)
                                val envVals = lctx.typeVarReprEnv
                                    .map { case (k, v) =>
                                        s"${k.name}#${k.optId.getOrElse("-")}->$v"
                                    }
                                    .mkString(",")
                                throw LoweringException(
                                  s"TypeVarSirTypeGenerator: cannot convert Fixed (Data) TypeVar to " +
                                      s"Unwrapped without resolving the concrete type. " +
                                      s"Type: ${input.sirType.show}, TypeVar: $tvStr\n" +
                                      s"typeVarReprEnv: {$envVals}",
                                  pos
                                )
                    case _ =>
                        convertAbstractFixedToTarget(input, representation, pos)
    }

    /** Dispatch a Fixed-source TypeVar value to a non-TypeVar target representation.
      *
      * Suspected DEAD CODE: TypeVarSirTypeGenerator is only invoked when `input.sirType` is a
      * TypeVar; in that case the only sensible targets are other TypeVar reprs (handled inline by
      * the caller). Concrete reprs as targets here would mean we have a Fixed-labeled value with
      * TypeVar sirType but the consumer expects a concrete shape — a path that should never trigger
      * if upstream resolution is correct.
      *
      * Kept temporarily with a print so we can enumerate any real call sites; convert to
      * `throw LoweringException` once we confirm none exist.
      */
    private def convertAbstractFixedToTarget(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        println(
          s"[TODO TypeVarSirTypeGenerator] convertAbstractFixedToTarget invoked at $pos: " +
              s"sirType=${input.sirType.show} target=$representation"
        )
        representation match
            case PrimitiveRepresentation.PackedData | SumCaseClassRepresentation.DataData |
                SumCaseClassRepresentation.DataConstr |
                SumCaseClassRepresentation.PackedSumDataList |
                ProductCaseClassRepresentation.ProdDataConstr |
                ProductCaseClassRepresentation.PackedDataList |
                ProductCaseClassRepresentation.ProdDataList |
                ProductCaseClassRepresentation.PairIntDataList |
                ProductCaseClassRepresentation.PackedDataMap |
                ProductCaseClassRepresentation.PackedArrayAsList |
                SumCaseClassRepresentation.SumDataAssocMap =>
                new RepresentationProxyLoweredValue(input, representation, pos)
            case _: ProductCaseClassRepresentation.ProdUplcConstr |
                _: SumCaseClassRepresentation.SumUplcConstr =>
                new RepresentationProxyLoweredValue(input, representation, pos)
            case sbl @ SumCaseClassRepresentation.SumBuiltinList(elemRepr) =>
                val r1 = input.toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
                new SumBuiltinListSirTypeGenerator(elemRepr)
                    .toRepresentation(r1, representation, pos)
            case spl @ SumCaseClassRepresentation.SumPairBuiltinList(_, _) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataAssocMap, pos)
                    .toRepresentation(spl, pos)
            case pbp: ProductCaseClassRepresentation.ProdBuiltinPair =>
                input
                    .toRepresentation(ProductCaseClassRepresentation.ProdDataConstr, pos)
                    .toRepresentation(pbp, pos)
            case _: ProductCaseClassRepresentation.ProdBuiltinArray =>
                val r1 = input.toRepresentation(
                  ProductCaseClassRepresentation.PackedArrayAsList,
                  pos
                )
                new RepresentationProxyLoweredValue(r1, representation, pos)
            case ProductCaseClassRepresentation.OneElementWrapper(_) =>
                SIRType.retrieveConstrDecl(input.sirType) match
                    case Left(message) =>
                        throw LoweringException(message, pos)
                    case Right(constrDecl) =>
                        if constrDecl.params.length != 1 then
                            throw LoweringException(
                              s"TypeVarSirTypeGenerator can't convert ${input.sirType.show} to $representation",
                              pos
                            )
                        val tp1 = constrDecl.params.head.tp
                        val inrepr =
                            lctx.typeGenerator(tp1).defaultTypeVarReperesentation(tp1)
                        val r1 = input.toRepresentation(inrepr, pos)
                        new RepresentationProxyLoweredValue(r1, representation, pos)
            case PrimitiveRepresentation.Constant =>
                val r1 = input.toRepresentation(PrimitiveRepresentation.PackedData, pos)
                input.sirType match
                    case p: SIRType.Primitive =>
                        lctx.typeGenerator(p)
                            .toRepresentation(r1, PrimitiveRepresentation.Constant, pos)
                    case _ =>
                        throw LoweringException(
                          s"TypeVarSirTypeGenerator can't convert from ${input.sirType.show} to $representation",
                          pos
                        )
            case ErrorRepresentation =>
                TypeNothingSirTypeGenerator.toRepresentation(input, representation, pos)
            case _: LambdaRepresentation =>
                new RepresentationProxyLoweredValue(input, representation, pos)
            case _ =>
                throw LoweringException(
                  s"TypeVarSirTypeGenerator can't convert from ${input.sirType.show} to $representation",
                  pos
                )
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        makeResolvedProxy(input, pos)
            .map(input1 => lctx.typeGenerator(input1.sirType).upcastOne(input1, targetType, pos))
            .getOrElse(
              throw LoweringException(
                s"TypeVarSirTypeGenerator does not support upcasting, got $targetType",
                pos
              )
            )
    }

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        constr.tp match {
            case tv: SIRType.TypeVar =>
                lctx.typeUnifyEnv.filledTypes.get(tv) match
                    case Some(tp1) =>
                        lctx.typeGenerator(tp1)
                            .genConstrLowered(constr, loweredArgs, optTargetType)
                    case None =>
                        throw LoweringException(
                          s"TypeVarSirTypeGenerator does not support constructors, got ${constr.name}",
                          constr.anns.pos
                        )
            case other =>
                throw LoweringException(
                  s"Internal error: call of TypeVarSirTypeGenerator.genConstr for type ${other.show}",
                  constr.anns.pos
                )
        }

    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        makeResolvedProxy(loweredScrutinee, sel.anns.pos)
            .map(input => lctx.typeGenerator(input.sirType).genSelect(sel, input))
            .getOrElse(
              throw LoweringException(
                s"Can't select on unresolved type variable ${loweredScrutinee.sirType.show}",
                sel.anns.pos
              )
            )
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        makeResolvedProxy(loweredScrutinee, matchData.anns.pos)
            .map(input =>
                lctx.typeGenerator(input.sirType).genMatch(matchData, input, optTargetType)
            )
            .getOrElse(
              throw LoweringException(
                s"TypeVarSirTypeGenerator does not support match",
                matchData.anns.pos
              )
            )
    }

    def makeResolvedProxy(
        input: LoweredValue,
        inPos: SIRPosition
    )(using lctx: LoweringContext): Option[LoweredValue] = {
        input.sirType match
            case tv: SIRType.TypeVar =>
                lctx.typeUnifyEnv.filledTypes.get(tv) match
                    case Some(resolvedType) =>
                        val gen = lctx.typeGenerator(resolvedType)
                        val repr =
                            if tv.isBuiltin then gen.defaultRepresentation(resolvedType)
                            else gen.defaultTypeVarReperesentation(resolvedType)
                        val proxy = new TypeRepresentationProxyLoweredValue(
                          input,
                          resolvedType,
                          repr,
                          inPos
                        )
                        Some(proxy)
                    case None =>
                        None
            case _ => None

    }

}
