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
                TypeVarRepresentation(SIRType.TypeVarKind.CanBeListAffected)
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
        TypeVarRepresentation(SIRType.TypeVarKind.CanBeListAffected)
    }

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        tp match {
            case tv: SIRType.TypeVar if tv.kind == SIRType.TypeVarKind.DefaultRepresentation =>
                // DefaultRepresentation: always native — TypeVar is used for inspection (Eq/Ord)
                defaultRepresentation(tp)
            case _ =>
                if lctx.nativeTypeVarRepresentation then defaultRepresentation(tp)
                else defaultDataRepresentation(tp)
        }

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
        // to packed data or lambda
        if input.representation == representation then input
        else {
            makeResolvedProxy(input, pos)
                .map(x => lctx.typeGenerator(x.sirType).toRepresentation(x, representation, pos))
                .getOrElse {
                    representation match
                        case tvr: TypeVarRepresentation =>
                            if tvr.isBuiltin then input
                            else // TODO: think about conversion between built-in and non-built-in
                                new RepresentationProxyLoweredValue(input, representation, pos)
                        case sumRepr: SumCaseClassRepresentation =>
                            sumRepr match {
                                case SumCaseClassRepresentation.DataConstr |
                                    SumCaseClassRepresentation.DataData =>
                                    new RepresentationProxyLoweredValue(input, representation, pos)
                                case SumCaseClassRepresentation.UplcConstr |
                                    SumCaseClassRepresentation.UplcConstrOnData |
                                    SumCaseClassRepresentation.PairIntDataList =>
                                    val r1 =
                                        input.toRepresentation(
                                          SumCaseClassRepresentation.DataConstr,
                                          pos
                                        )
                                    SumCaseSirTypeGenerator.toRepresentation(
                                      r1,
                                      representation,
                                      pos
                                    )
                                case SumCaseClassRepresentation.PackedSumDataList =>
                                    new RepresentationProxyLoweredValue(input, representation, pos)
                                case sbl @ SumCaseClassRepresentation.SumBuiltinList(elemRepr) =>
                                    val r1 = input.toRepresentation(
                                      SumCaseClassRepresentation.PackedSumDataList,
                                      pos
                                    )
                                    new SumBuiltinListSirTypeGenerator(elemRepr)
                                        .toRepresentation(r1, representation, pos)
                                case SumCaseClassRepresentation.SumDataAssocMap =>
                                    RepresentationProxyLoweredValue(input, representation, pos)
                                case spl @ SumCaseClassRepresentation.SumPairBuiltinList(_, _) =>
                                    input
                                        .toRepresentation(
                                          SumCaseClassRepresentation.SumDataAssocMap,
                                          pos
                                        )
                                        .toRepresentation(
                                          spl,
                                          pos
                                        )
                            }
                        case prodRepr: ProductCaseClassRepresentation =>
                            prodRepr match {
                                case ProductCaseClassRepresentation.ProdDataConstr =>
                                    new RepresentationProxyLoweredValue(input, representation, pos)
                                case ProductCaseClassRepresentation.PackedDataList |
                                    ProductCaseClassRepresentation.ProdDataList |
                                    ProductCaseClassRepresentation.UplcConstr |
                                    ProductCaseClassRepresentation.PairIntDataList =>
                                    val r1 = input.toRepresentation(
                                      ProductCaseClassRepresentation.ProdDataConstr,
                                      pos
                                    )
                                    ProductCaseSirTypeGenerator.toRepresentation(
                                      r1,
                                      representation,
                                      pos
                                    )
                                case PackedDataMap =>
                                    new RepresentationProxyLoweredValue(input, representation, pos)
                                case pbp: ProductCaseClassRepresentation.ProdBuiltinPair =>
                                    input
                                        .toRepresentation(ProdDataConstr, pos)
                                        .toRepresentation(
                                          pbp,
                                          pos
                                        )
                                case ProductCaseClassRepresentation.OneElementWrapper(repr) =>
                                    SIRType.retrieveConstrDecl(input.sirType) match
                                        case Left(message) =>
                                            throw LoweringException(
                                              message,
                                              pos
                                            )
                                        case Right(constrDecl) =>
                                            if constrDecl.params.length != 1 then {
                                                throw LoweringException(
                                                  s"TypeVarSirTypeGenerator can't convert ${input.sirType.show} to $representation",
                                                  pos
                                                )
                                            }
                                            val tp1 = constrDecl.params.head.tp
                                            val inrepr = lctx
                                                .typeGenerator(tp1)
                                                .defaultTypeVarReperesentation(tp1)
                                            val r1 = input.toRepresentation(inrepr, pos)
                                            new RepresentationProxyLoweredValue(
                                              r1,
                                              representation,
                                              pos
                                            )
                                // BuiltinArray representations
                                case _: ProductCaseClassRepresentation.ProdBuiltinArray =>
                                    // Convert to PackedArrayAsList for Data compatibility
                                    val r1 = input.toRepresentation(
                                      ProductCaseClassRepresentation.PackedArrayAsList,
                                      pos
                                    )
                                    new RepresentationProxyLoweredValue(r1, representation, pos)
                                case ProductCaseClassRepresentation.PackedArrayAsList =>
                                    // Already Data-compatible
                                    new RepresentationProxyLoweredValue(input, representation, pos)
                            }
                        case PrimitiveRepresentation.PackedData =>
                            new RepresentationProxyLoweredValue(input, representation, pos)
                        case PrimitiveRepresentation.Constant =>
                            if lctx.nativeTypeVarRepresentation then
                                // Value is already in native representation, no conversion needed
                                new RepresentationProxyLoweredValue(input, representation, pos)
                            else
                                val r1 = input.toRepresentation(
                                  PrimitiveRepresentation.PackedData,
                                  pos
                                )
                                input.sirType match {
                                    case p: SIRType.Primitive =>
                                        lctx.typeGenerator(p)
                                            .toRepresentation(
                                              r1,
                                              PrimitiveRepresentation.Constant,
                                              pos
                                            )
                                    case _ =>
                                        throw LoweringException(
                                          s"TypeVarSirTypeGenerator can't convert from ${input.sirType.show} to $representation",
                                          pos
                                        )
                                }
                        case ErrorRepresentation =>
                            TypeNothingSirTypeGenerator.toRepresentation(input, representation, pos)
                        case LambdaRepresentation(inRepr, outRepr) =>
                            RepresentationProxyLoweredValue(input, representation, pos)
                        case _ =>
                            throw LoweringException(
                              s"TypeVarSirTypeGenerator can't convert from ${input.sirType.show} to $representation",
                              pos
                            )
                }
        }
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

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        constr.tp match {
            case tv: SIRType.TypeVar =>
                lctx.typeUnifyEnv.filledTypes.get(tv) match
                    case Some(tp1) =>
                        lctx.typeGenerator(tp1).genConstr(constr)
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
                            else if tv.kind == SIRType.TypeVarKind.DefaultRepresentation then {
                                    gen.defaultRepresentation(resolvedType)
                            } else gen.defaultTypeVarReperesentation(resolvedType)
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
