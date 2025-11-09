package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.sir.*

import scala.annotation.tailrec

/** representation, depends on the type of the value.
  */
sealed trait LoweredValueRepresentation {
    def isPackedData: Boolean
    def isDataCentric: Boolean
    def isCompatibleOn(tp: SIRType, repr: LoweredValueRepresentation, pos: SIRPosition)(using
        lctx: LoweringContext
    ): Boolean =
        this == repr
    def isCompatibleWithType(tp: SIRType): Boolean
    def doc: Doc = Doc.text(this.toString)
    def show = doc.render(80)
}

sealed trait SumCaseClassRepresentation(
    override val isPackedData: Boolean,
    override val isDataCentric: Boolean
) extends LoweredValueRepresentation {

    override def isCompatibleWithType(tp: SIRType): Boolean = {
        SIRType.isSum(tp)
    }

}

object SumCaseClassRepresentation {

    /** Representation for sum case classes that are represented as a Data with DataConstr and
      * DataUnconstr operators to work with the data. the index of the constructor and x is a field.
      */
    case object DataConstr extends SumCaseClassRepresentation(true, true) {
        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): Boolean =
            repr match {
                case DataConstr               => true
                case TypeVarRepresentation(_) => true
                case other                    => false
            }

    }

    /** Representation for sum case classes that are represented as a Pair of Int and DataList.
      */
    case object PairIntDataList extends SumCaseClassRepresentation(false, true)

    /** Representation for sum case classes that are represented as a list of data elements. unlike
      * `DataConstr`, this representation does not use a constructor tag, but use unList and
      * unListData to work with the data.
      */
    case object SumDataList extends SumCaseClassRepresentation(false, true) {
        override def isCompatibleWithType(tp: SIRType): Boolean = {
            SIRType.retrieveDataDecl(tp) match
                case Left(_) => false
                case Right(decl) =>
                    decl.name == SIRType.List.dataDecl.name || decl.name == SIRType.BuiltinList.dataDecl.name
        }
    }

    /** List of pairs of data elements. result of unMapData
      */
    case object SumDataPairList extends SumCaseClassRepresentation(false, true) {

        override def isCompatibleWithType(tp: SIRType): Boolean = {
            SIRType.retrieveDataDecl(tp) match
                case Left(_) => false
                case Right(decl) =>
                    val isList =
                        decl.name == SIRType.List.dataDecl.name || decl.name == SIRType.BuiltinList.dataDecl.name
                    if isList then
                        retrieveListElementType(tp) match
                            case Some(elementType) =>
                                ProductCaseClassRepresentation.PairData.isPairOrTuple2(elementType)
                            case None => false
                    else false
        }

        def retrieveListElementType(tp: SIRType): Option[SIRType] = {
            tp match
                case SIRType.SumCaseClass(decl, typeArgs) =>
                    Some(typeArgs.head)
                case SIRType.TypeLambda(params, body) =>
                    retrieveListElementType(body)
                case _ =>
                    None

        }

    }

    /** SumDataPairList packed as AssocMap
      */
    case object SumDataAssocMap extends SumCaseClassRepresentation(true, true) {
        override def isCompatibleWithType(tp: SIRType): Boolean =
            SumDataPairList.isCompatibleWithType(tp)
    }

    /** packed in data representation as a list of data elements. i.e. unListData for unpacking into
      * DataList
      */
    case object PackedSumDataList extends SumCaseClassRepresentation(true, true) {
        override def isCompatibleWithType(tp: SIRType): Boolean = {
            SumDataList.isCompatibleWithType(tp)
        }
    }

    /** Representation as tern Constr(i,x1,...,xn) where i is the index of the constructor and x is
      * a field
      */
    case object UplcConstr extends SumCaseClassRepresentation(false, false)

    /** Representation as Constr(i,x1,...,xn) where i is the index of the constructor and x is a
      * field represented as data.
      */
    case object UplcConstrOnData extends SumCaseClassRepresentation(false, true)

}

sealed trait ProductCaseClassRepresentation(val isPackedData: Boolean, val isDataCentric: Boolean)
    extends LoweredValueRepresentation {

    override def isCompatibleWithType(tp: SIRType): Boolean = {
        SIRType.isProd(tp)
    }

}

object ProductCaseClassRepresentation {

    case object PackedDataList extends ProductCaseClassRepresentation(true, true)

    case object ProdDataList extends ProductCaseClassRepresentation(false, true)

    case object PackedDataMap extends ProductCaseClassRepresentation(true, true) {
        override def isCompatibleWithType(tp: SIRType): Boolean = {
            SIRType.retrieveConstrDecl(tp) match
                case Left(_) => false
                case Right(constrDecl) =>
                    constrDecl.name == "scalus.prelude.AssocMap" || constrDecl.name == "scalus.prelude.SortedMap"
        }
    }

    /** Data.Unconstr will give us a pair from data and index of the constructor.
      */
    case object ProdDataConstr extends ProductCaseClassRepresentation(true, true) {

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using
            LoweringContext
        ): Boolean =
            repr match {
                case ProdDataConstr           => true
                case TypeVarRepresentation(_) => true
                case other                    => false
            }
    }

    case object PairIntDataList extends ProductCaseClassRepresentation(false, true)

    /** Pair[Data, Data] ( unMapData will give us a pair of data elements. )
      */
    case object PairData extends ProductCaseClassRepresentation(false, true) {

        override def isCompatibleWithType(tp: SIRType): Boolean = {
            isPairOrTuple2(tp)
        }

        @tailrec
        def isPairOrTuple2(tp: SIRType): Boolean =
            tp match
                case SIRType.CaseClass(decl, typeArgs, _) =>
                    decl.name == "scalus.builtin.BuiltinPair"
                    ||
                    decl.name == "scala.Tuple2"
                case SIRType.TypeLambda(params, body) =>
                    isPairOrTuple2(body)
                case SIRType.TypeProxy(ref) =>
                    isPairOrTuple2(ref)
                case _ => false

    }

    case object UplcConstr extends ProductCaseClassRepresentation(false, false)

    case class OneElementWrapper(representation: LoweredValueRepresentation)
        extends ProductCaseClassRepresentation(
          representation.isPackedData,
          representation.isDataCentric
        ) {

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using
            LoweringContext
        ): Boolean =
            repr match {
                case OneElementWrapper(innerRepr) =>
                    val argType = OneElementWrapper.retrieveArgType(tp, pos)
                    representation.isCompatibleOn(argType, innerRepr, pos)
                case other => representation.isCompatibleOn(tp, other, pos)
            }
    }

    object OneElementWrapper {

        def retrieveArgType(tp: SIRType, pos: SIRPosition): SIRType = {
            SIRType.collectProd(tp) match {
                case Some(typeParams, constrDecl, typeArgs) =>
                    val typeParamsMap = typeParams.zip(typeArgs).toMap
                    val param = constrDecl.params.head
                    SIRType.substitute(param.tp, typeParamsMap, Map.empty)
                case None =>
                    tp match
                        case SIRType.TypeVar(name, optId, isBuiltin) => tp
                        case SIRType.FreeUnificator                  => tp
                        case SIRType.TypeLambda(params, body) =>
                            retrieveArgType(body, pos)
                        case SIRType.TypeProxy(ref) =>
                            retrieveArgType(ref, pos)
                        case _ =>
                            throw LoweringException(
                              s"OneElementWrapper can be used only with product case classes, but got $tp",
                              pos
                            )
            }
        }

    }

}

case class InOutRepresentationPair(
    inRepr: LoweredValueRepresentation,
    outRepr: LoweredValueRepresentation
)

/** Representation for lambda function. By default, lanbda-s accept default reperesentation for
  * input and output types. But when we pass functions to type-parametrized functions, then calling
  * party does not know about real parameter types and can't use default representation, so pass
  * parameters as packed data.
  *
  * So, we translate higher-order functions to packed data representation when pass as arguments to
  * type-parametrized functions.
  *
  * But some builtin function accept more then one representations, because they poplymorhiocj on
  * plutus level. (i.e. have builtin type variables). For example: makeCons work with SumDataList
  * and SumPairDataList, so we need to reevaluate representation when argument type is known.
  */
case class LambdaRepresentation(
    funTp: SIRType,
    canonicalRepresentationPair: InOutRepresentationPair,
) extends LoweredValueRepresentation {

    override def isPackedData: Boolean = false

    override def isDataCentric: Boolean = false

    override def isCompatibleOn(tp: SIRType, repr: LoweredValueRepresentation, pos: SIRPosition)(
        using LoweringContext
    ): Boolean = {
        repr match {
            case comparator: LambdaRepresentation =>
                val (inputType, outputType) = retrieveInputAndOutputType(tp, pos)
                val InOutRepresentationPair(inRepr, outRepr) = reprFun(inputType, pos)
                val InOutRepresentationPair(otherInRepr, otherOutRepr) =
                    comparator.reprFun(inputType, pos)
                inRepr.isCompatibleOn(inputType, otherInRepr, pos) &&
                outRepr.isCompatibleOn(outputType, otherOutRepr, pos)
            case TypeVarRepresentation(isBuiltin) =>
                isBuiltin || {
                    val (inputType, outputType) = retrieveInputAndOutputType(tp, pos)
                    val InOutRepresentationPair(inRepr, outRepr) = reprFun(inputType, pos)
                    isTypeVarCompatibleOn(inputType, inRepr, pos)
                    && isTypeVarCompatibleOn(outputType, outRepr, pos)
                }
            case _ => false
        }

    }

    def reprFun(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): InOutRepresentationPair = {
        SIRType.collectPolyOrFun(funTp) match
            case Some((typeVars, input, output)) =>
                val builtinTypeVars = typeVars.filter(_.isBuiltin)
                if builtinTypeVars.isEmpty then {
                    canonicalRepresentationPair
                    // this expression is equal to canonical representation pair
                    // in princiole, we cah recheck this in paranoid mode.
                    // val (inputTypeVars, _) = SIRType.partitionGround(tvs, input)
                    // val (outputTypeVars, _) = SIRType.partitionGround(tvs, output)
                    // val nInput =
                    //    if inputTypeVars.isEmpty then input
                    //    else SIRType.TypeLambda(inputTypeVars, input)
                    // val nOutput =
                    //    if outputTypeVars.isEmpty then output
                    //    else SIRType.TypeLambda(outputTypeVars, output)
                    // if we have no type variables in input and output, then
                } else
                    // in builtin typevars we must substitute types and receive context over substituted type variables
                    val tvGenContext = SIRType.createMinimalTypeVarGenerationContext(
                      0L,
                      List(funTp)
                    )
                    val renamedTp =
                        if SIRType.isTypeVarsUsedIn(typeVars, tp) then
                            val newTpTypevars =
                                typeVars.map(tv => (tv, tvGenContext.freshCopy(tv))).toMap
                            val renamingContext =
                                RenamingTypeVars.makeContext(newTpTypevars, tvGenContext)
                            RenamingTypeVars.inType(tp, renamingContext)
                        else tp
                    SIRUnify.topLevelUnifyType(renamedTp, input, SIRUnify.Env.empty) match {
                        case SIRUnify.UnificationSuccess(env, unificator) =>
                            val builtinSubstitutes = builtinTypeVars
                                .map(tv => (tv, env.filledTypes.getOrElse(tv, tv)))
                                .toMap
                            val substitutedInput =
                                SIRType.substitute(input, builtinSubstitutes, Map.empty)
                            val substitutedOutput =
                                SIRType.substitute(output, builtinSubstitutes, Map.empty)
                            val (inputTypeVars, _) = SIRType.partitionGround(
                              typeVars,
                              substitutedInput
                            )
                            val newInput =
                                if inputTypeVars.isEmpty then substitutedInput
                                else SIRType.TypeLambda(inputTypeVars, substitutedInput)
                            val (outputTypeVars, _) = SIRType.partitionGround(
                              typeVars,
                              substitutedOutput
                            )
                            val newOutput =
                                if outputTypeVars.isEmpty then substitutedOutput
                                else SIRType.TypeLambda(outputTypeVars, substitutedOutput)
                            InOutRepresentationPair(
                              lctx.typeGenerator(newInput).defaultRepresentation(newInput),
                              lctx.typeGenerator(newOutput).defaultRepresentation(newOutput)
                            )
                        case SIRUnify.UnificationFailure(path, left, right) =>
                            throw LoweringException(
                              s"Can't unify function type $tp with input type $input and output type $output, " +
                                  s"because of unification failure: ${path}.\nLeft: ${left}, Right: ${right}",
                              pos
                            )
                    }
            case None =>
                canonicalRepresentationPair
    }

    def isTypeVarCompatibleOn(
        tp: SIRType,
        repr: LoweredValueRepresentation,
        pos: SIRPosition
    )(using LoweringContext): Boolean =
        repr match {
            case TypeVarRepresentation(_) => true
            case othrLambdaRepr @ LambdaRepresentation(otherFunTp, otherCanonicalPair) =>
                val (inputType, outputType) = retrieveInputAndOutputType(tp, pos)
                val InOutRepresentationPair(inRepr, outRepr) =
                    othrLambdaRepr.reprFun(inputType, pos)
                isTypeVarCompatibleOn(inputType, inRepr, pos) &&
                isTypeVarCompatibleOn(outputType, outRepr, pos)
            case _ => repr.isPackedData
        }

    override def isCompatibleWithType(tp: SIRType): Boolean = {
        SIRUnify.topLevelUnifyType(funTp, tp, SIRUnify.Env.empty).isSuccess
    }

    override def doc: Doc = {
        PrettyPrinter.inParens(
          canonicalRepresentationPair.inRepr.doc + Doc.text(
            " -> "
          ) + canonicalRepresentationPair.outRepr.doc
        )
    }

    private def retrieveInputAndOutputType(tp: SIRType, pos: SIRPosition): (SIRType, SIRType) = {
        tp match {
            case SIRType.Fun(in, out)                  => (in, out)
            case SIRType.TypeLambda(_, body)           => retrieveInputAndOutputType(body, pos)
            case SIRType.TypeProxy(ref)                => retrieveInputAndOutputType(ref, pos)
            case tv @ SIRType.TypeVar(_, _, isBuiltin) => (tv, tv)
            case SIRType.FreeUnificator => (SIRType.FreeUnificator, SIRType.FreeUnificator)
            case SIRType.TypeNothing    => (SIRType.TypeNothing, SIRType.TypeNothing)
            case _ =>
                throw LoweringException(
                  s"Can't retrieve input type from ${tp.show}, which is not a function type",
                  pos
                )
        }
    }

}

sealed trait PrimitiveRepresentation(val isPackedData: Boolean, val isDataCentric: Boolean)
    extends LoweredValueRepresentation {

    override def isCompatibleWithType(tp: SIRType): Boolean = {
        tp match {
            case SIRType.Integer | SIRType.Data | SIRType.ByteString | SIRType.String |
                SIRType.Boolean | SIRType.Unit | SIRType.BLS12_381_G1_Element |
                SIRType.BLS12_381_G2_Element | SIRType.BLS12_381_MlResult =>
                true
            case _ => false
        }
    }

}

object PrimitiveRepresentation {
    case object PackedData extends PrimitiveRepresentation(true, true) {
        override def isCompatibleWithType(tp: SIRType): Boolean =
            super.isCompatibleWithType(tp) && tp != SIRType.Unit && tp != SIRType.BLS12_381_MlResult
    }

    case object Constant extends PrimitiveRepresentation(false, false)
}

/** TypeVarRepresentation is used for type variables. Usually this is a synonym for some other
  * specific-type representation. When this is builtin type variable, it can be freely used in any
  * type representation, but when it is not builtin (scala type-var) it can be used only with packed
  * data representation.
  */
case class TypeVarRepresentation(isBuiltin: Boolean) extends LoweredValueRepresentation {

    // assume that TypeVarDataRepresentation is a packed data.
    //  (this is not true for lambda, will check this in code. Usually in all places we also known type)
    override def isPackedData: Boolean = !isBuiltin

    override def isDataCentric: Boolean = isPackedData

    override def isCompatibleWithType(tp: SIRType): Boolean = true

    override def doc: Doc = {
        Doc.text("TypeVar") + (if isBuiltin then Doc.text("(B)") else Doc.empty)
    }

}

case object ErrorRepresentation extends LoweredValueRepresentation {
    override def isPackedData: Boolean = false

    override def isDataCentric: Boolean = false

    override def isCompatibleWithType(tp: SIRType): Boolean = true

}

object LoweredValueRepresentation {

    def constRepresentation(tp: SIRType)(using lc: LoweringContext): LoweredValueRepresentation = {
        tp match
            case SIRType.SumCaseClass(decl, typeArgs) =>
                SumCaseClassRepresentation.DataConstr
            case SIRType.CaseClass(constrDecl, targs, parent) =>
                ProductCaseClassRepresentation.ProdDataConstr
            case SIRType.TypeLambda(params, body) =>
                constRepresentation(body)
            case SIRType.Integer | SIRType.Data | SIRType.ByteString | SIRType.String |
                SIRType.Boolean | SIRType.Unit | SIRType.BLS12_381_G1_Element |
                SIRType.BLS12_381_G2_Element | SIRType.BLS12_381_MlResult =>
                PrimitiveRepresentation.Constant
            case SIRType.Fun(in, out) =>
                val inRepresentation = lc.typeGenerator(in).defaultRepresentation(in)
                val outRepresentation = lc.typeGenerator(out).defaultRepresentation(out)
                LambdaRepresentation(
                  tp,
                  InOutRepresentationPair(inRepresentation, outRepresentation)
                )
            case tv @ SIRType.TypeVar(_, _, isBuiltin) =>
                lc.typeUnifyEnv.filledTypes.get(tv) match
                    case Some(tp) => constRepresentation(tp)
                    case None =>
                        TypeVarRepresentation(isBuiltin)
            case SIRType.FreeUnificator =>
                TypeVarRepresentation(isBuiltin = false)
            case proxy: SIRType.TypeProxy =>
                constRepresentation(proxy.ref)
            case SIRType.TypeNothing => ErrorRepresentation
            case SIRType.TypeProxy(ref) =>
                constRepresentation(ref)
            case SIRType.TypeNonCaseModule(name) =>
                throw LoweringException(
                  "TypeNonCaseModule is not supported in lowered value representation",
                  SIRPosition.empty
                )
            case null =>
                throw LoweringException(
                  "Type is null, this is a bug in the compiler",
                  SIRPosition.empty
                )
    }

}
