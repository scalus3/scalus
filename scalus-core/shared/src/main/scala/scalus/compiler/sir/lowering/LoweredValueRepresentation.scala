package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.compiler.sir.*

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

    /** Representation for the builtin Data type and its constructors (Constr, Map, List, I, B).
      * Data values are stored as raw UPLC Data.
      */
    case object DataData extends SumCaseClassRepresentation(false, true) {
        override def isCompatibleWithType(tp: SIRType): Boolean = {
            tp match
                case SIRType.SumCaseClass(decl, _) if decl.name == SIRType.Data.name => true
                case SIRType.CaseClass(constrDecl, _, _)
                    if constrDecl.name == SIRType.Data.Constr.name ||
                        constrDecl.name == SIRType.Data.Map.name ||
                        constrDecl.name == SIRType.Data.List.name ||
                        constrDecl.name == SIRType.Data.I.name ||
                        constrDecl.name == SIRType.Data.B.name =>
                    true
                case _ => false
        }

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): Boolean =
            repr match {
                case DataData                 => true
                case TypeVarRepresentation(_) => true
                case _                        => false
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

    /** Representation for BuiltinArray[Data] as a native UPLC array.
      *
      * This is the default representation for BuiltinArray, providing O(1) indexed access.
      */
    case object ArrayData extends ProductCaseClassRepresentation(false, false) {
        override def isCompatibleWithType(tp: SIRType): Boolean = {
            tp match
                case SIRType.BuiltinArray(_) => true
                case _                       => false
        }

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): Boolean =
            repr match {
                case ArrayData                => true
                case TypeVarRepresentation(_) => true
                case _                        => false
            }
    }

    /** Representation for BuiltinArray[Data] packed as Data.List.
      *
      * This is used when the array needs to be Data-compatible (e.g., stored in a case class
      * field). The array is converted to a list and then wrapped in Data.List.
      */
    case object PackedArrayAsList extends ProductCaseClassRepresentation(true, true) {
        override def isCompatibleWithType(tp: SIRType): Boolean = {
            tp match
                case SIRType.BuiltinArray(_) => true
                case _                       => false
        }
    }

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
  *
  * LambdaRepresentation has two subtypes:
  *   - Normal: standard lambda using LamAbs/Apply
  *   - Delayed: unit lambda using Delay/Force (optimization for () => A)
  */
sealed trait LambdaRepresentation extends LoweredValueRepresentation {
    def funTp: SIRType

    /** True if this is a delayed unit lambda (uses Delay/Force instead of LamAbs/Apply) */
    def isDelayed: Boolean

    /** The output representation of the lambda */
    def outputRepresentation: LoweredValueRepresentation

    override def isPackedData: Boolean = false

    override def isDataCentric: Boolean = false

    override def isCompatibleWithType(tp: SIRType): Boolean = {
        SIRUnify.topLevelUnifyType(funTp, tp, SIRUnify.Env.empty).isSuccess
    }

    /** Get the representation pair for a specific input type. Subtypes implement differently. */
    def reprFun(tp: SIRType, pos: SIRPosition)(using LoweringContext): InOutRepresentationPair

    protected def retrieveInputAndOutputType(tp: SIRType, pos: SIRPosition): (SIRType, SIRType) = {
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

    protected def isTypeVarCompatibleOn(
        tp: SIRType,
        repr: LoweredValueRepresentation,
        pos: SIRPosition
    )(using LoweringContext): Boolean =
        repr match {
            case TypeVarRepresentation(_) => true
            case othrLambdaRepr: LambdaRepresentation =>
                val (inputType, outputType) = retrieveInputAndOutputType(tp, pos)
                val InOutRepresentationPair(inRepr, outRepr) =
                    othrLambdaRepr.reprFun(inputType, pos)
                isTypeVarCompatibleOn(inputType, inRepr, pos) &&
                isTypeVarCompatibleOn(outputType, outRepr, pos)
            case _ => repr.isPackedData
        }
}

object LambdaRepresentation {

    /** Normal lambda: LamAbs(id, body) called with Apply(f, arg). This is the standard
      * representation for all lambdas except unit lambdas.
      */
    case class Normal(
        funTp: SIRType,
        canonicalRepresentationPair: InOutRepresentationPair
    ) extends LambdaRepresentation {

        override def isDelayed: Boolean = false

        override def outputRepresentation: LoweredValueRepresentation =
            canonicalRepresentationPair.outRepr

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): Boolean = {
            repr match {
                case comparator: LambdaRepresentation.Normal =>
                    val (inputType, outputType) = retrieveInputAndOutputType(tp, pos)
                    val InOutRepresentationPair(inRepr, outRepr) = reprFun(inputType, pos)
                    val InOutRepresentationPair(otherInRepr, otherOutRepr) =
                        comparator.reprFun(inputType, pos)
                    inRepr.isCompatibleOn(inputType, otherInRepr, pos) &&
                    outRepr.isCompatibleOn(outputType, otherOutRepr, pos)
                case _: LambdaRepresentation.Delayed =>
                    // Normal and Delayed are NOT compatible
                    false
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

        override def reprFun(tp: SIRType, pos: SIRPosition)(using
            lctx: LoweringContext
        ): InOutRepresentationPair = {
            SIRType.collectPolyOrFun(funTp) match
                case Some((typeVars, input, output)) =>
                    val builtinTypeVars = typeVars.filter(_.isBuiltin)
                    if builtinTypeVars.isEmpty then {
                        canonicalRepresentationPair
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

        override def doc: Doc = {
            PrettyPrinter.inParens(
              canonicalRepresentationPair.inRepr.doc + Doc.text(
                " -> "
              ) + canonicalRepresentationPair.outRepr.doc
            )
        }
    }

    /** Delayed lambda: Delay(body) called with Force(f). This is an optimization for unit lambdas
      * (() => A) that saves 1 CEK step per invocation. Force doesn't take an argument, so there is
      * NO input representation.
      */
    case class Delayed(
        funTp: SIRType, // Fun(Unit, resultType)
        resultRepr: LoweredValueRepresentation
    ) extends LambdaRepresentation {

        override def isDelayed: Boolean = true

        override def outputRepresentation: LoweredValueRepresentation = resultRepr

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): Boolean = {
            repr match {
                case other: LambdaRepresentation.Delayed =>
                    // Both must be Delayed, then check output compatibility
                    val (_, outputType) = retrieveInputAndOutputType(tp, pos)
                    resultRepr.isCompatibleOn(outputType, other.resultRepr, pos)
                case _: LambdaRepresentation.Normal =>
                    // Normal and Delayed are NOT compatible
                    false
                case TypeVarRepresentation(isBuiltin) =>
                    isBuiltin || {
                        val (_, outputType) = retrieveInputAndOutputType(tp, pos)
                        isTypeVarCompatibleOn(outputType, resultRepr, pos)
                    }
                case _ => false
            }
        }

        /** For Delayed lambdas, reprFun returns a dummy input representation since it won't be used
          * (Force doesn't take an argument).
          */
        override def reprFun(tp: SIRType, pos: SIRPosition)(using
            LoweringContext
        ): InOutRepresentationPair = {
            InOutRepresentationPair(PrimitiveRepresentation.Constant, resultRepr)
        }

        override def doc: Doc = {
            Doc.text("Delayed(") + resultRepr.doc + Doc.text(")")
        }
    }

    /** Factory method for backwards compatibility - creates a Normal lambda representation */
    def apply(funTp: SIRType, canonicalRepresentationPair: InOutRepresentationPair): Normal =
        Normal(funTp, canonicalRepresentationPair)

    /** Extractor for pattern matching - matches both Normal and extracts funTp and pair */
    def unapply(lr: LambdaRepresentation): Option[(SIRType, InOutRepresentationPair)] =
        lr match {
            case Normal(funTp, pair) => Some((funTp, pair))
            case Delayed(funTp, resultRepr) =>
                Some((funTp, InOutRepresentationPair(PrimitiveRepresentation.Constant, resultRepr)))
        }
}

sealed trait PrimitiveRepresentation(val isPackedData: Boolean, val isDataCentric: Boolean)
    extends LoweredValueRepresentation {

    override def isCompatibleWithType(tp: SIRType): Boolean = {
        tp match {
            case SIRType.Integer | SIRType.Data() | SIRType.ByteString | SIRType.String |
                SIRType.Boolean | SIRType.Unit | SIRType.BLS12_381_G1_Element |
                SIRType.BLS12_381_G2_Element | SIRType.BLS12_381_MlResult | SIRType.BuiltinValue =>
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
            // BuiltinList[Data] -> SumDataList (native UPLC list of Data)
            case SIRType.BuiltinList(SIRType.Data()) =>
                SumCaseClassRepresentation.SumDataList
            // BuiltinList[BuiltinPair[Data, Data]] -> SumDataPairList
            case SIRType.BuiltinList(elemType)
                if ProductCaseClassRepresentation.PairData.isPairOrTuple2(elemType) =>
                SumCaseClassRepresentation.SumDataPairList
            // BuiltinList with non-Data element type - not supported as constant
            case SIRType.BuiltinList(elemType) =>
                throw LoweringException(
                  s"BuiltinList constant with non-Data element type ${elemType.show} is not supported. " +
                      s"Only BuiltinList[Data] and BuiltinList[BuiltinPair[Data,Data]] can be constants.",
                  SIRPosition.empty
                )
            // BuiltinArray[Data] -> use ArrayData representation (native UPLC array)
            case SIRType.BuiltinArray(SIRType.Data()) =>
                ProductCaseClassRepresentation.ArrayData
            // BuiltinArray with non-Data element type - not supported as constant
            case SIRType.BuiltinArray(elemType) =>
                throw LoweringException(
                  s"BuiltinArray constant with non-Data element type ${elemType.show} is not supported. " +
                      s"Only BuiltinArray[Data] can be a constant.",
                  SIRPosition.empty
                )
            case SIRType.SumCaseClass(decl, typeArgs) =>
                // Data type uses DataData representation, not DataConstr
                if decl.name == SIRType.Data.name then SumCaseClassRepresentation.DataData
                // scalus.prelude.List uses native UPLC list representation for Data-compatible elements
                // TODO: add containsFun check like in SirTypeUplcGenerator.apply
                else if decl.name == "scalus.prelude.List" || decl.name == SIRType.BuiltinList.name
                then
                    if typeArgs.nonEmpty && ProductCaseClassRepresentation.PairData
                            .isPairOrTuple2(typeArgs.head)
                    then SumCaseClassRepresentation.SumDataPairList
                    else SumCaseClassRepresentation.SumDataList
                else SumCaseClassRepresentation.DataConstr
            case SIRType.CaseClass(constrDecl, targs, parent) =>
                ProductCaseClassRepresentation.ProdDataConstr
            case SIRType.TypeLambda(params, body) =>
                constRepresentation(body)
            case SIRType.Integer | SIRType.ByteString | SIRType.String | SIRType.Boolean |
                SIRType.Unit | SIRType.BLS12_381_G1_Element | SIRType.BLS12_381_G2_Element |
                SIRType.BLS12_381_MlResult | SIRType.BuiltinValue =>
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
