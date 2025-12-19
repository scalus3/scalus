package scalus.compiler.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.SrcPos
import scalus.compiler.sir.*

import scala.annotation.unused
import scala.util.control.NonFatal

case class SIRTypeEnv(
    pos: SrcPos,
    vars: Map[Symbol, SIRType],
    forwardRefs: Map[Symbol, SIRType.TypeProxy] = Map.empty,
    trace: Boolean = false
)

case class TypingException(tpe: Type, pos: SrcPos, msg: String, cause: Throwable = null)
    extends RuntimeException(msg, cause)

case class SIRTypeWithTypeLambda(sirType: SIRType, optSirTypeLambda: Option[SIRType.TypeLambda])

class SIRTyper(using Context) {

    private val cachedDataDecl: MutableSymbolMap[DataDecl] = new MutableSymbolMap()

    def sirTypeInEnv(tp: Type, env0: SIRTypeEnv): SIRType = {
        val env = env0
        val retval =
            try sirTypeInEnvWithErr(tp.widen, env)
            catch
                case e: TypingException =>
                    if env.trace then
                        println(s"typing exception during sirTypeInEnv(${tp.show}), tp tree: ${tp}")
                    throw e
        if true then
            if !SIRType.checkAllProxiesFilled(retval) then
                throw new TypingException(tp, env.pos, s"Unfilled proxies in ${retval.show}")
        retval
    }

    private def sirTypeInEnvWithErr(tp: Type, env0: SIRTypeEnv): SIRType =
        val env = env0
        if env.trace then {
            println(
              s"sirTypeInEnvWithErr ${tp.show},  env=${env}"
            )
        }
        val retval = tp match
            case tpc: TermRef =>
                if tpc.typeSymbol.isTypeParam then unsupportedType(tp, s"TermRef ${tpc.show}", env)
                else if !(tpc.widen =:= tpc) then sirTypeInEnvWithErr(tp.widen, env)
                else unsupportedType(tp, s"TermRef ${tpc.show}", env)
            case tpc: TypeRef =>
                val sym = tpc.typeSymbol
                if tpc =:= defn.NothingType then SIRType.TypeNothing
                else if tpc.isTypeAlias then sirTypeInEnvWithErr(tpc.dealias, env)
                else if tpc =:= defn.AnyType then SIRType.FreeUnificator
                else if sym.isClass then makeSIRNonFunClassType(tpc, Nil, env)
                else if sym.isTypeParam then
                    env.vars.get(sym) match
                        case Some(t) => t
                        case None    =>
//                            val name = sym.fullName.show
                            unsupportedType(tp, s"Unfilled typeParam: ${tpc.show}", env)
                else if sym.isAliasType then
                    // looks like bug in a compiler
                    // println(s"stragen alias: ${tpc.show}, tpc.isTypeAlias=${tpc.isTypeAlias}, tp.isTypeAlias=${tp.isTypeAlias} sym.isAlias=${sym.isAliasType}, tpc.isSingleton=${tpc.isSingleton}")
                    // println(s"strange aliase: tree=${tpc}, symFullName=${sym.fullName}, dealias=${tpc.dealias.show}, deaslias.fullName=${tpc.dealias.typeSymbol.fullName}")
                    // throw new Exception("alias type")
                    sirTypeInEnvWithErr(tpc.dealias, env)
                else if tpc.isValueType then
                    val wtpc = tpc.widen
                    if wtpc != tpc then sirTypeInEnvWithErr(wtpc, env)
                    else makeSIRNonFunValueType(tpc, env)
                else unsupportedType(tpc, "TypeRef", env)
            case tpc: ConstantType =>
                // hmm, widen should have taken care of this
                sirTypeInEnvWithErr(tpc.widen, env)
            case _: SuperType =>
                ???
            case tpc: RefinedType =>
//                println(s"RefinedType ${tpc.show}")
                if tpc <:< defn.PolyFunctionType then {
                    // then this is a type of applyMethod
                    val applyTpe = tpc.member(nme.apply).info
                    sirTypeInEnvWithErr(applyTpe, env)
                } else {
                    sirTypeInEnvWithErr(tpc.parent, env)
                }
            case tp: AppliedType =>
                if tp.tycon.isRef(defn.MatchCaseClass) then
                    unsupportedType(tp, "MatchCaseClass", env)
                else if tp.typeSymbol.exists && tp.typeSymbol.isAliasType then
                    sirTypeInEnvWithErr(tp.dealias, env)
                else if defn.isFunctionType(tp) then {
                    makeSIRFunType(tp, env)
                } else {
                    tp.tycon match
                        case tpc: TypeRef =>
                            if tpc.isTypeAlias || tpc.symbol.isAliasType then
                                sirTypeInEnvWithErr(tpc.dealias.appliedTo(tp.args), env)
                            else {
                                tryMakeFunctionalInterface(tp, tp.typeSymbol, env) getOrElse
                                    makeSIRNonFunClassType(
                                      tpc,
                                      tp.args.map(sirTypeInEnv(_, env)),
                                      env
                                    )
                            }
                        case tpc: TypeLambda =>
                            unsupportedType(tp, s"TypeLambda ${tpc.show}", env)
                        case tpc: TermRef =>
                            unsupportedType(tp, s"TermRef ${tpc.show}", env)
                        case other =>
                            unsupportedType(tp, s"AppliedType ${tp.show}", env)
                }
            case tp: AnnotatedType =>
                sirTypeInEnvWithErr(tp.underlying, env)
            case tp: AndType =>
                findClassInAndType(tp) match
                    case Some(tpf) => makeSIRClassTypeNoTypeArgs(tpf, env)
                    case None =>
                        unsupportedType(tp, s"AndType", env)
            case orTp: OrType =>
                sirTypeInEnvWithErr(orTp.join, env)
            case tp: MatchType =>
                unsupportedType(tp, s"MatchType", env)
            case tp: ExprType =>
                sirTypeInEnvWithErr(tp.underlying, env)
            case tp: ParamRef =>
                val binder = tp.binder
                val paramName =
                    binder
                        .paramNames(tp.paramNum)
                        .show // TODO: better way to get the name as string
                SIRType.TypeVar(paramName, Some(tp.typeSymbol.hashCode), false)
            case tpc: ThisType =>
                sirTypeInEnv(tpc.underlying, env)
            case tpc: RecThis =>
                sirTypeInEnvWithErr(tpc.underlying, env)
            case tpc: RecType =>
                unsupportedType(tp, s"RecType", env)
            case tpm: MethodType =>
                makeSIRFunType(tpm, env)
            case tpp: PolyType =>
                val params = (tpp.paramNames zip tpp.paramRefs).map { (name, ref) =>
                    SIRType.TypeVar(name.show, Some(ref.typeSymbol.hashCode()), false)
                }
                SIRType.TypeLambda(params, sirTypeInEnvWithErr(tpp.resultType, env))
            case tpl: HKTypeLambda =>
                val params = (tpl.paramNames zip tpl.paramRefs).map { (name, ref) =>
                    SIRType.TypeVar(name.show, Some(ref.typeSymbol.hashCode()), false)
                }
                SIRType.TypeLambda(params, sirTypeInEnvWithErr(tpl.resType, env))
            case tpp: TypeBounds =>
                SIRType.FreeUnificator
            case NoPrefix =>
                unsupportedType(tp, "NoPrefix", env)
            // case tpf: FlexibleType =>
            //    sirTypeInEnv(tpf.underlying, env)
            case classInfo: ClassInfo =>
                makeSIRNonFunClassType(classInfo.appliedRef, Nil, env)
            case typeVar: TypeVar =>
                env.vars.get(typeVar.typeSymbol) match
                    case Some(t) => t
                    case None    =>
                        // this can be filled typeVar
                        typeVar.stripped match
                            case TypeParamRef(binder, idx) =>
                                val paramName = binder.paramNames(idx).show
                                // unsupportedType(tp, s"TypeVar ${typeVar.show}, name=${name}", env)
                                val symCode =
                                    if typeVar.typeSymbol == Symbols.NoSymbol then
                                        // (binder.typeSymbol.hashCode().toLong << 32) + idx
                                        binder.typeSymbol.hashCode() + idx
                                    else typeVar.typeSymbol.hashCode()
                                //  not sure, if typeVar,typeSymbol is exista and is unique.
                                // code as binding symbol ?
                                // TODO: make SymCode long to accept such encoding
                                SIRType.TypeVar(paramName, Some(symCode), false)
                            case other =>
                                // this is a filled typeVar, which can be substitutef
                                sirTypeInEnvWithErr(other, env)
            case other =>
                unsupportedType(tp, s"${tp.show}, tree=${tp}", env)
        if env.trace then
            println(
              s"retval for ${tp.show} (${tp.typeSymbol.fullName.toString}) is ${retval.show} ($retval)"
            )
        retval

    private def makeSIRClassTypeNoTypeArgs(tp: Type, env: SIRTypeEnv): SIRType = {
        // println(s"makeSIRClassTypeNoTypeArgs ${tp.show}")
        if defn.isFunctionType(tp) then makeFunTypeLambda(tp)
        else makeSIRNonFunClassType(tp, Nil, env)
    }

    private def makeSIRNonFunClassType(tp: Type, types: List[SIRType], env: SIRTypeEnv): SIRType = {
        val sym = tp.typeSymbol
        // println(s"makeSIRNonFumClassType ${sym.fullName.show} ${types.map(_.show)}, isFunctionType=${defn.isFunctionType(tp)}")
        val retval = (tryMakePrimitivePrimitive(sym, types) orElse
            tryMakeBuiltinType(sym, types, env) orElse
            tryMakeFunctionalInterface(tp, sym, env) orElse
            tryMakeCaseClassOrCaseParent(tp, sym, types, env) orElse
            tryMakeSynomimVaragsType(tp, sym, types, env) orElse
            tryMakeNonCaseModule(tp, sym, types, env)).getOrElse {
//            val name = sym.fullName.show
//            val typeArgs = types.map(_.show)
            unsupportedType(
              tp,
              s"tree=${tp}, isClass=${sym.isClass} isAliasType=${sym.isAliasType}, info:${sym.info}",
              env
            )
        }
        // println("nakeSIRNonFunClassType return " + retval)
        retval
    }

    private def makeSIRNonFunValueType(tpc: TypeRef, env: SIRTypeEnv) = {
        val sym = tpc.typeSymbol
        if sym == Symbols.requiredClass("scala.math.BigInt") then SIRType.Integer
        else
            // this is a custom value type,  check hidden val
            if tpc.typeSymbol.isTerm && !tpc.typeSymbol.isType then
                // sone strange type, which should be a TermRef,  not TypeRef
                //  (error in dotty  ???)
                val termSym = tpc.typeSymbol.asTerm
                sirTypeInEnvWithErr(termSym.info, env)
            else
                val argss =
                    sym.primaryConstructor.paramSymss.filter(_.exists(!_.isTypeParam)).flatten
                argss match
                    case Nil =>
                        unsupportedType(tpc, "ValueType without fields", env)
                    case head :: Nil =>
                        val headType = head.info
                        sirTypeInEnvWithErr(headType, env)
                    case _ =>
                        unsupportedType(
                          tpc,
                          "ValueType with more that one argument to ptrimary constryctir",
                          env
                        )
    }

    private def makeSIRFunType(tp: Type, env: SIRTypeEnv): SIRType = {
        if env.trace then println(s"makeSIRFunType ${tp.show}, env=${env}")
        tp match
            case mt: MethodType =>
                makeSIRMethodType(mt, env)
            case AppliedType(tycon, args) =>
                if defn.isFunctionType(tp) then
                    val retval = makeFunctionClassType(args.map(sirTypeInEnvWithErr(_, env)), env)
                    retval
                else unsupportedType(tp, "AppliedType as function", env)
            case _ =>
                ???
    }

    private def findClassInAndType(andType: AndType): Option[Type] = {
        ???
    }

    private def tryMakePrimitivePrimitive(
        symbol: Symbol,
        tpArgs: List[SIRType]
    ): Option[SIRType] = {
        if !tpArgs.isEmpty then None
        else if symbol == defn.BooleanType.typeSymbol || symbol == defn.BoxedBooleanClass then
            Some(SIRType.Boolean)
        else if symbol == Symbols.requiredClass("scalus.builtin.ByteString") then
            Some(SIRType.ByteString)
        else if symbol == Symbols.requiredClass("scalus.builtin.BuiltinValue") then
            Some(SIRType.BuiltinValue)
        else if symbol == Symbols.requiredClass("scala.math.BigInt") then Some(SIRType.Integer)
        else if symbol == defn.IntType.typeSymbol || symbol == defn.BoxedIntClass then
            Some(SIRType.Integer)
        else if symbol == defn.LongType.typeSymbol || symbol == defn.BoxedLongClass then
            Some(SIRType.Integer)
        else if symbol == defn.StringType.typeSymbol then Some(SIRType.String)
        else if symbol == defn.UnitClass then Some(SIRType.Unit)
        else None
    }

    private def tryMakeBuiltinType(
        symbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv
    ): Option[SIRType] = {
        if symbol == Symbols.requiredClass("scalus.builtin.Data") then Some(SIRType.Data.tp)
        else if symbol == Symbols.requiredClass("scalus.builtin.BuiltinArray") then
            tpArgs match
                case List(elemType) => Some(SIRType.BuiltinArray(elemType))
                case _ =>
                    throw TypingException(
                      symbol.info,
                      env.pos,
                      s"Array type should have one type argument, found ${tpArgs.length}"
                    )
        else if symbol == Symbols.requiredClass("scalus.builtin.BuiltinList") then
            tpArgs match
                case List(elemType) => Some(SIRType.BuiltinList(elemType))
                case _ =>
                    throw TypingException(
                      symbol.info,
                      env.pos,
                      s"List type should have one type argument, found ${tpArgs.length}"
                    )
        else if symbol == Symbols.requiredClass("scalus.builtin.BuiltinPair") then
            tpArgs match
                case List(a1, a2) => Some(SIRType.BuiltinPair(a1, a2))
                case _ =>
                    throw TypingException(
                      symbol.info,
                      env.pos,
                      s"Pair type should have two type arguments, found ${tpArgs.length}"
                    )
        else if symbol == Symbols.requiredClass("scalus.builtin.BLS12_381_G1_Element") then
            Some(SIRType.BLS12_381_G1_Element)
        else if symbol == Symbols.requiredClass("scalus.builtin.BLS12_381_G2_Element") then
            Some(SIRType.BLS12_381_G2_Element)
        else if symbol == Symbols.requiredClass("scalus.builtin.BLS12_381_MlResult") then
            Some(SIRType.BLS12_381_MlResult)
        else None
    }

    private def makeFunctionClassType(list: List[SIRType], env: SIRTypeEnv) = {
        list match
            case Nil =>
                throw TypingException(
                  Types.NoType,
                  env.pos,
                  "Function type should have at least one argument"
                )
            case head :: Nil =>
                // Function0
                SIRType.Fun(SIRType.Unit, head)
            case _ =>
                val args = list.init
                val res = list.last
                makeUnaryFun(args, res, env)
    }

    private def tryMakeCaseClassOrCaseParent(
        @unused originType: Type,
        typeSymbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv
    ): Option[SIRType] = {
        env.forwardRefs.get(typeSymbol) match
            case Some(tlProxy) =>
                if tlProxy.ref != null then
                    // think - is it safe, mb create proxy in any case.
                    Some(SIRType.typeApply(tlProxy, tpArgs))
                else if tpArgs.isEmpty then Some(tlProxy)
                else
                    val proxy = new SIRType.TypeProxy(null)
                    tlProxy.setCallback(tlProxy => proxy.ref = SIRType.typeApply(tlProxy, tpArgs))
                    Some(proxy)
            case None =>
                val tlProxy = new SIRType.TypeProxy(null)
                val proxy =
                    if tpArgs.isEmpty then tlProxy
                    else new SIRType.TypeProxy(null)
                tryMakeCaseClassOrSumTypeNoRec(
                  typeSymbol,
                  tpArgs,
                  env.copy(forwardRefs = env.forwardRefs.updated(typeSymbol, tlProxy)),
                  proxy,
                  tlProxy
                ).map(_.sirType)
    }

    private def tryMakeFunctionalInterface(
        originType: Type,
        @unused typeSymbol: Symbol,
        env: SIRTypeEnv
    ): Option[SIRType] = {
        if originType <:< Symbols.requiredClassRef("scalus.CompileDerivations")
            || originType.typeSymbol.hasAnnotation(
              Symbols.requiredClass("java.lang.FunctionalInterface")
            )
        then
            originType.baseClasses.find(b => defn.isFunctionClass(b)) match
                case None =>
                    report.warning(
                      s"type ${originType.show} marded as FuctionalInterface but not functional"
                    )
                    None
                case Some(baseFunction) =>
                    val t = originType.baseType(baseFunction)
                    if t == Types.NoType then {
                        report.warning(s"type ${originType.show} can't be transformed to function")
                        None
                    } else Some(sirTypeInEnvWithErr(t, env))
        else None
    }

    private def retrieveTypeParamsAndParamsFromConstructor(
        typeSymbol: Symbol,
        env: SIRTypeEnv
    ): (List[Symbol], List[Symbol]) = {
        typeSymbol.primaryConstructor.paramSymss match
            case Nil => (Nil, Nil)
            case List(args) =>
                if args.isEmpty then (Nil, Nil)
                else if args.exists(_.isTerm) then (Nil, args)
                else if args.exists(_.isType) then (args, Nil)
                else {
                    val msg =
                        s"Case class ${typeSymbol.showFullName} has strange primary constructor: ${args}"
                    throw TypingException(typeSymbol.info, env.pos, msg)
                }
            case List(frs, snd) =>
                if frs.exists(_.isType) && snd.exists(_.isTerm) then (frs, snd)
                else if frs.exists(_.isTerm) && snd.exists(_.isType) then (snd, frs)
                else if frs.exists(_.isType) && snd.exists(_.isType) then
                    val msg =
                        s"Case class ${typeSymbol.showFullName} has primary constructor with two type parametes list"
                    throw TypingException(typeSymbol.info, env.pos, msg)
                else if frs.exists(_.isTerm) && snd.isEmpty then (Nil, frs)
                else if frs.isEmpty && snd.exists(_.isType) then (Nil, snd)
                else if frs.exists(_.isTerm) && snd.isEmpty then (Nil, frs)
                else if frs.isEmpty && snd.exists(_.isTerm) then (snd, Nil)
                else {
                    val msg =
                        s"Case class ${typeSymbol.showFullName} has strange primary constructor: ${frs} ${snd}"
                    throw TypingException(typeSymbol.info, env.pos, msg)
                }
            case _ =>
                val msg =
                    s"Case class ${typeSymbol.showFullName} has primary constructor with multiply parameters list: ${typeSymbol.primaryConstructor.paramSymss}"
                throw TypingException(typeSymbol.info, env.pos, msg)
    }

    /** case classes and symbols.
      * @param typeSymbol
      * @param tpArgs
      * @param x$3
      * @return
      */
    private def tryMakeCaseClassOrSumTypeNoRec(
        typeSymbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv,
        thisProxy: SIRType.TypeProxy,
        thisTypeLambdaProxy: SIRType.TypeProxy
    ): Option[SIRTypeWithTypeLambda] = {
        if typeSymbol.children.isEmpty then
            val optParent = retrieveParentSymbol(typeSymbol, env)
            tryMakeCaseClassType(typeSymbol, tpArgs, env, thisProxy, thisTypeLambdaProxy, optParent)
        else tryMakeSumType(typeSymbol, tpArgs, env, thisProxy, thisTypeLambdaProxy)
    }

    private def retrieveParentSymbol(typeSymbol: Symbol, env: SIRTypeEnv): Option[Symbol] = {
        val parentSyms = typeSymbol.info.baseClasses.filter { bc =>
            val bcChildren = bc.children
            bcChildren.nonEmpty && !bc.flags.is(Flags.Transparent)
            && bcChildren.exists(_ == typeSymbol)
        }
        val optParent = parentSyms match
            case Nil         => None
            case head :: Nil => Some(head)
            case head :: tail =>
                val msg =
                    s"Class ${typeSymbol.showFullName} have two parents: ${head.showFullName} and ${tail.head.showFullName}"
                throw TypingException(typeSymbol.info, env.pos, msg)
        optParent
    }

    private def tryMakeCaseClassType(
        typeSymbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv,
        thisProxy: SIRType.TypeProxy,
        thisTypeLambdaProxy: SIRType.TypeProxy,
        optParentSym: Option[Symbol]
    ): Option[SIRTypeWithTypeLambda] = {
        // TODO: insert checks
        if typeSymbol.flags.is(Flags.Trait) || typeSymbol.flags.is(Flags.Abstract) then None
        else
            Some(
              makeCaseClassType(
                typeSymbol,
                tpArgs,
                env,
                thisProxy,
                thisTypeLambdaProxy,
                optParentSym
              )
            )
    }

    private def makeCaseClassType(
        typeSymbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv,
        thisProxy: SIRType.TypeProxy,
        thisTypeLambdaProxy: SIRType.TypeProxy,
        optParentSym: Option[Symbol]
    ): SIRTypeWithTypeLambda = {
        val retval = optParentSym match
            case Some(parentSym) =>
                val dataDecl = makeSumClassDataDecl(parentSym, env)
                val nakedType = dataDecl.constrType(typeSymbol.fullName.show)
                thisTypeLambdaProxy.ref = nakedType
                if tpArgs.isEmpty then
                    thisProxy.ref = nakedType
                    SIRTypeWithTypeLambda(nakedType, None)
                else
                    val cladType = SIRType.typeApply(nakedType, tpArgs)
                    thisProxy.ref = cladType
                    val tlNakedType = nakedType match
                        case tl: SIRType.TypeLambda => tl
                        case _ =>
                            throw new IllegalStateException(
                              s"nakedType should be TypeLambda, found ${nakedType.show}"
                            )
                    SIRTypeWithTypeLambda(cladType, Some(tlNakedType))
            case None =>
                val constrDecl = makeCaseClassConstrDecl(typeSymbol, env, None)
                if tpArgs.isEmpty then
                    val retval = SIRType.CaseClass(constrDecl, Nil, None)
                    thisProxy.ref = retval
                    thisTypeLambdaProxy.ref = retval
                    SIRTypeWithTypeLambda(retval, None)
                else {
                    val cladType = SIRType.CaseClass(constrDecl, tpArgs, None)
                    thisProxy.ref = cladType
                    val nakedType = SIRType.TypeLambda(
                      constrDecl.typeParams,
                      SIRType.CaseClass(constrDecl, constrDecl.typeParams, None)
                    )
                    thisTypeLambdaProxy.ref = nakedType
                    SIRTypeWithTypeLambda(cladType, Some(nakedType))
                }
        retval
    }

    private def tryMakeSumType(
        typeSymbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv,
        thisProxy: SIRType.TypeProxy,
        thisTypeLambdaProxy: SIRType.TypeProxy
    ): Option[SIRTypeWithTypeLambda] = {
        if typeSymbol.children.nonEmpty then
            Some(makeSumClassType(typeSymbol, tpArgs, env, thisProxy, thisTypeLambdaProxy))
        else None
    }

    private def makeCaseClassConstrDecl(
        typeSymbol: Symbol,
        env: SIRTypeEnv,
        optParentSym: Option[Symbol]
    ): ConstrDecl = {
        val name = typeSymbol.fullName.show
        val (typeParamSymbols, paramSymbols) =
            retrieveTypeParamsAndParamsFromConstructor(typeSymbol, env)
        val tparams =
            typeParamSymbols.map(s => SIRType.TypeVar(s.name.show, Some(s.hashCode), false))
        val nVars = env.vars ++ typeParamSymbols.zip(tparams)
        val nEnv = env.copy(vars = nVars)
        val params = paramSymbols.map { s =>
            val t = sirTypeInEnvWithErr(s.info, nEnv)
            TypeBinding(s.name.show, t)
        }
        val parentTypeArgs = optParentSym.toList.flatMap { parentSym =>
            val ct = constructorResultType(typeSymbol)
            val btp = ct.baseType(parentSym)
            btp match
                case AppliedType(ty, targs) =>
                    targs.map(t => sirTypeInEnvWithErr(t, nEnv))
                case _ => Nil

        }
        try
            ConstrDecl(
              name,
              params,
              tparams,
              parentTypeArgs,
              AnnotationsDecl.fromSym(typeSymbol)
            )
        catch
            case NonFatal(e) =>
                println("error in makeCaseClassConstrDecl")
                println(s"optParentSym=${optParentSym}")
                println(s"typeSymbol=${typeSymbol.showFullName}")
                println(s"constructorResultType=${constructorResultType(typeSymbol).show}")
                println(s"isEnum = ${typeSymbol.is(Flags.Enum)}")
                throw e
    }

    def constructorResultType(typeSymbol: Symbol): Type = {
        if typeSymbol.primaryConstructor == NoSymbol then
            if typeSymbol.is(Flags.Enum) then typeSymbol.info
            else NoType
        else
            typeSymbol.primaryConstructor.info match
                case polyType: PolyType =>
                    polyType.resType match
                        case mt: MethodType =>
                            mt.resType
                        case _ =>
                            throw new IllegalStateException(
                              s"PolyType.resType should be methd type for constructor of ${typeSymbol.showFullName}"
                            )
                case mt: MethodType =>
                    mt.resType
                case NoType =>
                    NoType
                case other =>
                    throw new IllegalStateException(
                      s"Assumed that the type of constructir for ${typeSymbol.showFullName} shoukd be PolyType or methodTypeor NoTpe, we have ${other}"
                    )
    }

    private def makeSumClassType(
        typeSymbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv,
        thisProxy: SIRType.TypeProxy,
        thisTypeLambdaProxy: SIRType.TypeProxy
    ): SIRTypeWithTypeLambda = {
        val dataDecl = makeSumClassDataDecl(typeSymbol, env)
        // thisProxy.ref = dataDecl.tp
        val retval = dataDecl.tp match {
            case tl @ SIRType.TypeLambda(params, res) =>
                if tpArgs.isEmpty then
                    throw TypingException(
                      typeSymbol.info,
                      env.pos,
                      s"Type ${typeSymbol.showFullName} should have ${params.length} type arguments, found 0"
                    )
                else if params.length != tpArgs.length then
                    throw TypingException(
                      typeSymbol.info,
                      env.pos,
                      s"Type ${typeSymbol.showFullName} should have ${params.length} type arguments, found ${tpArgs.length}"
                    )
                else {
                    val cladType = SIRType.typeApply(tl, tpArgs)
                    thisProxy.ref = cladType
                    thisTypeLambdaProxy.ref = tl
                    SIRTypeWithTypeLambda(cladType, Some(tl))
                }
            case _ =>
                if tpArgs.nonEmpty then
                    throw TypingException(
                      typeSymbol.info,
                      env.pos,
                      s"Type ${typeSymbol.showFullName} should not have type arguments, found ${tpArgs.length}"
                    )
                else {
                    thisProxy.ref = dataDecl.tp
                    thisTypeLambdaProxy.ref = dataDecl.tp
                    SIRTypeWithTypeLambda(dataDecl.tp, None)
                }
        }

        // val retval =
        //    if tpArgs.isEmpty then dataDecl.tp
        //    else SIRType.typeApply(dataDecl.tp, tpArgs)
        // thisProxy.ref = retval
        retval
    }

    private def makeSumClassDataDecl(
        typeSymbol: Symbol,
        env: SIRTypeEnv
    ): DataDecl = {
        cachedDataDecl.get(typeSymbol) match
            case Some(retval) => retval
            case None =>
                val retval = makeSumClassDataDeclNoCache(typeSymbol, env)
                cachedDataDecl.update(typeSymbol, retval)
                retval
    }

    private def makeSumClassDataDeclNoCache(
        typeSymbol: Symbol,
        env: SIRTypeEnv
    ): DataDecl = {

        // result list is ordered by the order of the children in the typeSymbol (the same as in text)
        val constrDecls = typeSymbol.children.map { s =>
            if s.children.isEmpty then makeCaseClassConstrDecl(s, env, Some(typeSymbol))
            else
                val syntethicName = SIRType.syntheticNarrowConstrDeclName(typeSymbol.fullName.show)
                val sirTypeParams =
                    s.typeParams.map(tps =>
                        SIRType.TypeVar(tps.name.show, Some(tps.hashCode), false)
                    )
                val newVars = s.typeParams.zip(sirTypeParams).toMap
                val nEnv = env.copy(vars = env.vars ++ newVars)
                val parentTypeArgs = constructorResultType(s).baseType(typeSymbol) match
                    case AppliedType(tycon, targs) =>
                        targs.map(t => sirTypeInEnvWithErr(t, nEnv))
                    case _ => Nil
                val paramType = env.forwardRefs.get(s) match
                    case Some(s) =>
                        SIRType.typeApply(s, sirTypeParams)
                    case None =>
                        val dataDecl = makeSumClassDataDecl(s, nEnv)
                        SIRType.typeApply(dataDecl.tp, sirTypeParams)
                val params = List(TypeBinding("value", paramType))
                ConstrDecl(
                  syntethicName,
                  params,
                  sirTypeParams,
                  parentTypeArgs,
                  AnnotationsDecl.fromSym(s)
                )
        }
        val typeParams =
            typeSymbol.typeParams.map(tp =>
                SIRType.TypeVar(tp.name.show, Some(tp.hashCode()), false)
            )
        DataDecl(
          typeSymbol.fullName.show,
          constrDecls,
          typeParams,
          AnnotationsDecl.fromSym(typeSymbol)
        )
    }

    private def tryMakeSynomimVaragsType(
        tp: Type,
        typeSymbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv
    ): Option[SIRType] = {
        if typeSymbol == Symbols.requiredClass("scala.collection.immutable.Seq") then {
            tpArgs match
                case List(elemType) =>
                    Some(SIRType.Varargs(elemType))
                case _ =>
                    throw TypingException(
                      tp,
                      env.pos,
                      s"Seq type should have one type argument, found ${tpArgs.length}"
                    )
        } else None
    }

    private def tryMakeNonCaseModule(
        tp: Type,
        typeSymbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv
    ): Option[SIRType] = {
        if typeSymbol.flags.is(Flags.Module) || typeSymbol.flags.is(Flags.Package) then {
            val name = typeSymbol.fullName.show
            if tpArgs.nonEmpty then {
                val msg = s"Module type ${typeSymbol.showFullName} should not have type arguments"
                Some(typeError(tp, msg, env, throwError = true))
            } else {
                Some(SIRType.TypeNonCaseModule(name))
            }
        } else {
            None
        }
    }

    private def makeUnaryFun(args: List[SIRType], res: SIRType, env: SIRTypeEnv): SIRType = {
        args match
            case Nil          => res
            case head :: Nil  => SIRType.Fun(head, res)
            case head :: tail => SIRType.Fun(head, makeUnaryFun(tail, res, env))
    }

    private def makeSIRMethodType(mt: MethodType, env: SIRTypeEnv): SIRType = {
        val params = mt.paramNames.zip(mt.paramInfos).map { (_, tp) =>
            sirTypeInEnvWithErr(tp, env)
        }
        val res = sirTypeInEnvWithErr(mt.resultType, env)
        makeUnaryFun(params, res, env)
    }

    private def makeFunTypeLambda(fn: Type): SIRType = ???

    private def typeError(
        tpe: Type,
        msg: String,
        env: SIRTypeEnv,
        throwError: Boolean = true,
        cause: Throwable = null
    ): SIRType = {
        throw TypingException(tpe, env.pos, msg)
    }

    private def unsupportedType(
        tpe: Type,
        msg: String,
        env: SIRTypeEnv,
        throwError: Boolean = true
    ): SIRType = {
        typeError(
          tpe,
          s"unsupported type: ${tpe.typeSymbol.showFullName} $msg at ${env.pos.sourcePos.show}",
          env,
          throwError
        )
    }

}
