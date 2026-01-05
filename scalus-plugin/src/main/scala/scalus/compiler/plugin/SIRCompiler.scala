package scalus.compiler.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.toTermName
import dotty.tools.dotc.core.NameKinds.{DefaultGetterName, InlineAccessorName}
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition, SrcPos}
import scalus.serialization.flat.FlatInstances.{ModuleHashSetReprFlat, given}
import scalus.serialization.flat.{EncoderState, Flat}
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, Binding, ConstrDecl, DataDecl, Module, SIR, SIRBuiltins, SIRDefaultOptions, SIRPosition, SIRType, SIRUnify, SIRVersion, TargetLoweringBackend, TypeBinding}
import scalus.uplc.DefaultUni
import scalus.*

import scala.annotation.{tailrec, unused}
import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}
import scala.language.implicitConversions
import scala.util.control.NonFatal

case class FullName(name: String)
object FullName:
    def apply(sym: Symbol)(using Context): FullName = FullName(sym.fullName.toString)

/** @param dataTypeSymbol
  *   \- the symbol of the base data type
  * @param dataTypeParams
  *   \- type parameters of the base data type
  * @param constructorsSymbols
  *   \- constructors symbols in the order of definition
  * @param parentSymbol
  *   \- the parent symbol of the base data type, if it exists
  */
case class AdtTypeInfo(
    dataTypeSymbol: Symbol,
    dataTypeParams: List[Type],
    constructorsSymbols: List[Symbol],
    parentSymbol: Option[Symbol]
)

/** Information about a constructor call.
  * @param name
  *   Name of the constructor.
  * @param dataInfo
  *   Type information of the base data type.
  */
case class AdtConstructorCallInfo(
    shortName: String,
    fullName: String,
    dataInfo: AdtTypeInfo
)

object AdtConstructorCallInfo {
    def apply(constructorTypeSymbol: Symbol, dataInfo: AdtTypeInfo)(using
        Context
    ): AdtConstructorCallInfo =
        AdtConstructorCallInfo(
          constructorTypeSymbol.name.show,
          constructorTypeSymbol.fullName.show,
          dataInfo
        )
}

enum Recursivity:
    case NonRec
    case Rec

case class TopLevelBinding(fullName: FullName, recursivity: Recursivity, body: SIR)

enum CompileDef:
    case Compiling
    case Compiled(binding: TopLevelBinding)

case class SIRCompilerOptions(
    backend: String = SIRDefaultOptions.targetLoweringBackend.toString,
    // universalDataRepresentation: Boolean =
    //    (SIRDefaultOptions.targetLoweringBackend == TargetLoweringBackend.SirToUplcV3Lowering),
    // linkInRuntime: Boolean = SIRDefaultOptions.runtimeLinker,
    writeSirToFile: Boolean = SIRDefaultOptions.writeSirToFile,
    debugLevel: Int = SIRDefaultOptions.debugLevel,
) {

    def universalDataRepresentation = backend == TargetLoweringBackend.SirToUplcV3Lowering.toString

}

object SIRCompilerOptions {
    val default: SIRCompilerOptions = SIRCompilerOptions()
}

final class SIRCompiler(
    options: SIRCompilerOptions = SIRCompilerOptions.default
)(using
    ctx: Context
) {
    import SIRCompiler.Env
    import tpd.*
    private val DefaultFunSIRBuiltins: Map[Symbol, SIR.Builtin] = Macros.generateBuiltinsMap(ctx)
    private val BigIntSymbol = requiredModule("scala.math.BigInt")
    private val BigIntClassSymbol = requiredClass("scala.math.BigInt")
    private val ByteStringClassSymbol = requiredClass("scalus.builtin.ByteString")
    private val DataClassSymbol = requiredClass("scalus.builtin.Data")
    private val DataModuleSymbol = requiredModule("scalus.builtin.Data")
    // Data case class companion apply methods (for Data.I(x), Data.B(x))
    private val DataIApplySymbol =
        requiredClass("scalus.builtin.Data.I").companionModule.requiredMethod("apply")
    private val DataBApplySymbol =
        requiredClass("scalus.builtin.Data.B").companionModule.requiredMethod("apply")
    // Note: Data.Constr, Data.List, Data.Map are handled by default case class compilation
    private val PairSymbol = requiredClass("scalus.builtin.BuiltinPair")
    private val ScalusBuiltinListClassSymbol = requiredClass("scalus.builtin.BuiltinList")
    private val ScalusBuiltinArrayClassSymbol = requiredClass("scalus.builtin.BuiltinArray")
    private val StringContextSymbol = requiredModule("scala.StringContext")
    private val StringContextApplySymbol = StringContextSymbol.requiredMethod("apply")
    private val Tuple2Symbol = requiredClass("scala.Tuple2")
    private val NothingSymbol = defn.NothingClass
    private val NullSymbol = defn.NullClass
    private val ByteStringModuleSymbol = requiredModule("scalus.builtin.ByteString")
    private val ByteStringSymbolHex = ByteStringModuleSymbol.requiredMethod("hex")
    private val ByteStringSymbolUtf8 = ByteStringModuleSymbol.requiredMethod("utf8")
    private val BuiltinValueModuleSymbol = requiredModule("scalus.builtin.BuiltinValue")
    private val FromDataSymbol = requiredClass("scalus.builtin.FromData")
    private val ToDataSymbol = requiredClass("scalus.builtin.ToData")
    private val moduleToExprSymbol = Symbols.requiredModule("scalus.compiler.sir.ModuleToExpr")
    private val sirBodyAnnotation = requiredClass("scalus.compiler.sir.SIRBodyAnnotation")
    private val sirModuleWithDepsType = requiredClassRef("scalus.compiler.sir.SIRModuleWithDeps")
    private val sirModuleWithDepsModule = requiredModule("scalus.compiler.sir.SIRModuleWithDeps")

    private val typer = new SIRTyper
    private val pmCompiler = new PatternMatchingCompiler(this)

    extension (t: Type)
        def isPair: Boolean = t.typeConstructor.classSymbol == PairSymbol
        def isList: Boolean = t.typeConstructor.classSymbol == ScalusBuiltinListClassSymbol
        def isArray: Boolean = t.typeConstructor.classSymbol == ScalusBuiltinArrayClassSymbol
        def isMethodType: Boolean = t.isInstanceOf[MethodType]

    extension (self: Symbol)
        def caseFields: List[Symbol] =
            if !self.isClass then Nil
            else
                self.asClass.paramAccessors.collect {
                    case sym if sym.is(dotty.tools.dotc.core.Flags.CaseAccessor) => sym.asTerm
                }

    extension (t: Tree) def isList: Boolean = t.tpe.isList

    extension (t: Tree) def isArray: Boolean = t.tpe.isArray

    extension (t: Tree) def isPair: Boolean = t.tpe.isPair

    extension (t: Tree) def isLiteral: Boolean = compileConstant.isDefinedAt(t)
    extension (t: Tree) def isData: Boolean = t.tpe <:< DataClassSymbol.typeRef

    // Helper methods for creating annotations with inline call stack
    private[plugin] def mkAnns(pos: SrcPos, env: Env): AnnotationsDecl =
        AnnotationsDecl(
          SIRPosition.fromSrcPos(pos).copy(inlinedFrom = env.inlineCallStack)
        )

    private[plugin] def mkAnnsFromSourcePosition(pos: SourcePosition, env: Env): AnnotationsDecl =
        AnnotationsDecl(
          SIRPosition.fromSourcePosition(pos).copy(inlinedFrom = env.inlineCallStack)
        )

    private[plugin] def mkAnnsFromSymIn(
        sym: Symbol,
        inPos: SourcePosition,
        env: Env
    ): AnnotationsDecl = {
        val optComment = sym.defTree match
            case memberDef: MemberDef => memberDef.rawComment.map(_.raw)
            case _                    => None
        val pos =
            if sym.sourcePos == NoSourcePosition then inPos
            else if inPos == NoSourcePosition then sym.sourcePos
            else inPos
        AnnotationsDecl(
          SIRPosition.fromSourcePosition(pos).copy(inlinedFrom = env.inlineCallStack),
          optComment
        )
    }

    opaque type LocalBindingFlags = Int
    object LocalBindingFlags {
        val None: LocalBindingFlags = 0
        val ErasedOnDataRepr: LocalBindingFlags = 1 << 0
    }

    case class LocalBinding(
        displayName: String,
        tp: SIRType,
        symbol: Symbol,
        isGlobalDef: Boolean,
        recursivity: Recursivity,
        body: AnnotatedSIR,
        pos: SourcePosition,
        flags: LocalBindingFlags
    ):

        def fullName(using Context): FullName = FullName(symbol)

        def variableKey: VariableKey =
            if isGlobalDef then VariableKey.fromName(displayName)
            else VariableKey(displayName, Some(symbol))

        def varName: String =
            variableKey.varName

    end LocalBinding

    // case class LocalSubmodule(
    //    name: String,
    //    symbol: Symbol,
    //    bindings: Seq[LocalBindingOrSubmodule],
    //    pos: SourcePosition
    // ) extends LocalBindingOrSubmodule

    private val globalDefs: mutable.LinkedHashMap[FullName, CompileDef] =
        mutable.LinkedHashMap.empty
    private val globalDataDecls: mutable.LinkedHashMap[FullName, DataDecl] =
        mutable.LinkedHashMap.empty

    case class SuperBinding(
        // here the full name orifinal symbol.
        //  for now, we use the same name.
        name: String,
        // type of the binding
        tp: SIRType,
        // origin symbol (parent of this class) where the method is defined
        parentSymbol: Symbol,
        // child symbol (this class) where the specialized method should be added
        childSymbol: Symbol,
        // specialized body
        body: SIR,
        // is this body changed over original ?
        isChanged: Boolean
    ) {

        def fullName(using Context): FullName =
            FullName(name)

    }

    private val CompileAnnot = requiredClassRef("scalus.Compile").symbol.asClass
    private val ScalusDebugAnnot = requiredClassRef("scalus.ScalusDebug").symbol.asClass
    private val IgnoreAnnot = requiredClassRef("scalus.Ignore").symbol.asClass

    private val uplcIntrinsicAnnot = Symbols.requiredClass("scalus.builtin.uplcIntrinsic")

    private def builtinFun(s: Symbol): Option[SIR.Builtin] = {
        DefaultFunSIRBuiltins
            .get(s)
            .orElse {
                s.getAnnotation(uplcIntrinsicAnnot) match
                    case Some(annot) =>
                        annot.argumentConstantString(0) match
                            case Some(name) =>
                                val sym = Symbols.requiredMethod(s"scalus.builtin.Builtins.${name}")
                                if !sym.exists then
                                    report.error(
                                      s"Unknown builtin name in uplcAnnotation: ${name} (symbol scalus.builtin.Builtins.${name} not exists)",
                                      s.srcPos
                                    )
                                    None
                                else
                                    DefaultFunSIRBuiltins.get(sym).orElse {
                                        report.error(
                                          s"Default builtins not contains $name",
                                          s.srcPos
                                        )
                                        None
                                    }
                            case None =>
                                report.error(
                                  "uplcIntrinsic annotation should have a string argument with builtin name",
                                  s.srcPos
                                )
                                None
                    case None => None
            }
    }

    def compileModule(tree: Tree): Unit = {

        /** return typedef and set of string, which are full names of submodules, which contains
          * derivations.
          * @param tree
          * @return
          */
        def collectTypeDefs(tree: Tree): List[(TypeDef, Int)] = {
            tree match
                case EmptyTree            => Nil
                case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
                case cd: TypeDef =>
                    if cd.symbol.hasAnnotation(CompileAnnot)
                    then
                        val debugLevel = cd.symbol.getAnnotation(ScalusDebugAnnot) match
                            case Some(annot) =>
                                annot.argumentConstant(0).map(_.intValue).getOrElse(1)
                            case None => 0
                        List((cd, debugLevel))
                    else List.empty
                case vd: ValDef =>
                    // println(s"valdef $vd")
                    Nil // module instance
                case Import(_, _) => Nil
        }

        val allTypeDefs = collectTypeDefs(tree)
        // println("allTypedefs: " + allTypeDefs.map { case (td, mode) =>
        //    s"${td.name} ${td.isClassDef}"
        // })

        allTypeDefs.foreach((td, debugLevel) => compileTypeDef(td, debugLevel))
    }

    private def compileTypeDef(td: TypeDef, annotDebugLevel: Int): Unit = {
        val start = System.currentTimeMillis()
        val tpl = td.rhs.asInstanceOf[Template]

        /*
        val staticInheritanceParents = td.tpe.parents.flatMap { p =>
            val hasAnnotation = p.typeSymbol.hasAnnotation(CompileAnnot)
            if hasAnnotation then
                if p.typeSymbol.fullName.toString.startsWith("scalus.prelude.") then Some(p)
                else
                    throw new RuntimeException(
                      s"Unsopported parent: ${p.typeSymbol.fullName.toString}, we support only builtin prelude validators as base classes "
                    )
            else None
        }*/

        val typeParams = td.tpe.typeParams
        val typeParamsSymbols = typeParams.map(_.paramRef.typeSymbol)
        val sirTypeParams = typeParamsSymbols.map { tps =>
            SIRType.TypeVar(tps.name.show, Some(tps.hashCode), false)
        }
        val sirTypeVars = (typeParamsSymbols zip sirTypeParams).toMap

        val baseEnv = Env.empty.copy(
          thisTypeSymbol = td.tpe.typeSymbol,
          typeVars = sirTypeVars,
          debug = options.debugLevel > 0 || annotDebugLevel > 0,
        )

        val bindings = tpl.body.flatMap { tree =>
            compileTreeInModule(baseEnv, td, tree).map { lb =>
                Binding(lb.fullName.name, lb.tp, lb.body)
            }
        }

        // val bindings = localBindings.foldRight(List.empty[LocalBinding]) {
        //    (element, bindings) => element +: bindings
        // }

        /*
        val possibleOverrides = staticInheritanceParents.flatMap { p =>
            bindings.map { lb =>
                p.typeSymbol.fullName.show + "." + lb.name -> lb
            }
        }.toMap

        val superBindings = staticInheritanceParents.flatMap { p =>
            sirLoader.findAndReadModule(p.typeSymbol.fullName.show, true) match
                case Left(message) =>
                    /*
                    error(
                      GenericError(
                        s"Builtin module ${p.typeSymbol.showFullName} not found, check is you installatin is complete: ${message}",
                        p.typeSymbol.srcPos
                      ),
                      None
                    )
                  
         */
                    None
                case Right(module) =>
                    val parentTypeParams = p.typeParams
                    val parentTypeParamsSymbols = parentTypeParams.map(_.paramRef.typeSymbol)
                    val parentTypeArgs = td.tpe.baseType(p.typeSymbol) match
                        case AppliedType(_, args) =>
                            args.map { a =>
                                sirTypeInEnv(a, p.typeSymbol.srcPos, baseEnv)
                            }
                        case _ => Nil
                    val parentTypeVars = (parentTypeParamsSymbols zip parentTypeArgs).toMap
                    val env = baseEnv.copy(typeVars = baseEnv.typeVars ++ parentTypeVars)
                    applyStaticInheritanceInModule(
                      p.typeSymbol,
                      module,
                      env,
                      possibleOverrides
                    )
        }

        val nonOverridedSupers = superBindings.filter { b =>
            !possibleOverrides.contains(b.name)
        }

        val superNames = superBindings.map(_.name).toSet

        val bindingsWithSpecialized =
            bindings.map { b =>
                if superBindings.nonEmpty then
                    val (newBody, changed) = applyStaticInheritanceInSIR(
                      Symbols.NoSymbol,
                      b.body,
                      Env.empty.copy(thisTypeSymbol = td.tpe.typeSymbol),
                      Map.empty,
                      superNames
                    )
                    Binding(b.fullName.name, b.tp, newBody)
                else Binding(b.fullName.name, b.tp, b.body)
            } ++ nonOverridedSupers.map(b => Binding(b.fullName.name, b.tp, b.body))
         */

        val time = System.currentTimeMillis() - start
        if bindings.isEmpty then {
            if options.debugLevel > 0 then
                report.echo(
                  s"skipping empty Scalus module ${td.name} in ${time}ms"
                )
        } else {
            val moduleName = td.symbol.fullName.toString
            val module =
                Module(
                  SIRVersion,
                  moduleName,
                  false,
                  None,
                  bindings
                )

            val moduleTree = convertFlatToTree(
              module,
              ModuleHashSetReprFlat,
              moduleToExprSymbol,
              td.span,
              options.debugLevel > 0
            )

            val externalModuleVars = gatherExternalModules(moduleName, module, Map.empty)
            val listDepsExpr = buildDepsTree(moduleName, externalModuleVars, td.srcPos)

            td.symbol.addAnnotation(
              Annotations.Annotation(sirBodyAnnotation, List(moduleTree, listDepsExpr), td.span)
            )
            if options.writeSirToFile || !td.symbol.flags.is(Flags.Module) then {
                // we should write sir to fiel for builtin traits (Validator, ParameterizedValidator)
                //  because applying of static inheritance happens in the compiler.
                // TODO: enhance SIR to have extends clause in modules and move static inheritance to runtime
                writeModule(module, td.symbol.fullName.toString)
            }

            if options.debugLevel > 0 then
                report.echo(
                  s"compiled Scalus module ${td.name} [${td.symbol.fullName.toString}] definitions: ${bindings.map(_.name)} in ${time}ms"
                )
        }
    }

    private def writeModule(module: Module, className: String): Unit = {
        val suffix = ".sir"
        val outputDirectory = ctx.settings.outputDir.value
        val pathParts = className.split('.')
        val dir = pathParts.init.foldLeft(outputDirectory)(_.subdirectoryNamed(_))
        val filename = pathParts.last
        val output = dir.fileNamed(filename + suffix).bufferedOutput
        val fl = summon[Flat[Module]]
        val bitSize = fl.bitSize(module)
        val enc = EncoderState(bitSize / 8 + 1)
        fl.encode(module, enc)
        enc.filler()
        output.write(enc.buffer)
        output.close()
    }

    /** Creates [[AdtTypeInfo]] based on a compiler Type.
      *
      * We support these cases:
      *   1. case class Foo(a: Int, b: String) (case 1)
      *   1. case object Bar (case 2)
      *   1. enum Base { case A ...} (case 3)
      *   1. enum Base { case B(a, b) } (case 4)
      *   1. sealed abstract class Base; object Base { case object A extends Base } (case 5)
      *   1. sealed abstract class Base; object Base { case class B(a:Int, b: String) extends Base }
      *      (case 6)
      *   1. scala.Tuple2 (case 7)
      */
    def getAdtTypeInfo(constrTpe: Type): AdtTypeInfo = {
        val typeSymbol = constrTpe.widen.dealias.typeSymbol
        // println(s"getAdtInfoFromConstroctorType: ${typeSymbol.fullName.show}, $constrTpe")
        // look for a base `sealed abstract class`. If it exists, we are in case 5 or 6
        val optAdtBaseTypeSymbol = constrTpe.baseClasses.find(b =>
            // println(s"base class: ${b.show} ${b.flags.flagsString}")
            // TODO:  recheck.  Why ! trait ?
            b.flags.isAllOf(Flags.Sealed | Flags.Abstract) && !b.flags.is(Flags.Trait)
        )

        val typeArgs = constrTpe match
            case AppliedType(_, args) => args
            case _                    => Nil

        if constrTpe.typeConstructor =:= Tuple2Symbol.typeRef
        then AdtTypeInfo(typeSymbol, typeArgs, List(typeSymbol), None)
        else
            optAdtBaseTypeSymbol match
                case None => // case 1 or 2
                    AdtTypeInfo(typeSymbol, typeArgs, List(typeSymbol), None)
                case Some(baseClassSymbol) =>
                    val adtBaseType = constrTpe.baseType(baseClassSymbol)
                    val baseDataParams = adtBaseType match
                        case AppliedType(_, args) => args
                        case _                    => Nil
                    val parents = baseClassSymbol.baseClasses.filter(cf =>
                        cf.children.contains(baseClassSymbol)
                    )
                    val optParent =
                        if parents.isEmpty then None
                        else if parents.size == 1 then parents.headOption
                        else
                            error(
                              GenericError(
                                s"Multiple parents for ${baseClassSymbol.fullName.show}: ${parents.map(_.fullName.show).mkString(", ")}",
                                baseClassSymbol.srcPos
                              ),
                              None
                            )
                    AdtTypeInfo(
                      baseClassSymbol,
                      baseDataParams,
                      baseClassSymbol.children,
                      optParent
                    )
    }

    /** Creates [[AdtConstructorCallInfo]] based on a constructor type.
      *
      * We support these cases:
      *   1. case class Foo(a: Int, b: String) (case 1)
      *   1. case object Bar (case 2)
      *   1. enum Base { case A ...} (case 3)
      *   1. enum Base { case B(a, b) } (case 4)
      *   1. sealed abstract class Base; object Base { case object A extends Base } (case 5)
      *   1. sealed abstract class Base; object Base { case class B(a:Int, b: String) extends Base }
      *      (case 6)
      *   1. scala.Tuple2 (case 7)
      */
    private def getAdtConstructorCallInfo(constrTpe: Type): AdtConstructorCallInfo = {
        val typeInfo = getAdtTypeInfo(constrTpe)
        if constrTpe.isSingleton then // case 3, 5
            AdtConstructorCallInfo(constrTpe.termSymbol, typeInfo)
        else // case 1, 2, 4, 6, 7
            val typeSymbol = constrTpe.widen.dealias.typeSymbol
            AdtConstructorCallInfo(typeSymbol, typeInfo)
    }

    def primaryConstructorParams(typeSymbol: Symbol): List[Symbol] = {
        val fields = typeSymbol.primaryConstructor.paramSymss.flatten.filter(s => s.isTerm)
        // debugInfo(s"caseFields: ${typeSymbol.fullName} $fields")
        fields
    }

    def primaryConstructorTypeParams(typeSymbol: Symbol): List[Symbol] = {
        val fields = typeSymbol.primaryConstructor.paramSymss.flatten.filter(s => s.isType)
        // debugInfo(s"caseFields: ${typeSymbol.fullName} $fields")
        fields
    }

    private def getCachedDataDecl(dataInfo: AdtTypeInfo, env: Env, srcPos: SrcPos): DataDecl = {
        val dataFullName = FullName(dataInfo.dataTypeSymbol)
        // debugInfo(s"compileNewConstructor2: dataTypeSymbol $dataTypeSymbol, dataName $dataName, constrName $constrName, children ${constructors}")
        globalDataDecls.getOrElseUpdate(dataFullName, makeDataDecl(dataInfo, env, srcPos))
    }

    private def makeDataDecl(dataInfo: AdtTypeInfo, env: Env, srcPos: SrcPos) = {
        val dataFullName = FullName(dataInfo.dataTypeSymbol)
        val dataTypeParams = dataInfo.dataTypeParams.map { tp =>
            SIRType.TypeVar(tp.typeSymbol.name.show, None, false)
        }
        val constrDecls = dataInfo.constructorsSymbols.map { sym =>
            makeConstrDecl(env, srcPos, sym)
        }
        val sourcePos =
            if dataInfo.dataTypeSymbol.srcPos.sourcePos == NoSourcePosition then srcPos.sourcePos
            else dataInfo.dataTypeSymbol.srcPos.sourcePos
        val optComment = dataInfo.dataTypeSymbol.defTree match
            case memberDef: MemberDef =>
                memberDef.rawComment.map(_.raw)
            case _ => None
        val anns = AnnotationsDecl(
          SIRPosition.fromSourcePosition(sourcePos),
          optComment
        )
        scalus.compiler.sir.DataDecl(dataFullName.name, constrDecls, dataTypeParams, anns)
    }

    def makeConstrDecl(env: Env, srcPos: SrcPos, constrSymbol: Symbol): ConstrDecl = {
        // val typeParams =
        //    constrSymbol.typeParams.map(tp =>
        //        SIRType.TypeVar(tp.name.show, Some(tp.hashCode), false)
        //    )
        // val envTypeVars1 = constrSymbol.typeParams.foldLeft(env.typeVars) { case (acc, tp) =>
        //    acc + (tp -> SIRType.TypeVar(tp.name.show, Some(tp.hashCode), false))
        // }
        val pcTypeParams = primaryConstructorTypeParams(constrSymbol).map(tp =>
            SIRType.TypeVar(tp.name.show, Some(tp.hashCode), false)
        )
        val envTypeVars2 = primaryConstructorTypeParams(constrSymbol).foldLeft(env.typeVars) {
            case (acc, tp) =>
                acc + (tp -> SIRType.TypeVar(tp.name.show, Some(tp.hashCode), false))
        }
        val nEnv = env.copy(typeVars = envTypeVars2)
        val params = primaryConstructorParams(constrSymbol).map { p =>
            val pType = sirTypeInEnv(p.info, srcPos, nEnv)
            TypeBinding(p.name.show, pType)
        }
        val constrType = typer.constructorResultType(constrSymbol)
        val optBaseClass = constrSymbol.info.baseClasses.find { b =>
            b.flags.is(Flags.Sealed) && b.children.contains(constrSymbol)
        }
        val baseTypeArgs = optBaseClass
            .flatMap { bs =>
                constrType.baseType(bs) match
                    case AppliedType(_, args) =>
                        Some(args.map(a => sirTypeInEnv(a, srcPos, nEnv)))
                    case _ => None
            }
            .getOrElse(Nil)
        // TODO: add substitution for parent type params
        // scalus.compiler.sir.ConstrDecl(sym.name.show, SIRVarStorage.DEFAULT, params, typeParams, baseTypeArgs)
        val pos = SIRPosition.fromSrcPos(srcPos)
        val comment = constrSymbol.defTree match
            case memberDef: MemberDef =>
                memberDef.rawComment.map(_.raw)
            case _ => None
        val anns = AnnotationsDecl(pos, comment)
        try
            scalus.compiler.sir.ConstrDecl(
              constrSymbol.fullName.show,
              params,
              pcTypeParams,
              baseTypeArgs,
              anns
            )
        catch
            case NonFatal(e) =>
                println(
                  s"Error making ConstrDecl for ${constrSymbol.fullName.show}: ${e.getMessage}"
                )
                println("Symbol: " + constrSymbol)
                println(s"pcTypeParams: $pcTypeParams")
                println(
                  s"primaryConstructorTypeParams: ${primaryConstructorTypeParams(constrSymbol)}"
                )
                println(s"params: $params")
                throw e

    }

    private def compileNewConstructor(
        env: Env,
        nakedType: Type,
        fullType: Type,
        args: List[Tree],
        srcPos: SrcPos
    ): AnnotatedSIR = {
        if nakedType.isGenericTuple then
            val nArgs = args.size
            if nArgs == 1 || nArgs == 2 then
                compileNewConstructorNoTuple(env, nakedType, fullType, args, srcPos)
            else
                val decl = makeGenericTupleDecl(nArgs, srcPos)
                val constrName = s"scala.Tuple${nArgs}"
                val targs = fullType match
                    case AppliedType(nt, targs) =>
                        targs.map(t => sirTypeInEnv(t, srcPos, env)).toList
                    case _ =>
                        error(
                          GenericError(
                            s"Tuple${nArgs} should nave n type arguments, buf fullType is: ${fullType.show}",
                            srcPos
                          ),
                          (1 to nArgs).map(_ => SIRType.TypeNothing).toList
                        )
                SIR.Constr(
                  constrName,
                  decl,
                  args.map(compileExpr(env, _)),
                  SIRType.typeApply(decl.constrType(constrName), targs),
                  AnnotationsDecl.fromSrcPos(srcPos)
                )
        else compileNewConstructorNoTuple(env, nakedType, fullType, args, srcPos)

    }

    private def compileNewConstructorNoTuple(
        env: Env,
        nakedType: Type,
        fullType: Type,
        args: List[Tree],
        srcPos: SrcPos
    ): AnnotatedSIR = {
        val constructorCallInfo = getAdtConstructorCallInfo(nakedType)
        val argsE = args.map(compileExpr(env, _))
        val dataDecl = getCachedDataDecl(constructorCallInfo.dataInfo, env, srcPos)
        // constructor body as: constr arg1 arg2 ...
        val (pos, optComment) = constructorCallInfo.dataInfo.constructorsSymbols.find(
          _.fullName.show == constructorCallInfo.fullName
        ) match
            case Some(sym) =>
                sym.defTree match
                    case memberDef: MemberDef =>
                        (SIRPosition.fromSrcPos(memberDef.srcPos), memberDef.rawComment.map(_.raw))
                    case _ =>
                        (SIRPosition.fromSrcPos(srcPos), None)
            case None =>
                report.warning(
                  s"Constructor symbol not found: ${constructorCallInfo.fullName}",
                  srcPos
                )
                (SIRPosition.fromSrcPos(srcPos), None)
        val anns = AnnotationsDecl(pos, optComment)
        SIR.Constr(
          constructorCallInfo.fullName,
          dataDecl,
          argsE,
          sirTypeInEnv(fullType, srcPos, env),
          anns
        )
    }

    // Parameterless case class constructor of an enum
    private def isConstructorVal(symbol: Symbol, @unused tpe: Type): Boolean =
        /* println(
        s"isConstructorVal: ${symbol.flags.isAllOf(Flags.EnumCase)} $symbol: ${tpe.show} <: ${tpe.widen.show}, ${symbol.flagsString}"
      )  */
        symbol.flags.isAllOf(Flags.EnumCase)

    // Check if the object is an instance of a case class (for copy method support)
    private def isCaseClassInstance(obj: Tree): Boolean = {
        val ts = obj.tpe.widen.dealias.typeSymbol
        ts.isClass && ts.caseFields.nonEmpty
    }

    // Get the effective method name, seeing through inline accessors.
    // Inline accessors are named like `inline$methodName$uniqueId`, this extracts `methodName`.
    // For regular methods, returns the name as-is.
    private def effectiveMethodName(symbol: Symbol): String =
        symbol.name match
            case InlineAccessorName(underlying) => underlying.toString
            case name                           => name.toString

    def error[A](error: CompilationError, @unused defaultValue: A): A = {
        report.error(error.message, error.srcPos)
        throw new RuntimeException(error.message)
    }

    /** Compile an identifier or a qualified select, possible type-applied
      * @param env - env in which we compile
      * @param e - select extpression or identifier
      * @param taTree - or same as e or TypeApply(e, targs), typechecked
      * @param targs - type arguments.
      * @return appropriate SIR
      */
    private def compileIdentOrQualifiedSelect(
        env: Env,
        e: Tree,
        taTree: Tree,
        @unused targs: List[Tree]
    ): AnnotatedSIR = {

        val name = e.symbol.name.show
        val symbolId = e.symbol.id
        val variableKey = VariableKey(name, Some(symbolId))
        val fullName = FullName(e.symbol)
        val isInLocalEnv = env.vars.contains(variableKey)
        val isInGlobalEnv = globalDefs.contains(fullName)

        // println( s"compileIdentOrQualifiedSelect1: ${e.symbol} $name $fullName, term: ${e.show}, loc/glob: $isInLocalEnv/$isInGlobalEnv, env: ${env}" )
        val (sirVar, origType) = (isInLocalEnv, isInGlobalEnv) match
            case (true, true) =>
                val localType = env.vars(variableKey)
                globalDefs(fullName) match
                    case CompileDef.Compiled(TopLevelBinding(_, _, body)) =>
                        val globalType = body.tp
                        if globalType != localType then
                            error(
                              TypeMismatch(
                                e.symbol.fullName.toString,
                                localType,
                                globalType,
                                e.srcPos
                              ),
                              ()
                            )
                    case _ =>
                (
                  SIR.Var(
                    e.symbol.fullName.toString,
                    localType,
                    AnnotationsDecl.fromSymIn(e.symbol, e.srcPos.sourcePos)
                  ),
                  localType
                )
            // local def, use the name without full qualification
            case (true, false) =>
                val localType = env.vars(variableKey)
                (
                  SIR.Var(
                    variableKey.varName,
                    localType,
                    AnnotationsDecl.fromSymIn(e.symbol, e.srcPos.sourcePos)
                  ),
                  localType
                )
            // global def, use full name
            case (false, true) =>
                val origType = sirTypeInEnv(taTree.tpe.widen, e.srcPos, env)
                val varType =
                    if isNoArgsMethod(e.symbol) then
                        // TODO: if we have type parameters, then we should apply one
                        SIRType.Fun(SIRType.Unit, origType)
                    else origType
                (
                  SIR.Var(
                    e.symbol.fullName.toString,
                    varType,
                    AnnotationsDecl.fromSymIn(e.symbol, e.srcPos.sourcePos)
                  ),
                  origType
                )
            case (false, false) =>
                // println( s"external var: module ${e.symbol.owner.fullName.toString()}, ${e.symbol.fullName.toString()}" )
                val origType = sirTypeInEnv(e.tpe.widen.dealias, e.srcPos, env.copy(debug = true))
                val valType =
                    if isNoArgsMethod(e.symbol) then SIRType.Fun(SIRType.Unit, origType)
                    else origType
                val (moduleName, valName) =
                    (e.symbol.owner.fullName.toString, e.symbol.fullName.toString)

                // Note: Data constructors (Data.I, Data.B, etc.) are now handled
                // in compileExpr via Apply cases that emit the appropriate builtins

                (
                  SIR.ExternalVar(
                    moduleName,
                    valName,
                    valType,
                    AnnotationsDecl.fromSymIn(e.symbol, e.srcPos.sourcePos)
                  ),
                  origType
                )
        // TODO: check that this is not from an apply call.
        if isNoArgsMethod(e.symbol) && !SIRType.isPolyFunOrFun(origType)
        then
            val anns = AnnotationsDecl.fromSrcPos(e.srcPos)
            val applySirType = sirTypeInEnv(taTree.tpe.widen.dealias, e.srcPos, env)
            SIR.Apply(
              sirVar,
              SIR.Const(scalus.uplc.Constant.Unit, SIRType.Unit, anns),
              applySirType,
              anns
            )
        else sirVar
    }

    private def isNoArgsMethod(sym: Symbol): Boolean = {
        sym.flags.is(Flags.Method) && !sym.flags.is(Flags.CaseAccessor) &&
        sym.paramSymss.flatten.forall(_.isType)
    }

    sealed trait CompileMemberDefResult
    object CompileMemberDefResult {
        case class Compiled(b: LocalBinding) extends CompileMemberDefResult
        case class Builtin(name: String, tp: SIRType) extends CompileMemberDefResult
        case class Ignored(tp: SIRType) extends CompileMemberDefResult
        case object NotSupported extends CompileMemberDefResult
    }

    private def compileValDef(
        env: Env,
        vd: ValDef,
        isModuleDef: Boolean
    ): CompileMemberDefResult = {
        val name = vd.name
        // vars are not supported
        if vd.symbol.flags.is(Flags.Mutable) then
            error(VarNotSupported(vd, vd.srcPos), None)
            CompileMemberDefResult.NotSupported
        /*
            lazy vals are not supported
            but we use givens for To/FromData instances
            which are compiled as final lazy vals
            Those are supported. They have the Given flag.
            Thus, we need to ignore them here.
         */
        else if vd.symbol.flags.isAllOf(Flags.Lazy, butNot = Flags.Given) then
            error(LazyValNotSupported(vd, vd.srcPos), None)
            CompileMemberDefResult.NotSupported
        // ignore @Ignore annotated statements
        else if vd.symbol.hasAnnotation(IgnoreAnnot) then
            CompileMemberDefResult.Ignored(sirTypeInEnv(vd.tpe, vd.srcPos, env))
        else

            // TODO store comments in the SIR
            // vd.rawComment
            val rhsFixed =
                if isFunctionalInterface(
                      vd.tpe
                    ) || vd.tpe.typeSymbol.isAnonymousClass && vd.tpe.baseClasses.exists(
                      isFunctionalInterfaceSymbol(_)
                    )
                then {
                    // TODO: check that interface is function
                    tryFixFunctionalInterface(env, vd.rhs).getOrElse {
                        // report.error(
                        //  s"[1] Functional interface not found for ${vd.rhs.show}" + "\n" +
                        //      s"tree:  ${vd.rhs}" + "\n",
                        //  vd.srcPos
                        // )
                        vd.rhs
                    }
                } else vd.rhs
            val bodyExpr = compileExpr(env, rhsFixed)
            val valSirType = sirTypeInEnv(vd.tpe.widen, vd.srcPos, env)
            // insert Apply if the left part hava a type T and right: Unit=>T
            val bodyExpr1 =
                if SIRType.isPolyFunOrFunUnit(bodyExpr.tp)
                then
                    if !SIRType.isPolyFunOrFunUnit(valSirType) then
                        SIR.Apply(
                          bodyExpr,
                          SIR.Const(scalus.uplc.Constant.Unit, SIRType.Unit, AnnotationsDecl.empty),
                          valSirType,
                          AnnotationsDecl.fromSrcPos(vd.srcPos)
                        )
                    else bodyExpr
                else bodyExpr

            // Here we more precise then scala compiler.
            val bindingSirType =
                if bodyExpr1.tp.isInstanceOf[SIRType.CaseClass] && valSirType
                        .isInstanceOf[SIRType.SumCaseClass]
                then bodyExpr1.tp
                else valSirType

            CompileMemberDefResult.Compiled(
              LocalBinding(
                name.show,
                bindingSirType,
                vd.symbol,
                isModuleDef,
                Recursivity.NonRec,
                bodyExpr1,
                vd.sourcePos,
                calculateLocalBindingFlags(vd.tpe)
              )
            )
    }

    private def tryFixFunctionalInterface(
        env: Env,
        tree: Tree
    ): Option[Tree] = {
        //  TODO: chekc that tpe is actually inherited from FunctionN
        // println(s"fix function interface for ${tree.show}")
        // println(s"fix function interface tree = ${tree}")
        tree match
            case Typed(x, tpt)
                if tpt.tpe.typeSymbol.hasAnnotation(
                  Symbols.requiredClass("java.lang.FunctionalInterface")
                ) =>
                tryFixFunctionalInterface(env, x)
            case Inlined(call, List(), expansion) =>
                val newExpansion = tryFixFunctionalInterface(env, expansion)
                newExpansion
            case Block(
                  List(ddef: DefDef),
                  Block(List(typed), Apply(initCn, List()))
                ) =>
                // println(s"Determinated function interface: ${ddef.show}")
                // println(s"typed: ${typed.show}")
                // println(s"initCn: ${initCn.show}")

                val call = Ident(TermRef(NoPrefix, ddef.symbol))
                Some(Block(List(ddef), Closure(Nil, call, EmptyTree)))
            // Handle blocks with local definitions followed by a lambda
            // This handles cases like: { val x = ...; val y = ...; (a, b) => ... }
            case Block(stats, expr) =>
                tryFixFunctionalInterface(env, expr).map { fixedExpr =>
                    Block(stats, fixedExpr)
                }
            case other =>
                // report.error("Function interface not found")
                // println(s"transstate functoon interface call unchanged: ${other.show}")
                // println(s"tree: ${other}")
                None
    }

    private def compileDefDef(
        env: Env,
        dd: DefDef,
        isGlobalDef: Boolean
    ): CompileMemberDefResult = {
        // ignore inline defs and @Ignore annotated statements
        if dd.symbol.flags.is(Flags.Inline) || dd.symbol.hasAnnotation(IgnoreAnnot) then
            CompileMemberDefResult.Ignored(sirTypeInEnv(dd.tpe, dd.srcPos, env))
        else
            // TODO store comments in the SIR
            // dd.rawComment
            val debug = env.debug || dd.symbol.fullName.toString == "b"
            if debug then
                println(
                  s"compileDefDef: ${dd.symbol.fullName.toString}, params: ${dd.paramss.map(_.map(_.show))}, type: ${dd.tpe.show}, rhs: ${dd.rhs.show}"
                )
            val params = dd.paramss.flatten.collect { case vd: ValDef => vd }
            val typeParams = dd.paramss.flatten.collect { case td: TypeDef => td }
            val sirTypeParams = typeParams.map { td =>
                SIRType.TypeVar(td.symbol.name.show, Some(td.symbol.hashCode), false)
            }
            val typeParamsMap =
                typeParams.zip(sirTypeParams).map { case (tp, tv) => (tp.symbol, tv) }.toMap
            val (paramVars, paramKeys) =
                if params.isEmpty
                then {
                    val unitKey = VariableKey.fromName("_")
                    val unitVar = SIR.Var("_", SIRType.Unit, AnnotationsDecl.empty)
                    (List(unitVar), List(unitKey))
                } /* Param for () argument */
                else {
                    val keys = params.map { v =>
                        VariableKey(v.name.show, Some(v.symbol.id))
                    }
                    val vars = params.map { case v: ValDef =>
                        val tEnv =
                            SIRTypeEnv(v.srcPos, env.typeVars ++ typeParamsMap)
                        val vType = sirTypeInEnv(v.tpe, tEnv)
                        val variableKey = VariableKey(v.name.show, Some(v.symbol.id))
                        val anns = AnnotationsDecl.fromSymIn(v.symbol, v.srcPos.sourcePos)
                        SIR.Var(variableKey.varName, vType, anns)
                    }
                    (vars, keys)
                }

            val paramEnvEntries = paramKeys.zip(paramVars).map { case (k, v) => (k, v.tp) }
            val body =
                if dd.rhs.tpe.typeSymbol.isAnonymousClass && dd.rhs.tpe.baseClasses.exists(sym =>
                        isFunctionalInterfaceSymbol(sym)
                    )
                then tryFixFunctionalInterface(env, dd.rhs).getOrElse(dd.rhs)
                else dd.rhs
            val selfKey =
                if isGlobalDef then VariableKey.fromName(FullName(dd.symbol).name)
                else VariableKey(dd.symbol.name.show, Some(dd.symbol.id))
            val selfTypeFromDef = sirTypeInEnv(dd.tpe, SIRTypeEnv(dd.srcPos, env.typeVars))
            // Problem that when self-type is type-lambda, then typevars in params and in sekdfType can be different.
            // i.e.dd.tpe return one set of variable, params - other.
            // So, we need to reassemble them and use type variables from params, to be consistent with body type.
            // (i.e. body.tpe should be consistent with selfType)
            val nTypeVars = env.typeVars ++ typeParamsMap
            val nVars = env.vars ++ paramEnvEntries + (selfKey -> selfTypeFromDef)
            val nEnv1 = env.copy(vars = nVars, typeVars = nTypeVars)
            // println(s"compileDefDef: this valdef ${v.show}, thisTpt: ${thisTpt.show}, thisRhs: ${thisRhs.show}, thisTpt.tpe: ${thisTpt.tpe.show}, thisTpt.symbol: ${thisTpt.symbol}, thisTpt.symbol == thisTypeSymbol: ${thisTpt.symbol == env.thisTypeSymbol}, thisTypeSymbol: ${env.thisTypeSymbol}")
            // println(s"rhs tree: ${dd.rhs}")
            val funBodyExpr = dd.rhs match
                case Inlined(_, List(v @ ValDef(cnName, thisTpt, thisRhs)), expanded)
                    if cnName.show.endsWith("_this") =>
                    val nEnv2 = nEnv1.copy(thisVal = v.symbol)
                    try compileExpr(nEnv2, expanded)
                    catch
                        case NonFatal(ex) =>
                            println(
                              s"ex:${ex.getMessage}"
                            )
                            println(s"dd.rhs=${dd.rhs.show}")
                            println(s"rhs tree: ${dd.rhs}")
                            println(
                              s"valdef ${v.show}, thisTpt: ${thisTpt.show},  thisTpt.tpe: ${thisTpt.tpe.show}, thisTpt.symbol: ${thisTpt.symbol}, thisTpt.symbol == thisTypeSymbol: ${thisTpt.symbol == env.thisTypeSymbol}, thisTypeSymbol: ${env.thisTypeSymbol}"
                            )
                            throw ex
                case _ =>
                    try compileExpr(env.copy(vars = nVars, typeVars = nTypeVars), body)
                    catch
                        case NonFatal(ex) =>
                            println(
                              s"ex:${ex.getMessage}"
                            )
                            println(s"dd.rhs=${dd.rhs.show}")
                            println(s"rhs tree: ${dd.rhs}")
                            dd.rhs match
                                case Inlined(_, List(v @ ValDef(name, tpt, rhs)), expanded) =>
                                    println(
                                      s"!catched valdef ${v.show}, _this: ${name.show.endsWith("_this")}"
                                    )
                                    println(
                                      s"tpt.symbol ${tpt.tpe.typeSymbol}, ==thisTypeSymbol: ${tpt.symbol == env.thisTypeSymbol}, thisTypeSymbol: ${env.thisTypeSymbol}"
                                    )
                                case _ =>
                            throw ex
            if debug then
                println(
                  s"compileDefDef: ${dd.symbol.fullName.toString}, btype0: ${funBodyExpr.tp.show}"
                )
            val (tpFromBody, bodyExpr) = {
                val (tp0, body0) = assembleMethodWithTypeFromBody(
                  dd.paramss,
                  sirTypeParams,
                  paramVars,
                  funBodyExpr,
                  selfTypeFromDef,
                  dd.srcPos,
                  Map.empty,
                  typeFromDefMismatchWasFound = false,
                  debug = debug
                )
                if params.isEmpty then
                    val unitVar = SIR.Var(
                      "_",
                      SIRType.Unit,
                      AnnotationsDecl.fromSrcPos(dd.srcPos)
                    )
                    tp0 match
                        case SIRType.TypeLambda(tp0TypeParams, tp0Body) =>
                            val tp = SIRType.TypeLambda(
                              tp0TypeParams,
                              SIRType.Fun(SIRType.Unit, tp0Body)
                            )
                            val body = body0 match
                                case SIR.Cast(
                                      sir,
                                      SIRType.TypeLambda(tp1TypeParams, tp1Body),
                                      anns
                                    ) =>
                                    val body1 = SIR.Cast(sir, tp1Body, anns)
                                    SIR.LamAbs(unitVar, body1, tp1TypeParams, anns)
                                case _ =>
                                    // impossible, but we should handle it
                                    SIR.LamAbs(
                                      unitVar,
                                      body0,
                                      tp0TypeParams,
                                      AnnotationsDecl.fromSrcPos(dd.srcPos)
                                    )
                            (tp, body)
                        case _ =>
                            val tp = SIRType.Fun(SIRType.Unit, tp0)
                            val body = SIR.LamAbs(
                              unitVar,
                              body0,
                              List.empty,
                              AnnotationsDecl.fromSrcPos(dd.srcPos)
                            )
                            (tp, body)
                else (tp0, body0)
            }
            val selfType = tpFromBody
            if debug then
                println(
                  s"compileDefDef: tpFromBody: ${tpFromBody.show}\n" +
                      s"selfTypeFromDef: ${selfTypeFromDef.show}\n" +
                      s"bodyExpr.tp: ${bodyExpr.tp.show}\n"
                )

            val lbFlags = tryMethodResultType(dd) match {
                case Some(rtp) => calculateLocalBindingFlags(rtp)
                case None      => LocalBindingFlags.None
            }
            CompileMemberDefResult.Compiled(
              LocalBinding(
                dd.name.show,
                selfType,
                dd.symbol,
                isGlobalDef,
                Recursivity.Rec,
                bodyExpr,
                dd.sourcePos,
                lbFlags
              )
            )
    }

    /** Assemble method type from ddef body type and types of paraneteres. (withput special handling
      * of method without parameters, which will be applied later)
      */
    private def assembleMethodWithTypeFromBody(
        paramss: List[ParamClause],
        sirTypeParams: List[SIRType.TypeVar],
        paramVars: List[SIR.Var],
        bodyExpr: AnnotatedSIR,
        typeFromDef: SIRType,
        pos: SrcPos,
        typeParamsMapping: Map[SIRType.TypeVar, SIRType.TypeVar],
        typeFromDefMismatchWasFound: Boolean = false,
        debug: Boolean = false
    ): (SIRType, AnnotatedSIR) = {

        /** let we have SIR which depends on type parameters, with tyoe lile SIR.Let(x: T,
          * Apply(Apply(mkConst,x),mkNil) ) which have type depended from T. (i.e. List[T] in our
          * case) We change it to have type TypeLambda(T, List[T]) by preprending type lambda to the
          * tp. Note, that this is not atype calculation, we assume that index is typevar is mathed
          * withtype-vars in sir and just wrapp type in type-lambda.
          */
        def prependTypeLambda(
            sir: SIR,
            typeParams: List[SIRType.TypeVar]
        ): SIR = {
            if typeParams.isEmpty then sir
            else
                sir match
                    case SIR.Cast(sir, tp, anns) =>
                        SIR.Cast(sir, SIRType.TypeLambda(typeParams, tp), anns)
                    case SIR.Constr(name, decl, args, tp, anns) =>
                        SIR.Constr(
                          name,
                          decl,
                          args,
                          SIRType.TypeLambda(typeParams, tp),
                          anns
                        )
                    case SIR.Decl(data, term) =>
                        SIR.Decl(data, prependTypeLambda(term, typeParams))
                    case SIR.LamAbs(param, term, tps, anns) =>
                        SIR.LamAbs(
                          param,
                          term,
                          typeParams ++ tps,
                          anns
                        )
                    case other: AnnotatedSIR =>
                        SIR.Cast(
                          other,
                          SIRType.TypeLambda(typeParams, other.tp),
                          AnnotationsDecl.fromSrcPos(pos)
                        )
        }

        if paramss.isEmpty then {
            if debug then
                println(s"assembleMethodWithTypeFromBody:  paramss: ${paramss} ,  paramss.isEmpty")
            if !typeFromDefMismatchWasFound then
                val retType = SIRType.substitute(typeFromDef, typeParamsMapping, Map.empty)
                SIRUnify.topLevelUnifyType(
                  retType,
                  bodyExpr.tp,
                  SIRUnify.Env.empty.withoutUpcasting
                ) match
                    case SIRUnify.UnificationSuccess(env, r) =>
                        (retType, bodyExpr)
                    case SIRUnify.UnificationFailure(_, _, _) =>
                        if debug then
                            println(
                              s"assembleMethodWithTypeFromBody: last step, failure to unify: ${retType.show} and ${bodyExpr.tp.show}, casting body to type"
                            )
                        (retType, SIR.Cast(bodyExpr, retType, AnnotationsDecl.fromSrcPos(pos)))
            else (bodyExpr.tp, bodyExpr)
        } else
            val firstList = paramss.head
            val next = paramss.tail
            val isTypeList = firstList.exists {
                case td: TypeDef => true
                case _           => false
            }
            val isVarList =
                if isTypeList then false
                else
                    firstList.exists {
                        case vd: ValDef => true
                        case _          => false
                    }
            if firstList.isEmpty then {
                // empty parameter lists, in UPLC represented as Unit argument
                // we adding hangling of empty parameters later (in compileDefDef), so here just skip.
                assembleMethodWithTypeFromBody(
                  next,
                  sirTypeParams,
                  paramVars,
                  bodyExpr,
                  typeFromDef,
                  pos,
                  typeParamsMapping,
                  typeFromDefMismatchWasFound,
                  debug
                )
                // val tp = SIRType.Fun(SIRType.Unit, nextTp)
                // val unused = SIR.Var("_", SIRType.Unit, AnnotationsDecl.fromSrcPos(pos))
                // val body = SIR.LamAbs(unused, nextBody, List.empty, AnnotationsDecl.fromSrcPos(pos))
                // (tp, body)
            } else if isTypeList then {
                // type lambda
                val (currentTps, nextTps) = sirTypeParams.splitAt(firstList.size)
                val (nextTypeFromDef, nextTypeVarMap, mismatchWasFound) = typeFromDef match {
                    case SIRType.TypeLambda(tvs, nextTypeFromDef) =>
                        if tvs.length != currentTps.length then {
                            if !typeFromDefMismatchWasFound then
                                report.warning(
                                  s"Type from definition has ${tvs.length} type parameters, but ${currentTps.length} expected",
                                  pos
                                )
                        }
                        val nextTypeVarMap = tvs
                            .zip(currentTps)
                            .foldLeft(
                              typeParamsMapping
                            ) { case (acc, (tvFromDef, tvFromParam)) =>
                                acc + (tvFromDef -> tvFromParam)
                            }
                        (nextTypeFromDef, nextTypeVarMap, true)
                    case _ =>
                        if !typeFromDefMismatchWasFound then
                            report.warning(
                              s"Type from definition is not a type lambda as it should be: ${typeFromDef.show}",
                              pos
                            )
                        (typeFromDef, typeParamsMapping, true)
                }
                val (nextType, nextBody) =
                    assembleMethodWithTypeFromBody(
                      next,
                      nextTps,
                      paramVars,
                      bodyExpr,
                      nextTypeFromDef,
                      pos,
                      nextTypeVarMap,
                      mismatchWasFound || typeFromDefMismatchWasFound,
                      debug
                    )
                val tp = SIRType.TypeLambda(
                  currentTps,
                  nextType
                )
                val body = prependTypeLambda(nextBody, currentTps)
                // TODO: eliminate use of SIR in subterms of annotatedSIR
                (tp, body.asInstanceOf[AnnotatedSIR])
            } else if isVarList then
                // function
                val (currentVars, nextVars) = paramVars.splitAt(firstList.size)
                val (nextTypeFromDef, mismatchFoundInStep) =
                    currentVars.foldLeft(typeFromDef, false) { case ((accType, mismatchFound), e) =>
                        accType match
                            case SIRType.Fun(_, out) =>
                                (out, mismatchFound)
                            case SIRType.TypeProxy(SIRType.Fun(_, out)) =>
                                (out, mismatchFound)
                            case SIRType.TypeLambda(params, body) =>
                                if !mismatchFound then
                                    report.warning(
                                      s"Type from definition is type lambda, but expected function type: ${accType.show}",
                                      pos
                                    )
                                body match {
                                    case SIRType.Fun(_, out) =>
                                        (out, true)
                                    case _ =>
                                        (body, true)
                                }
                            case _ =>
                                if !mismatchFound then
                                    report.warning(
                                      s"Type from definition is not a function type as it should be: ${accType.show}",
                                      pos
                                    )
                                (accType, true)
                    }
                val (outTp, outSIR) =
                    assembleMethodWithTypeFromBody(
                      next,
                      sirTypeParams,
                      nextVars,
                      bodyExpr,
                      nextTypeFromDef,
                      pos,
                      typeParamsMapping,
                      mismatchFoundInStep || typeFromDefMismatchWasFound,
                      debug
                    )
                currentVars.foldRight((outTp, outSIR)) { (p, acc) =>
                    val (accTp, accSir) = acc
                    val nextTp = SIRType.Fun(p.tp, accTp)
                    val nextSir = SIR.LamAbs(p, accSir, List.empty, p.anns)
                    (nextTp, nextSir)
                }
            else {
                error(
                  GenericError("Parameter list should be either type or value", pos),
                  assembleMethodWithTypeFromBody(
                    next,
                    sirTypeParams,
                    paramVars,
                    bodyExpr,
                    typeFromDef,
                    pos,
                    typeParamsMapping,
                    typeFromDefMismatchWasFound,
                    debug
                  )
                )
            }

    }

    private def compileStmt(
        env: Env,
        stmt: Tree,
        isModuleDef: Boolean = false
    ): CompileMemberDefResult = {
        // report.echo(s"compileStmt  ${stmt.show} in ${env}")
        stmt match
            case vd: ValDef =>
                // (isModuleDef && env.mode == ScalusCompilationMode.OnlyDerivations)
                compileValDef(env, vd, isModuleDef)
            case dd: DefDef =>
                compileDefDef(env, dd, isModuleDef)
            case x =>
                val body = compileExpr(env, x)
                CompileMemberDefResult.Compiled(
                  LocalBinding(
                    s"__${stmt.source.file.name.takeWhile(_.isLetterOrDigit)}_line_${stmt.srcPos.line}",
                    body.tp,
                    NoSymbol,
                    isModuleDef,
                    Recursivity.NonRec,
                    body,
                    stmt.sourcePos,
                    LocalBindingFlags.None
                  )
                )
    }

    private def compileBlock(env: Env, stmts: immutable.List[Tree], expr: Tree): AnnotatedSIR = {
        if env.debug then println(s"compileBlock: ${stmts.map(_.show).mkString("\n")}")
        val exprs = ListBuffer.empty[LocalBinding]
        val exprEnv = stmts.foldLeft(env) {
            case (env, _: Import)  => env // ignore local imports
            case (env, _: TypeDef) => env // ignore local type definitions
            case (env, stmt) =>
                compileStmt(env, stmt) match
                    case CompileMemberDefResult.Compiled(bind) =>
                        exprs += bind
                        env + (bind.variableKey -> bind.tp)
                    case _ => env
        }
        val exprExpr = compileExpr(exprEnv, expr)
        if env.debug then
            println(s"compileBlock: expr=${expr.show}")
            println(s"compileBlock: exprExprs.tp=${exprExpr.tp.show}")
        val retval = exprs.foldRight(exprExpr) { (bind, sirExpr) =>
            val flags =
                if bind.recursivity == Recursivity.Rec then SIR.LetFlags.Recursivity
                else SIR.LetFlags.None
            SIR.Let(
              List(Binding(bind.variableKey.varName, bind.tp, bind.body)),
              sirExpr,
              flags,
              AnnotationsDecl.fromSourcePosition(expr.sourcePos)
            )
        }
        if env.debug then
            println(
              s"compileBlock: retval.tp=${retval.tp.show}, ${retval.tp} isSumCaseClass=${retval.tp
                      .isInstanceOf[SIRType.SumCaseClass]}"
            )
        retval
    }

    /* Sometimes the compiler leaves Inlined nodes,
     * which we need to skip to get to the actual expression
     * otherwise,say, constants are not compiled correctly
     */
    object SkipInline {
        @tailrec
        def unapply(expr: Tree): Some[Tree] =
            expr match
                case Inlined(EmptyTree, Nil, t) => unapply(t)
                case _                          => Some(expr)
    }

    private val compileConstant: PartialFunction[Tree, scalus.uplc.Constant] = {
        case l @ Literal(c: Constant) =>
            c.tag match
                case Constants.BooleanTag => scalus.uplc.Constant.Bool(c.booleanValue)
                case Constants.StringTag  => scalus.uplc.Constant.String(c.stringValue)
                case Constants.UnitTag    => scalus.uplc.Constant.Unit
                case _ => error(LiteralTypeNotSupported(c, l.srcPos), scalus.uplc.Constant.Unit)
        case t @ Apply(bigintApply, List(SkipInline(literal)))
            if bigintApply.symbol == BigIntSymbol.requiredMethod(
              "apply",
              List(defn.StringClass.typeRef)
            ) =>
            literal match
                case Literal(c) if c.tag == Constants.StringTag =>
                    scalus.uplc.Constant.Integer(BigInt(c.stringValue))
                case _ =>
                    error(
                      GenericError(
                        s"""BigInt(${literal.show}) is not a constant expression.
                             |Try using String literals, like BigInt("123")
                             |""".stripMargin,
                        t.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )
        case t @ Apply(bigintApply, List(SkipInline(literal)))
            if bigintApply.symbol == BigIntSymbol.requiredMethod(
              "apply",
              List(defn.IntType)
            ) =>
            literal match
                case Literal(c) if c.tag == Constants.IntTag =>
                    scalus.uplc.Constant.Integer(BigInt(c.intValue))
                case Inlined(_, _, Typed(Literal(c), tpt))
                    if c.tag == Constants.IntTag && tpt.tpe =:= defn.IntType =>
                    scalus.uplc.Constant.Integer(BigInt(c.intValue))
                case _ =>
                    error(
                      GenericError(
                        s"""BigInt(${literal.show})(tree: $literal) is not a constant expression.
                               |Try using Int literals, like BigInt(123)
                               |""".stripMargin,
                        t.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )

        case Apply(i, List(SkipInline(literal)))
            if i.symbol == BigIntSymbol.requiredMethod("int2bigInt") =>
            literal match
                case Literal(c) if c.tag == Constants.IntTag =>
                    scalus.uplc.Constant.Integer(BigInt(c.intValue))
                case _ =>
                    error(
                      GenericError(
                        s"""You are trying to implicitly convert an Int expression `${literal.show}` to a BigInt constant.
                               |This is not supported.
                               |Try using Int literals, like BigInt(123) or change the type of the expression `${literal.show}` to BigInt
                               |""".stripMargin,
                        i.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )

        case expr if expr.symbol == ByteStringModuleSymbol.requiredMethod("empty") =>
            scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.empty)
        case expr if expr.symbol == DataModuleSymbol.requiredMethod("unit") =>
            scalus.uplc.Constant.Data(
              scalus.builtin.Data.Constr(0, scalus.prelude.List.Nil)
            )
        case Apply(expr, List(SkipInline(literal)))
            if expr.symbol == ByteStringModuleSymbol.requiredMethod("fromHex") =>
            literal match
                case Literal(c) if c.tag == Constants.StringTag =>
                    scalus.uplc.Constant.ByteString(
                      scalus.builtin.ByteString.fromHex(c.stringValue)
                    )
                case _ =>
                    error(
                      GenericError(
                        s"""ByteString.fromHex only accepts String literals, like ByteString.fromHex("deadbeef")
                             |But you provided `${literal.show}`, which is not a String literal.
                             |""".stripMargin,
                        expr.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )
        case Apply(expr, List(SkipInline(literal)))
            if expr.symbol == ByteStringModuleSymbol.requiredMethod("fromString") =>
            literal match
                case Literal(c) if c.tag == Constants.StringTag =>
                    scalus.uplc.Constant.ByteString(
                      scalus.builtin.ByteString.fromString(c.stringValue)
                    )
                case _ =>
                    error(
                      GenericError(
                        s"""ByteString.fromString only accepts String literals, like ByteString.fromString("deadbeef")
                               |But you provided `${literal.show}`, which is not a String literal.
                               |""".stripMargin,
                        expr.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )
        // hex"deadbeef" as ByteString using Scala 3 StringContext extension
        case expr @ Apply(
              Apply(
                byteStringHex,
                List(Apply(stringContextApply, List(SeqLiteral(List(Literal(c)), _))))
              ),
              List(SeqLiteral(List(), _))
            )
            if byteStringHex.symbol == ByteStringSymbolHex
                && stringContextApply.symbol == StringContextApplySymbol =>
            try scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.fromHex(c.stringValue))
            catch
                case NonFatal(e) =>
                    error(
                      GenericError(
                        s"""Hex string `${c.stringValue}` is not a valid hex string.
                           |Make sure it contains only hexadecimal characters (0-9, a-f, A-F)
                           |Error: ${e.getMessage}
                           |""".stripMargin,
                        expr.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )
        // utf8"hello" as ByteString using Scala 3 StringContext extension
        case expr @ Apply(
              Apply(
                byteStringUtf8,
                List(Apply(stringContextApply, List(SeqLiteral(List(Literal(c)), _))))
              ),
              List(SeqLiteral(List(), _))
            )
            if byteStringUtf8.symbol == ByteStringSymbolUtf8
                && stringContextApply.symbol == StringContextApplySymbol =>
            try scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.fromString(c.stringValue))
            catch
                case NonFatal(e) =>
                    error(
                      GenericError(
                        s"""UTF-8 string `${c.stringValue}` could not be converted to ByteString.
                           |Error: ${e.getMessage}
                           |""".stripMargin,
                        expr.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )
    }

    private def typeReprToDefaultUni(tpe: Type, list: Tree): DefaultUni =
        if tpe =:= BigIntClassSymbol.typeRef then DefaultUni.Integer
        else if tpe =:= defn.StringClass.typeRef then DefaultUni.String
        else if tpe =:= defn.BooleanClass.typeRef then DefaultUni.Bool
        else if tpe =:= defn.UnitClass.typeRef then DefaultUni.Unit
        else if tpe =:= DataClassSymbol.typeRef then DefaultUni.Data
        else if tpe =:= ByteStringClassSymbol.typeRef then DefaultUni.ByteString
        else if tpe.isPair then
            val List(t1, t2) = tpe.dealias.argInfos
            DefaultUni.Pair(typeReprToDefaultUni(t1, list), typeReprToDefaultUni(t2, list))
        else if tpe.isList then
            val t1 = tpe.dealias.argInfos.head
            DefaultUni.List(typeReprToDefaultUni(t1, list))
        else error(NotBuiltinTypeInBuiltinListConstruction(tpe, list), DefaultUni.Unit)

    private def compileBigIntOps(
        env: Env,
        lhs: Tree,
        op: Name,
        rhs: Tree,
        optree: Tree
    ): AnnotatedSIR =
        op match
            case nme.PLUS =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.addInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.srcPos.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.MINUS =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.subtractInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.MUL =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.multiplyInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.DIV =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.divideInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.MOD =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.remainderInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.LT =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.LE =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanEqualsInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.GT =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanInteger,
                    compileExpr(env, rhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(rhs.sourcePos)
                  ),
                  compileExpr(env, lhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.GE =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanEqualsInteger,
                    compileExpr(env, rhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(rhs.sourcePos)
                  ),
                  compileExpr(env, lhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.EQ =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.equalsInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.NE =>
                val anns = AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                SIR.Not(
                  SIR.Apply(
                    SIR.Apply(
                      SIRBuiltins.equalsInteger,
                      compileExpr(env, lhs),
                      SIRType.Integer ->: SIRType.Boolean,
                      AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                    ),
                    compileExpr(env, rhs),
                    SIRType.Boolean,
                    anns
                  ),
                  anns
                )
            case _ =>
                error(
                  UnsupportedBigIntOp(op.show, optree.srcPos),
                  SIR.Error(
                    "Unsupported BigInt operation",
                    AnnotationsDecl.fromSourcePosition(optree.sourcePos)
                  )
                )

    private def compileMatch(tree: Match, env: Env, isUnchecked: Boolean = false): AnnotatedSIR = {
        pmCompiler.compileMatch(tree, env, isUnchecked)
    }

    private def compileBuiltinPairMethods(
        env: Env,
        fun: Name,
        pair: Tree,
        tree: Tree
    ): AnnotatedSIR =
        fun.show match
            case "fst" =>
                val expr = compileExpr(env, pair)
                expr.tp match
                    case SIRType.BuiltinPair(t1, t2) =>
                        SIR.Apply(
                          SIRBuiltins.fstPair,
                          expr,
                          t1,
                          AnnotationsDecl.fromSourcePosition(tree.sourcePos)
                        )
                    case other =>
                        println(s"before type Mismatch error: exp.tp==${expr.tp.show}")
                        println(s"it is not BuiltinPair")
                        error(
                          TypeMismatch(
                            fun.toString,
                            SIRType.BuiltinPair(
                              SIRType.TypeVar("A", None, true),
                              SIRType.TypeVar("B", None, true)
                            ),
                            other,
                            tree.srcPos
                          ),
                          SIR.Error(
                            "Type mismatch",
                            AnnotationsDecl.fromSourcePosition(tree.sourcePos)
                          )
                        )
            case "snd" =>
                val expr = compileExpr(env, pair)
                expr.tp match
                    case SIRType.BuiltinPair(t1, t2) =>
                        SIR.Apply(
                          SIRBuiltins.sndPair,
                          expr,
                          t2,
                          AnnotationsDecl.fromSourcePosition(tree.sourcePos)
                        )
                    case other =>
                        error(
                          TypeMismatch(
                            fun.toString,
                            SIRType.BuiltinPair(
                              SIRType.TypeVar("A", None, true),
                              SIRType.TypeVar("B", None, true)
                            ),
                            other,
                            tree.srcPos
                          ),
                          SIR.Error("", AnnotationsDecl.fromSourcePosition(tree.sourcePos))
                        )
            case _ =>
                error(
                  UnsupportedPairFunction(fun.toString, tree.srcPos),
                  SIR.Error("", AnnotationsDecl.fromSourcePosition(tree.sourcePos))
                )

    private def compileBuiltinPairConstructor(
        env: Env,
        a: Tree,
        b: Tree,
        tpe1: Tree,
        tpe2: Tree,
        tree: Tree
    ): AnnotatedSIR =
        if env.debug then
            println(
              s"compileBuiltinPairConstructor: ${a.show}, ${b.show}, tpe1: $tpe1, tpe2: $tpe2"
            )
        // We can create a Pair by either 2 literals as (con pair...)
        // or 2 Data variables using MkPairData builtin
        if a.isLiteral && b.isLiteral then
            SIR.Const(
              scalus.uplc.Constant.Pair(compileConstant(a), compileConstant(b)),
              SIRType.BuiltinPair(
                sirTypeInEnv(a.tpe, a.srcPos, env),
                sirTypeInEnv(b.tpe, b.srcPos, env)
              ),
              AnnotationsDecl.fromSrcPos(tree.srcPos)
            )
        else if a.isData && b.isData then
            val exprA = compileExpr(env, a)
            val exprB = compileExpr(env, b)
            // val typeB = SIRType.TypeVar("B")
            SIR.Apply(
              SIR.Apply(
                SIRBuiltins.mkPairData,
                exprA,
                SIRType.Fun(exprB.tp, SIRType.BuiltinPair(exprA.tp, exprB.tp)),
                AnnotationsDecl.fromSrcPos(a.srcPos)
              ),
              exprB,
              SIRType.BuiltinPair(exprA.tp, exprB.tp),
              AnnotationsDecl.fromSrcPos(tree.srcPos)
            )
        else
            //  TODO: implement generic mkPair ?
            error(
              PairConstructionError(
                a,
                tpe1,
                a.isLiteral,
                a.isData,
                b,
                tpe2,
                b.isLiteral,
                b.isData,
                tree.srcPos
              ),
              SIR.Error("", AnnotationsDecl.fromSrcPos(tree.srcPos))
            )

    private def compileBuiltinListMethods(
        env: Env,
        lst: Tree,
        fun: Name,
        targs: List[Tree],
        tree: Tree
    ): AnnotatedSIR =
        if env.debug then
            println(s"compileBuiltinListMethods: ${lst.show}, fun: $fun, targs: $targs")
        val posAnns = AnnotationsDecl.fromSrcPos(tree.srcPos)
        fun.show match
            case "head" =>
                val exprA = compileExpr(env, lst)
                exprA.tp match
                    case SIRType.List(t) =>
                        SIR.Apply(
                          SIRBuiltins.headList,
                          exprA,
                          t,
                          posAnns
                        )
                    case SIRType.BuiltinList(t) =>
                        SIR.Apply(
                          SIRBuiltins.headList,
                          exprA,
                          t,
                          posAnns
                        )
                    case other =>
                        error(
                          TypeMismatch(
                            fun.toString,
                            SIRType.List(SIRType.TypeVar("A", None, false)),
                            other,
                            tree.srcPos
                          ),
                          SIR.Error("", posAnns)
                        )
            case "tail" =>
                val exprArg = compileExpr(env, lst)
                SIR.Apply(
                  SIRBuiltins.tailList,
                  exprArg,
                  exprArg.tp,
                  posAnns
                )
            case "isEmpty" =>
                SIR.Apply(
                  SIRBuiltins.nullList,
                  compileExpr(env, lst),
                  SIRType.Boolean,
                  posAnns
                )
            case _ =>
                error(UnsupportedListFunction(fun.toString, lst.srcPos), SIR.Error("", posAnns))

    /** Builds a BuiltinList from a sequence of elements. This is the shared implementation used by
      * both compileBuiltinListConstructor and compileBuiltinArrayConstructor.
      */
    private def builtinSeq(
        env: Env,
        ex: Tree,
        tpe: Tree,
        tree: Tree,
        debugName: String
    ): AnnotatedSIR =
        val tpeE = typeReprToDefaultUni(tpe.tpe, tree)
        val tpeTp = sirTypeInEnv(tpe.tpe, tree.srcPos, env)
        val builtinListTp = SIRType.BuiltinList(tpeTp)
        val anns = AnnotationsDecl.fromSrcPos(tree.srcPos)
        if env.debug then
            println(
              s"$debugName: tpeE: $tpeE, tpeTp: $tpeTp, builtinListTp.show=${builtinListTp.show}"
            )
        ex match
            case SeqLiteral(args, _) =>
                val allLiterals = args.forall(arg => compileConstant.isDefinedAt(arg))
                if allLiterals then
                    val lits = args.map(compileConstant)
                    SIR.Const(
                      scalus.uplc.Constant.List(tpeE, lits),
                      builtinListTp,
                      AnnotationsDecl.fromSrcPos(tree.srcPos)
                    )
                else
                    val nil: AnnotatedSIR = SIR.Const(
                      scalus.uplc.Constant.List(tpeE, Nil),
                      SIRType.BuiltinList.Nil(),
                      AnnotationsDecl.fromSrcPos(ex.srcPos)
                    )
                    val retval = args.foldRight(nil) { (arg, acc) =>
                        SIR.Apply(
                          SIR.Apply(
                            SIRBuiltins.mkCons,
                            compileExpr(env, arg),
                            SIRType.Fun(builtinListTp, builtinListTp),
                            anns
                          ),
                          acc,
                          builtinListTp,
                          anns
                        )
                    }
                    if env.debug then
                        println(s"$debugName: retval: $retval")
                        println(s"$debugName: retval.tp: ${retval.tp.show}")
                    retval
            case _ =>
                error(UnsupportedListApplyInvocation(tree, tpe, tree.srcPos), SIR.Error("", anns))

    private def compileBuiltinListConstructor(
        env: Env,
        ex: Tree,
        list: Tree,
        tpe: Tree,
        tree: Tree
    ): AnnotatedSIR =
        if env.debug then
            println(s"compileBuiltinListConstructor: ${ex.show}, list: $list, tpe: $tpe")
        builtinSeq(env, ex, tpe, tree, "compileBuiltinListConstructor")

    // Compiles BuiltinArray(args*) as listToArray(BuiltinList(args*))
    private def compileBuiltinArrayConstructor(
        env: Env,
        ex: Tree,
        array: Tree,
        tpe: Tree,
        tree: Tree
    ): AnnotatedSIR =
        if env.debug then
            println(s"compileBuiltinArrayConstructor: ${ex.show}, array: $array, tpe: $tpe")
        val tpeTp = sirTypeInEnv(tpe.tpe, tree.srcPos, env)
        val builtinArrayTp = SIRType.BuiltinArray(tpeTp)
        val anns = AnnotationsDecl.fromSrcPos(tree.srcPos)

        // Build the list using the shared helper
        val listExpr = builtinSeq(env, ex, tpe, tree, "compileBuiltinArrayConstructor")

        // Then wrap with listToArray
        SIR.Apply(
          SIRBuiltins.listToArray,
          listExpr,
          builtinArrayTp,
          anns
        )

    // Compiles array methods: length, apply(idx)
    private def compileBuiltinArrayMethods(
        env: Env,
        arr: Tree,
        fun: Name,
        tree: Tree,
        idx: Option[Tree] = None
    ): AnnotatedSIR =
        if env.debug then println(s"compileBuiltinArrayMethods: ${arr.show}, fun: $fun, idx: $idx")
        val posAnns = AnnotationsDecl.fromSrcPos(tree.srcPos)
        fun match
            case nme.length =>
                val exprA = compileExpr(env, arr)
                SIR.Apply(
                  SIRBuiltins.lengthOfArray,
                  exprA,
                  SIRType.Integer,
                  posAnns
                )
            case nme.apply =>
                idx match
                    case Some(idxTree) =>
                        val exprA = compileExpr(env, arr)
                        val exprIdx = compileExpr(env, idxTree)
                        val elemType = exprA.tp match
                            case SIRType.BuiltinArray(t) => t
                            case _ =>
                                error(
                                  GenericError(
                                    s"Expected array type, got ${exprA.tp.show}",
                                    tree.srcPos
                                  ),
                                  SIRType.TypeNothing
                                )
                        SIR.Apply(
                          SIR.Apply(
                            SIRBuiltins.indexArray,
                            exprA,
                            SIRType.Fun(SIRType.Integer, elemType),
                            posAnns
                          ),
                          exprIdx,
                          elemType,
                          posAnns
                        )
                    case None =>
                        error(
                          GenericError(
                            s"array.apply requires an index argument",
                            tree.srcPos
                          ),
                          SIR.Error("", posAnns)
                        )
            case _ =>
                error(
                  GenericError(s"Unsupported array function: $fun", arr.srcPos),
                  SIR.Error("", posAnns)
                )

    private def compileApply(
        env0: Env,
        f: Tree,
        targs: List[Tree],
        args: List[Tree],
        applyTpe: Type,
        applyTree: Apply
    ): AnnotatedSIR = {

        def retrieveAnnsData(tpe: Type): Map[String, SIR] = {
            if tpe.baseType(FromDataSymbol).exists then Map("fromData" -> SIR.Const.boolean(true))
            else if tpe.baseType(ToDataSymbol).exists then Map("toData" -> SIR.Const.boolean(true))
            else Map.empty[String, SIR]
        }

        if env0.debug then
            println(
              s"compileApply: ${f.show}, targs: $targs, args: $args, applyTpe: ${applyTpe.show}, applyTree: $applyTree"
            )
        val env = fillTypeParamInTypeApply(f.symbol, targs, env0)
        // val isNoSymApply = f match
        //    case Select(qual, nme.apply) if qual.symbol ==
        val (fSir, annsData0) = f match
            case Select(qual, nme.apply) if isFunctionalInterface(qual.tpe) =>
                val annsData = retrieveAnnsData(qual.tpe)
                // val isFunctionalInterface = qual.tpe.typeSymbol.hasAnnotation(
                //  Symbols.requiredClass("java.lang.FunctionalInterface")
                // )
                val nArgs = args.length
                val functionN = defn.FunctionSymbol(nArgs)
                val baseFunction = qual.tpe.baseType(functionN)
                val fSir = if qual.tpe.isSingleton && baseFunction.exists then
                    val termSymbol = qual.tpe.termSymbol
                    if termSymbol.exists then
                        val scalusCompileDerivations =
                            Symbols.requiredClass("scalus.CompileDerivations")
                        val isDerived = termSymbol.name.toString.startsWith("derived$")
                        if isDerived && !qual.tpe.baseType(scalusCompileDerivations).exists then
                            error(
                              GenericError(
                                s"""You are trying to apply a derived function `${qual.show}` with ${nArgs} arguments,
                                       |but the function is not marked as CompileDerivations and will not be available in Scalus.
                                       |""".stripMargin,
                                f.srcPos
                              ),
                              SIR.Error(
                                "Reference to non-marked function",
                                AnnotationsDecl.fromSrcPos(f.srcPos)
                              )
                            )
                        else
                            // module for derivation is a companion object of an appropriative
                            compileExpr(env, qual)
                    else
                        val message =
                            s"Can't resolve term symbol for singleton  ${qual.tpe.show}"
                        error(
                          GenericError(message, f.srcPos),
                          SIR.Error(message, AnnotationsDecl.fromSrcPos(f.srcPos))
                        )
                else
                    this.tryFixFunctionalInterface(env, qual) match
                        case Some(lambda) =>
                            compileExpr(env, lambda)
                        case None =>
                            compileExpr(env, qual)
                (fSir, annsData)
            case Select(qual, nme.apply) =>
                (compileExpr(env, f), Map.empty[String, SIR])
            case _ =>
                // check that this is method wich return From/To Data
                val annsData = extractResultType(f.tpe.widen, false, false) match
                    case Some(resultType) =>
                        retrieveAnnsData(resultType)
                    case None =>
                        // mb this is an application of an appropriative object.
                        retrieveAnnsData(f.tpe.widen)
                (compileExpr(env, f), annsData)
        val applySirType = sirTypeInEnv(applyTpe, applyTree.srcPos, env)
        val argsSir = args.map(compileExpr(env, _))
        // problem -- sometimes compiler create a proxy, where ToData/FromData is partially applied.
        // we can check this, assuming that result type is not function which returns FromData/ToData
        val mbPartiallyApplied = retrieveAnnsData(applyTpe.widen).nonEmpty
        val annsData = if mbPartiallyApplied then Map.empty else annsData0
        val applyAnns = AnnotationsDecl.fromSrcPos(applyTree.srcPos) ++ annsData
        if argsSir.isEmpty then
            SIR.Apply(
              fSir,
              SIR.Const(scalus.uplc.Constant.Unit, SIRType.Unit, applyAnns),
              applySirType,
              applyAnns
            )
        else
            // (f : (arg1 -> args2 -> ... -> res))
            // Apply(f, arg1) arg2 -> ... -> res)
            //  ....
            // Apply(...Apply(... f, arg1), arg2),,, ) res)
            val partTpes = argsSir.foldRight(applySirType)((a, acc) => SIRType.Fun(a.tp, acc))
            val (applyExpr, applyTpe) = argsSir.foldLeft((fSir, partTpes)) { (acc, arg) =>
                val (fun, tp) = acc
                val nTp = tp match
                    case SIRType.Fun(t1, t2) =>
                        t2
                    case _ =>
                        error(
                          TypeMismatch(
                            "Function type",
                            SIRType.Fun(
                              SIRType.TypeVar("A", None, false),
                              SIRType.TypeVar("B", None, false)
                            ),
                            tp,
                            f.srcPos
                          ),
                          SIRType.Unit
                        )
                (SIR.Apply(fun, arg, nTp, applyAnns), nTp)
            }
            applyExpr
    }

    /** Compile a throw expression to SIR
      *
      * {{{
      *    throw new Exception("error msg") // becomes SIR.Error("error msg")
      *    throw new CustomException // becomes SIR.Error("CustomException")
      *    throw foo() // becomes SIR.Error("foo()")
      *    inline def err(inline msg: String) = throw new RuntimeException(msg)
      *    err("test") // becomes SIR.Error("test")
      * }}}
      */
    private def compileThrowException(env: Env, ex: Tree): AnnotatedSIR =
        val ann = AnnotationsDecl.fromSrcPos(ex.srcPos)
        val msg = ex match
            case SkipInline(
                  Apply(
                    Select(New(tpt), nme.CONSTRUCTOR),
                    immutable.List(SkipInline(arg), _*)
                  )
                ) if tpt.tpe <:< defn.ThrowableType =>
                compileExpr(env, arg)
            case SkipInline(Apply(Select(New(tpt), nme.CONSTRUCTOR), Nil))
                if tpt.tpe <:< defn.ThrowableType =>
                SIR.Const(
                  scalus.uplc.Constant.String(tpt.symbol.showName),
                  SIRType.String,
                  ann
                )
            case SkipInline(term) =>
                SIR.Const(
                  scalus.uplc.Constant.String(term.show),
                  SIRType.String,
                  ann
                )
        SIR.Error(msg, ann)

    private def compileEquality(
        env: Env,
        lhs: Tree,
        op: Name,
        rhs: Tree,
        srcPos: SrcPos
    ): AnnotatedSIR = {
        lazy val lhsExpr = compileExpr(env, lhs)
        lazy val rhsExpr = compileExpr(env, rhs)
        val lhsTpe = lhs.tpe.widen.dealias
        val posAnns = AnnotationsDecl.fromSrcPos(srcPos)
        if lhsTpe =:= BigIntClassSymbol.typeRef then
            // common mistake: comparing BigInt with Int literal, e.g. BigInt(1) == 1
            rhs match
                case Literal(l) =>
                    report.error(
                      s"""You are trying to compare a BigInt and non-BigInt literal:
                           |  ${lhs.show} ${op.show} ${rhs.show}.
                           |
                           |Convert the literal to BigInt before comparing:
                           |  ${lhs.show} ${op} BigInt(${l.show})
                           |""".stripMargin,
                      rhs.srcPos
                    )
                    SIR.Error(
                      "Equality is only allowed between the same types",
                      posAnns
                    )
                case _ =>
                    val eq =
                        SIR.Apply(
                          SIR.Apply(
                            SIRBuiltins.equalsInteger,
                            lhsExpr,
                            SIRType.Integer ->: SIRType.Boolean,
                            AnnotationsDecl.fromSrcPos(lhs.srcPos)
                          ),
                          rhsExpr,
                          SIRType.Boolean,
                          posAnns
                        )
                    if op == nme.EQ then eq else SIR.Not(eq, posAnns)
        else if lhsTpe =:= defn.BooleanType then
            if op == nme.EQ then
                SIR.IfThenElse(
                  lhsExpr,
                  rhsExpr,
                  SIR.IfThenElse(
                    rhsExpr,
                    SIR.Const(
                      scalus.uplc.Constant.Bool(false),
                      SIRType.Boolean,
                      posAnns
                    ),
                    SIR.Const(
                      scalus.uplc.Constant.Bool(true),
                      SIRType.Boolean,
                      posAnns
                    ),
                    SIRType.Boolean,
                    posAnns
                  ),
                  SIRType.Boolean,
                  posAnns
                )
            else
                SIR.IfThenElse(
                  lhsExpr,
                  SIR.IfThenElse(
                    rhsExpr,
                    SIR.Const(scalus.uplc.Constant.Bool(false), SIRType.Boolean, posAnns),
                    SIR.Const(scalus.uplc.Constant.Bool(true), SIRType.Boolean, posAnns),
                    SIRType.Boolean,
                    posAnns
                  ),
                  rhsExpr,
                  SIRType.Boolean,
                  posAnns
                )
        else if lhsTpe =:= ByteStringClassSymbol.typeRef then
            val eq =
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.equalsByteString,
                    lhsExpr,
                    SIRType.ByteString ->: SIRType.Boolean,
                    posAnns
                  ),
                  rhsExpr,
                  SIRType.Boolean,
                  posAnns
                )
            if op == nme.EQ then eq else SIR.Not(eq, posAnns)
        else if lhsTpe =:= defn.StringClass.typeRef then
            val eq =
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.equalsString,
                    lhsExpr,
                    SIRType.String ->: SIRType.Boolean,
                    posAnns
                  ),
                  rhsExpr,
                  SIRType.Boolean,
                  posAnns
                )
            if op == nme.EQ then eq else SIR.Not(eq, posAnns)
        else if lhsTpe <:< DataClassSymbol.typeRef && !(lhsTpe =:= NothingSymbol.typeRef || lhsTpe =:= NullSymbol.typeRef)
        then
            val eq =
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.equalsData,
                    lhsExpr,
                    SIRType.Fun(SIRType.Data.tp, SIRType.Boolean),
                    posAnns
                  ),
                  rhsExpr,
                  SIRType.Boolean,
                  posAnns
                )
            if op == nme.EQ then eq else SIR.Not(eq, posAnns)
        else
            report.error(
              s"""Equality check operations (`==`, `!=`) are only allowed between these primitive types:
                 | BigInt, Boolean, ByteString, String, Data
                 |
                 |You are trying to compare these types:
                 |  ${lhs.tpe.widen.show} ${op} ${rhs.tpe.widen.show}
                 |  ---------- dealiaing to ------------
                 |  ${lhs.tpe.widen.dealias.show} ${op} ${rhs.tpe.widen.dealias.show}
                 |
                 |Make sure you compare values of the same supported type.
                 |
                 |If you want to compare data types try using `===` operator from Prelude.
                 |
                 |Or convert the data to a supported type before comparing,
                 |e.g. toData(x) == toData(y)
                 |
                 |""".stripMargin,
              srcPos
            )
            SIR.Error("Equality is only allowed between the same types", posAnns)
    }

    def compileExpr[T](env: Env, tree: Tree)(using Context): AnnotatedSIR = {
        if env.debug then {
            println(s"compileExpr: ${tree.showIndented(2)}")
        }
        if compileConstant.isDefinedAt(tree) then
            val const = compileConstant(tree)
            SIR.Const(
              const,
              sirTypeInEnv(tree.tpe, tree.srcPos, env),
              AnnotationsDecl.fromSrcPos(tree.srcPos)
            )
        else {
            try compileExpr2(env.copy(level = env.level + 1), tree)
            catch
                case NonFatal(e) =>
                    println(s"compileExpr: NonFatal exception: ${e.getMessage}")
                    println(s"expr:  ${tree.show}");
                    println(s"Error during compileExpr2,  tree=${tree}")
                    throw e
        }
    }

    private def compileExpr2(env: Env, tree: Tree)(using Context): AnnotatedSIR = {
        tree match
            case If(cond, t, f) =>
                if env.debug then println(s"compileExpr2: If ${cond.show}, ${t.show}, ${f.show}")
                val nEnv = env.copy(level = env.level + 1)
                val ct = compileExpr(nEnv, t)
                val cf = compileExpr(nEnv, f)
                val sirTp = sirTypeInEnv(tree.tpe, tree.srcPos, env)
                SIR.IfThenElse(
                  compileExpr(nEnv, cond),
                  ct,
                  cf,
                  sirTp,
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            case m: Match => compileMatch(m, env)
            // throw new Exception("error msg")
            // Supports any exception type that uses first argument as message
            case Apply(Ident(nme.throw_), immutable.List(ex)) => compileThrowException(env, ex)
            // Boolean operators
            // Use <:< (subtype) instead of =:= (equality) to handle union types like
            // OrType(ConstantType(true), ConstantType(false)) which appears in match expressions
            case Select(lhs, op) if lhs.tpe.widen <:< defn.BooleanType && op == nme.UNARY_! =>
                val lhsExpr = compileExpr(env, lhs)
                SIR.Not(lhsExpr, AnnotationsDecl.fromSrcPos(tree.srcPos))
            case app @ Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen <:< defn.BooleanType && op == nme.ZAND =>
                val lhsExpr = compileExpr(env, lhs)
                val rhsExpr = compileExpr(env, rhs)
                SIR.And(lhsExpr, rhsExpr, AnnotationsDecl.fromSrcPos(tree.srcPos))
            case Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen <:< defn.BooleanType && op == nme.ZOR =>
                val lhsExpr = compileExpr(env, lhs)
                val rhsExpr = compileExpr(env, rhs)
                SIR.Or(lhsExpr, rhsExpr, AnnotationsDecl.fromSrcPos(tree.srcPos))
            // Equality and inequality: ==, !=
            case Apply(Select(lhs, op), List(rhs)) if op == nme.EQ || op == nme.NE =>
                compileEquality(env, lhs, op, rhs, tree.srcPos)
            // BuiltinValue.empty - compile to unValueData(emptyDataMap)
            case expr if expr.symbol == BuiltinValueModuleSymbol.requiredMethod("empty") =>
                val emptyDataMap = SIR.Const(
                  scalus.uplc.Constant.Data(scalus.builtin.Data.Map(scalus.prelude.List.Nil)),
                  SIRType.Data.tp,
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
                SIR.Apply(
                  SIRBuiltins.unValueData,
                  emptyDataMap,
                  SIRType.BuiltinValue,
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            // BUILTINS
            case bi: Select if builtinFun(bi.symbol).isDefined =>
                if env.debug then println(s"compileExpr: builtinFun: ${bi.symbol}")
                builtinFun(bi.symbol).get
            case bi: Ident if builtinFun(bi.symbol).isDefined =>
                builtinFun(bi.symbol).get
            // BigInt stuff
            case Apply(optree @ Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= BigIntClassSymbol.typeRef =>
                compileBigIntOps(env, lhs, op, rhs, optree)
            case Select(expr, op)
                if expr.tpe.widen =:= BigIntClassSymbol.typeRef && op == nme.UNARY_- =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.subtractInteger,
                    SIR.Const(
                      scalus.uplc.Constant.Integer(BigInt(0)),
                      SIRType.Integer,
                      AnnotationsDecl.empty
                    ),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(tree.sourcePos)
                  ),
                  compileExpr(env, expr),
                  SIRType.Integer,
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            // Data constructors: Data.I(x), Data.B(x), Data.Constr(tag, args), etc.
            case Apply(fn, args) if fn.symbol == DataIApplySymbol =>
                val posAnns = AnnotationsDecl.fromSrcPos(tree.srcPos)
                val arg = compileExpr(env, args.head)
                SIR.Apply(SIRBuiltins.iData, arg, SIRType.Data.tp, posAnns)
            case Apply(fn, args) if fn.symbol == DataBApplySymbol =>
                val posAnns = AnnotationsDecl.fromSrcPos(tree.srcPos)
                val arg = compileExpr(env, args.head)
                SIR.Apply(SIRBuiltins.bData, arg, SIRType.Data.tp, posAnns)
            // Note: Data.Constr, Data.List, Data.Map are compiled as regular case class
            // constructors - the lowering handles the conversion to constrData/listData/mapData
            // List BUILTINS
            case TypeApply(Select(lst, fun), targs) if lst.isList =>
                compileBuiltinListMethods(env, lst, fun, targs, tree)
            case Select(lst, fun) if lst.isList =>
                compileBuiltinListMethods(env, lst, fun, Nil, tree)
            case tree @ TypeApply(Select(list, name), immutable.List(tpe))
                if name == termName("empty") && list.tpe =:= requiredModule(
                  "scalus.builtin.BuiltinList"
                ).typeRef =>
                val tpeE = typeReprToDefaultUni(tpe.tpe, tree)
                SIR.Const(
                  scalus.uplc.Constant.List(tpeE, Nil),
                  SIRType.BuiltinList(sirTypeInEnv(tpe.tpe, tree.srcPos, env)),
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            case Apply(
                  TypeApply(Select(list, name), immutable.List(tpe)),
                  immutable.List(arg)
                ) if name == termName("::") && list.isList =>
                val argE = compileExpr(env, arg)
                val exprType = sirTypeInEnv(tree.tpe, tree.srcPos, env)
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.mkCons,
                    argE,
                    SIRType.Fun(exprType, exprType),
                    AnnotationsDecl.fromSrcPos(arg.srcPos)
                  ),
                  compileExpr(env, list),
                  exprType,
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            case tree @ Apply(
                  TypeApply(Select(list, nme.apply), immutable.List(ltpe)),
                  immutable.List(ex)
                ) if list.tpe =:= requiredModule("scalus.builtin.BuiltinList").typeRef =>
                compileBuiltinListConstructor(env, ex, list, ltpe, tree)
            // Array BUILTINS
            // array.length, array(idx)
            case Select(arr, fun) if arr.isArray =>
                compileBuiltinArrayMethods(env, arr, fun, tree)
            case Apply(Select(arr, fun), immutable.List(idx)) if arr.isArray =>
                compileBuiltinArrayMethods(env, arr, fun, tree, Some(idx))
            // BuiltinArray(args*) => listToArray(BuiltinList(args*))
            case tree @ Apply(
                  TypeApply(Select(array, nme.apply), immutable.List(atpe)),
                  immutable.List(ex)
                ) if array.tpe =:= requiredModule("scalus.builtin.BuiltinArray").typeRef =>
                compileBuiltinArrayConstructor(env, ex, array, atpe, tree)
            // Pair BUILTINS
            // PAIR
            case Select(pair, fun) if pair.isPair =>
                compileBuiltinPairMethods(env, fun, pair, tree)
            case Apply(
                  TypeApply(Select(pair, nme.apply), immutable.List(tpe1, tpe2)),
                  immutable.List(a, b)
                ) if pair.tpe =:= requiredModule("scalus.builtin.BuiltinPair").typeRef =>
                compileBuiltinPairConstructor(env, a, b, tpe1, tpe2, tree)
            // new Constr(args)
            case Apply(TypeApply(con @ Select(f, nme.CONSTRUCTOR), targs), args) =>
                compileNewConstructor(env, f.tpe, tree.tpe.widen, args, tree)
            case Apply(con @ Select(f, nme.CONSTRUCTOR), args) =>
                compileNewConstructor(env, f.tpe, tree.tpe.widen, args, tree)
            // (a, b) as scala.Tuple2.apply(a, b)
            // we need to special-case it because we use scala-library 2.13.x
            // which does not include TASTy so we can't access the method body
            case Apply(TypeApply(app @ Select(f, nme.apply), targs), args)
                if app.symbol.fullName.show == "scala.Tuple2$.apply" =>
                compileNewConstructor(env, tree.tpe, tree.tpe.widen, args, tree)
            case Apply(app @ Select(f, nme.apply), args)
                if app.symbol.fullName.show == "scala.Tuple2$.apply" =>
                compileNewConstructor(env, tree.tpe, tree.tpe, args, tree)
            // extension 'list' on immutableSeq as varargs
            case Apply(TypeApply(listCn, List(targ)), List(x))
                if x.tpe.widen.typeSymbol == Symbols.requiredClass(
                  "scala.collection.immutable.Seq"
                ) &&
                    listCn.symbol.name.show == "list" =>
                val elemType = x.tpe.widen match
                    case AppliedType(tycon, List(elemType)) =>
                        elemType
                    case AnnotatedType(AppliedType(tycon, List(elemType)), annot) =>
                        elemType
                    case _ =>
                        error(
                          GenericError(
                            s"Expected a Seq type, but found ${x.tpe.widen.show}: tree ${x.tpe.widen}",
                            x.srcPos
                          ),
                          defn.NothingType
                        )
                val sirElemType = sirTypeInEnv(elemType, tree.srcPos, env)
                val xSir = compileExpr(env, x)
                SIR.Select(
                  xSir,
                  "list",
                  SIRType.List(sirElemType),
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            /* case class Test(a: Int)
             * val t = Test(42)
             * is translated to
             * val t = Test.apply(42), where Test.apply is a synthetic method of a companion object
             * We need to compile it as a primary constructor.
             * This also handles inline accessors like inline$apply$... generated when inline methods
             * call case class constructors.
             */
            case Apply(TypeApply(apply, targs), args)
                if apply.symbol.flags
                    .is(Flags.Synthetic) && apply.symbol.owner.flags.is(Flags.ModuleClass)
                    && effectiveMethodName(apply.symbol) == "apply" =>
                val classSymbol: Symbol = apply.symbol.owner.linkedClass
                compileNewConstructor(env, classSymbol.typeRef, tree.tpe.widen, args, tree)
            case Apply(apply @ Select(f, _), args)
                if apply.symbol.flags
                    .is(Flags.Synthetic) && apply.symbol.owner.flags.is(Flags.ModuleClass)
                    && effectiveMethodName(apply.symbol) == "apply" =>
                // get a class symbol from a companion object
                val classSymbol: Symbol = apply.symbol.owner.linkedClass
                compileNewConstructor(env, classSymbol.typeRef, tree.tpe.widen, args, tree)
            // f.apply[A, B](arg) => Apply(f, arg)
            case a @ Apply(applied @ TypeApply(fun @ Select(f, nme.apply), targs), args) =>
                if f.symbol.is(Flags.Module) then
                    // apply method in a companion object
                    compileApply(env, fun, targs, args, tree.tpe, a)
                else if defn.isFunctionType(fun.tpe.widen) || applied.tpe.isMethodType then
                    /* When we have something like this:
                     * (f: [A] => List[A] => A, a: A) => f[Data](a)
                     * f.tpe will be a MethodType
                     */
                    compileApply(env, f, targs, args, tree.tpe, a)
                else
                    error(
                      ExpressionNotSupported(a.show, tree.srcPos),
                      SIR.Error(
                        "Unsupported apply expression",
                        AnnotationsDecl.fromSrcPos(tree.srcPos)
                      )
                    )
            // case a @ Apply(applied @ TypeApply(fun @ Select(f, nme.apply), targs), args) =>
            //    ???
            // f.apply(arg) => Apply(f, arg)
            case a @ Apply(Select(f, nme.apply), args) if defn.isFunctionType(f.tpe.widen) =>
                compileApply(env, f, Nil, args, tree.tpe, a)
            case TypeApply(id @ Ident(a), targs) =>
                if isConstructorVal(tree.symbol, tree.tpe) then
                    compileNewConstructor(env, id.tpe.widen, tree.tpe, Nil, tree)
                else compileIdentOrQualifiedSelect(env, id, tree, targs)
            case Ident(a) =>
                if isConstructorVal(tree.symbol, tree.tpe) then
                    compileNewConstructor(env, tree.tpe, tree.tpe, Nil, tree)
                else compileIdentOrQualifiedSelect(env, tree, tree, Nil)
            // ignore asInstanceOf
            case TypeApply(Select(e, nme.asInstanceOf_), _) =>
                compileExpr(env, e)
            case TypeApply(sel @ Select(obj, ident), targs) =>
                // can't be field (because fields are not type-applyable)
                if isConstructorVal(tree.symbol, tree.tpe) then
                    compileNewConstructor(env, sel.tpe, tree.tpe, targs, tree)
                // TODO: now we have-no virtual calls with template parameters,
                //   but if we want to add it, we well need extend case here
                // else if isVirtualCall
                else compileIdentOrQualifiedSelect(env, sel, tree, targs)
            // case class User(name: String, age: Int)
            // val user = User("John", 42) => \u - u "John" 42
            // user.name => \u name age -> name
            case sel @ Select(obj, ident) =>
                val ts = obj.tpe.widen.dealias.typeSymbol
                lazy val fieldIdx = ts.caseFields.indexOf(sel.symbol)
                // Handle copy$default$N: compile as field select
                // user.copy$default$1 becomes user.name (for the first field)
                if ident.is(DefaultGetterName) && isCaseClassInstance(obj) then
                    ident match
                        case DefaultGetterName(nme.copy, idx) =>
                            val fields = ts.caseFields
                            if idx < fields.length then
                                val fieldName = fields(idx).name.show
                                val objSir = compileExpr(env, obj)
                                val fieldType = sirTypeInEnv(sel.tpe.widen.dealias, sel, env)
                                SIR.Select(
                                  objSir,
                                  fieldName,
                                  fieldType,
                                  AnnotationsDecl.fromSrcPos(sel.srcPos)
                                )
                            else
                                error(
                                  GenericError(
                                    s"copy$$default$$$idx index out of bounds for ${ts.name.show}",
                                    sel.srcPos
                                  ),
                                  SIR.Error(
                                    "Invalid copy default getter",
                                    AnnotationsDecl.fromSrcPos(sel.srcPos)
                                  )
                                )
                        case _ =>
                            // Not a copy default getter, fall through to normal handling
                            compileIdentOrQualifiedSelect(env, tree, tree, Nil)
                else if ts.isClass && fieldIdx >= 0 then
                    val lhs = compileExpr(env, obj)
                    val selType = sirTypeInEnv(sel.tpe.widen.dealias, sel, env)
                    SIR.Select(lhs, ident.show, selType, AnnotationsDecl.fromSrcPos(sel.srcPos))
                // else if obj.symbol.isPackageDef then
                // compileExpr(env, obj)
                else if isConstructorVal(tree.symbol, tree.tpe) then
                    compileNewConstructor(env, tree.tpe, tree.tpe, Nil, tree.srcPos)
                // else if obj.symbol == Symbols.NoSymbol then
                //    val ts = obj.tpe.widen.dealias.typeSymbol
                //    println(
                //      s"ts=${ts}, obj.tpe.widen = ${obj.tpe.widen.show},  obj=${obj.show}, ident=${ident}"
                //    )
                //
                //    ???
                else if env.thisVal != Symbols.NoSymbol && sel.symbol == env.thisVal then
                    // this.field or just field - not yet implemented
                    error(
                      GenericError(
                        s"Access to 'this' reference is not yet supported: ${tree.show}",
                        tree.srcPos
                      ),
                      SIR.Error(
                        "this reference not supported",
                        AnnotationsDecl.fromSrcPos(tree.srcPos)
                      )
                    )
                else if isVirtualCall(tree, obj, ident) then
                    compileVirtualCall(env, tree, obj, ident)
                else {
                    try compileIdentOrQualifiedSelect(env, tree, tree, Nil)
                    catch
                        case NonFatal(ex) =>
                            println(s"Exception during compiling select: ${sel.show}")
                            throw ex
                }
            // Ignore type application
            //   actually now all current applications are typeapplications
            case TypeApply(f, targs) =>
                val nEnv = fillTypeParamInTypeApply(f.symbol, targs, env)
                compileExpr(nEnv, f)
            // case class copy method: obj.copy(field = value)
            // The Scala compiler desugars copy to pass all arguments (explicit and defaulted)
            // We compile it as a Constr call
            case Apply(TypeApply(Select(obj, nme.copy), targs), args) if isCaseClassInstance(obj) =>
                compileNewConstructor(env, obj.tpe.widen.dealias, tree.tpe.widen, args, tree)
            case Apply(Select(obj, nme.copy), args) if isCaseClassInstance(obj) =>
                compileNewConstructor(env, obj.tpe.widen.dealias, tree.tpe.widen, args, tree)
            // Generic Apply
            case a @ Apply(pf @ TypeApply(f, targs), args) =>
                compileApply(env, f, targs, args, tree.tpe, a)
            case app @ Apply(f, args) =>
                compileApply(env, f, Nil, args, tree.tpe, app)
            // (x: T) => body
            case Block(
                  immutable.List(dd @ DefDef(nme.ANON_FUN, _, _, _)),
                  Closure(_, Ident(nme.ANON_FUN), _)
                ) =>
                compileStmt(env, dd) match
                    case CompileMemberDefResult.Compiled(b) => b.body
                    case ignored @ CompileMemberDefResult.Ignored(tp) =>
                        error(
                          GenericError(s"Ignoring closure, dd=${dd.show}", tree.srcPos),
                          SIR.Error("Ignored closure", AnnotationsDecl.fromSrcPos(tree.srcPos))
                        )
                    case CompileMemberDefResult.Builtin(name, tp) =>
                        error(
                          GenericError("Builtin library can be part of user code", tree.srcPos),
                          SIR.Error("Builtin definition", AnnotationsDecl.fromSrcPos(tree.srcPos))
                        )
                    case _ =>
                        // assume,  that if we have here unsupported, error is already reported.
                        SIR.Error(
                          "Closure with not supported form",
                          AnnotationsDecl.fromSrcPos(tree.srcPos)
                        )
            case Block(stmt, expr)   => compileBlock(env, stmt, expr)
            case Typed(expr, tpTree) =>
                // if env.debug then println(s"typed-here: expr=${expr.show}, tpTree=${tpTree.show}")
                // Check if the type has @unchecked annotation and the expr is a Match
                val isUncheckedMatch = expr match {
                    case _: Match =>
                        tpTree.tpe match {
                            case AnnotatedType(_, ann) if ann.symbol == defn.UncheckedAnnot => true
                            case _                                                          => false
                        }
                    case _ => false
                }
                val expr1 =
                    if isFunctionalInterface(tpTree.tpe) then
                        tryFixFunctionalInterface(env, expr).getOrElse(expr)
                    else expr
                val term = expr1 match {
                    case m: Match if isUncheckedMatch => compileMatch(m, env, isUnchecked = true)
                    case _                            => compileExpr(env, expr1)
                }
                val tp = sirTypeInEnv(tpTree.tpe, tree.srcPos, env)
                if expr.tpe.widen =:= tpTree.tpe.widen then term
                else
                    if env.debug then
                        println(
                          s"Typed: ${expr.show} with type ${expr.tpe.show} to ${tpTree.tpe.show} (${tp.show})"
                        )
                    SIR.Cast(term, tp, AnnotationsDecl.fromSrcPos(tree.srcPos))
            case Inlined(call, bindings, expr) =>
                val newEnv =
                    if call.isEmpty then env
                    else env.pushInlineCall(SIRPosition.fromSrcPos(call.srcPos))
                val r = compileBlock(newEnv, bindings, expr)
                // val t = r.asTerm.show
                // report.info(s"Inlined: ${bindings}, ${expr.show}\n${t}", Position(SourceFile.current, globalPosition, 0))
                r
            case Return(expr, _) =>
                error(
                  ReturnNotSupported(expr, tree.srcPos),
                  SIR.Error("Return not supported", AnnotationsDecl.fromSrcPos(tree.srcPos))
                )
            case Assign(lhs, rhs) =>
                error(
                  ExpressionNotSupported("Variable assignment", tree.srcPos),
                  SIR.Error(
                    "Unsupported assign expression",
                    AnnotationsDecl.fromSrcPos(tree.srcPos)
                  )
                )
            case Try(_, _, _) =>
                error(
                  ExpressionNotSupported("'try-catch-finally'", tree.srcPos),
                  SIR.Error("Unsupported try expression", AnnotationsDecl.fromSrcPos(tree.srcPos))
                )
            case WhileDo(cond, body) =>
                error(
                  ExpressionNotSupported("'while' expression", tree.srcPos),
                  SIR.Error("Unsupported while expression", AnnotationsDecl.fromSrcPos(tree.srcPos))
                )
            case SeqLiteral(elems, elemtpt) =>
                val sirElemType = sirTypeInEnv(elemtpt.tpe, tree.srcPos, env)
                val sirElems = elems.map(compileExpr(env, _))
                val listType = SIRType.List(sirElemType)
                val listDataDecl = SIRType.List.dataDecl
                val nil0: AnnotatedSIR = SIR.Constr(
                  SIRType.List.Nil.constrDecl.name,
                  listDataDecl,
                  Nil,
                  listType,
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
                val list = sirElems.foldRight(nil0) { (elem, acc) =>
                    SIR.Constr(
                      SIRType.List.Cons.name,
                      listDataDecl,
                      List(elem, acc),
                      listType,
                      AnnotationsDecl.fromSrcPos(tree.srcPos)
                    )
                }
                SIR.Constr(
                  SIRType.Varargs.name,
                  SIRType.Varargs.dataDecl,
                  List(list),
                  SIRType.Varargs(sirElemType),
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            case x =>
                println(s"Not supported expression, tree=${x}")
                error(
                  ExpressionNotSupported(x.show, tree.srcPos),
                  SIR.Error("Unsupported expression", AnnotationsDecl.fromSrcPos(tree.srcPos))
                )
    }

    private def fillTypeParamInTypeApply(sym: Symbol, targs: List[Tree], env: Env): Env = {
        val tparams = sym.typeParams
        val targsSirTypes = targs.map(t =>
            val wt = t.tpe.widen
            sirTypeInEnv(wt, t.srcPos, env)
        )
        val nTypeVars = tparams
            .zip(targsSirTypes)
            .map { case (tp, v) =>
                tp -> v
            }
            .toMap
        env.copy(typeVars = env.typeVars ++ nTypeVars)
    }

    def compileToSIR(tree: Tree, debug: Boolean): SIR = {
        if debug then {
            println(s"compileToSIR: ${tree.show}")
        }
        compileExpr(Env.empty.copy(debug = debug), tree)
    }

    // def sirType(tp: Type, srcPos: SrcPos): SIRType = {
    //        sirTypeInEnv(tp, SIRTypesHelper.SIRTypeEnv(srcPos, Map.empty))
    // }

    def sirTypeInEnv(tp: Type, srcPos: SrcPos, env: Env): SIRType = {
        // TODO: add option for tracing typer.  Now - set flag if needed
        sirTypeInEnv(
          tp,
          SIRTypeEnv(srcPos, env.typeVars, trace = if false then env.debug else false)
        )
    }

    protected def sirTypeInEnv(tp: Type, env: SIRTypeEnv): SIRType = {
        try typer.sirTypeInEnv(tp, env)
        catch
            case e: TypingException =>
                if e.cause == null then report.error(e.getMessage, e.pos)
                else report.error(e.getMessage + " caused by " + e.cause.getMessage, e.pos)
                if env.trace then e.printStackTrace()
                if true then throw e
                SIRType.TypeNothing
    }

    private def isVirtualCall(@unused tree: Tree, qualifier: Tree, name: Name): Boolean = {
        val qualifierSym = qualifier.symbol
        if qualifierSym == Symbols.NoSymbol then {
            // this can be a case when we apply inline lamnda or function,
            //  todo: control more tightly
            false
        } else
            val declDenotation = qualifier.tpe.widen.decl(name)
            !declDenotation.exists
    }

    private def compileVirtualCall(
        env: Env,
        tree: Tree,
        qualifier: Tree,
        name: Name
    ): AnnotatedSIR = {
        if env.debug then println("compile virtual call: " + tree.show)
        val qualifierSym = qualifier.symbol
        val qualifierTypeSym = qualifier.tpe.typeSymbol
        if qualifierSym == Symbols.NoSymbol then
            error(
              GenericError(s"Cannot resolve symbol ${qualifier.show}", tree.srcPos),
              ()
            )
        val member = qualifierSym.info.member(name)
        if !member.exists then
            error(
              GenericError(s"Member ${name.show} not found in ${qualifierSym.show}", tree.srcPos),
              SIR.Error("Member not found", AnnotationsDecl.fromSrcPos(tree.srcPos))
            )
        else
            member.info match
                case _: MethodType | _: PolyType =>
                    if member.symbol.flags.is(Flags.Method) then
                        if env.debug then
                            println(
                              s"virtual call: ${qualifierSym.show} ${qualifierTypeSym.show} ${member.symbol.fullName} is method"
                            )
                    SIR.ExternalVar(
                      qualifierTypeSym.fullName.toString,
                      member.symbol.fullName.toString,
                      sirTypeInEnv(member.info, tree.srcPos, env),
                      AnnotationsDecl.fromSrcPos(tree.srcPos)
                    )
                case _ =>
                    error(
                      GenericError(
                        s"Overriden method in ${qualifierSym.show} should be a method or a polytype",
                        tree.srcPos
                      ),
                      SIR.Error("Invalid overriding", AnnotationsDecl.fromSrcPos(tree.srcPos))
                    )
    }

    private def compileTreeInModule(
        env: Env,
        @unused td: TypeDef,
        tree: Tree
    ): Option[LocalBinding] = {

        tree match
            case dd: DefDef =>
                val toProcess = (
                  !dd.symbol.flags.is(Flags.Synthetic) ||
                      dd.symbol.name.startsWith("derived")
                ) && !dd.symbol.hasAnnotation(IgnoreAnnot)
                if toProcess then
                    compileStmt(
                      env,
                      dd,
                      isModuleDef = true
                    ) match
                        case CompileMemberDefResult.Compiled(b) => Some(b)
                        case _                                  => None
                else None
            case vd: ValDef =>
                val toProcess = (
                  !vd.symbol.flags.isOneOf(Flags.Synthetic | Flags.Case)
                      ||
                          vd.symbol.name.startsWith("derived") && !vd.symbol.flags.is(
                            Flags.Case
                          )
                ) && !vd.symbol.hasAnnotation(IgnoreAnnot)
                if toProcess then
                    // println(s"valdef: ${vd.symbol.fullName}")
                    compileStmt(
                      env,
                      vd,
                      isModuleDef = true
                    ) match
                        case CompileMemberDefResult.Compiled(b) =>
                            Some(b)
                        case CompileMemberDefResult.Builtin(name, tp) =>
                            None
                        case CompileMemberDefResult.Ignored(tp) =>
                            None
                        case CompileMemberDefResult.NotSupported =>
                            error(
                              GenericError(
                                s"Not supported: ${vd.symbol.fullName.show}",
                                vd.srcPos
                              ),
                              None
                            )
                else None
            case cd: TypeDef if cd.symbol.flags.is(Flags.Module) =>
                // submodules are not supported for now.
                None
            case _ =>
                None

    }

    private def makeGenericTupleDecl(size: Int, srcPos: SrcPos): DataDecl = {
        // we will generate tupleN decl for each type.
        //  (because we have no generic tupel with AnyKind in SIR)
        //  theoreticalle we can add all to "scala.::*" as in scala.
        //  but not sure if exists use-case when it needed.
        //  (case over generic tuple with different arity in the user program)
        val tupleName = s"scala.Tuple$size"
        this.globalDataDecls.get(FullName(tupleName)) match
            case Some(decl) =>
                decl
            case None =>
                val tupleNameHash = tupleName.hashCode
                val tupleTypeParams =
                    (1 to size)
                        .map(i => SIRType.TypeVar(s"T$i", Some(tupleNameHash + i * 5), false))
                        .toList
                val tupleParams =
                    tupleTypeParams.zipWithIndex
                        .map((t, i) => TypeBinding(s"_${i + 1}", t))
                        .toList
                val tupleDecl = DataDecl(
                  tupleName,
                  List(
                    ConstrDecl(
                      tupleName,
                      tupleParams,
                      tupleTypeParams,
                      List.empty,
                      AnnotationsDecl.fromSrcPos(srcPos)
                    )
                  ),
                  tupleTypeParams,
                  AnnotationsDecl.fromSrcPos(srcPos)
                )
                this.globalDataDecls.put(FullName(tupleName), tupleDecl)
                tupleDecl
    }

    @tailrec
    private def extractResultType(tpe: Type, isResult: Boolean, required: Boolean): Option[Type] = {
        tpe match {
            case mt: MethodType =>
                // TODO: adopt to carrying case
                // extractResultType(mt.resType, true, true)
                Some(mt.resType)
            case pt: PolyType =>
                extractResultType(pt.resType, isResult, required)
            case rt: RefinedType =>
                extractResultType(rt.underlying, isResult, required)
            case _ =>
                if isResult then Some(tpe) else None

        }
    }

    private def tryMethodResultType(dd: DefDef): Option[Type] = {
        if !dd.paramss.exists(_.exists {
                case vd: ValDef => true
                case _          => false
            })
        then Some(dd.tpe)
        else extractResultType(dd.tpe, false, false)
    }

    private def isFunctionalInterface(tpe: Type): Boolean = {
        isFunctionalInterfaceSymbol(tpe.typeSymbol)
    }

    private def isFunctionalInterfaceSymbol(typeSymbol: Symbol): Boolean = {
        typeSymbol.hasAnnotation(
          Symbols.requiredClass("java.lang.FunctionalInterface")
        )
    }

    /*
    private def applyStaticInheritanceInModule(
        parentSym: Symbol,
        module: scalus.compiler.sir.Module,
        env: Env,
        possibleOverrides: Map[String, LocalBinding]
    ): List[SuperBinding] = {
        val thisClassNames = module.defs.map(_.name).toSet
        for {
            binding <- module.defs
        } yield {
            val (nSIR, isChanged) =
                applyStaticInheritanceInSIR(
                  parentSym,
                  binding.value,
                  env,
                  possibleOverrides,
                  thisClassNames
                )
            SuperBinding(
              binding.name,
              binding.tp,
              parentSym,
              env.thisTypeSymbol,
              nSIR,
              isChanged
            )
        }
    }
     */

    /*
    private def applyStaticInheritanceInSIR(
        parentSym: Symbol,
        sir: SIR,
        env: Env,
        possibleOverrides: Map[String, LocalBinding],
        thisClassNames: Set[String]
    ): (SIR, Boolean) = {
        sir match
            case asir: AnnotatedSIR =>
                applyStaticInheritanceInAnnotatedSIR(
                  parentSym,
                  asir,
                  env,
                  possibleOverrides,
                  thisClassNames
                )
            case SIR.Decl(data, term) =>
                val (newTerm, changed) =
                    applyStaticInheritanceInSIR(
                      parentSym,
                      term,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                SIR.Decl(data, newTerm) -> changed
    }

    private def applyStaticInheritanceInAnnotatedSIR(
        parentSym: Symbol,
        sir: AnnotatedSIR,
        env: Env,
        possibleOverrides: Map[String, LocalBinding],
        thisClassNames: Set[String]
    ): (AnnotatedSIR, Boolean) = {
        sir match
            case SIR.ExternalVar(moduleName, name, tp, anns) =>
                possibleOverrides.get(name) match
                    case Some(binding) =>
                        SIR.ExternalVar(
                          env.thisTypeSymbol.fullName.toString,
                          binding.fullName.name,
                          tp,
                          anns
                        ) -> true
                    case None =>
                        if thisClassNames.contains(name) then
                            SIR.ExternalVar(
                              env.thisTypeSymbol.fullName.toString,
                              name,
                              tp,
                              anns
                            ) -> true
                        else sir -> false
            case SIR.Var(name, tp, anns) =>
                sir -> false
            case SIR.Let(binding, body, flags, anns) =>
                var bindingIsChanged = false
                val newBinding = binding.map { b =>
                    val (newBody, changed) =
                        applyStaticInheritanceInSIR(
                          parentSym,
                          b.value,
                          env,
                          possibleOverrides,
                          thisClassNames
                        )
                    if changed then bindingIsChanged = true
                    Binding(b.name, b.tp, newBody)
                }
                val (newBody, bodyChanged) =
                    applyStaticInheritanceInSIR(
                      parentSym,
                      body,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                SIR.Let(
                  newBinding,
                  newBody,
                  flags,
                  anns
                ) -> (bindingIsChanged || bodyChanged)
            case SIR.LamAbs(param, term, typeParams, anns) =>
                val (newTerm, termChanged) =
                    applyStaticInheritanceInSIR(
                      parentSym,
                      term,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val newSIR = SIR.LamAbs(param, newTerm, typeParams, anns)
                (newSIR, termChanged)
            case SIR.Apply(f, arg, tp, anns) =>
                val (newF, fChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      f,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val (newArg, argChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      arg,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val newSIR = SIR.Apply(newF, newArg, tp, anns)
                (newSIR, fChanged || argChanged)
            case SIR.Select(obj, name, tp, anns) =>
                val (newObj, objChanged) =
                    applyStaticInheritanceInSIR(
                      parentSym,
                      obj,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val newSIR = SIR.Select(newObj, name, tp, anns)
                (newSIR, objChanged)
            case SIR.Const(_, _, _) =>
                sir -> false
            case SIR.And(x, y, anns) =>
                val (newX, xChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      x,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val (newY, yChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      y,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val newSIR = SIR.And(newX, newY, anns)
                (newSIR, xChanged || yChanged)
            case SIR.Or(x, y, anns) =>
                val (newX, xChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      x,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val (newY, yChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      y,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val newSIR = SIR.Or(newX, newY, anns)
                (newSIR, xChanged || yChanged)
            case SIR.Not(x, anns) =>
                val (newX, xChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      x,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val newSIR = SIR.Not(newX, anns)
                (newSIR, xChanged)
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                val (newCond, condChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      cond,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val (newT, tChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      t,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val (newF, fChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      f,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val newSIR = SIR.IfThenElse(newCond, newT, newF, tp, anns)
                (newSIR, condChanged || tChanged || fChanged)
            case SIR.Builtin(name, tp, anns) =>
                sir -> false
            case SIR.Error(msg, anns, cause) =>
                sir -> false
            case SIR.Constr(name, dataDecl, args, tp, anns) =>
                var argsAreChanged = false
                val newArgs = args.map { arg =>
                    val (newArg, changed) =
                        applyStaticInheritanceInSIR(
                          parentSym,
                          arg,
                          env,
                          possibleOverrides,
                          thisClassNames
                        )
                    if changed then argsAreChanged = true
                    newArg
                }
                val newSIR = SIR.Constr(name, dataDecl, newArgs, tp, anns)
                (newSIR, argsAreChanged)
            case SIR.Match(scrutinee, cases, tp, anns) =>
                var casesAreChanged = false
                val newCases = cases.map { c =>
                    val (newCaseBody, changed) =
                        applyStaticInheritanceInSIR(
                          parentSym,
                          c.body,
                          env,
                          possibleOverrides,
                          thisClassNames
                        )
                    if changed then casesAreChanged = true
                    SIR.Case(c.pattern, newCaseBody, c.anns)
                }
                val (newScrutinee, scrutineeChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      scrutinee,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val newSIR = SIR.Match(newScrutinee, newCases, tp, anns)
                (newSIR, scrutineeChanged || casesAreChanged)
            case SIR.Cast(expr, tp, anns) =>
                val (newExpr, exprChanged) =
                    applyStaticInheritanceInAnnotatedSIR(
                      parentSym,
                      expr,
                      env,
                      possibleOverrides,
                      thisClassNames
                    )
                val newSIR = SIR.Cast(newExpr, tp, anns)
                (newSIR, exprChanged)
    }
     */

    private def calculateLocalBindingFlags(tp: Type): LocalBindingFlags = {
        if tp.baseType(FromDataSymbol).exists || tp.baseType(ToDataSymbol).exists then
            LocalBindingFlags.ErasedOnDataRepr
        else LocalBindingFlags.None
    }

    def gatherExternalModules(
        myModuleName: String,
        module: Module,
        acc: Map[String, SIR.ExternalVar]
    ): Map[String, SIR.ExternalVar] = {
        module.defs.foldLeft(acc) { (acc, binding) =>
            gatherExternalModulesFromSir(myModuleName, binding.value, acc)
        }
    }

    def gatherExternalModulesFromSir(
        myModuleName: String,
        sir: SIR,
        acc: Map[String, SIR.ExternalVar]
    ): Map[String, SIR.ExternalVar] = {
        sir match {
            case expr: AnnotatedSIR =>
                gatherExternalModulesFromSirExpr(myModuleName, expr, acc)
            case SIR.Decl(_, term) =>
                gatherExternalModulesFromSir(myModuleName, term, acc)
        }
    }

    private def gatherExternalModulesFromSirExpr(
        myModuleName: String,
        sir: AnnotatedSIR,
        acc: Map[String, SIR.ExternalVar]
    ): Map[String, SIR.ExternalVar] = {
        sir match
            case v @ SIR.ExternalVar(moduleName, _, _, _) =>
                if moduleName == myModuleName then acc
                else
                    acc.get(moduleName) match
                        case Some(_) => acc
                        case None =>
                            acc.updated(moduleName, v)
            case SIR.Let(binding, body, _, _) =>
                val acc1 =
                    binding.foldLeft(acc)((acc, b) =>
                        gatherExternalModulesFromSir(myModuleName, b.value, acc)
                    )
                gatherExternalModulesFromSir(myModuleName, body, acc1)
            case SIR.LamAbs(_, term, _, _) =>
                gatherExternalModulesFromSir(myModuleName, term, acc)
            case SIR.Apply(f, arg, tp, anns) =>
                val acc1 = gatherExternalModulesFromSir(myModuleName, f, acc)
                gatherExternalModulesFromSir(myModuleName, arg, acc1)
            case SIR.Select(obj, _, _, _) =>
                gatherExternalModulesFromSir(myModuleName, obj, acc)
            case SIR.And(x, y, _) =>
                val acc1 = gatherExternalModulesFromSir(myModuleName, x, acc)
                gatherExternalModulesFromSir(myModuleName, y, acc1)
            case SIR.Or(x, y, _) =>
                val acc1 = gatherExternalModulesFromSir(myModuleName, x, acc)
                gatherExternalModulesFromSir(myModuleName, y, acc1)
            case SIR.Not(x, _) =>
                gatherExternalModulesFromSir(myModuleName, x, acc)
            case SIR.IfThenElse(cond, t, f, _, _) =>
                val acc1 = gatherExternalModulesFromSir(myModuleName, cond, acc)
                val acc2 = gatherExternalModulesFromSir(myModuleName, t, acc1)
                gatherExternalModulesFromSir(myModuleName, f, acc2)
            case SIR.Constr(_, _, args, _, _) =>
                args.foldLeft(acc) { (acc, arg) =>
                    gatherExternalModulesFromSir(myModuleName, arg, acc)
                }
            case SIR.Match(scrutinee, cases, _, _) =>
                val acc1 = gatherExternalModulesFromSir(myModuleName, scrutinee, acc)
                cases.foldLeft(acc1) { (acc, c) =>
                    gatherExternalModulesFromSir(myModuleName, c.body, acc)
                }
            case SIR.Cast(expr, _, _) =>
                gatherExternalModulesFromSir(myModuleName, expr, acc)
            case SIR.Error(_, _, _) | SIR.Var(_, _, _) | SIR.Builtin(_, _, _) |
                SIR.Const(_, _, _) =>
                acc

    }

    /** Build tree which represent 'ModuleWithDeps' constant with dependencies for current module.
      * return (tree for moduleWithDeps, optionsl limitation for backend.)
      */
    def buildDepsTree(
        currentModuleName: String,
        dependencies: Map[String, SIR.ExternalVar],
        srcPos: SrcPos
    ): Tree = {
        // var requireV3Lowering = false
        val moduleWithDepsRefs = dependencies.map { case (name, externalVar) =>
            name -> {
                val cName = name.replace("$", "")
                val moduleSym = Symbols.requiredModule(cName)
                val moduleRef = tpd.ref(moduleSym).withSpan(srcPos.span)
                // println(s"moduleRef: ${moduleRef.show}")
                // println(s"flags: ${moduleSym.flags.flagsString}")
                if !moduleSym.isCompleted then {
                    try
                        // try complete by getting info
                        @unused val info_ = moduleSym.info
                    catch
                        case NonFatal(ex) =>
                            report.error(
                              s"Module ${cName}, referenced from var ${externalVar.name} at  ${externalVar.anns.pos.show} in module $currentModuleName is not completed\n" +
                                  s"usually this means, that non-scalus object to scalus program",
                              srcPos
                            )
                }

                if !moduleSym.isCompleted || moduleSym.info.findDecl(
                      Plugin.SIR_MODULE_VAL_NAME.toTermName,
                      Flags.EmptyFlags
                    ) == SymDenotations.NoDenotation
                then {
                    val module =
                        if externalVar.name.endsWith("derived$FromData") || externalVar.name
                                .endsWith("derived$ToData")
                        then
                            // this invocation will be replaced by linker if backend use universal data representation.
                            //   (i.e. if this is S3LoweringBackend)
                            // we generate SIR for all backends,  so -- not produce error, this ExternalVars will be
                            // replacedf
                            val moduleSym = Symbols.requiredModule(
                              "scalus.builtin.internal.UniversalDataConversion"
                            )
                            Module(
                              SIRVersion,
                              moduleSym.fullName.toString,
                              false,
                              Some("S3LoweringBackend"),
                              List.empty
                            )
                        else
                            report.error(
                              s"Module ${cName}, referenced from var ${externalVar.name} at  ${externalVar.anns.pos.show} is not found\n" +
                                  s"on-resolved var: ${externalVar}",
                              srcPos
                            )
                            // write empty module instean.
                            Module(SIRVersion, s"notfound:${cName}", false, None, List.empty)
                    val moduleToExprSym = Symbols.requiredModule("scalus.compiler.sir.ModuleToExpr")
                    val moduleTree = {
                        convertFlatToTree(
                          module,
                          ModuleHashSetReprFlat,
                          moduleToExprSym,
                          srcPos.span,
                          false
                        )
                    }
                    val depsTree = tpd.ref(defn.NilModule).withSpan(srcPos.span)
                    val moduleWithDeps =
                        tpd.ref(sirModuleWithDepsModule)
                            .select(sirModuleWithDepsModule.requiredMethod("apply"))
                            .appliedToArgs(List(moduleTree, depsTree))
                            .withSpan(srcPos.span)
                    moduleWithDeps
                } else
                    val valSirModuleSym = moduleSym.requiredMethod(Plugin.SIR_MODULE_VAL_NAME)
                    val sirTree =
                        moduleRef.select(valSirModuleSym).withSpan(srcPos.span)
                    val valSirDepsSym = moduleSym.requiredMethod(Plugin.SIR_DEPS_VAL_NAME)
                    val depsTree =
                        moduleRef.select(valSirDepsSym).withSpan(srcPos.span)
                    val moduleWithDeps = tpd
                        .ref(sirModuleWithDepsModule)
                        .select(sirModuleWithDepsModule.requiredMethod("apply"))
                        .appliedToArgs(List(sirTree, depsTree))
                    moduleWithDeps
            }
        }
        val listDeps = moduleWithDepsRefs.values.toList
        val listDepsExpr =
            ref(sirModuleWithDepsModule)
                .select(sirModuleWithDepsModule.requiredMethod("list"))
                .appliedTo(SeqLiteral(listDeps, TypeTree(sirModuleWithDepsType)))
        listDepsExpr
    }

}

object SIRCompiler {

    case class Env(
        vars: Map[VariableKey, SIRType],
        typeVars: Map[Symbol, SIRType],
        debug: Boolean = false,
        level: Int = 0,
        resolvedClasses: Map[Symbol, SIRType] = Map.empty,
        // set during compilation of object
        thisTypeSymbol: Symbol = Symbols.NoSymbol,

        //  during deifnition of inline method, this is variable,
        //  which contains binding of 'this' object.
        thisVal: Symbol = Symbols.NoSymbol,
        createEx: RuntimeException = new RuntimeException("Env.create.stacktrace"),
        // Stack of inline call sites (most recent first)
        inlineCallStack: List[SIRPosition] = Nil
    ) {

        def ++(bindings: Iterable[(VariableKey, SIRType)]): Env = {
            copy(vars = vars ++ bindings)
        }

        def +(ntpe: (VariableKey, SIRType)): Env = {
            copy(vars = vars + ntpe)
        }

        def withDebug: Env = copy(debug = true)

        def withoutDebug: Env = copy(debug = false)

        def pushInlineCall(pos: SIRPosition): Env =
            if pos.isEmpty then this
            else copy(inlineCallStack = pos :: inlineCallStack)

    }

    object Env {

        def empty: Env = Env(Map.empty, Map.empty)

    }

}
