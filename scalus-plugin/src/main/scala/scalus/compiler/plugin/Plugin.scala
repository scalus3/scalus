package scalus.compiler.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.transform.{ElimByName, Pickler, PostTyper}
import dotty.tools.dotc.typer.Implicits
import dotty.tools.dotc.util.SrcPos
import scalus.*
import scalus.serialization.flat.FlatInstances.SIRHashConsedFlat
import scalus.compiler.sir.SIRPosition

import scala.language.implicitConversions

class Plugin extends StandardPlugin {
    val name: String = "scalus"
    override val description: String = "Compile Scala to Scalus IR"

    // val compiledSirs: mutable.Map[String, SIR] = mmutable.Map.empty

    /** For enabling debug in scalus
      *
      * Note: In Scala 3.7+, the `init` method is deprecated in favor of `initialize(options)(using
      * Context)`. When upgrading to Scala 3.7+, rename this method to `initialize` and add the
      * `(using Context)` parameter.
      */
    override def init(options: List[String]): List[PluginPhase] = {
        val debugLevel = options
            .find(_.startsWith("debugLevel="))
            .map(_.substring("debugLevel=".length))
            .map(_.toInt)
            .getOrElse(0)
        new ScalusPreparePhase(debugLevel) :: ScalusPhase(debugLevel) :: Nil
    }
}

object Plugin {

    // TODO: check that we have no variable with such name in the source code
    val SIR_MODULE_VAL_NAME = "sirModule"
    val SIR_DEPS_VAL_NAME = "sirDeps"

    // Module name for top-level definitions in scalus.compiler package
    // The file compiler.scala generates a synthetic module named compiler$package
    // IMPORTANT: We MUST NOT use "scalus.compiler" as a module name because on case-insensitive
    // filesystems (like macOS), this would conflict with "scalus.Compiler" class file
    val newCompilerModuleName = "scalus.compiler.compiler$package"

    def retrieveCompilerOptions(
        posTree: tpd.Tree,
        isCompilerDebug: Boolean
    )(using Context): tpd.Tree = {
        // Try new package first, fall back to old for backward compatibility
        // TODO(0.15): Remove scalus.Compiler.Options fallback after removing deprecated API
        val compilerOptionType =
            try requiredClassRef("scalus.compiler.Options")
            catch case _: Exception => requiredClassRef("scalus.Compiler.Options")
        if !ctx.phase.allowsImplicitSearch then
            println(
              s"ScalusPhase: Implicit search is not allowed in phase ${ctx.phase.phaseName}. "
            )
        summon[Context].typer.inferImplicit(
          compilerOptionType,
          tpd.EmptyTree,
          posTree.span
        ) match {
            case Implicits.SearchSuccess(tree, ref, level, isExtension) =>
                if isCompilerDebug then report.echo(s"Found compiler options: ${tree.show}")
                tree
            case failure @ Implicits.SearchFailure(_) =>
                if isCompilerDebug then {
                    report.warning(
                      s"ScalusPhase: No compiler options found, using default options",
                      posTree.srcPos.startPos
                    )
                    report.warning(s"search result: ${failure.show}")
                }
                // Try new package first, fall back to old for backward compatibility
                // Wrap entirely in try-catch since the module/method might not be available yet
                // TODO(0.15): Remove scalus.Compiler fallback after removing deprecated API
                val (scalusCompilerModule, defaultOptionsMethod) = {
                    try
                        val mod = requiredModule(newCompilerModuleName)
                        val method = mod.requiredMethod("defaultOptions")
                        (mod, method)
                    catch
                        case _: Exception =>
                            // Fall back to old module
                            val mod = requiredModule("scalus.Compiler")
                            val method = mod.requiredMethod("defaultOptions")
                            (mod, method)
                }
                tpd.ref(scalusCompilerModule)
                    .select(defaultOptionsMethod)
                    .withSpan(posTree.span)
        }

    }

}

/** A prepare phase which should run before pickling to create additional variables in objects,
  * where SIR will be stored.
  */
class ScalusPreparePhase(debugLevel: Int) extends PluginPhase with IdentityDenotTransformer {

    val phaseName = "ScalusPrepare"

    // We need to run before the "pickler" phase to have SIR available for pickling
    override val runsAfter: Set[String] = Set(PostTyper.name)
    override val runsBefore: Set[String] = Set(Pickler.name)

    override def changesMembers: Boolean = true

    override def allowsImplicitSearch: Boolean = true

    override def prepareForTypeDef(tree: tpd.TypeDef)(using Context): Context = {
        // bug in dotty: sometimes we called with the wrong phase in context
        if summon[Context].phase != this then
            prepareForTypeDef(tree)(using summon[Context].withPhase(this))
        else ctx
    }

    override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
        val compileAnnot = requiredClassRef("scalus.Compile").symbol.asClass
        if tree.symbol.hasAnnotation(compileAnnot) && tree.symbol.is(Flags.Module) then
            val preprocessor = new SIRPreprocessor(this, debugLevel)
            preprocessor.transformTypeDef(tree)
        else tree
    }

    override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = {
        // Support both old (scalus.Compiler) and new (scalus.compiler.package$package) locations
        // The new module may not exist during compilation of scalus-core itself, so wrap in try-catch
        // TODO(0.15): Remove old scalus.Compiler symbol handling after removing deprecated API
        val oldCompilerModule = requiredModule("scalus.Compiler")

        // Try to get new module symbols - wrap entirely in try-catch since the module
        // might exist but methods might not be available yet during compilation
        val newCompileSymbol: Option[Symbol] =
            try Some(requiredModule(Plugin.newCompilerModuleName).requiredMethod("compile"))
            catch case _: Exception => None
        val newCompileWithOptionsSymbol: Option[Symbol] =
            try
                Some(
                  requiredModule(Plugin.newCompilerModuleName).requiredMethod("compileWithOptions")
                )
            catch case _: Exception => None
        val newCompileDebugSymbol: Option[Symbol] =
            try Some(requiredModule(Plugin.newCompilerModuleName).requiredMethod("compileDebug"))
            catch case _: Exception => None
        val newCompileDebugWithOptionsSymbol: Option[Symbol] =
            try
                Some(
                  requiredModule(Plugin.newCompilerModuleName).requiredMethod(
                    "compileDebugWithOptions"
                  )
                )
            catch case _: Exception => None

        val compileSymbol = oldCompilerModule.requiredMethod("compile")
        val compileWithOptionsSymbol = oldCompilerModule.requiredMethod("compileWithOptions")
        val compileDebugSymbol = oldCompilerModule.requiredMethod("compileDebug")
        val compileDebugWithOptionsSymbol =
            oldCompilerModule.requiredMethod("compileDebugWithOptions")

        def matchesSymbol(sym: Symbol, oldSym: Symbol, newSymOpt: Option[Symbol]): Boolean =
            sym == oldSym || newSymOpt.contains(sym)

        // Logging to diagnose transformation issues
        if debugLevel > 0 then
            println(
              s"ScalusPrepare.transformApply: checking tree at ${tree.srcPos.sourcePos.source}:${tree.srcPos.line}"
            )
            println(s"  tree.symbol = ${tree.symbol.showFullName}")
            println(s"  compileSymbol = ${compileSymbol.showFullName}")
            println(s"  newCompileSymbol = ${newCompileSymbol.map(_.showFullName)}")
            println(s"  tree.symbol == compileSymbol: ${tree.symbol == compileSymbol}")

        if matchesSymbol(tree.symbol, compileSymbol, newCompileSymbol) then
            if debugLevel > 0 then
                println(
                  s"  -> Transforming compile to compileWithOptions at ${tree.srcPos.sourcePos.source}:${tree.srcPos.line}"
                )
            val optionsTree = Plugin.retrieveCompilerOptions(tree, isCompilerDebug = false)
            val newArgs = optionsTree :: tree.args
            // Use new symbols for the transformed call if available, otherwise old
            val targetSymbol = newCompileWithOptionsSymbol.getOrElse(compileWithOptionsSymbol)
            val newFun = tpd.ref(targetSymbol).withSpan(tree.fun.span)
            cpy.Apply(tree)(fun = newFun, args = newArgs).withSpan(tree.span)
        else if matchesSymbol(tree.symbol, compileDebugSymbol, newCompileDebugSymbol) then
            if debugLevel > 0 then
                println(
                  s"  -> Transforming compileDebug to compileDebugWithOptions at ${tree.srcPos.sourcePos.source}:${tree.srcPos.line}"
                )
            val optionsTree = Plugin.retrieveCompilerOptions(tree, isCompilerDebug = true)
            val newArgs = optionsTree :: tree.args
            // Use new symbols for the transformed call if available, otherwise old
            val targetSymbol =
                newCompileDebugWithOptionsSymbol.getOrElse(compileDebugWithOptionsSymbol)
            val newFun = tpd.ref(targetSymbol).withSpan(tree.fun.span)
            cpy.Apply(tree)(fun = newFun, args = newArgs).withSpan(tree.span)
        else tree
    }

}

/** A plugin phase that compiles Scala code to Scalus Intermediate Representation (SIR).
  *
  * It's a two-phase process:
  *   1. Compile Scala code to [[scalus.compiler.sir.SIR]] and store it in JARs
  *   1. Link SIR to the final code by replacing calls to `compile` and `compileDebug` with a string
  *      literal that contains the encoded SIR and a call to `decodeStringLatin1` that decodes it
  *      back.
  */
class ScalusPhase(debugLevel: Int) extends PluginPhase {
    import tpd.*

    val phaseName = "Scalus"

    // We need to run after the "first transform" phase to have some optimizations applied,
    // like inlining, constant folding, etc.
    override val runsAfter: Set[String] = Set("firstTransform")
    // We need to run before the "patternMatcher" phase to have the SIR available for pattern matching
    override val runsBefore: Set[String] = Set("patternMatcher", ElimByName.name)

    override def allowsImplicitSearch: Boolean = true

    /** Compiles the current compilation unit to SIR and stores it in JARs as .sir file.
      */
    override def prepareForUnit(tree: Tree)(using Context): Context = {
        // bug in dotty: sometimes we called with the wrong phase in context
        try
            if summon[Context].phase != this then
                prepareForUnit(tree)(using summon[Context].withPhase(this))
            else
                if debugLevel > 0 then
                    report.echo(s"Scalus: ${ctx.compilationUnit.source.file.name}")
                val options = SIRCompilerOptions()
                // val sirLoader = createSirLoader
                val compiler = new SIRCompiler(options)
                compiler.compileModule(tree)
                ctx
        catch
            case scala.util.control.NonFatal(ex) =>
                ex.printStackTrace()
                throw ex
    }

    override def prepareForApply(tree: tpd.Apply)(using Context): Context = {
        // bug in dotty: sometimes we called with the wrong phase in context. set phase themself.
        if summon[Context].phase != this then ctx.withPhase(this)
        else ctx
    }

    /** Replaces calls to `compile`, `compileWithOptions`, `compileDebug`, and
      * `compileDebugWithOptions` with a fully linked Flat-encoded [[scalus.compiler.sir.SIR]]
      * representation. Supports both old (scalus.Compiler) and new (scalus.compiler.package)
      * locations.
      */
    override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
        try
            // Support both old (scalus.Compiler) and new (scalus.compiler.package$package) locations
            // The new module may not exist during compilation of scalus-core itself, so wrap in try-catch
            // TODO(0.15): Remove old scalus.Compiler symbol handling after removing deprecated API
            val oldCompilerModule = requiredModule("scalus.Compiler")

            // Try to get new module symbols - wrap entirely in try-catch since the module
            // might exist but methods might not be available yet during compilation
            val newCompileSymbol: Option[Symbol] =
                try Some(requiredModule(Plugin.newCompilerModuleName).requiredMethod("compile"))
                catch case _: Exception => None
            val newCompileWithOptionsSymbol: Option[Symbol] =
                try
                    Some(
                      requiredModule(Plugin.newCompilerModuleName).requiredMethod(
                        "compileWithOptions"
                      )
                    )
                catch case _: Exception => None
            val newCompileDebugSymbol: Option[Symbol] =
                try
                    Some(
                      requiredModule(Plugin.newCompilerModuleName).requiredMethod("compileDebug")
                    )
                catch case _: Exception => None
            val newCompileDebugWithOptionsSymbol: Option[Symbol] =
                try
                    Some(
                      requiredModule(Plugin.newCompilerModuleName).requiredMethod(
                        "compileDebugWithOptions"
                      )
                    )
                catch case _: Exception => None

            val compileSymbol = oldCompilerModule.requiredMethod("compile")
            val compileWithOptionsSymbol = oldCompilerModule.requiredMethod("compileWithOptions")
            val compileDebugSymbol = oldCompilerModule.requiredMethod("compileDebug")
            val compileDebugWithOptionsSymbol =
                oldCompilerModule.requiredMethod("compileDebugWithOptions")

            def matchesSymbol(sym: Symbol, oldSym: Symbol, newSymOpt: Option[Symbol]): Boolean =
                sym == oldSym || newSymOpt.contains(sym)

            if matchesSymbol(tree.fun.symbol, compileWithOptionsSymbol, newCompileWithOptionsSymbol)
                || matchesSymbol(
                  tree.fun.symbol,
                  compileDebugWithOptionsSymbol,
                  newCompileDebugWithOptionsSymbol
                )
                || matchesSymbol(tree.fun.symbol, compileSymbol, newCompileSymbol)
                || matchesSymbol(tree.fun.symbol, compileDebugSymbol, newCompileDebugSymbol)
            then
                // report.echo(s"transformApply: ${tree.showIndented(2)}")

                val isCompileDebug =
                    matchesSymbol(
                      tree.fun.symbol,
                      compileDebugWithOptionsSymbol,
                      newCompileDebugWithOptionsSymbol
                    )
                        || matchesSymbol(tree.fun.symbol, compileDebugSymbol, newCompileDebugSymbol)
                val isWithOptions =
                    matchesSymbol(
                      tree.fun.symbol,
                      compileWithOptionsSymbol,
                      newCompileWithOptionsSymbol
                    )
                        || matchesSymbol(
                          tree.fun.symbol,
                          compileDebugWithOptionsSymbol,
                          newCompileDebugWithOptionsSymbol
                        )

                val (optionsTree, code) = {
                    if isWithOptions then (tree.args(0), tree.args(1))
                    else (Plugin.retrieveCompilerOptions(tree, isCompileDebug), tree.args(0))
                }
                // val sirLoader = createSirLoader
                val compiler = new SIRCompiler()
                val start = System.currentTimeMillis()
                val sirResult = compiler.compileToSIR(code, isCompileDebug)

                if isCompileDebug then
                    val time = System.currentTimeMillis() - start
                    report.echo(
                      s"Scalus compileDebug at ${tree.srcPos.sourcePos.source}:${tree.srcPos.line} in $time ms"
                    )

                val flatTree = convertFlatToTree(
                  sirResult,
                  SIRHashConsedFlat,
                  requiredModule("scalus.compiler.sir.ToExprHSSIRFlat"),
                  tree.span,
                  isCompileDebug
                )

                val result =
                    if true then
                        val myModuleName = s"${tree.srcPos.sourcePos.source}:${tree.srcPos.line}"
                        val dependencyEntries = compiler.gatherExternalModulesFromSir(
                          myModuleName,
                          sirResult,
                          Map.empty
                        )
                        val depsTree =
                            try compiler.buildDepsTree(myModuleName, dependencyEntries, tree.srcPos)
                            catch
                                case scala.util.control.NonFatal(ex) =>
                                    println(
                                      "Error building deps tree,  myModuleName=" + myModuleName
                                    )
                                    throw ex;
                        val SIRLinkerModule = requiredModule(
                          "scalus.compiler.sir.linking.SIRLinker"
                        )
                        val SIRLinkerMethod = SIRLinkerModule.requiredMethod("link")
                        val sirPos =
                            createSIRPositionTree(SIRPosition.fromSrcPos(tree.srcPos), tree.span)
                        val linkerOptionsTree = createLinkerOptionsTree(optionsTree, tree.srcPos)
                        val sirLinkerCall = tpd
                            .ref(SIRLinkerMethod)
                            .appliedTo(
                              flatTree,
                              sirPos,
                              depsTree,
                              linkerOptionsTree
                            )
                            .withSpan(tree.span)
                        sirLinkerCall
                    else flatTree
                result
            /*
                case Apply(fun, args) if fun.symbol == compileSymbol || fun.symbol == compileDebugSymbol =>
                    report.error(
                      s"ScalusPhase: Expected two list of arguments for ${fun.symbol.fullName}, but found one",
                      tree.srcPos
                    )
                    println(s"tree: ${tree.show}")
                    tree
             */
            else tree
        catch
            case scala.util.control.NonFatal(ex) =>
                ex.printStackTrace()
                throw ex
    end transformApply

    override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree =
        // If the template has a compile annotation, we need to add a variable for SIR
        val compileAnnot = requiredClassRef("scalus.Compile").symbol.asClass
        if tree.symbol.hasAnnotation(compileAnnot) && tree.symbol.is(Flags.Module) then {
            val sirBodyAnnotation =
                requiredClass("scalus.compiler.sir.SIRBodyAnnotation")
            tree.symbol.getAnnotation(sirBodyAnnotation) match
                case Some(annotation) =>
                    val moduleSIR = annotation.arguments.head
                    val depsMap = annotation.arguments.tail.head
                    tree.rhs match
                        case template: tpd.Template =>
                            // Add a variable for SIR in the template
                            val newBody = template.body.map {
                                case vd: ValDef
                                    if vd.symbol.name.toString == Plugin.SIR_MODULE_VAL_NAME =>
                                    cpy.ValDef(vd)(rhs = moduleSIR)
                                case vd: ValDef
                                    if vd.symbol.name.toString == Plugin.SIR_DEPS_VAL_NAME =>
                                    // If the variable already exists, we can skip it
                                    cpy.ValDef(vd)(rhs = depsMap)
                                case e => e
                            }
                            val newTemplate = cpy.Template(template)(body = newBody)
                            cpy.TypeDef(tree)(rhs = newTemplate)
                        case _ =>
                            report.warning(
                              s"ScalusPhase: Expected a template for ${tree.symbol.fullName}, but found: ${tree.rhs.show}",
                              tree.srcPos
                            )
                            tree
                case None =>
                    println(
                      s"ScalusPhase: SIRBodyAnnotation not found for ${tree.symbol.fullName}"
                    )
                    tree
        } else tree

    /*
    private def createSirLoader(using Context): SIRLoader = {
        new SIRLoader(
          SIRLoaderOptions(
            ClassPath.expandPath(ctx.settings.classpath.value, expandStar = true),
            Option(ctx.settings.outputDir.value.toURL),
            Some("./shared/src/main/resources/predefined-sirs/")
          )
        )
    }
     */

    /*
    private def retrieveCompilerOptions(
        posTree: Tree,
        isCompilerDebug: Boolean
    )(using Context): SIRCompilerOptions = {

        // Default options
        var backend: String = SIRDefaultOptions.targetLoweringBackend.toString
        var generateErrorTraces: Boolean = SIRDefaultOptions.generateErrorTraces
        var optimizeUplc: Boolean = SIRDefaultOptions.optimizeUplc
        var debug: Boolean = false
        var codeDebugLevel: Int = 0
        var useUniversalDataConversion: Boolean = false
        var runtimeLinker: Boolean = SIRDefaultOptions.runtimeLinker
        var writeSIRToFile: Boolean = SIRDefaultOptions.writeSIRToFile

        def parseTargetLoweringBackend(value: Tree, vals: List[Tree]): Unit = {
            var parsed = true
            value match {
                case Ident(name) =>
                    if name.startsWith("$lessinit$greater$default")
                    then
                        // This is a default value for a parameter,
                        backend = SIRDefaultOptions.targetLoweringBackend.toString
                    else if name.toString.startsWith("targetLoweringBackend$")
                    then
                        // This is a default value for a parameter,
                        findValdefWithName(name, vals) match
                            case Some(vd) =>
                                parseTargetLoweringBackend(vd.rhs, Nil)
                            case None =>
                                report.warning(
                                  s"ScalusPhase: Expected a value definition for ${name}, but not found in the template",
                                  value.srcPos
                                )
                    else backend = name.toString
                case Select(_, name) =>
                    backend = name.toString
                case _ =>
                    parsed = false
                    report.warning(
                      s"ScalusPhase: Expected an identifier or select expression for targetLoweringBackend, but found: ${value.show}",
                      value.srcPos
                    )
            }
            if backend.equals("SirToUplcV3Lowering") then useUniversalDataConversion = true
            if parsed then {
                // check that the backend is valid
                if !(backend == "SimpleSirToUplcLowering" || backend == "SirToUplc110Lowering" || backend == "SirToUplcV3Lowering")
                then
                    report.warning(
                      s"ScalusPhase: Unknown targetLoweringBackend: ${backend}, using default: ${SIRDefaultOptions.targetLoweringBackend}",
                      value.srcPos
                    )
                    backend = SIRDefaultOptions.targetLoweringBackend.toString
                if debugLevel > 0 || isCompilerDebug then
                    println(
                      s"parseTargetLoweringBackend: ${backend}, useUniversalDataConversion=${useUniversalDataConversion}"
                    )
            } else {
                report.warning(
                  s"ScalusPhase: Failed to parse targetLoweringBackend, using default: ${backend}",
                  value.srcPos
                )
            }
        }

        def parseBooleanValue(
            paramName: String,
            value: Tree,
            vals: List[Tree]
        ): Option[Boolean] = {
            value match {
                case Literal(Constant(flag: Boolean)) => Some(flag)
                //  tree:Select(Select(Select(Ident(scalus),Compiler),Options),$lessinit$greater$default$2)
                case Select(_, name) if name.toString.startsWith("$lessinit$greater$default") =>
                    // This is a default value for a parameter,
                    None
                case Ident(name) =>
                    if name.startsWith("$lessinit$greater$default")
                    then None
                    else if name.toString.startsWith(paramName + "$") then {
                        findValdefWithName(name, vals) match
                            case Some(vd) =>
                                parseBooleanValue(paramName, vd.rhs, Nil)
                            case None =>
                                report.warning(
                                  s"ScalusPhase: Expected a value definition for ${name}, but not found in the template\n"
                                      + (if vals.isEmpty then "vals is empty"
                                         else "vals: " + vals.map(_.show).mkString(", ")),
                                  value.srcPos
                                )
                                None
                    } else {
                        val expectedParam = paramName + "$1"
                        report.warning(
                          s"ScalusPhase: Expected a boolean literal, but found identifier: ${value.show}, name=${name}, paramName=$paramName, exp=${expectedParam}",
                          value.srcPos
                        )
                        throw new RuntimeException("QQQ")
                        None
                    }
                case _ =>
                    report.warning(
                      s"ScalusPhase: Expected a boolean literal, but found: ${value.show}\ntree:${value}",
                      value.srcPos
                    )
                    None
            }
        }

        def parseIntValue(paramName: String, value: Tree, vals: List[Tree]): Option[Int] = {
            value match {
                case Literal(Constant(num: Int)) => Some(num)
                case Select(_, name)
                    if name.toString.startsWith("$lessinit$greater$default") ||
                        name.toString.startsWith(paramName + "$") =>
                    None
                case Ident(name) =>
                    if name.startsWith("$lessinit$greater$default")
                    then None
                    else if name.toString.startsWith(paramName + "$") then
                        findValdefWithName(name, vals) match
                            case Some(vd) =>
                                parseIntValue(paramName, vd.rhs, Nil)
                            case None =>
                                report.warning(
                                  s"ScalusPhase: Expected a value definition for ${name}, but not found in the template\n"
                                      + (if vals.isEmpty then "(empty vals)"
                                         else
                                             "vals: " + vals
                                                 .map(_.show)
                                                 .mkString(", ")),
                                  value.srcPos
                                )
                                None
                    else {
                        report.warning(
                          s"ScalusPhase: Expected an integer literal, but found identifier: ${value.show}",
                          value.srcPos
                        )
                        None
                    }
                case _ =>
                    report.warning(
                      s"ScalusPhase: Expected an integer literal, but found: ${value.show}",
                      posTree.srcPos.startPos
                    )
                    None // default value if parsing fails
            }
        }

        // Parse the arguments of the compiler options
        //  Note, that default values should be synchronized with the default value in scalus.compiler.Options class
        //  (which is unaccessibke from here, so we should keep them in sync manually)
        def parseArg(arg: Tree, idx: Int, vals: List[Tree]): Unit = {
            arg match
                case tpd.NamedArg(name, value) =>
                    if name.toString == "targetLoweringBackend" then
                        parseTargetLoweringBackend(value, vals)
                    else if name.toString == "generateErrorTraces" then
                        generateErrorTraces = parseBooleanValue("generateErrorTraces", value, vals)
                            .getOrElse(SIRDefaultOptions.generateErrorTraces)
                    else if name.toString == "optimizeUplc" then {
                        optimizeUplc = parseBooleanValue("optimizeUplc", value, vals).getOrElse(
                          SIRDefaultOptions.optimizeUplc
                        )
                    } else if name.toString == "debug" then
                        debug = parseBooleanValue("debug", value, vals).getOrElse(false)
                    else if name.toString == "debugLevel" then {
                        codeDebugLevel = parseIntValue("debugLevel", value, vals).getOrElse(0)
                        if debug && codeDebugLevel == 0 then codeDebugLevel = 10
                    } else if name.toString == "runtimeLinker" then
                        runtimeLinker = parseBooleanValue("runtimeLinker", value, vals).getOrElse(
                          SIRDefaultOptions.runtimeLinker
                        )
                    else if name.toString == "writeSIRToFile" then
                        writeSIRToFile = parseBooleanValue("writeSIRToFile", value, vals).getOrElse(
                          SIRDefaultOptions.writeSIRToFile
                        )
                    else {
                        report.warning(
                          s"ScalusPhase: Unknown compiler option: $name",
                          posTree.srcPos.startPos
                        )
                    }
                case value =>
                    idx match
                        case 0 => // targetLoweringBackend
                            parseTargetLoweringBackend(value, vals)
                        case 1 => // generateErrorTraces
                            generateErrorTraces =
                                parseBooleanValue("generateErrorTraces", value, vals).getOrElse(
                                  SIRDefaultOptions.generateErrorTraces
                                )
                        case 2 => // optimizeUplc
                            optimizeUplc = parseBooleanValue("optimizeUplc", value, vals).getOrElse(
                              SIRDefaultOptions.optimizeUplc
                            )
                        case 3 => // runtimeLinker
                            runtimeLinker =
                                parseBooleanValue("runtimeLinker", value, vals).getOrElse(
                                  SIRDefaultOptions.runtimeLinker
                                )
                        case 4 => // writeSIRToFile
                            writeSIRToFile =
                                parseBooleanValue("writeSIRToFile", value, vals).getOrElse(
                                  SIRDefaultOptions.writeSIRToFile
                                )
                        case 5 => // debugLevel
                            codeDebugLevel = parseIntValue("debugLevel", value, vals).getOrElse(0)
                        case 6 => // debug
                            debug =
                                parseBooleanValue("debug", value, vals).getOrElse(isCompilerDebug)
                            if debug && codeDebugLevel == 0 then codeDebugLevel = 10
                        case _ =>
                            report.warning(
                              s"ScalusPhase: too many position argiments for scalus.compiler.Options, expected max 4, but found ${idx + 1}",
                              posTree.srcPos.startPos
                            )
        }

        def parseCompilerOptionsApply(app: tpd.Apply, stats: List[Tree]): Unit = {
            // println("compiler options apply: " + app.show)
            if app.fun.symbol != Symbols.requiredMethod(
                  "scalus.compiler.Options.apply"
                )
            then
                report.warning(
                  "expected scalus.compiler.Options.apply",
                  posTree.srcPos
                )
            app.args.zipWithIndex.foreach { case (arg, idx) =>
                parseArg(arg, idx, stats)
            }
        }

        def findValdefWithName(name: Name, vals: List[Tree]): Option[ValDef] = {
            vals.collectFirst {
                case vd: ValDef if vd.symbol.name == name => vd
            }
        }

        val compilerOptionType = requiredClassRef("scalus.compiler.Options")
        if !ctx.phase.allowsImplicitSearch then
            println(
              s"ScalusPhase: Implicit search is not allowed in phase ${ctx.phase.phaseName}. "
            )
        summon[Context].typer.inferImplicit(
          compilerOptionType,
          EmptyTree,
          posTree.span
        ) match {
            case Implicits.SearchSuccess(tree, ref, level, isExtension) =>
                // report.echo(s"Found compiler options: ${tree.show}")
                // report.echo(s"deftree=${tree.symbol.defTree.show}")
                val deftree = tree.symbol.defTree
                if deftree.isEmpty then
                    report.warning(
                      s"CompilerOptions found but deftree is empty, compiler options may be incomplete",
                      posTree.srcPos
                    )
                else if deftree.isInstanceOf[tpd.DefDef] then
                    val defDefTree = deftree.asInstanceOf[tpd.DefDef]
                    val underTyped = defDefTree.rhs match
                        case tpd.Typed(obj, tp) =>
                            obj
                        case _ => defDefTree
                    underTyped match
                        case app @ tpd.Apply(obj, args) =>
                            // report.echo(s"defDefTree.rhs.apply, args=${args}")
                            parseCompilerOptionsApply(app, Nil)
                        case Block(stats, app @ tpd.Apply(obj, args)) =>
                            // report.echo(s"defDefTree.rhs.block.apply, args=${args}")
                            parseCompilerOptionsApply(app, stats)
                        case _ =>
                            report.warning(
                              s"ScalusPhase: Expected a call to scalus.compiler.Options.apply, but found: ${underTyped.show}" +
                                  s"ntree:${underTyped}",
                              posTree.srcPos
                            )
                else report.warning("defdef expected as compiler options", deftree.srcPos)
            case failure @ Implicits.SearchFailure(_) =>
                if isCompilerDebug && debugLevel > 20 then {
                    report.warning(
                      s"ScalusPhase: No compiler options found, using default options",
                      posTree.srcPos.startPos
                    )
                    report.warning(s"search result: ${failure.show}")
                } else {
                    // report.echo("No compiler options found, using default options")
                }
        }
        val retval = SIRCompilerOptions(
          backend = backend,
          debugLevel = if debugLevel == 0 then codeDebugLevel else debugLevel
        )
        if isCompilerDebug then
            println(
              s"retrieveCompilerOptions, retval=${retval}, useUniversalDataConversion=${useUniversalDataConversion}"
            )
        retval
    }
     */

    def createLinkerOptionsTree(
        optionsTree: tpd.Tree,
        pos: SrcPos
    )(using Context): tpd.Tree = {
        import tpd.*
        val linkerOptionsModule = requiredModule("scalus.compiler.sir.linking.SIRLinkerOptions")
        val linkerCreateTree = ref(linkerOptionsModule).select(
          linkerOptionsModule.requiredMethod("fromCompilerOptions")
        )
        val args = List(optionsTree)
        Apply(linkerCreateTree, args).withSpan(pos.span)
    }

}
