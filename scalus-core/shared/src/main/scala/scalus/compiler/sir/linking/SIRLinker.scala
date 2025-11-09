package scalus.compiler.sir.linking

import scalus.compiler.sir.*

import scala.annotation.unused
import scala.collection.mutable

case class SIRLinkerOptions(
    useUniversalDataConversion: Boolean,
    /** If true, the linker will print errors to the console, otherwise they are accessible over
      * errorlog
      */
    printErrors: Boolean,
    debugLevel: Int,
)

object SIRLinkerOptions {
    def fromCompilerOptions(compilerOptions: scalus.Compiler.Options): SIRLinkerOptions = {
        SIRLinkerOptions(
          useUniversalDataConversion =
              compilerOptions.targetLoweringBackend == scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
          printErrors = true,
          debugLevel = compilerOptions.debugLevel
        )
    }
}

/** Links SIR definitions and data declarations into a single SIR module.
  *
  * This class is responsible for linking SIR definitions and data declarations to create a single
  * SIR module.
  *
  * It traverses the SIR tree and links external definitions and data declarations to the global
  * definitions and data declarations.
  */
class SIRLinker(options: SIRLinkerOptions, moduleDefs: Map[String, Module]) {

    import SIRLinker.LinkingDefState
    import SIRLinker.SIRLinkedBinding

    private val globalDefs: mutable.LinkedHashMap[String, LinkingDefState] =
        mutable.LinkedHashMap.empty
    private val globalDataDecls: mutable.LinkedHashMap[String, DataDecl] =
        mutable.LinkedHashMap.empty
    private val moduleDefsCache: mutable.Map[String, mutable.LinkedHashMap[String, SIR]] =
        mutable.LinkedHashMap.empty.withDefaultValue(mutable.LinkedHashMap.empty)

    private val debugLevel: Int = if options.debugLevel != 0 then options.debugLevel else 0

    private var errorLog: List[(String, SIRPosition)] = List.empty

    private var requireV3Backend: Boolean = false

    def retrieveErrors: List[(String, SIRPosition)] = {
        errorLog
    }

    // private val sirLoader = new SIRLoader(options.loaderOptions)

    private def error[A](message: String, pos: SIRPosition, defaultValue: A): A = {
        if options.printErrors then println(s"Error: $message at ${pos.show}")
        else errorLog :+= (message, pos)
        defaultValue
    }

    def link(sir: SIR, pos: SIRPosition): SIR = {
        if debugLevel > 1 then
            println(
              s"Linking SIR at ${pos.show}, options=$options, modules: ${moduleDefs.keys.mkString(", ")}"
            )
            moduleDefs.get("scalus.prelude.List$") match
                case Some(m) =>
                    println(s"Prelude module found with defs: ${m.defs.map(_.name).mkString(", ")}")
                case None => println("Prelude module not found")
        val processed = traverseAndLink(sir, pos)
        val full: SIR = globalDefs.values.foldRight(processed) { case (state, acc) =>
            state match
                case LinkingDefState.Linked(b) =>
                    SIR.Let(
                      List(Binding(b.name, b.body.tp, b.body)),
                      acc match {
                          case annssir: AnnotatedSIR => annssir
                          case _ =>
                              val msg =
                                  s"Unexpected Decl. In binding ${b.name} in SIRLinker.link"
                              error(
                                msg,
                                pos,
                                SIR.Error(msg, AnnotationsDecl.empty.copy(pos = pos))
                              )
                      },
                      b.flags,
                      AnnotationsDecl.empty.copy(pos = pos)
                    )
                case LinkingDefState.Linking =>
                    val message = s"Linking in progress for ${state}"
                    error(
                      message,
                      pos,
                      SIR.Error(
                        message,
                        AnnotationsDecl.empty.copy(pos = pos)
                      )
                    )

        }
        val dataDecls = globalDataDecls.foldRight(full: SIR) { case ((_, decl), acc) =>
            SIR.Decl(decl, acc)
        }
        dataDecls
    }

    private def traverseAndLink(sir: SIR, pos: SIRPosition): SIR = sir match
        case SIR.Decl(data, term) =>
            SIR.Decl(data, traverseAndLink(term, pos))
        case ans: AnnotatedSIR =>
            traverseAndLinkExpr(ans, pos)

    private def traverseAndLinkExpr(sir: AnnotatedSIR, pos: SIRPosition): AnnotatedSIR = sir match
        case v @ SIR.ExternalVar(moduleName, name, tp, ann) if !globalDefs.contains(name) =>
            if moduleName == "scalus.builtin.internal.UniversalDataConversion$" then
                if name != "scalus.builtin.internal.UniversalDataConversion$.fromData" &&
                    name != "scalus.builtin.internal.UniversalDataConversion$.toData"
                then
                    val msg =
                        s"Unknown external variable in universal data conversion module: ${name}"
                    error(msg, ann.pos, v)
                // For fromData/toData, we allow them as ExternalVar here
                // They will be handled during UPLC lowering in Apply position
            else linkDefinition(moduleName, name, pos, tp, ann)
            v
        case v @ SIR.Let(bindings, body, flags, anns) =>
            val nBingings =
                bindings.map(b => Binding(b.name, b.tp, traverseAndLink(b.value, pos)))
            val nBody = traverseAndLink(body, pos)
            SIR.Let(nBingings, nBody, flags, anns)
        case SIR.LamAbs(param, term, typeParams, anns) =>
            SIR.LamAbs(param, traverseAndLink(term, pos), typeParams, anns)
        case SIR.Apply(f, arg, tp, anns) =>
            val fReplaced =
                if options.useUniversalDataConversion then
                    anns.data.get("fromData") match
                        case Some(v) =>
                            SIR.ExternalVar(
                              "scalus.builtin.internal.UniversalDataConversion$",
                              "scalus.builtin.internal.UniversalDataConversion$.fromData",
                              SIRType.Fun(SIRType.Data, tp),
                              AnnotationsDecl.empty.copy(pos = f.anns.pos)
                            )
                        case None =>
                            anns.data.get("toData") match
                                case Some(v) =>
                                    SIR.ExternalVar(
                                      "scalus.builtin.internal.UniversalDataConversion$",
                                      "scalus.builtin.internal.UniversalDataConversion$.toData",
                                      SIRType.Fun(arg.tp, SIRType.Data),
                                      AnnotationsDecl.empty.copy(pos = f.anns.pos)
                                    )
                                case None => f
                else f
            val nF = traverseAndLinkExpr(fReplaced, pos)
            val nArg = traverseAndLinkExpr(arg, pos)
            SIR.Apply(nF, nArg, tp, anns)
        case SIR.And(lhs, rhs, anns) =>
            val nLhs = traverseAndLinkExpr(lhs, pos)
            val nRhs = traverseAndLinkExpr(rhs, pos)
            SIR.And(nLhs, nRhs, anns)
        case SIR.Or(lhs, rhs, anns) =>
            val nLhs = traverseAndLinkExpr(lhs, pos)
            val nRhs = traverseAndLinkExpr(rhs, pos)
            SIR.Or(nLhs, nRhs, anns)
        case SIR.Not(term, anns) => SIR.Not(traverseAndLinkExpr(term, pos), anns)
        case SIR.IfThenElse(cond, t, f, tp, anns) =>
            val nCond = traverseAndLinkExpr(cond, pos)
            val nT = traverseAndLinkExpr(t, pos)
            val nR = traverseAndLinkExpr(f, pos)
            SIR.IfThenElse(nCond, nT, nR, tp, anns)
        case SIR.Constr(name, data, args, tp, anns) =>
            globalDataDecls.put(data.name, data)
            val nArgs = args.map(a => traverseAndLink(a, pos))
            SIR.Constr(name, data, nArgs, tp, anns)
        case SIR.Match(scrutinee, cases, rhsType, anns) =>
            val nScrutinee = traverseAndLinkExpr(scrutinee, pos)
            val nCases =
                cases.map(c => SIR.Case(c.pattern, traverseAndLink(c.body, pos), c.anns))
            SIR.Match(nScrutinee, nCases, rhsType, anns)
        case SIR.Select(scrutinee, field, tp, anns) =>
            val nScrutinee = traverseAndLink(scrutinee, pos)
            SIR.Select(nScrutinee, field, tp, anns)
        case SIR.Cast(term, tp, anns) =>
            SIR.Cast(traverseAndLinkExpr(term, pos), tp, anns)
        case other => other

    private def findAndLinkDefinition(
        defs: collection.Map[String, SIR],
        fullName: String,
        @unused tp: SIRType,
        srcPos: SIRPosition
    ): Boolean = {
        val found = defs.get(fullName)
        for sir <- found do
            globalDefs.update(fullName, LinkingDefState.Linking)
            val nSir = traverseAndLink(sir, srcPos)
            // TODO: research.  removing 'remove' triggers fail of  scalus.CompilerPluginTest. 'compile fieldAsData macro'
            globalDefs.remove(fullName)
            globalDefs.update(
              fullName,
              LinkingDefState.Linked(SIRLinkedBinding(fullName, SIR.LetFlags.Recursivity, nSir))
            )
        found.isDefined
    }

    private def linkDefinition(
        moduleName: String,
        fullName: String,
        pos: SIRPosition,
        tp: SIRType,
        anns: AnnotationsDecl
    ): Unit = {
        // println(s"linkDefinition: ${fullName}")
        retrieveModule(moduleName, pos) match
            case Left(filename) =>
                error(
                  s"Module not found during linking: ${moduleName} , missing filename: ${filename} referenced for name ${fullName} from ${anns.pos.file}: ${anns.pos.startLine + 1}",
                  pos,
                  ()
                )
            case Right(defs) =>
                if !findAndLinkDefinition(defs, fullName, tp, pos) then
                    error(
                      s"Symbol not found during linking: ${fullName} in module ${moduleName} at ${pos.show}",
                      anns.pos,
                      ()
                    )
    }

    private def retrieveModule(
        moduleName: String,
        srcPos: SIRPosition
    ): Either[String, mutable.LinkedHashMap[String, SIR]] = {
        moduleDefsCache.get(moduleName) match
            case Some(defs) => Right(defs)
            case None =>
                moduleDefs.get(moduleName) match
                    case Some(module) =>
                        validateSIRVersion(module, moduleName, srcPos)
                        val defsMap = mutable.LinkedHashMap.from(
                          module.defs.map(d => d.name -> d.value)
                        )
                        moduleDefsCache.put(moduleName, defsMap)
                        Right(defsMap)
                    case None =>
                        Left(s"Can't find module ${moduleName} in dependenies")
    }

    private def validateSIRVersion(
        module: Module,
        moduleName: String,
        srcPos: SIRPosition
    ): Unit = {
        if (module.version._1 != SIRVersion._1)
            || (module.version._1 == SIRVersion._1
                && SIRVersion._2 < module.version._2)
        then
            error(
              s"""During linking I've found that a module '$moduleName' has an incompatible SIR version: ${module.version} (expected: ${SIRVersion}).
                   |This can happen if you try to link a module compiled with a different version of Scalus.
                   |Please, recompile the module with the version of Scalus that has the SIR version ${SIRVersion}
                   |""".stripMargin,
              srcPos,
              ()
            )
    }

}

object SIRLinker {

    class SIRLinkedBinding(
        val name: String,
        val flags: SIR.LetFlags,
        val body: SIR
    )

    enum LinkingDefState {
        case Linking extends LinkingDefState
        case Linked(binding: SIRLinkedBinding) extends LinkingDefState
    }

    def link(
        sir: SIR,
        pos: SIRPosition,
        deps: List[SIRCompiled],
        options: SIRLinkerOptions
    ): SIR = {
        if options.debugLevel > 0 then
            println(s"Linking SIR with deps: ${deps.map(_.sirModule.name).mkString(", ")}")
        val modules = readModules(deps)
        val linker = new SIRLinker(options, modules)
        val linked = linker.link(sir, pos)
        RemoveRecursivity(linked)
    }

    def readModules(deps: List[SIRCompiled]): Map[String, Module] = {
        var retval: Map[String, Module] = Map.empty
        val queue: mutable.Queue[SIRCompiled] = scala.collection.mutable.Queue.empty
        queue.enqueueAll(deps)
        while queue.nonEmpty do
            val dep = queue.dequeue()
            retval.get(dep.sirModule.name) match
                case Some(_) => // already added
                case None =>
                    retval += (dep.sirModule.name -> dep.sirModule)
                    queue.enqueueAll(dep.sirDeps)
        retval
    }

}
