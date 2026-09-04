package scalus.tsexport

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.Using
import scala.quoted.*
import scala.tasty.inspector.*

/** Walks TASTy trees and builds a [[TsModule]] from `@JSExport*`-annotated symbols. */
object ExportCollector {

    case class Result(module: TsModule, errors: List[ExportError])

    private val JSExportTopLevelAnnot = "scala.scalajs.js.annotation.JSExportTopLevel"
    private val JSExportAnnot = "scala.scalajs.js.annotation.JSExport"
    private val JSExportStaticAnnot = "scala.scalajs.js.annotation.JSExportStatic"
    private val JSExportAllAnnot = "scala.scalajs.js.annotation.JSExportAll"
    private val TsTypeAnnot = "scalus.interop.TsType"
    private val TsNameAnnot = "scalus.interop.TsName"
    private val TsIgnoreAnnot = "scalus.interop.TsIgnore"
    private val DeprecatedAnnot = "scala.deprecated"

    /** Scala.js facades for JavaScript classes TypeScript already declares globally, mapped to the
      * TypeScript name. A class extending one of these gets an `extends` clause in the .d.ts so
      * that the members the platform gives it - `message`, `name` and `stack` on an `Error` - are
      * visible to a consumer. Every entry is a global in TypeScript's `lib.es5.d.ts`.
      */
    private val nativeJsBases: Map[String, String] = List(
      "Error",
      "EvalError",
      "RangeError",
      "ReferenceError",
      "SyntaxError",
      "TypeError",
      "URIError"
    ).map(name => s"scala.scalajs.js.$name" -> name).toMap

    /** @param tastyRoots
      *   directories containing .tasty files of the modules to export
      * @param classpath
      *   full dependency classpath of those modules
      * @param sourceRoot
      *   base directory for resolving relative TASTy source paths (docs fallback)
      * @param excludes
      *   Scala FQN prefixes to skip (declarations and errors)
      */
    def collect(
        tastyRoots: List[String],
        classpath: List[String],
        sourceRoot: String,
        excludes: List[String] = Nil
    ): Result = {
        val decls = mutable.ListBuffer.empty[TsDecl]
        val errors = mutable.ListBuffer.empty[ExportError]

        val inspector = new Inspector {
            def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
                import quotes.reflect.*

                def excluded(sym: Symbol): Boolean =
                    excludes.exists(p => sym.fullName.startsWith(p))

                // ---- annotation helpers -------------------------------------------------
                // the compiler stores annotations in reverse source order; undo that
                def annots(sym: Symbol, fqn: String): List[Term] =
                    sym.annotations.reverse.filter(_.tpe.typeSymbol.fullName == fqn)

                def annotStringArg(a: Term): Option[String] = a match
                    case Apply(_, Literal(StringConstant(s)) :: _) => Some(s)
                    case _                                         => None

                /** Annotations of a module: the source `object X` attaches them to the module val;
                  * from the module class we reach it via companionModule.
                  */
                def symAndModuleAnnots(sym: Symbol, fqn: String): List[Term] = {
                    val own = annots(sym, fqn)
                    if own.nonEmpty then own
                    else if sym.flags.is(Flags.Module) then annots(sym.companionModule, fqn)
                    else Nil
                }

                def topLevelExportNames(sym: Symbol): List[String] =
                    symAndModuleAnnots(sym, JSExportTopLevelAnnot).flatMap(annotStringArg) match
                        case Nil if symAndModuleAnnots(sym, JSExportTopLevelAnnot).nonEmpty =>
                            List(sym.name.stripSuffix("$"))
                        case names => names

                def tsNameOf(sym: Symbol): Option[String] =
                    symAndModuleAnnots(sym, TsNameAnnot).flatMap(annotStringArg).headOption

                def tsTypeOf(sym: Symbol): Option[TsType] =
                    annots(sym, TsTypeAnnot)
                        .flatMap(annotStringArg)
                        .headOption
                        .map(TsType.Verbatim(_))

                /** A constructor `val` is a single declaration that surfaces twice, as a
                  * constructor parameter and as a property. Scala puts a plain annotation on the
                  * parameter symbol and an `@(Ann @field)` one on the field symbol, so an override
                  * written once would otherwise narrow only one of the two faces and leave the
                  * .d.ts contradicting itself - `new RedeemerBudget("garbage", …)` type-checking
                  * against a `readonly tag: "Spend" | …`. Look at both symbols for either face.
                  */
                def tsTypeOfParamOrField(owner: Symbol, sym: Symbol): Option[TsType] =
                    tsTypeOf(sym).orElse {
                        val name = sym.name
                        (owner.declaredFields ++ owner.primaryConstructor.paramSymss.flatten)
                            .find(other => other != sym && other.name == name)
                            .flatMap(tsTypeOf)
                    }

                // ---- discover exported ClassDefs (pre-pass for the name registry) -------
                val exportedClasses = mutable.ListBuffer.empty[(Symbol, List[String])]
                for tasty <- tastys do {
                    object finder extends TreeAccumulator[Unit] {
                        def foldTree(u: Unit, tree: Tree)(owner: Symbol): Unit = tree match
                            case c: ClassDef =>
                                val names = topLevelExportNames(c.symbol)
                                if names.nonEmpty && !excluded(c.symbol) then
                                    exportedClasses += ((c.symbol, names))
                                foldOverTree(u, tree)(owner)
                            case _ => foldOverTree(u, tree)(owner)
                    }
                    finder.foldTree((), tasty.ast)(tasty.ast.symbol)
                }

                val knownNames: Map[String, String] =
                    exportedClasses.map { case (sym, names) => sym.fullName -> names.head }.toMap

                // ---- chase queue for referenced non-exported js.Object types ------------
                val chased = mutable.LinkedHashMap.empty[Symbol, String]

                /** Whether we are mapping a type the CALLER supplies. Parameters are inputs; a
                  * return type or a class field is an output. A chased interface inherits the
                  * position it was reached from, so the config traits nested inside an input stay
                  * inputs. Only input-only interfaces get `readonly` arrays: tightening an output
                  * would break `const logs: string[] = result.logs`.
                  */
                var inInputPosition = false

                /** Off while we walk the chase graph purely to settle array directions, so the same
                  * unexportable type is not reported once per traversal.
                  */
                var recordErrors = true
                def atPosition[A](input: Boolean)(body: => A): A = {
                    val saved = inInputPosition
                    inInputPosition = input
                    try body
                    finally inInputPosition = saved
                }

                /** symbol -> reached ONLY from input positions so far */
                val chasedInputOnly = mutable.Map.empty[Symbol, Boolean]

                def chase(sym: Symbol): Option[String] =
                    // an excluded type must not be queued for an `export interface`; the member
                    // that names it is reported instead of emitting a dangling reference
                    if excluded(sym) then None
                    else {
                        chasedInputOnly(sym) =
                            chasedInputOnly.getOrElse(sym, true) && inInputPosition
                        Some(chased.getOrElseUpdate(sym, tsNameOf(sym).getOrElse(sym.name)))
                    }

                val mapper = new TypeMapper(knownNames, chase)

                // ---- docs ---------------------------------------------------------------
                /** Scala's own `@deprecated` is the idiomatic way to retire an API and the form
                  * MiMa and the compiler key off, so it has to reach the .d.ts as the TSDoc tag -
                  * otherwise a member marked deprecated in Scala ships looking supported. A
                  * `@deprecated` written by hand in the Scaladoc wins, so an author can always
                  * phrase the TypeScript message differently.
                  */
                def deprecationOf(sym: Symbol): Option[String] =
                    annots(sym, DeprecatedAnnot).headOption.map { a =>
                        val args = a match
                            case Apply(_, as) => as
                            case _            => Nil
                        def argAt(index: Int, name: String): Option[String] = {
                            val named = args.collectFirst {
                                case NamedArg(`name`, Literal(StringConstant(v))) => v
                            }
                            val positional = args
                                .filter {
                                    case NamedArg(_, _) => false
                                    case _              => true
                                }
                                .lift(index)
                                .collect { case Literal(StringConstant(v)) => v }
                            named.orElse(positional).filter(_.nonEmpty)
                        }
                        val message = argAt(0, "message").fold("")(m => s" $m")
                        val since = argAt(1, "since").fold("")(v => s" (since $v)")
                        s"@deprecated$message$since"
                    }

                def docOf(sym: Symbol): Option[TsDoc] = {
                    val written = sym.docstring
                        .flatMap(DocConverter.convert)
                        .orElse(docFromSource(sym))
                    val fromAnnotation = deprecationOf(sym)
                    written match
                        case Some(doc)
                            if fromAnnotation.isDefined &&
                                !doc.lines.exists(_.startsWith("@deprecated")) =>
                            Some(TsDoc(doc.lines :+ fromAnnotation.get))
                        case Some(doc) => Some(doc)
                        case None      => fromAnnotation.map(line => TsDoc(List(line)))
                }

                def docFromSource(sym: Symbol): Option[TsDoc] =
                    for
                        pos <- sym.pos
                        // an inspected source file usually reports empty content, so fall back
                        // to reading it off disk; Some("") must not short-circuit that
                        content <- pos.sourceFile.content.filter(_.nonEmpty).orElse {
                            val p = Paths.get(pos.sourceFile.path)
                            val resolved =
                                if p.isAbsolute then p else Paths.get(sourceRoot).resolve(p)
                            if Files.exists(resolved) then Some(Files.readString(resolved))
                            else None
                        }
                        doc <- extractPrecedingDoc(content, pos.start)
                    yield doc

                def extractPrecedingDoc(content: String, defStart: Int): Option[TsDoc] = {
                    val before = content.substring(0, math.min(defStart, content.length))
                    val end = before.lastIndexOf("*/")
                    if end < 0 || !ownsPrecedingDoc(before.substring(end + 2)) then None
                    else
                        val start = before.lastIndexOf("/**")
                        if start < 0 || start > end then None
                        else DocConverter.convert(before.substring(start, end + 2))
                }

                // ---- member building ----------------------------------------------------
                def context(owner: Symbol, m: Symbol): String =
                    s"${owner.fullName.stripSuffix("$")}.${m.name}"

                def mapType(
                    tpe: TypeRepr,
                    ctx: String,
                    overrideTs: Option[TsType]
                ): Option[TsType] =
                    overrideTs match
                        case Some(t) => Some(t)
                        case None =>
                            mapper.map(tpe, ctx) match
                                case Right(t) => Some(t)
                                case Left(e) =>
                                    if recordErrors && !excludes.exists(p => e.member.startsWith(p))
                                    then errors += e
                                    None

                def containsUndefined(t: TsType): Boolean = t match
                    case TsType.Union(ms) => ms.contains(TsType.Named("undefined"))
                    case _                => false

                def stripUndefined(t: TsType): TsType = t match
                    case TsType.Union(ms) =>
                        ms.filterNot(_ == TsType.Named("undefined")) match
                            case single :: Nil => single
                            case many          => TsType.Union(many)
                    case other => other

                /** Trailing params that have defaults or are UndefOr become optional. */
                def markOptional(params: List[(String, TsType, Boolean)]): List[TsParam] = {
                    val optionalFlags = params
                        .foldRight((List.empty[Boolean], true)) {
                            case ((_, tpe, hasDefault), (acc, tailOptional)) =>
                                val opt = tailOptional && (hasDefault || containsUndefined(tpe))
                                (opt :: acc, opt)
                        }
                        ._1
                    params.zip(optionalFlags).map { case ((name, tpe, _), opt) =>
                        TsParam(name, if opt then stripUndefined(tpe) else tpe, opt)
                    }
                }

                /** Upper bounds that constrain nothing in TypeScript are not worth emitting. */
                val trivialBounds = Set(
                  "scala.Any",
                  "scala.AnyRef",
                  "scala.Matchable",
                  "java.lang.Object",
                  "scala.scalajs.js.Any"
                )

                /** A type parameter's upper bound, if it is meaningful and maps cleanly.
                  *
                  * A bound that cannot be mapped is dropped rather than reported: the resulting
                  * `<A>` is a widening, so it never makes the output unsound, and a Scala-only
                  * bound must not block the whole file.
                  */
                def typeParamOf(td: TypeDef, ctx: String): TsTypeParam = {
                    val bound = td.rhs match
                        case tbt: TypeBoundsTree
                            if !trivialBounds.contains(tbt.hi.tpe.typeSymbol.fullName) =>
                            mapper.map(tbt.hi.tpe, ctx).toOption
                        case _ => None
                    TsTypeParam(td.name, bound)
                }

                /** Type parameters of a class or trait, in declaration order. */
                def classTypeParams(sym: Symbol): List[TsTypeParam] = sym.tree match
                    case cd: ClassDef =>
                        cd.constructor.leadingTypeParams
                            .map(typeParamOf(_, sym.fullName.stripSuffix("$")))
                    case _ => Nil

                def methodOverload(owner: Symbol, m: Symbol): Option[TsOverload] = {
                    val ctx = context(owner, m)
                    m.tree match
                        case dd: DefDef =>
                            val rawParams = dd.termParamss.flatMap(_.params).map { p =>
                                val overrideTs =
                                    if m.isClassConstructor then
                                        tsTypeOfParamOrField(owner, p.symbol)
                                    else tsTypeOf(p.symbol)
                                (
                                  p.name,
                                  atPosition(input = true)(mapType(p.tpt.tpe, ctx, overrideTs)),
                                  p.symbol.flags.is(Flags.HasDefault)
                                )
                            }
                            val ret = atPosition(input = false)(
                              mapType(dd.returnTpt.tpe, ctx, tsTypeOf(m))
                            )
                            if rawParams.exists(_._2.isEmpty) || ret.isEmpty then None
                            else
                                Some(
                                  TsOverload(
                                    dd.leadingTypeParams.map(typeParamOf(_, ctx)),
                                    markOptional(rawParams.map { case (n, t, d) => (n, t.get, d) }),
                                    ret.get,
                                    docOf(m)
                                  )
                                )
                        case _ => None
                }

                def isGetterDef(m: Symbol): Boolean = m.tree match
                    case dd: DefDef => dd.termParamss.isEmpty
                    case _          => false

                def visibleMember(m: Symbol): Boolean =
                    !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) &&
                        !m.flags.is(Flags.Synthetic) && !m.flags.is(Flags.Artifact) &&
                        !m.isClassConstructor && !m.name.contains("$default$") &&
                        !m.name.endsWith("_=") && // var setters: the var itself comes from fields
                        annots(m, TsIgnoreAnnot).isEmpty

                def isJsObjectSubtype(sym: Symbol): Boolean =
                    sym.typeRef.baseClasses.exists(_.fullName == "scala.scalajs.js.Object")

                def posOrder(m: Symbol): Int = m.pos.map(_.start).getOrElse(Int.MaxValue)

                /** JavaScript names a member is exported under.
                  *
                  * `@JSExport` without an argument, and the implied export of every public member
                  * of a JS class or an `@JSExportAll` class, use the Scala name;
                  * `@JSExport("other")` uses `other`. A member may carry both.
                  */
                def exportedNames(m: Symbol, annotFqn: String, implied: Boolean): List[String] = {
                    val explicit = annots(m, annotFqn).map(annotStringArg(_).getOrElse(m.name))
                    val all = if implied then m.name :: explicit else explicit
                    all.distinct
                }

                /** Pairs each exportable symbol with every JavaScript name it is exported under. */
                def withExportedNames(
                    syms: List[Symbol],
                    annotFqn: String,
                    implied: Boolean
                ): List[(Symbol, String)] =
                    syms
                        .filter(visibleMember)
                        .flatMap(m => exportedNames(m, annotFqn, implied).map(m -> _))

                /** Builds Method/Property members from symbols paired with their exported names. */
                def buildMembers(
                    owner: Symbol,
                    methods: List[(Symbol, String)],
                    fields: List[(Symbol, String)],
                    static: Boolean
                ): List[TsMember] = {
                    val methodMembers = methods
                        .groupBy(_._2)
                        .toList
                        .map { case (name, overloadPairs) =>
                            val sorted = overloadPairs.map(_._1).sortBy(posOrder)
                            if sorted.forall(isGetterDef) then
                                // parameterless def -> readonly property
                                val m = sorted.head
                                val ctx = context(owner, m)
                                val tpe = m.tree match
                                    case dd: DefDef => mapType(dd.returnTpt.tpe, ctx, tsTypeOf(m))
                                    case _          => None
                                tpe.map { t =>
                                    (
                                      (posOrder(m), name),
                                      TsMember.Property(
                                        name,
                                        if containsUndefined(t) then stripUndefined(t) else t,
                                        readonly = true,
                                        optional = containsUndefined(t),
                                        static = static,
                                        doc = docOf(m)
                                      )
                                    )
                                }
                            else
                                val overloads =
                                    sorted.flatMap(methodOverload(owner, _)).sortBy(_.params.size)
                                if overloads.isEmpty then None
                                else
                                    Some(
                                      (
                                        (posOrder(sorted.head), name),
                                        TsMember.Method(name, overloads, static = static)
                                      )
                                    )
                        }
                        .flatten
                    val fieldMembers = fields.flatMap { case (f, name) =>
                        val ctx = context(owner, f)
                        val tpe = f.tree match
                            case vd: ValDef =>
                                mapType(vd.tpt.tpe, ctx, tsTypeOfParamOrField(owner, f))
                            case _ => None
                        tpe.map { t =>
                            (
                              (posOrder(f), name),
                              TsMember.Property(
                                name,
                                if containsUndefined(t) then stripUndefined(t) else t,
                                readonly = !f.flags.is(Flags.Mutable),
                                optional = containsUndefined(t),
                                static = static,
                                doc = docOf(f)
                              )
                            )
                        }
                    }
                    // sorted by source position, then by exported name so that the two names of
                    // one aliased symbol never depend on the grouping map's iteration order
                    (methodMembers ++ fieldMembers).sortBy(_._1).map(_._2)
                }

                /** Base classes that contribute members to a subclass's JavaScript API.
                  *
                  * Only user types: everything in `scala.*` and `java.*` - js.Object, Any, Object,
                  * and the native JS types whose members TypeScript already knows - would be noise.
                  *
                  * A generic base is skipped as well. Its members are read off its own trees, so
                  * `class StringBox extends Box[String]` would re-emit `value: A` with nothing
                  * binding `A`, which makes tsc reject the whole file. Substituting the type
                  * arguments would mean typing members through `memberType` instead of their
                  * DefDef, so the safe subset is kept until a facade actually needs it.
                  */
                def contributingBases(sym: Symbol): List[Symbol] =
                    sym.typeRef.baseClasses.filter(b =>
                        b != sym && !b.fullName.startsWith("scala.") &&
                            !b.fullName.startsWith("java.") && classTypeParams(b).isEmpty
                    )

                /** The JavaScript-native base a class extends, as TypeScript names it.
                  *
                  * `contributingBases` deliberately skips everything in `scala.*`, which is right
                  * for `js.Object` but wrong for `js.Error`: TypeScript's own `Error` declares
                  * `message`, `name` and `stack`, and a subclass emitted without `extends Error`
                  * has none of them, so `err.message` fails to compile against a class that carries
                  * it at runtime. Re-declaring `message` in Scala is not the fix - a `val message`
                  * clashes with the inherited native member.
                  *
                  * Restricted to this table on purpose. A user base class contributes its members
                  * to the subclass body instead (see `ownAndInheritedMembers`), so writing
                  * `extends` for one would declare them twice.
                  *
                  * The most derived base wins: `baseClasses` is the linearisation, so a
                  * `js.TypeError` subclass matches `TypeError` before `Error`.
                  */
                def nativeJsSuperClass(sym: Symbol): Option[String] =
                    sym.typeRef.baseClasses.iterator
                        .filter(_ != sym)
                        .flatMap(b => nativeJsBases.get(b.fullName))
                        .nextOption()

                /** Members declared by one class, under the export rules of that class. */
                def declaredMembers(owner: Symbol): List[TsMember] = {
                    val exportAll = isJsObjectSubtype(owner) ||
                        symAndModuleAnnots(owner, JSExportAllAnnot).nonEmpty
                    buildMembers(
                      owner,
                      withExportedNames(owner.declaredMethods, JSExportAnnot, exportAll),
                      withExportedNames(owner.declaredFields, JSExportAnnot, exportAll),
                      static = false
                    )
                }

                def memberName(m: TsMember): String = m match
                    case TsMember.Method(name, _, _)            => name
                    case TsMember.Property(name, _, _, _, _, _) => name
                    case _: TsMember.Ctor                       => "constructor"

                /** A class's own members followed by the ones it inherits.
                  *
                  * Scala.js exports inherited members too, so leaving them out understated the API.
                  * Names are deduplicated most-derived-first, which also means a base-only overload
                  * of an overridden name is not emitted.
                  */
                def ownAndInheritedMembers(sym: Symbol): List[TsMember] = {
                    val perClass = (sym :: contributingBases(sym)).map(declaredMembers)
                    perClass
                        .foldLeft((List.empty[TsMember], Set.empty[String])) {
                            case ((acc, seen), members) =>
                                val fresh = members.filterNot(m => seen.contains(memberName(m)))
                                (acc ++ fresh, seen ++ fresh.map(memberName))
                        }
                        ._1
                }

                def classMembers(sym: Symbol, ctorDoc: Option[TsDoc]): List[TsMember] = {
                    // Every public constructor, primary and secondary, becomes one overload of a
                    // single Ctor member. Reading only the primary made every secondary invisible,
                    // so `new Ctors(head, tail)` was a TypeScript error against a class that
                    // accepts it at runtime.
                    // `declarations`, not `declaredMethods`: the latter excludes constructors
                    // entirely, so it sees no secondary at all. The primary is put first
                    // explicitly rather than relying on declaration order.
                    val ctorSymbols =
                        (sym.primaryConstructor +: sym.declarations.filter(
                          _.isClassConstructor
                        )).distinct
                            .filter(c => c.exists && !c.flags.is(Flags.Private))
                    val ctor = ctorSymbols.flatMap(c => methodOverload(sym, c)) match
                        case Nil       => Nil
                        case overloads =>
                            // no source fallback here: the only comment preceding a primary
                            // constructor is the class's own, which is emitted separately
                            val written = ctorSymbols.head.docstring
                                .flatMap(DocConverter.convert)
                                .orElse(ctorDoc)
                            // A constructor carrying Scala's `@deprecated` must say so in the
                            // .d.ts, the same as any other member. It does not go through `docOf`
                            // (see above), so the tag has to be folded in here - without it a
                            // deprecated constructor shipped looking supported, and `Emulator`'s
                            // was the documented way to build one.
                            val doc = deprecationOf(ctorSymbols.head) match
                                case Some(tag)
                                    if !written.exists(
                                      _.lines.exists(_.startsWith("@deprecated"))
                                    ) =>
                                    Some(TsDoc(written.map(_.lines).getOrElse(Nil) :+ tag))
                                case _ => written
                            List(TsMember.Ctor(overloads.map(_.params), doc))

                    val statics = {
                        val mod = sym.companionModule
                        if !mod.exists then Nil
                        else {
                            val mc = mod.moduleClass
                            val sMethods =
                                withExportedNames(mc.declaredMethods, JSExportStaticAnnot, false)
                            val sFields =
                                withExportedNames(mc.declaredFields, JSExportStaticAnnot, false)
                            buildMembers(mc, sMethods, sFields, static = true)
                        }
                    }
                    ctor ++ ownAndInheritedMembers(sym) ++ statics
                }

                /** Emits a `@JSExportTopLevel` def: the first name is canonical, the rest become
                  * deprecated alias exports, as they do for classes.
                  */
                def emitTopLevelFun(owner: Symbol, m: Symbol): Unit = {
                    val names = topLevelExportNames(m)
                    methodOverload(owner, m).foreach { overload =>
                        decls += TsDecl.Fun(names.head, List(overload), names.tail)
                    }
                }

                // ---- emit exported declarations ----------------------------------------
                for (sym, names) <- exportedClasses do {
                    val canonical = names.head
                    val aliases = names.tail
                    if sym.flags.is(Flags.Module) then {
                        // object: ConstObj of its @JSExport members; JSExportTopLevel defs -> Fun
                        // same rule as a class: a js.Object (or @JSExportAll) singleton exports
                        // every public member, others only the annotated ones
                        decls += TsDecl.ConstObj(
                          canonical,
                          ownAndInheritedMembers(sym),
                          docOf(sym)
                        )
                        for m <- sym.declaredMethods if annots(m, JSExportTopLevelAnnot).nonEmpty
                        do emitTopLevelFun(sym, m)
                    } else {
                        val (clsDoc, ctorDoc) = docOf(sym)
                            .map(DocConverter.splitConstructorTag)
                            .getOrElse((None, None))
                        decls += TsDecl.Cls(
                          canonical,
                          classTypeParams(sym),
                          nativeJsSuperClass(sym),
                          classMembers(sym, ctorDoc),
                          clsDoc,
                          deprecatedAliases = aliases
                        )
                    }
                }

                // top-level @JSExportTopLevel defs inside NON-exported objects
                for tasty <- tastys do {
                    object funFinder extends TreeAccumulator[Unit] {
                        def foldTree(u: Unit, tree: Tree)(owner: Symbol): Unit = tree match
                            case c: ClassDef
                                if c.symbol.flags.is(Flags.Module) &&
                                    topLevelExportNames(c.symbol).isEmpty &&
                                    !excluded(c.symbol) =>
                                for
                                    m <- c.symbol.declaredMethods
                                    if annots(m, JSExportTopLevelAnnot).nonEmpty
                                do emitTopLevelFun(c.symbol, m)
                                foldOverTree(u, tree)(owner)
                            case _ => foldOverTree(u, tree)(owner)
                    }
                    funFinder.foldTree((), tasty.ast)(tasty.ast.symbol)
                }

                // ---- process the chase queue -------------------------------------------
                // Two phases. Emitting while still discovering would freeze a type's array
                // direction before a later sibling reaches it from an OUTPUT position: the flag
                // only ever moves input->output, so an interface emitted too early keeps a
                // `readonly` it should have lost, and a consumer assigning it to a mutable array
                // gets TS2322 against a published declaration file.
                def inputOnlyOf(sym: Symbol): Boolean = chasedInputOnly.getOrElse(sym, false)

                // phase 1: walk to a fixpoint, recording nothing, until no symbol is newly
                // reachable and no direction still changes
                val settledWith = mutable.Map.empty[Symbol, Boolean]
                recordErrors = false
                var changed = true
                while changed do {
                    changed = false
                    for (sym, _) <- chased.toList do {
                        val direction = inputOnlyOf(sym)
                        if !settledWith.get(sym).contains(direction) then {
                            settledWith(sym) = direction
                            changed = true
                            atPosition(input = direction)(ownAndInheritedMembers(sym))
                        }
                    }
                }
                recordErrors = true

                // phase 2: every direction is final, so emit each interface exactly once
                for (sym, name) <- chased.toList do {
                    val inputOnly = inputOnlyOf(sym)
                    decls += TsDecl.Iface(
                      name,
                      classTypeParams(sym),
                      atPosition(input = inputOnly)(ownAndInheritedMembers(sym)),
                      docOf(sym),
                      inputOnly = inputOnly
                    )
                }

                // ---- resolve doc links now that every declaration name is known ---------
                val declaredTsNames = decls.toList.map(_.name).toSet ++
                    decls.toList.collect {
                        case c: TsDecl.Cls => c.deprecatedAliases
                        case f: TsDecl.Fun => f.deprecatedAliases
                    }.flatten
                val tsNameByFqn = knownNames ++
                    chased.map { case (sym, name) => sym.fullName -> name }
                // a simple name resolves only while it is unambiguous
                val tsNameBySimpleName = tsNameByFqn
                    .groupBy(_._1.split('.').last)
                    .collect { case (simple, only) if only.sizeIs == 1 => simple -> only.head._2 }
                def tsNameForLink(target: String): Option[String] =
                    if declaredTsNames.contains(target) then Some(target)
                    else tsNameByFqn.get(target).orElse(tsNameBySimpleName.get(target))
                val resolved = TsModule.mapDocs(
                  TsModule(decls.toList),
                  DocConverter.resolveLinks(_, tsNameForLink)
                )
                decls.clear()
                decls ++= resolved.decls
            }
        }

        val tastyFiles = tastyRoots.flatMap(walkTasty)
        // An inspection that fails (typically stale class directories left by an incremental
        // build, or a missing classpath entry) must NOT silently produce an empty .d.ts.
        val inspected = TastyInspector.inspectAllTastyFiles(tastyFiles, Nil, classpath)(inspector)
        val inspectionErrors =
            if tastyFiles.isEmpty then
                List(
                  ExportError(
                    "<tasty-inspector>",
                    s"no .tasty files found under ${tastyRoots.mkString(", ")}; compile the modules first"
                  )
                )
            else if !inspected then
                List(
                  ExportError(
                    "<tasty-inspector>",
                    "TASTy inspection failed (see the unpickling errors above). Stale class " +
                        "directories are the usual cause: clean the JS projects and recompile."
                  )
                )
            else Nil
        // defensive: the Scala.js linker would also reject duplicate top-level export
        // names, but chased interface names and aliases are ours alone to check
        val allNames = decls.toList.map(_.name) ++
            decls.toList.collect {
                case c: TsDecl.Cls => c.deprecatedAliases
                case f: TsDecl.Fun => f.deprecatedAliases
            }.flatten
        val collisionErrors = allNames
            .groupBy(identity)
            .collect { case (name, occurrences) if occurrences.sizeIs > 1 => name }
            .toList
            .sorted
            .map(n => ExportError(n, s"duplicate top-level TypeScript declaration name '$n'"))
        Result(TsModule(decls.toList), inspectionErrors ++ errors.toList ++ collisionErrors)
    }

    /** `@Ann`, `@Ann(args)`, `@pkg.Ann`, and the meta-annotation form `@(Ann @field)(args)`. */
    private val annotationPattern = raw"@(?:\w+(?:\.\w+)*|\([^)]*\))(?:\(.*\))?"

    private val annotationOnly = annotationPattern.r

    /** Whitespace, annotations, and the modifier/keyword head of the definition itself. */
    private val definitionHead =
        raw"(?:" + annotationPattern + raw"\s*)*(?:(?:private|protected|final|override|implicit|lazy|inline" +
            raw"|infix|open|transparent|abstract|sealed|opaque|case|given|def|val|var|class" +
            raw"|trait|object|type|enum)\b\s*)*"

    private val definitionHeadRe = definitionHead.r

    /** Does the definition starting right after `between` own the doc comment before it?
      *
      * Only blank lines and standalone annotations may separate the two, ending with the modifier
      * and keyword head of that very definition. A complete definition in between - a one-line
      * annotated member, say - means the comment documents that one instead.
      */
    private[tsexport] def ownsPrecedingDoc(between: String): Boolean = {
        val lines = between.linesIterator.map(_.trim).toList
        lines match
            case Nil => true
            case _ =>
                lines.init.forall(l => l.isEmpty || annotationOnly.matches(l)) &&
                definitionHeadRe.matches(lines.last)
    }

    private def walkTasty(root: String): List[String] = {
        import scala.jdk.CollectionConverters.*
        val p = Paths.get(root)
        if !Files.exists(p) then Nil
        else
            // Files.walk holds an open directory stream; close it
            Using.resource(Files.walk(p)) {
                _.iterator().asScala
                    .filter(_.toString.endsWith(".tasty"))
                    .map(_.toString)
                    .toList
                    .sorted
            }
    }
}
