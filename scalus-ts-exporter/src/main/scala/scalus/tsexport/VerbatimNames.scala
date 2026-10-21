package scalus.tsexport

/** Checks that every type name inside `@TsType` text is declared.
  *
  * The exporter writes `@TsType` text verbatim, so nothing else checks the names in it. A text like
  * `Partial<ProtocolParamsLike>` gives a `.d.ts` that does not compile when no declaration is named
  * `ProtocolParamsLike`. Before 1.4.0 this happened with `PoolRegistration`.
  *
  * This is a scan for identifiers, not a TypeScript parser. It skips string literals, property keys
  * and parameter names (an identifier before `:` or `?:`), member names after `.`, and the names a
  * text binds itself (`K` in `[K in keyof T]`, `U` in `infer U`). It accepts TypeScript keywords,
  * primitives and the common global types, the declarations of the module, and the type parameters
  * in scope. Known gaps: in a conditional type `A extends B ? X : Y` the scan reads `X` as a key
  * and does not check it, and a generic function type `<T>(x: T) => T` reports `T`.
  */
object VerbatimNames {

    private val keywords = Set(
      "readonly",
      "keyof",
      "typeof",
      "extends",
      "infer",
      "in",
      "is",
      "asserts",
      "unique",
      "new",
      "as"
    )

    private val primitives = Set(
      "string",
      "number",
      "boolean",
      "bigint",
      "symbol",
      "undefined",
      "null",
      "void",
      "never",
      "unknown",
      "any",
      "object",
      "this",
      "true",
      "false"
    )

    /** Global types from TypeScript's `lib.es2020.d.ts` that a `@TsType` text may name. */
    private val globals = Set(
      "Array",
      "ReadonlyArray",
      "Promise",
      "PromiseLike",
      "Record",
      "Partial",
      "Required",
      "Readonly",
      "Pick",
      "Omit",
      "Exclude",
      "Extract",
      "NonNullable",
      "Parameters",
      "ReturnType",
      "Awaited",
      "Uint8Array",
      "ArrayBuffer",
      "Map",
      "ReadonlyMap",
      "Set",
      "ReadonlySet",
      "Date",
      "Error",
      "Function",
      "Iterable",
      "IterableIterator",
      "AsyncIterable",
      "RegExp",
      "Symbol",
      "BigInt"
    )

    /** Names TypeScript itself declares; a `@TsType` text may use them without a declaration. */
    val builtins: Set[String] = keywords ++ primitives ++ globals

    /** The type names a TypeScript type text refers to, in order of first use. */
    def referencedNames(text: String): List[String] = {
        val code = withoutStringLiterals(text)
        val tokens = List.newBuilder[(String, Int, Int)]
        var i = 0
        while i < code.length do {
            val c = code.charAt(i)
            if isIdentStart(c) then
                var end = i + 1
                while end < code.length && isIdentPart(code.charAt(end)) do end += 1
                tokens += ((code.substring(i, end), i, end))
                i = end
            else if c.isDigit then
                // a number literal type, such as `0` or `1e3`; skip it whole
                while i < code.length && isIdentPart(code.charAt(i)) do i += 1
            else i += 1
        }
        val all = tokens.result()
        // names the text binds itself: `K` in `[K in keyof T]` and `U` in `infer U`
        val bound = all
            .sliding(2)
            .collect {
                case List((name, _, _), ("in", _, _))    => name
                case List(("infer", _, _), (name, _, _)) => name
            }
            .toSet
        all.collect {
            case (name, start, end)
                if !bound(name) && !isMemberName(code, start) && !isKeyOrParam(code, end) =>
                name
        }.distinct
    }

    /** Errors for every `@TsType` text in `module` that names an undeclared type. */
    def check(module: TsModule): List[ExportError] = {
        val declared = module.decls.map(_.name).toSet ++ module.decls.collect {
            case c: TsDecl.Cls => c.deprecatedAliases
            case f: TsDecl.Fun => f.deprecatedAliases
        }.flatten

        def inType(t: TsType, scope: Set[String], member: String): List[ExportError] = t match
            case TsType.Verbatim(text) =>
                referencedNames(text)
                    .filterNot(n => builtins(n) || declared(n) || scope(n))
                    .map(n =>
                        ExportError(
                          member,
                          s"@TsType(\"$text\") names '$n', which is neither declared in the " +
                              "output nor a TypeScript built-in"
                        )
                    )
            case TsType.Named(_)         => Nil
            case TsType.Union(ms)        => ms.flatMap(inType(_, scope, member))
            case TsType.Intersect(ms)    => ms.flatMap(inType(_, scope, member))
            case TsType.Arr(elem)        => inType(elem, scope, member)
            case TsType.Index(value)     => inType(value, scope, member)
            case TsType.Generic(_, args) => args.flatMap(inType(_, scope, member))
            case TsType.Func(params, ret) =>
                params.flatMap(p => inType(p.tpe, scope, member)) ++ inType(ret, scope, member)

        def names(tps: List[TsTypeParam]): Set[String] = tps.map(_.name).toSet

        def inOverload(o: TsOverload, scope: Set[String], member: String): List[ExportError] = {
            val inner = scope ++ names(o.typeParams)
            o.params.flatMap(p => inType(p.tpe, inner, member)) ++ inType(o.ret, inner, member)
        }

        def inMember(m: TsMember, scope: Set[String], owner: String): List[ExportError] =
            m match
                case TsMember.Ctor(overloads, _) =>
                    overloads.flatten.flatMap(p => inType(p.tpe, scope, s"$owner.constructor"))
                case TsMember.Method(name, overloads, _) =>
                    overloads.flatMap(inOverload(_, scope, s"$owner.$name"))
                case p: TsMember.Property => inType(p.tpe, scope, s"$owner.${p.name}")

        module.decls.flatMap {
            case c: TsDecl.Cls =>
                c.members.flatMap(inMember(_, names(c.typeParams), c.name))
            case i: TsDecl.Iface =>
                i.members.flatMap(inMember(_, names(i.typeParams), i.name))
            case f: TsDecl.Fun      => f.overloads.flatMap(inOverload(_, Set.empty, f.name))
            case o: TsDecl.ConstObj => o.members.flatMap(inMember(_, Set.empty, o.name))
        }.distinct
    }

    private def isIdentStart(c: Char): Boolean = c.isLetter || c == '_' || c == '$'

    private def isIdentPart(c: Char): Boolean = isIdentStart(c) || c.isDigit

    /** `B` in `A.B`: a member of a namespace or an enum, checked through `A` alone. */
    private def isMemberName(code: String, start: Int): Boolean = {
        var j = start - 1
        while j >= 0 && code.charAt(j).isWhitespace do j -= 1
        // `...rest` is a spread, not a member access
        j >= 0 && code.charAt(j) == '.' && !(j >= 1 && code.charAt(j - 1) == '.')
    }

    /** `key` in `{ key: T }`, `{ key?: T }`, `(key: T) => R` or `[key: string]: T`. */
    private def isKeyOrParam(code: String, end: Int): Boolean = {
        var j = end
        while j < code.length && code.charAt(j).isWhitespace do j += 1
        if j < code.length && code.charAt(j) == '?' then j += 1
        while j < code.length && code.charAt(j).isWhitespace do j += 1
        j < code.length && code.charAt(j) == ':'
    }

    /** `text` with the body of every `"…"`, `'…'` and `` `…` `` literal blanked out. */
    private def withoutStringLiterals(text: String): String = {
        val out = new StringBuilder(text.length)
        var quote: Option[Char] = None
        var i = 0
        while i < text.length do {
            val c = text.charAt(i)
            quote match
                case Some(q) =>
                    if c == '\\' then
                        out.append("  ")
                        i += 1
                    else if c == q then
                        out.append(c)
                        quote = None
                    else out.append(' ')
                case None =>
                    if c == '"' || c == '\'' || c == '`' then quote = Some(c)
                    out.append(c)
            i += 1
        }
        out.toString
    }
}
