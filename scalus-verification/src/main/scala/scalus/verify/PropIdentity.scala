package scalus.verify

import scalus.compiler.sir.{SIR, SIRType}

/** The identity of a statement by its content (docs/design/verification-overview.md §4.4): a hash
  * of its logical nodes, the names and types of its variables, the SIR of its tests, and the
  * functions it calls, by name.
  *
  * What is left out is what does not make the statement another one:
  *   - where it is written. SIR carries source positions in its annotations, and none of them is
  *     read, so a statement moved to another line, or another test, is the same;
  *   - the code of a function it calls. A call names its function, and the function's code is the
  *     programs' part of a kept result's fingerprint (§5.6): where that code changes, the statement
  *     is the same one, and what is kept for it is stale;
  *   - the ids the compiler gives type variables and quantified variables, and the numbers it makes
  *     names unique with, which a compilation makes afresh. A statement's variable is `x_1042`
  *     where `forAll` binds `x` at offset 1042 of its file, and `time-79636` where a contract takes
  *     it from the parameters of its function's SIR, whose number depends on what else was compiled
  *     with it.
  *
  * A function is known by its name: functions that are not the same need names that are not the
  * same, which a kept result holds to ([[KeptResults]]).
  *
  * A variable is known by its place among the binders: a statement's variable also by the name it
  * is written with, under which a counterexample's value is kept, and a variable of a test only by
  * its place, as the compiler names it. It is the content as written: a statement's variable
  * renamed, or a test written another way, is another statement.
  */
private[verify] object PropIdentity {

    /** The identity of `prop`: 16 hexadecimal digits of the SHA-256 of its content, as a
      * fingerprint's parts are.
      */
    def of(prop: Prop): String = {
        val out = Writer()
        out.prop(prop)
        Fingerprint.hash(out.result)
    }

    /** A name the compiler made unique, with a number after the name as written, and before the
      * field of a variable that stands for a field of another.
      */
    private val numbered = """^([^.]*?)[_-]\d+(\..*)?$""".r

    /** A variable's name as it is written, without the number the compiler made it unique with: `_`
      * and the offset of a statement's binder in its file, or `-` and the plugin's count of a
      * function's parameter in its SIR, either of which another compilation changes. The identity
      * of a statement and the names a counterexample's values are kept under both take it.
      */
    private[verify] def written(name: String): String = name match
        case numbered(written, field) => written + Option(field).getOrElse("")
        case _                        => name

    /** Writes a statement as a tree of nodes and atoms. An atom is written with its length, so that
      * no two contents are written alike.
      */
    private final class Writer {
        private val text = new StringBuilder

        def result: String = text.toString

        /** The variables bound where the writer is, by their names, with what they are written as.
          */
        private var scope = Map.empty[String, String]

        /** The binders written so far: the place of the next one. */
        private var binders = 0

        /** Binds the variable `name`, as `written` and its place, until the [[scoped]] it is in
          * ends, and returns what it is written as.
          */
        private def bind(name: String, written: String): String = {
            binders += 1
            val as = s"$written#$binders"
            scope = scope.updated(name, as)
            as
        }

        /** Writes `body`, with the variables it binds bound in it only. */
        private def scoped(body: => Unit): Unit = {
            val outside = scope
            body
            scope = outside
        }

        /** What a variable that `name` refers to is written as: one bound in the statement as it
          * was bound, another, which only a malformed statement has, by its name.
          */
        private def reference(name: String): String = scope.getOrElse(name, name)

        /** Binds a statement's variable, and writes it: by its name as written, and its type. */
        private def variable(ident: PropExpr.Ident[?]): Unit = {
            node("ident") { atom(bind(ident.name, written(ident.name))); tpe(ident.tp) }
        }

        private def atom(value: String): Unit = {
            text.append(value.length).append(':').append(value).append(' ')
        }

        private def node(tag: String)(body: => Unit): Unit = {
            text.append('(').append(tag).append(' ')
            body
            text.append(')')
        }

        def prop(prop: Prop): Unit = prop match {
            case Prop.Bool(expr)         => node("bool")(this.expr(expr))
            case Prop.Denotes(expr)      => node("denotes")(this.expr(expr))
            case Prop.Equal(left, right) => node("equal") { expr(left); expr(right) }
            case Prop.Call(fn, arg, result, total, body) =>
                node("call") {
                    atom(fn.name)
                    atom(total.toString)
                    expr(arg)
                    scoped { variable(result); this.prop(body) }
                }
            case Prop.Forall(ident, body) =>
                node("forall")(scoped { variable(ident); this.prop(body) })
            // A witness is of the variables around the binder, not of its own.
            case Prop.Exists(ident, witness, body) =>
                node("exists") {
                    witness.fold(atom("-"))(expr)
                    scoped { variable(ident); this.prop(body) }
                }
            case Prop.And(a, b)     => node("and") { this.prop(a); this.prop(b) }
            case Prop.Or(a, b)      => node("or") { this.prop(a); this.prop(b) }
            case Prop.Implies(a, b) => node("implies") { this.prop(a); this.prop(b) }
            case Prop.Iff(a, b)     => node("iff") { this.prop(a); this.prop(b) }
            case Prop.Not(a)        => node("not")(this.prop(a))
        }

        private def expr(expr: PropExpr[?]): Unit = expr match
            case PropExpr.Ident(name, _, tp) => node("ident") { atom(reference(name)); tpe(tp) }
            case PropExpr.SIRExpr(sir)       => sirTerm(sir)

        private def sirTerm(sir: SIR): Unit = sir match {
            case SIR.Var(name, tp, _) => node("var") { atom(reference(name)); tpe(tp) }
            case SIR.ExternalVar(module, name, tp, _) =>
                node("external") { atom(module); atom(name); tpe(tp) }
            // The bindings of a recursive let are bound in their values, those of another one each
            // after its value.
            case SIR.Let(bindings, body, flags, _) =>
                node("let") {
                    atom(flags.toString)
                    scoped {
                        if flags.isRec then
                            val names = bindings.map(binding => bind(binding.name, ""))
                            bindings.zip(names).foreach { (binding, name) =>
                                node("binding") {
                                    atom(name)
                                    tpe(binding.tp)
                                    sirTerm(binding.value)
                                }
                            }
                        else
                            bindings.foreach { binding =>
                                node("binding") {
                                    tpe(binding.tp)
                                    sirTerm(binding.value)
                                    atom(bind(binding.name, ""))
                                }
                            }
                        sirTerm(body)
                    }
                }
            case SIR.LamAbs(param, term, typeParams, _) =>
                node("lambda") {
                    typeParams.foreach(typeVariable => atom(typeVariable.name))
                    scoped {
                        atom(bind(param.name, ""))
                        tpe(param.tp)
                        sirTerm(term)
                    }
                }
            case SIR.Apply(f, arg, tp, _) => node("apply") { sirTerm(f); sirTerm(arg); tpe(tp) }
            case SIR.Select(scrutinee, field, tp, _) =>
                node("select") { sirTerm(scrutinee); atom(field); tpe(tp) }
            case SIR.Const(constant, tp, _) => node("const") { atom(constant.toString); tpe(tp) }
            case SIR.And(a, b, _)           => node("and") { sirTerm(a); sirTerm(b) }
            case SIR.Or(a, b, _)            => node("or") { sirTerm(a); sirTerm(b) }
            case SIR.Not(a, _)              => node("not")(sirTerm(a))
            case SIR.IfThenElse(cond, t, f, tp, _) =>
                node("if") { sirTerm(cond); sirTerm(t); sirTerm(f); tpe(tp) }
            case SIR.Builtin(builtin, tp, _) => node("builtin") { atom(builtin.toString); tpe(tp) }
            case SIR.Error(message, _, _)    => node("error")(sirTerm(message))
            case SIR.Constr(name, data, args, tp, _) =>
                node("constr") { atom(name); atom(data.name); args.foreach(sirTerm); tpe(tp) }
            case SIR.Match(scrutinee, cases, tp, _) =>
                node("match") {
                    sirTerm(scrutinee)
                    cases.foreach(matchCase =>
                        node("case")(scoped {
                            pattern(matchCase.pattern)
                            sirTerm(matchCase.body)
                        })
                    )
                    tpe(tp)
                }
            case SIR.Cast(term, tp, _) => node("cast") { sirTerm(term); tpe(tp) }
            case SIR.Decl(data, term) =>
                node("decl") {
                    atom(data.name)
                    data.constructors.foreach(constructor =>
                        node("constructor") {
                            atom(constructor.name)
                            constructor.params.foreach(param => { atom(param.name); tpe(param.tp) })
                        }
                    )
                    sirTerm(term)
                }
        }

        private def pattern(pattern: SIR.Pattern): Unit = pattern match
            case SIR.Pattern.Constr(constr, bindings, typeBindings) =>
                node("constr") {
                    atom(constr.name)
                    bindings.foreach(name => atom(bind(name, "")))
                    typeBindings.foreach(tpe)
                }
            case SIR.Pattern.Const(value) => node("const")(sirTerm(value))
            case SIR.Pattern.Wildcard     => atom("_")

        private def tpe(tp: SIRType): Unit = tpe(tp, Set.empty)

        /** A type, by the names of what it is made of. A recursive type refers to itself through a
          * proxy, which is followed once.
          */
        private def tpe(tp: SIRType, seen: Set[SIRType.TypeProxy]): Unit = tp match
            case SIRType.Annotated(inner, _) => tpe(inner, seen)
            case SIRType.Fun(in, out)        => node("fun") { tpe(in, seen); tpe(out, seen) }
            case SIRType.CaseClass(constr, args, _) =>
                node("case") { atom(constr.name); args.foreach(tpe(_, seen)) }
            case SIRType.SumCaseClass(decl, args) =>
                node("sum") { atom(decl.name); args.foreach(tpe(_, seen)) }
            case SIRType.TypeLambda(params, body) =>
                node("for") { params.foreach(param => atom(param.name)); tpe(body, seen) }
            case typeVariable: SIRType.TypeVar => node("tv")(atom(typeVariable.name))
            case proxy: SIRType.TypeProxy =>
                if seen.contains(proxy) then atom("<recursive>")
                else
                    val ref = proxy.ref
                    if ref == null then atom("<unresolved>") else tpe(ref, seen + proxy)
            case other => atom(other.show)
    }
}
