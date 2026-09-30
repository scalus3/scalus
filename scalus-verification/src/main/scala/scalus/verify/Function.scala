package scalus.verify

import scalus.cardano.ledger.Language
import scalus.compiler.Options
import scalus.compiler.sir.{SIR, SIRType, TargetLoweringBackend}
import scalus.compiler.sir.lowering.{InOutRepresentationPair, LambdaRepresentation, LoweredValueRepresentation, LoweringContext}
import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator
import scalus.uplc.{PlutusV3, Program}
import scalus.verify.uplcblaster.UplcBlaster

/** The typed name of a function in a [[FunctionTable]].
  *
  * A call in a [[Prop]] stores only this. Each proof method looks the function up by name, in the
  * table it is given, and takes the [[Representation]] it needs.
  *
  * `name` identifies the function. For a `@Compile` definition it is the fully-qualified name SIR
  * uses for it, such as `scalus.cardano.onchain.plutus.prelude.Math$.clamp`, the `name` of the
  * `ExternalVar` a call to it compiles to. A statement read back from SIR therefore refers to the
  * same entry. A function without a SIR definition of its own, such as a lambda or an `inline def`,
  * has a synthetic name, which contains no dot and so never collides with a qualified one.
  *
  * The name is not a file name or a Lean identifier: how a function is exported is the exporter's
  * business.
  */
final case class FunctionRef[A, R](name: String) {

    /** The last segment of the name, for reports: `clamp`. */
    def displayName: String = name.substring(name.lastIndexOf('.') + 1)
}

object FunctionRef {

    /** Names a method of a `@Compile` object without compiling it to a UPLC program. */
    inline def apply[A, R](inline f: A => R): FunctionRef[A, R] =
        FunctionRef[A, R](FunctionMacro.qualifiedName(f))

    inline def apply[A, B, R](inline f: (A, B) => R): FunctionRef[(A, B), R] =
        FunctionRef[(A, B), R](FunctionMacro.qualifiedName(f))

    inline def apply[A, B, C, R](inline f: (A, B, C) => R): FunctionRef[(A, B, C), R] =
        FunctionRef[(A, B, C), R](FunctionMacro.qualifiedName(f))
}

/** One way of seeing a function, needed by some proof method. Keys compare by identity. */
final class Representation[T] private (val name: String) {
    override def toString: String = name
}

object Representation {

    /** The compiled UPLC program, which a `blaster-uplc` proof is about. */
    val Uplc: Representation[Program] = new Representation("uplc")

    /** The function's SIR, which `lean-direct` translates into a Lean definition. */
    val Sir: Representation[SIR] = new Representation("sir")

    /** How the [[Uplc]] program takes its parameters and returns its result. */
    val UplcSignature: Representation[UplcSignature] = new Representation("uplc-signature")

    /** A Lean term the function is declared equal to, for `lean-direct`. It is a claim until it is
      * proved, typically by `blaster-uplc` (design doc §6.3).
      */
    val LeanMapping: Representation[String] = new Representation("lean-mapping")

    /** A representation for a proof method defined elsewhere. */
    def custom[T](name: String): Representation[T] = new Representation(name)
}

/** How a compiled program takes its parameters and returns its result.
  *
  * The V3 lowering passes every value across a function's boundary in its type's default
  * representation: a `BigInt` as an integer constant, a plain case class as a builtin list of
  * `Data` fields, a sum type as `Data`, a type marked `@UplcRepr(UplcConstr)` as `constr` terms.
  * The default depends on the type, its annotations and the target, so it is computed here with the
  * same call the lowering makes. A program can be applied to another only when both agree on it.
  */
enum UplcSignature {

    /** The V3 lowering's representation of each parameter, in order, and of the result. */
    case Represented(
        parameters: List[LoweredValueRepresentation],
        result: LoweredValueRepresentation
    )

    /** Another lowering backend, whose calling convention is its own. */
    case Backend(backend: TargetLoweringBackend)

    /** Whether two signatures are the same calling convention. Representations compare by
      * [[LoweredValueRepresentation.stableKey]], which ignores the identity of type references.
      */
    def agrees(other: UplcSignature): Boolean = (this, other) match
        case (Represented(parameters, result), Represented(otherParameters, otherResult)) =>
            parameters.map(_.stableKey) == otherParameters.map(_.stableKey) &&
            result.stableKey == otherResult.stableKey
        case _ => this == other

    def show: String = this match
        case Represented(parameters, result) =>
            (parameters.map(_.show) :+ result.show).mkString(" -> ")
        case Backend(backend) => s"the $backend backend"
}

object UplcSignature {

    /** The signature of a function of type `tp` and `arity` parameters, lowered with `options` for
      * `language`, which is the program's own: `PlutusV3.compile` lowers for Plutus V3.
      */
    def of(tp: SIRType, arity: Int, options: Options, language: Language): UplcSignature =
        if options.targetLoweringBackend != TargetLoweringBackend.SirToUplcV3Lowering then
            Backend(options.targetLoweringBackend)
        else
            // The target settings of SirToUplcV3Lowering.newLoweringContext, without the support
            // bindings it lowers, which default representations do not read.
            val protocolVersion =
                if language == Language.PlutusV4 then Language.PlutusV4.introducedInVersion
                else options.targetProtocolVersion
            given LoweringContext =
                LoweringContext(targetLanguage = language, targetProtocolVersion = protocolVersion)
            val (parameters, result) = split(SirTypeUplcGenerator.defaultRepresentation(tp), arity)
            Represented(parameters, result)

    private def split(
        representation: LoweredValueRepresentation,
        arity: Int
    ): (List[LoweredValueRepresentation], LoweredValueRepresentation) =
        if arity == 0 then Nil -> representation
        else
            representation match
                case LambdaRepresentation(_, InOutRepresentationPair(parameter, rest)) =>
                    val (parameters, result) = split(rest, arity - 1)
                    (parameter :: parameters) -> result
                case other =>
                    throw new IllegalArgumentException(
                      s"a function of $arity more parameters has the representation ${other.show}"
                    )
}

/** One entry of a [[FunctionTable]]: a named function and the representations it has, one per proof
  * method that can use it.
  *
  * `arity` is the number of parameters the function takes. A function of several parameters takes
  * them in a call as one tuple of `arity` values, while its compiled programs are curried: they
  * take the values one at a time. A function of one parameter whose type is a tuple takes the whole
  * tuple. Representations are computed on first use, so an entry costs nothing for a method nobody
  * runs.
  */
final class FunctionDef[A, R] private (
    val ref: FunctionRef[A, R],
    val arity: Int,
    representations: Map[Representation[?], FunctionDef.Lazy]
) {
    require(arity > 0, s"a function takes at least one parameter: ${ref.name}")

    def name: String = ref.name

    def get[T](representation: Representation[T]): Option[T] =
        representations.get(representation).map(_.value.asInstanceOf[T])

    /** The representation a proof method needs. Its absence means the method cannot handle this
      * function, which is an error in the setup, so it throws.
      */
    def apply[T](representation: Representation[T]): T =
        get(representation).getOrElse(
          throw new NoSuchElementException(s"function $name has no $representation representation")
        )

    def has(representation: Representation[?]): Boolean = representations.contains(representation)

    /** The names of the representations this entry has. */
    def available: Set[String] = representations.keySet.map(_.name)

    def withRepresentation[T](representation: Representation[T], value: => T): FunctionDef[A, R] =
        new FunctionDef(
          ref,
          arity,
          representations.updated(representation, FunctionDef.Lazy(value))
        )

    def withLeanMapping(term: String): FunctionDef[A, R] =
        withRepresentation(Representation.LeanMapping, term)

    override def toString: String = s"$name [${available.toList.sorted.mkString(", ")}]"
}

object FunctionDef {

    /** A representation computed on first use, then kept. */
    final class Lazy private (compute: () => Any) {
        lazy val value: Any = compute()
    }

    object Lazy {
        def apply(value: => Any): Lazy = new Lazy(() => value)
    }

    /** An entry of one parameter with no representations yet, under a qualified name. Prefer the
      * overloads that take a method reference, which cannot get the name wrong.
      */
    def qualified[A, R](name: String): FunctionDef[A, R] = qualified(name, 1)

    /** An entry of `arity` parameters with no representations yet, under a qualified name. */
    def qualified[A, R](name: String, arity: Int): FunctionDef[A, R] = {
        require(name.contains('.'), s"a qualified name contains a dot: $name")
        new FunctionDef(FunctionRef(name), arity, Map.empty)
    }

    /** An entry of one parameter with no representations yet, for a function without a SIR
      * definition of its own.
      */
    def synthetic[A, R](name: String): FunctionDef[A, R] = synthetic(name, 1)

    /** An entry of `arity` parameters with no representations yet, for a function without a SIR
      * definition of its own.
      */
    def synthetic[A, R](name: String, arity: Int): FunctionDef[A, R] = {
        require(
          name.nonEmpty && !name.contains('.'),
          s"a synthetic name is non-empty and has no dot, so it cannot collide with a qualified one: $name"
        )
        new FunctionDef(FunctionRef(name), arity, Map.empty)
    }

    /** Adds what compiling a function gives: its SIR, its UPLC program and that program's
      * signature.
      */
    def fromCompiled[A, R](
        entry: FunctionDef[A, R],
        compiled: PlutusV3[?]
    ): FunctionDef[A, R] =
        entry
            .withRepresentation(Representation.Sir, compiled.sir)
            .withRepresentation(Representation.Uplc, compiled.program)
            .withRepresentation(
              Representation.UplcSignature,
              UplcSignature.of(compiled.sir.tp, entry.arity, compiled.options, compiled.language)
            )

    /** A one-parameter `@Compile` method, named after it and compiled with the module's pinned
      * options ([[UplcBlaster.options]]): `FunctionDef(Helpers.double)`.
      */
    inline def apply[A, R](inline f: A => R): FunctionDef[A, R] =
        fromCompiled(
          qualified[A, R](FunctionMacro.qualifiedName(f)),
          PlutusV3.compile(f)(using UplcBlaster.options)
        )

    /** A two-parameter `@Compile` method, taking its arguments as a pair. */
    inline def apply[A, B, R](inline f: (A, B) => R): FunctionDef[(A, B), R] =
        fromCompiled(
          qualified[(A, B), R](FunctionMacro.qualifiedName(f), 2),
          PlutusV3.compile(f)(using UplcBlaster.options)
        )

    /** A three-parameter `@Compile` method, taking its arguments as a triple:
      * `FunctionDef(Math.clamp)`.
      */
    inline def apply[A, B, C, R](inline f: (A, B, C) => R): FunctionDef[(A, B, C), R] =
        fromCompiled(
          qualified[(A, B, C), R](FunctionMacro.qualifiedName(f), 3),
          PlutusV3.compile(f)(using UplcBlaster.options)
        )

    /** Any one-parameter function under a synthetic name, compiled with the pinned options:
      * `FunctionDef.named("div10", (x: BigInt) => BigInt(10) / x)`.
      */
    inline def named[A, R](name: String, inline f: A => R): FunctionDef[A, R] =
        fromCompiled(
          synthetic[A, R](name),
          PlutusV3.compile(f)(using UplcBlaster.options)
        )

    /** Any two-parameter function under a synthetic name, taking its arguments as a pair. */
    inline def named[A, B, R](name: String, inline f: (A, B) => R): FunctionDef[(A, B), R] =
        fromCompiled(
          synthetic[(A, B), R](name, 2),
          PlutusV3.compile(f)(using UplcBlaster.options)
        )

    /** Any three-parameter function under a synthetic name, taking its arguments as a triple. */
    inline def named[A, B, C, R](
        name: String,
        inline f: (A, B, C) => R
    ): FunctionDef[(A, B, C), R] =
        fromCompiled(
          synthetic[(A, B, C), R](name, 3),
          PlutusV3.compile(f)(using UplcBlaster.options)
        )
}

/** The functions a set of statements calls, by name. */
final class FunctionTable private (entries: Map[String, FunctionDef[?, ?]]) {

    /** The function `ref` names. A missing one is an error in how the statements were set up, not a
      * property of the code, so it throws rather than returning an `Option`.
      */
    def apply[A, R](ref: FunctionRef[A, R]): FunctionDef[A, R] =
        entries.get(ref.name) match
            case Some(definition) => definition.asInstanceOf[FunctionDef[A, R]]
            case None =>
                throw new NoSuchElementException(s"no function named ${ref.name} in the table")

    def contains(name: String): Boolean = entries.contains(name)

    def definitions: Iterable[FunctionDef[?, ?]] = entries.values

    /** Adds `definition`. Two different entries under one name are rejected, because a call would
      * then be ambiguous. Extend an entry with `withRepresentation` before adding it instead.
      */
    def +(definition: FunctionDef[?, ?]): FunctionTable =
        entries.get(definition.name) match
            case Some(existing) if existing ne definition =>
                throw new IllegalArgumentException(
                  s"two different functions are named ${definition.name}"
                )
            case _ => new FunctionTable(entries.updated(definition.name, definition))
}

object FunctionTable {
    val empty: FunctionTable = new FunctionTable(Map.empty)

    def apply(definitions: FunctionDef[?, ?]*): FunctionTable = definitions.foldLeft(empty)(_ + _)
}
