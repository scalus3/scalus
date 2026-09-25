package scalus.verify

import scalus.compiler.sir.SIR
import scalus.uplc.{PlutusV3, Program}

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

/** One way of seeing a function, needed by some proof method. Keys compare by identity. */
final class Representation[T] private (val name: String) {
    override def toString: String = name
}

object Representation {

    /** The function as ordinary Scala, which the `scalacheck` method ([[Prop.check]]) calls.
      * [[FunctionDef.scalacheck]] gives it its type.
      */
    val Scalacheck: Representation[Any => Any] = new Representation("scalacheck")

    /** The compiled UPLC program, which a `blaster-uplc` proof is about. */
    val Uplc: Representation[Program] = new Representation("uplc")

    /** The function's SIR, which `lean-direct` translates into a Lean definition. */
    val Sir: Representation[SIR] = new Representation("sir")

    /** A Lean term the function is declared equal to, for `lean-direct`. It is a claim until it is
      * proved, typically by `blaster-uplc` (design doc §6.3).
      */
    val LeanMapping: Representation[String] = new Representation("lean-mapping")

    /** A representation for a proof method defined elsewhere. */
    def custom[T](name: String): Representation[T] = new Representation(name)
}

/** One entry of a [[FunctionTable]]: a named function and the representations it has, one per proof
  * method that can use it.
  *
  * A function of several parameters takes them as one tuple. Representations are computed on first
  * use, so an entry costs nothing for a method nobody runs.
  */
final class FunctionDef[A, R] private (
    val ref: FunctionRef[A, R],
    representations: Map[Representation[?], FunctionDef.Lazy]
) {
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

    def scalacheck: Option[A => R] = get(Representation.Scalacheck).map(_.asInstanceOf[A => R])

    def withRepresentation[T](representation: Representation[T], value: => T): FunctionDef[A, R] =
        new FunctionDef(ref, representations.updated(representation, FunctionDef.Lazy(value)))

    def withScalacheck(f: A => R): FunctionDef[A, R] =
        withRepresentation(Representation.Scalacheck, f.asInstanceOf[Any => Any])

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

    /** An entry with no representations yet, under a qualified name. Prefer the overloads that take
      * a method reference, which cannot get the name wrong.
      */
    def qualified[A, R](name: String): FunctionDef[A, R] = {
        require(name.contains('.'), s"a qualified name contains a dot: $name")
        new FunctionDef(FunctionRef(name), Map.empty)
    }

    /** An entry with no representations yet, for a function without a SIR definition of its own. */
    def synthetic[A, R](name: String): FunctionDef[A, R] = {
        require(
          name.nonEmpty && !name.contains('.'),
          s"a synthetic name is non-empty and has no dot, so it cannot collide with a qualified one: $name"
        )
        new FunctionDef(FunctionRef(name), Map.empty)
    }

    /** Adds what compiling a function gives: its SIR, its UPLC program and its Scala form. */
    def fromCompiled[A, R](
        entry: FunctionDef[A, R],
        compiled: PlutusV3[?],
        scalacheck: A => R
    ): FunctionDef[A, R] =
        entry
            .withRepresentation(Representation.Sir, compiled.sir)
            .withRepresentation(Representation.Uplc, compiled.program)
            .withScalacheck(scalacheck)

    /** A one-parameter `@Compile` method, named after it and compiled with the module's pinned
      * options ([[ProofTargets.options]]): `FunctionDef(Helpers.double)`.
      */
    inline def apply[A, R](inline f: A => R): FunctionDef[A, R] =
        fromCompiled(
          qualified[A, R](FunctionMacro.qualifiedName(f)),
          PlutusV3.compile(f)(using ProofTargets.options),
          a => f(a)
        )

    /** A two-parameter `@Compile` method, taking its arguments as a pair. */
    inline def apply[A, B, R](inline f: (A, B) => R): FunctionDef[(A, B), R] =
        fromCompiled(
          qualified[(A, B), R](FunctionMacro.qualifiedName(f)),
          PlutusV3.compile(f)(using ProofTargets.options),
          args => f(args._1, args._2)
        )

    /** A three-parameter `@Compile` method, taking its arguments as a triple:
      * `FunctionDef(Math.clamp)`.
      */
    inline def apply[A, B, C, R](inline f: (A, B, C) => R): FunctionDef[(A, B, C), R] =
        fromCompiled(
          qualified[(A, B, C), R](FunctionMacro.qualifiedName(f)),
          PlutusV3.compile(f)(using ProofTargets.options),
          args => f(args._1, args._2, args._3)
        )

    /** Any one-parameter function under a synthetic name, compiled with the pinned options:
      * `FunctionDef.named("div10", (x: BigInt) => BigInt(10) / x)`.
      */
    inline def named[A, R](name: String, inline f: A => R): FunctionDef[A, R] =
        fromCompiled(
          synthetic[A, R](name),
          PlutusV3.compile(f)(using ProofTargets.options),
          a => f(a)
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
