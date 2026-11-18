package scalus.verify

import scalus.compiler.sir.{AnnotatedSIR, SIR, SIRPosition}

/** A clause of a function's specification, where its code states it: `Spec.ensures(c)`,
  * `ensuring(r => c)` or `Spec.expects(c)` ([[scalus.cardano.onchain.plutus.prelude.Spec]]), at
  * `line` and `column` of `file`.
  *
  * A clause of an inline function, as a validator's handler, is in the code of every function that
  * calls it, and is one clause there, of its file, line and column. Where one function's code has
  * it more than once, as a handler inlined at two places, each is a clause of its own, numbered by
  * `occurrence` in the order of the code. A clause without a position in the source is placed in
  * the function whose code has it, `file` being `<function>`.
  */
final case class Clause(file: String, line: Int, column: Int, kind: Clause.Kind, occurrence: Int) {

    /** The clause as a person finds it: its kind, its file's name, and its line. */
    def where: String = {
        val name = file.substring(file.lastIndexOf('/') + 1)
        val again = if occurrence > 1 then s" (occurrence $occurrence)" else ""
        s"${kind.written} at $name:$line$again"
    }
}

object Clause {

    enum Kind(val written: String) {

        /** `Spec.ensures(c)`: where the function returns through it, `c` holds of the parameters.
          */
        case Ensures extends Kind("Spec.ensures")

        /** `e.ensuring(r => c)`: where `e` returns `r`, `c` holds of it. */
        case Ensuring extends Kind("ensuring")

        /** `Spec.expects(c)`: the function's callers establish `c`. */
        case Expects extends Kind("Spec.expects")
    }

    /** The clause at the node `node`, a call of the clause's function, of the kind `kind`, in the
      * code of the function `function`, the `occurrence`-th of that place there.
      *
      * It is where the code states its condition, the call's last argument, or the body of that
      * argument where it is a lambda. `Spec.ensures` is inline: its call is that of
      * `Spec.holdsOnReturn(_ => condition)`, at the definition of `Spec.ensures`, and only the
      * condition is in the code that states it. The call itself is the place of a condition that
      * has none.
      */
    private[verify] def at(node: SIR, kind: Kind, function: String, occurrence: Int): Clause = {
        val stated = positionOf(condition(node))
        val position = if stated.file.isEmpty then positionOf(node) else stated
        if position.file.isEmpty then Clause(s"<$function>", 0, 0, kind, occurrence)
        else Clause(position.file, position.startLine + 1, position.startColumn, kind, occurrence)
    }

    /** The condition of the call `node`: its last argument, or the body of that argument. */
    private def condition(node: SIR): SIR = node match
        case SIR.Apply(_, argument, _, _) => unwrapped(argument)
        case other                        => other

    private def unwrapped(sir: SIR): SIR = sir match
        case SIR.Cast(inner, _, _)     => unwrapped(inner)
        case SIR.LamAbs(_, body, _, _) => body
        case other                     => other

    private def positionOf(node: SIR): SIRPosition = node match
        case SIR.Decl(_, term)       => positionOf(term)
        case annotated: AnnotatedSIR => annotated.anns.pos

    /** The kind of the clause of a call of the function `name` in SIR, if it is one. */
    private[verify] def kindOf(name: String): Option[Kind] = name match
        case Obligations.Ensures  => Some(Kind.Ensures)
        case Obligations.Ensuring => Some(Kind.Ensuring)
        case Obligations.Expects  => Some(Kind.Expects)
        case _                    => None
}

/** How a clause of a function's specification stands in a verifier ([[Verifier.specifications]]).
  */
enum ClauseStatus {

    /** Proved by the statements `statements`, with the tactic `tactic`, by proofs of `kinds`. A
      * statement proves a clause where it is a statement the verifier makes of the clause, or where
      * the tactic finds it the same statement as one of those: the same proposition, about the same
      * programs. `within` says what the statements are of where it is less than every value: the
      * limits of their variables ([[Props.forAllDataLimited]]), one for each variable that has
      * them. Beyond them the clause is not checked.
      */
    case Checked(
        statements: List[String],
        tactic: String,
        kinds: Set[ProofKind],
        within: List[String]
    )

    /** Declared not proved, with the reason: by [[Verifier.open]] of the statement `by`, or of the
      * clause itself, where `by` is empty.
      */
    case Open(by: String, reason: String)

    /** A `Spec.expects` that no function of the verifier calls: no caller here owes it. */
    case NotOwed

    /** Neither proved nor declared open. `candidates` are the statements that would check it, where
      * the verifier has any; `missing` says what is to be declared or proved.
      */
    case Unchecked(candidates: List[String], missing: String)
}

/** A clause, the functions of the verifier whose code states it, and how it stands. */
final case class Specification(clause: Clause, functions: List[String], status: ClauseStatus) {

    /** One line about the clause: where it is, of what, and how it stands. */
    def describe: String = {
        val of = s"${clause.where}, in ${functions.mkString(", ")}"
        status match
            case ClauseStatus.Checked(statements, tactic, kinds, within) =>
                val limited =
                    if within.isEmpty then "" else s", only where ${within.mkString("; ")}"
                s"$of: checked by ${statements.mkString(", ")} ($tactic, " +
                    s"${kinds.mkString(", ")})$limited"
            case ClauseStatus.Open(by, reason) =>
                val declared = if by.isEmpty then "" else s", $by"
                s"$of: open$declared: $reason"
            case ClauseStatus.NotOwed => s"$of: no function here calls it"
            case ClauseStatus.Unchecked(candidates, missing) =>
                val stated =
                    if candidates.isEmpty then ""
                    else s" (stated by ${candidates.mkString(", ")})"
                s"$of: not checked$stated: $missing"
    }
}

/** What a tactic says a statement is, to tell that two statements are one: the proposition and the
  * programs, without the backend's side ([[Tactic.identity]]).
  */
private[verify] final case class StatementIdentity(
    tactic: String,
    statement: String,
    programs: List[String]
)
