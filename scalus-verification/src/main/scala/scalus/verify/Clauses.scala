package scalus.verify

import scalus.compiler.sir.{AnnotatedSIR, SIR, SIRPosition}

/** A clause of a function's specification, where its code states it: `Spec.ensures(c)`,
  * `ensuring(r => c)` or `Spec.expects(c)` ([[scalus.cardano.onchain.plutus.prelude.Spec]]), at
  * `line` of `file`. A clause of an inline function, as a validator's handler, is in the code of
  * every function that calls it, and is one clause, of its file and line.
  */
final case class Clause(file: String, line: Int, kind: Clause.Kind) {

    /** The clause as a person finds it: its file's name, its line, and its kind. */
    def where: String = {
        val name = file.substring(file.lastIndexOf('/') + 1)
        s"${kind.written} at $name:$line"
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

    /** The clause at the node `node`, a call of the clause's function, of the kind `kind`.
      *
      * It is where the code states its condition, the call's last argument, or the body of that
      * argument where it is a lambda. `Spec.ensures` is inline: its call is that of
      * `Spec.holdsOnReturn(_ => condition)`, at the definition of `Spec.ensures`, and only the
      * condition is in the code that states it. The call itself is the place of a condition that
      * has none.
      */
    private[verify] def at(node: SIR, kind: Kind): Clause = {
        val stated = positionOf(condition(node))
        val position = if stated.file.isEmpty then positionOf(node) else stated
        if position.file.isEmpty then Clause("", 0, kind)
        else Clause(position.file, position.startLine + 1, kind)
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
      * statement proves a clause where it is the statement the verifier makes of the clause, or
      * where the tactic finds it the same statement as one of those: the same proposition, about
      * the same programs.
      */
    case Checked(statements: List[String], tactic: String, kinds: Set[ProofKind])

    /** Declared not proved, by [[Verifier.open]] of the statement `statement`, with the reason. */
    case Open(statement: String, reason: String)

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
            case ClauseStatus.Checked(statements, tactic, kinds) =>
                s"$of: checked by ${statements.mkString(", ")} ($tactic, ${kinds.mkString(", ")})"
            case ClauseStatus.Open(statement, reason) =>
                s"$of: open, $statement: $reason"
            case ClauseStatus.NotOwed => s"$of: no function here calls it"
            case ClauseStatus.Unchecked(candidates, missing) =>
                val by =
                    if candidates.isEmpty then ""
                    else s" (stated by ${candidates.mkString(", ")})"
                s"$of: not checked$by: $missing"
    }
}

/** What a tactic says a statement is, to tell that two statements are one: the proposition and the
  * programs of its fingerprint ([[Tactic.fingerprint]]), without the backend's side.
  */
private[verify] final case class StatementIdentity(
    tactic: String,
    statement: String,
    programs: List[String]
)
