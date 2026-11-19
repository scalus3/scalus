package scalus.verify.uplcblaster

import scalus.compiler.sir.SIRType
import scalus.verify.SirTypes
import scalus.verify.lean.LeanTypes

/** How [[UplcBlaster]] prefers to pass a quantified variable of a case class or an enum to Lean:
  * one [[VariableMapping.Choice]] for every type, and others for types by name
  * (docs/design/verification-details/uplc-blaster.md, "Mapping a variable").
  *
  * {{{
  * VariableMapping.auto                                   // by the statement's form: the default
  * VariableMapping.rules.preferLeanTypes                  // a Lean type wherever one exists
  * VariableMapping.rules                                  // the tactic's rules, for every type
  * VariableMapping.rules.forType[ScriptContext](VariableMapping.Choice.LeanType)
  * }}}
  *
  * The choice of a type is asked at each node of a variable that is of it, from the root down: a
  * case class or an enum not inside a list or a map. A choice that cannot be taken there falls back
  * to the rules: `Data` where the type has no `Data` form, a Lean type where none exists, and
  * either where the variable asks for a witness, which a variable with more values than its type
  * cannot stand for. A node where a type contains itself and repeats is `Data` whatever is chosen.
  *
  * A statement over a variable of a Lean type ([[scalus.verify.lean.LeanTypeBinding]]) is of the
  * values of that type. With `isValid` of it as a premise, that is the statement over the Scala
  * type's `Data` exactly: `isValid` holds of a `Data` only where it is the encoding of a valid
  * value of the Lean type. Without it, it is the statement over the Scala type where the Lean type
  * has a counterpart of every value of the Scala type, which is checked of samples, not proved:
  * choosing a Lean type is choosing that assumption. A refutation is of a value of the Scala type
  * whatever the mapping: the replay repairs Lean's value where it is none, and checks it.
  *
  * [[VariableMapping.Choice.Auto]] chooses by the statement's form, so that it assumes nothing: a
  * Lean type where it is exact or where the mapping cannot matter, and the rules elsewhere.
  *
  * The mapping shows in a check's text, and so in what its result rests on: a result kept under one
  * mapping is stale under another.
  */
final case class VariableMapping(
    default: VariableMapping.Choice,
    byType: Map[String, VariableMapping.Choice]
) {

    /** A Lean type for every type that has one ([[scalus.verify.lean.LeanTypeBinding.known]]),
      * where it may be one, and the rules elsewhere.
      */
    def preferLeanTypes: VariableMapping = copy(default = VariableMapping.Choice.LeanType)

    /** This mapping, with `choice` for the type of the declaration `name`. */
    def forTypeNamed(name: String, choice: VariableMapping.Choice): VariableMapping =
        copy(byType = byType.updated(name, choice))

    /** This mapping, with `choice` for the type `A`. */
    inline def forType[A](choice: VariableMapping.Choice): VariableMapping =
        forTypeNamed(SirTypes.declarationName(SirTypes.unwrap(LeanTypes.typeOf[A])), choice)

    /** The choice for a node of type `tp`. */
    def choiceFor(tp: SIRType): VariableMapping.Choice =
        byType.getOrElse(SirTypes.declarationName(SirTypes.unwrap(tp)), default)
}

object VariableMapping {

    /** What is preferred for a node. */
    enum Choice {

        /** The tactic's rules: by fields, but `Data` where by fields the node would be more than
          * [[MappingPlan.mostByFields]] Lean variables, and where its type repeats.
          */
        case Rules

        /** By fields, however many Lean variables that is. */
        case ByFields

        /** As one `Data`, restricted to the encodings of the type's values. */
        case AsData

        /** As a variable of the Lean type that exists for the type
          * ([[scalus.verify.lean.LeanTypeBinding]]), whose `Data` the programs are given.
          */
        case LeanType

        /** By the statement's form: a variable itself, of a type a Lean type exists for, is of that
          * type where its statement assumes it valid, with `isValid` of it as a premise, of which
          * the statement over the Lean type is exactly the one over its `Data`; and where the
          * statement is a negative control, declared with `refute`, whose refutation is of a value
          * of the Scala type whatever the mapping. Elsewhere the rules map it, and a part of it
          * below its root. Measured on the examples (2026-11-12), a Lean type is faster there, up
          * to four times, and slower than `Data` for some statements of contexts with limits.
          */
        case Auto
    }

    /** The tactic's rules, for every type. */
    val rules: VariableMapping = VariableMapping(Choice.Rules, Map.empty)

    /** [[Choice.Auto]], for every type. */
    val auto: VariableMapping = VariableMapping(Choice.Auto, Map.empty)

    /** What a tactic does where nothing is said: [[auto]]. */
    val default: VariableMapping = auto
}
