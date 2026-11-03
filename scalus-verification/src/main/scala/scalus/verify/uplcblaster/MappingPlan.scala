package scalus.verify.uplcblaster

import scalus.compiler.sir.SIRType
import scalus.verify.PropIdentity

/** How the variables of a statement reach Lean: for each decided node of each variable, whether it
  * is mapped by fields or as `Data` (docs/design/verification-details/uplc-blaster.md, "Mapping a
  * variable").
  *
  * A node is a part of a variable's value, named by its path of fields and constructors from the
  * variable, as the Lean variables are named: `out.address.credential`. A node is decided where it
  * is a case class or an enum that is not inside a list or a map. A leaf, a list and a map are one
  * Lean variable each, and are not decided. The root of a variable mapped as `Data` is a node
  * whatever its type, as that of a `forAllData` variable of a list.
  *
  * [[UplcBlaster.lower]] decides each node as it maps the variable, before anything below the node
  * is mapped, and records the decision: so the plan is what the Lean variables are. A decision
  * reads only the statement and the types. Today it is the one the statement writes: every node by
  * fields for a `forAll` or an `exists` variable, and the variable as `Data` for a `forAllData`
  * one. A plan [[MappingPlan.Fixed]] for a check maps the nodes it names as it says instead.
  *
  * @param nodes
  *   the decided nodes of the statement's variables, in the order the variables are quantified, and
  *   each variable's from its root down
  */
final case class MappingPlan(nodes: List[MappingPlan.Node]) {

    /** The plan, a node on a line, with the variables named as the statement writes them: a name
      * that two variables have is marked with `#` and the variable's place, counted from 1.
      */
    def show: String = {
        // A variable's name, as written, is what its nodes' paths start with.
        val names = nodes
            .map(node => node.variable -> PropIdentity.written(node.path).takeWhile(_ != '.'))
            .toMap
        val repeated =
            names.values
                .groupBy(identity)
                .collect { case (name, all) if all.size > 1 => name }
                .toSet
        nodes
            .map { node =>
                val name = names(node.variable)
                val path = PropIdentity.written(node.path)
                if repeated(name) then
                    node.show(s"$name#${node.variable + 1}${path.drop(name.length)}")
                else node.show(path)
            }
            .mkString("\n")
    }
}

object MappingPlan {

    /** How a decided node reaches Lean. */
    enum Mapping {

        /** The node is its children, each decided in turn: a case class its fields, and an enum an
          * integer that chooses the constructor and the fields of every constructor.
          */
        case ByFields

        /** The node and all below it are one Lean `Data` variable, restricted to the encodings of
          * the values of the node's type, and decoded in every test that uses it.
          */
        case AsData
    }

    /** The quantifier that introduces a variable. */
    enum Quantifier {
        case ForAll, Exists
    }

    /** Why a node is mapped as it is. */
    enum Reason {

        /** The statement quantifies over the variable with `quantifier`, which maps every case
          * class and enum by fields.
          */
        case Quantified(quantifier: Quantifier)

        /** The statement quantifies over the `Data` encodings of the variable's type with
          * `quantifier`: it writes `forAllData`, or the parameter
          * `A @UplcRepr(UplcRepresentation.Data)`.
          */
        case OverEncodings(quantifier: Quantifier)

        /** As [[OverEncodings]], with limits on the lengths of its collections
          * (`forAllDataLimited`), whose guards walk the variable's `Data`.
          */
        case Limited

        /** The plan the check was given maps the node so ([[MappingPlan.Fixed]]). */
        case ByFixedPlan

        /** The reason in words. */
        def describe: String = this match
            case Quantified(quantifier) =>
                s"${word(quantifier)}, which maps every case class and enum by fields"
            case OverEncodings(quantifier) =>
                s"${word(quantifier)} over the Data encodings of its type, which maps it as Data"
            case Limited     => "forAllDataLimited, whose guards walk the variable's Data"
            case ByFixedPlan => "fixed by the plan the check was given"
    }

    /** Mappings fixed for a check in place of the tactic's own decisions: for the tactic's tests,
      * and for measuring its rules on the suites (docs/design/verification-details/uplc-blaster.md,
      * "Mapping a variable", step 4). A statement has no way to choose a mapping, and a user none
      * to fix one.
      *
      * It is asked about each decided node of a `forAll` or an `exists` variable, from the root
      * down, before the node is expanded: a node it maps as `Data` is not expanded, and nothing
      * below it is asked about. A variable over `Data` encodings is mapped before it is expanded,
      * and is not asked about. The mapping it fixes is the node's in the check's text, and so in
      * its fingerprint.
      *
      * @param name
      *   what the plan is, in the tactic's name ([[UplcBlaster.name]]): the results kept for one
      *   statement under two plans are two, and each stays where the other is checked
      * @param paths
      *   the nodes it names, each written as the statement writes it: lowering fails where one is
      *   no decided node of the statement's variables, so that a check that misses it does not run
      *   as if it had not
      * @param decide
      *   the mapping of the node at a path written as the statement writes it,
      *   `out.address.stakingCredential`, of a type, a case class or an enum, in the variable of a
      *   place ([[Node.variable]]); none for the tactic's own
      */
    private[verify] final class Fixed(
        val name: String,
        val paths: Set[String],
        decide: (Int, String, SIRType) => Option[Mapping]
    ) {

        /** The mapping of the node at `path`, of type `tp`, in the `variable`-th variable, or none
          * for the tactic's own.
          */
        def mapping(variable: Int, path: String, tp: SIRType): Option[Mapping] =
            decide(variable, path, tp)
    }

    private[verify] object Fixed {

        /** No mapping fixed: every node is the tactic's to decide. */
        val none: Fixed = new Fixed("", Set.empty, (_, _, _) => None)

        /** The nodes at `paths`, in any variable, as `Data`, and the others as the tactic decides.
          */
        def asData(paths: String*): Fixed = {
            val fixed = paths.toSet
            new Fixed(
              s"${fixed.toList.sorted.mkString(", ")} as Data",
              fixed,
              (_, path, _) => Option.when(fixed.contains(path))(Mapping.AsData)
            )
        }
    }

    /** A quantifier as a statement writes it. */
    private def word(quantifier: Quantifier): String = quantifier match
        case Quantifier.ForAll => "forAll"
        case Quantifier.Exists => "exists"

    /** A decided node: the statement variable it is part of; its path; its type; its mapping; and
      * why it is mapped so.
      *
      * @param variable
      *   the variable's place among those that [[UplcBlaster.lower]] quantifies over, counted from
      *   0 in the order it meets them, since two variables can have one name. The variable of an
      *   `existsLet`, which is replaced by its witness, and a call's result, which a test binds,
      *   are none of them.
      */
    final case class Node(
        variable: Int,
        path: String,
        tp: SIRType,
        mapping: Mapping,
        reason: Reason
    ) {

        /** The node on a line, with its variable named as the statement writes it. */
        def show: String = show(PropIdentity.written(path))

        /** The node on a line, with its path shown as `shown`. */
        private[uplcblaster] def show(shown: String): String =
            s"$shown: ${tp.show}, $mapping, ${reason.describe}"
    }

    /** The plan of a statement without variables of case classes or enums. */
    val empty: MappingPlan = MappingPlan(Nil)
}
