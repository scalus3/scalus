package scalus.verify

/** A named proposition declared in a [[Verifier]], and where it comes from. */
final class Statement private[verify] (val name: String, val prop: Prop, val origin: Origin) {
    override def toString: String = s"Statement($name)"
}

/** Where a statement comes from. */
enum Origin {

    /** Declared with [[Verifier.statement]]. */
    case Explicit

    /** The contract of `function`, declared with [[Verifier.contract]]. A total one claims that the
      * function returns where its precondition holds.
      */
    case Contract(function: FunctionRef[?, ?], total: Boolean)
}

/** Which proof mechanism checked an artifact. */
enum ProofKind {

    /** Closed by Blaster through Z3, which Lean admits with the axiom `blasterProven`. */
    case Blaster

    /** Checked by the Lean kernel, with no axiom beyond Lean's own. */
    case LeanKernel

    /** Decided by evaluation with `native_decide`, which trusts the Lean compiler through the axiom
      * `Lean.ofReduceBool`.
      */
    case LeanNative
}

/** Backend-specific proof material retained with a theorem.
  *
  * A concrete tactic supplies an implementation containing its actual proof term, certificate, or
  * check record. The kind describes how that material was checked; it is not the material itself.
  */
trait ProofArtifact {
    def kind: ProofKind
}

/** Evidence supplied by a proof tactic, including its artifact and the lemmas it used. */
final class Proof private[verify] (
    val artifact: ProofArtifact,
    val usedLemmas: List[Theorem]
) {
    def kinds: Set[ProofKind] = Set(artifact.kind) ++ usedLemmas.flatMap(_.proof.kinds)
}

object Proof {
    private[verify] def apply(artifact: ProofArtifact): Proof = new Proof(artifact, Nil)

    private[verify] def apply(
        artifact: ProofArtifact,
        usedLemmas: List[Theorem]
    ): Proof = new Proof(artifact, usedLemmas)
}

/** A statement paired with a proof. It can be supplied as a lemma for later goals. */
final class Theorem private[verify] (val statement: Statement, val proof: Proof)

/** The context passed to a tactic. Lemmas are the theorems currently available in the verifier. */
final case class Goal(
    statement: Statement,
    functions: FunctionTable,
    lemmas: List[Theorem]
)

trait Tactic {
    def name: String
    def discharge(goal: Goal): VerificationResult
}

/** The result of a tactic or verifier run. The verifier registers a theorem for a proven statement.
  */
enum VerificationResult {
    case Proven(proof: Proof)
    case Refuted(proof: Proof)
    case Inconclusive(reason: String)
}

/** A runtime context for functions, named statements, and proved lemmas.
  *
  * Proof and refutation artifacts carry backend-specific data, including the checked target
  * identity when needed. Tactics decide whether artifacts and imported lemmas are compatible. The
  * verifier checks registration and explicit function calls.
  */
final class Verifier private () {
    private var functionTable: FunctionTable = FunctionTable.empty
    private var declarations: Map[String, Statement] = Map.empty
    private var proven: Map[String, Theorem] = Map.empty
    private var generatedStatementNumber: Long = 0L

    def functions: FunctionTable = functionTable
    def statements: Iterable[Statement] = declarations.values
    def theorems: Iterable[Theorem] = proven.values

    def addFunction(definition: FunctionDef[?, ?]): Unit =
        functionTable = functionTable + definition

    /** Adds all entries together; a conflicting name leaves the old table intact. */
    def addFunctions(table: FunctionTable): Unit = {
        val updated = table.definitions.foldLeft(functionTable)(_ + _)
        functionTable = updated
    }

    def statement(name: String, prop: Prop): Statement = declare(name, prop, Origin.Explicit)

    /** Declares the contract of a function, built by [[Props.contract]] or [[Props.totalContract]].
      * It is proved like any statement, and [[contracts]] finds it by its function.
      */
    def contract(name: String, contract: Contract): Statement =
        declare(name, contract.prop, Origin.Contract(contract.function, contract.total))

    /** The contracts declared for `function`, by name. */
    def contracts(function: FunctionRef[?, ?]): List[Statement] =
        declarations.values
            .filter(_.origin match
                case Origin.Contract(of, _) => of == function
                case Origin.Explicit        => false)
            .toList
            .sortBy(_.name)

    private def declare(name: String, prop: Prop, origin: Origin): Statement = {
        require(name.trim.nonEmpty, "a statement name must be non-empty")
        require(!declarations.contains(name), s"a statement named $name is already declared")
        require(!proven.contains(name), s"a theorem named $name is already available")
        val declared = new Statement(name, prop, origin)
        declarations = declarations.updated(name, declared)
        declared
    }

    /** Registers a statement under a generated name local to this verifier. */
    def statement(prop: Prop): Statement = {
        var next = generatedStatementNumber + 1L
        var name = s"statement_$next"
        while declarations.contains(name) || proven.contains(name) do
            next += 1L
            name = s"statement_$next"
        generatedStatementNumber = next
        statement(name, prop)
    }

    /** Imports a theorem proved in another context for use as a lemma. */
    def addTheorem(theorem: Theorem): Unit = {
        require(
          declarations.get(theorem.statement.name).forall(_ eq theorem.statement),
          s"a different statement named ${theorem.statement.name} is already declared"
        )
        proven.get(theorem.statement.name) match
            case Some(existing) if existing.statement ne theorem.statement =>
                throw new IllegalArgumentException(
                  s"a different theorem named ${theorem.statement.name} is already available"
                )
            case _ => proven = proven.updated(theorem.statement.name, theorem)
    }

    /** Runs the tactic and registers a successful theorem for later goals. */
    def prove(statement: Statement, tactic: Tactic): VerificationResult = {
        require(
          declarations.get(statement.name).exists(_ eq statement),
          s"statement ${statement.name} is not registered in this verifier"
        )
        tactic.discharge(
          Goal(statement, functionTable, proven.values.toList.sortBy(_.statement.name))
        ) match
            case VerificationResult.Proven(proof) =>
                proof.usedLemmas.foreach { lemma =>
                    require(
                      proven.get(lemma.statement.name).exists(_ eq lemma),
                      s"tactic ${tactic.name} used an unavailable lemma ${lemma.statement.name}"
                    )
                }
                require(proof.artifact != null, s"tactic ${tactic.name} supplied no proof artifact")
                val theorem = new Theorem(statement, proof)
                proven = proven.updated(statement.name, theorem)
                VerificationResult.Proven(proof)
            case VerificationResult.Refuted(proof)       => VerificationResult.Refuted(proof)
            case VerificationResult.Inconclusive(reason) => VerificationResult.Inconclusive(reason)
    }

    /** The earlier name for [[prove]]. */
    def verify(statement: Statement, tactic: Tactic): VerificationResult = prove(statement, tactic)

}

object Verifier {
    def empty: Verifier = new Verifier()
}
