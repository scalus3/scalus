package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.require
import scalus.cardano.onchain.plutus.prelude.Spec
import scalus.cardano.onchain.plutus.prelude.Spec.ensuring
import scalus.compiler.Compile
import scalus.compiler.sir.SIR
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.lean.LeanServer.{Message, Result, Severity}

import java.nio.file.{Files, Path}
import scala.concurrent.duration.*

/** A helper of a contract's own, and not of the library: the rewrite inlines it. */
@Compile
object CallsAsUnknownsHelpers {
    def size(xs: prelude.List[BigInt]): BigInt = xs.length

    /** A helper that does not loop: the rewrite leaves its call, and its key is that of its code.
      */
    def firstList(xss: prelude.List[prelude.List[BigInt]]): prelude.List[BigInt] = xss match
        case prelude.List.Cons(head, _) => head
        case prelude.List.Nil           => prelude.List.Nil

    /** A helper that does not loop and does not read its parameter. */
    def one(p: BigInt): BigInt = BigInt(1)

    /** A function value that does not read its parameter, which a program can bind to a name. */
    val constantOne: BigInt => BigInt = p => BigInt(1)

    /** The length of a list that begins with `constantOne` of a value that fails where `c` is not
      * positive, called by a name bound to it, as an inline instance's `self`. It loops and is no
      * library's, so the rewrite writes its code into the program, the name among it.
      */
    def lengthWithOne(c: BigInt, xs: prelude.List[BigInt]): BigInt = {
        val g = constantOne
        prelude.List.Cons({ val x = { require(c > BigInt(0)); BigInt(0) }; g(x) }, xs).length
    }
}

/** Functions whose code states a contract: the rewrite assumes it of the unknowns of their calls.
  */
@Compile
object CallsAsUnknownsContracts {

    /** A walk with a contract. */
    def count(xs: prelude.List[BigInt]): BigInt = xs.length.ensuring(n => n >= BigInt(0))

    /** A function whose code is a call of one with a contract: it has that contract. */
    def counted(xs: prelude.List[BigInt]): BigInt = count(xs)

    /** A function with a contract that does not loop, whose contract speaks of a walk. */
    def prepend(x: BigInt, xs: prelude.List[BigInt]): prelude.List[BigInt] =
        prelude.List.Cons(x, xs).ensuring(ys => ys.length == xs.length + BigInt(1))

    /** A contract that `UplcBlaster` proves without unknowns. */
    def twice(x: BigInt): BigInt = (x + x).ensuring(r => r == x * BigInt(2))

    /** A contract that nothing here proves. */
    def thrice(x: BigInt): BigInt = (x + x + x).ensuring(r => r == x * BigInt(3))

    /** A contract proved from that of `thrice`, which it calls. */
    def ninefold(x: BigInt): BigInt = thrice(thrice(x)).ensuring(r => r == x * BigInt(9))

    /** A precondition that reads the list's head: it fails on the empty list, where the function,
      * whose precondition is no part of its code, returns 0, and its guarantee is false.
      */
    def lengthOfSome(xs: prelude.List[BigInt]): BigInt = {
        Spec.expects(xs.head == xs.head)
        xs.length
    }.ensuring(n => n >= BigInt(1))

    /** A contract through two functions whose code calls the next: `outer` has `swapped`'s, which
      * has `difference`'s, of its arguments the other way round.
      */
    def difference(a: BigInt, b: BigInt): BigInt = (a - b).ensuring(r => r == a - b)

    def swapped(a: BigInt, b: BigInt): BigInt = difference(b, a)

    def outer(a: BigInt, b: BigInt): BigInt = swapped(a, b)

    private val notNegative: BigInt => Boolean = n => n >= BigInt(0)

    /** A postcondition that is no function literal: no contract the rewrite reads. */
    def countedBy(xs: prelude.List[BigInt]): BigInt = xs.length.ensuring(notNegative)
}

/** The tactic of calls as unknowns ([[CallsAsUnknowns]]): what it makes unknowns of, and that a
  * statement of every list, which Lean does not finish with the walk, it proves with the walk as an
  * unknown.
  */
class CallsAsUnknownsTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    private val longerThanNothing =
        forAll[prelude.List[BigInt]](xs => (xs.length > BigInt(0)) ==>: (xs.length >= BigInt(1)))

    private def unknownsOf(prop: Prop): List[String] =
        CallsAsUnknowns
            .rewrite(prop, CallsAsUnknowns.inLibrary)
            .fold(reason => fail(reason), _.unknowns.map(_.written))

    test("the premise's and the claim's walk of one list are one unknown") {
        assert(
          unknownsOf(longerThanNothing) == List(
            "scalus.cardano.onchain.plutus.prelude.List$.length"
          )
        )
    }

    test("a helper of the contract is inlined, so that its walk is the claim's") {
        val prop = forAll[prelude.List[BigInt]](xs =>
            (CallsAsUnknownsHelpers.size(xs) > BigInt(0)) ==>: (xs.length >= BigInt(1))
        )
        assert(unknownsOf(prop) == List("scalus.cardano.onchain.plutus.prelude.List$.length"))
    }

    test("a walk of a helper's result is the walk of the helper's code written out") {
        // Vesting's clause reads its datum with `configOf(datum)`, and the validator with that
        // code written out: the two signature checks are one walk.
        val prop = forAll[prelude.List[prelude.List[BigInt]]](xss =>
            (CallsAsUnknownsHelpers.firstList(xss).length > BigInt(0)) ==>: (xss match {
                case prelude.List.Cons(head, _) => head
                case prelude.List.Nil           => prelude.List.Nil
            }).length >= BigInt(1)
        )
        assert(unknownsOf(prop) == List("scalus.cardano.onchain.plutus.prelude.List$.length"))
    }

    test("a helper's argument that may fail stays in the key of a walk of its result") {
        // `firstList({ require(...); xss })` fails where `firstList(xss)` returns.
        val prop = forAll[BigInt, prelude.List[prelude.List[BigInt]]]((x, xss) =>
            (CallsAsUnknownsHelpers.firstList(xss).length >= BigInt(0)) ==>: (
              CallsAsUnknownsHelpers.firstList { require(x > BigInt(0)); xss }.length >= BigInt(0)
            )
        )
        assert(unknownsOf(prop).size == 2)
    }

    test("walks of two lists are two unknowns") {
        val prop = forAll[prelude.List[BigInt], prelude.List[BigInt]]((xs, ys) =>
            (xs.length > ys.length) ==>: (xs.length >= BigInt(1))
        )
        assert(unknownsOf(prop).size == 2)
    }

    test("a walk whose argument may fail is not one with an argument that cannot") {
        // `{ require(...); xs }.length` fails where `xs.length` returns: the two calls have two
        // unknowns, or one unknown's `None` would stand for both.
        val prop = forAll[BigInt, prelude.List[BigInt]]((x, xs) =>
            (xs.length >= BigInt(0)) ==>: ({ require(x > BigInt(0)); xs }.length >= BigInt(0))
        )
        assert(unknownsOf(prop).size == 2)
    }

    test("a walk of constants is no unknown") {
        // It has one value, which Lean computes: an unknown of it would lose it.
        val prop =
            forAll[BigInt](x => prelude.List.Cons(BigInt(1), prelude.List.Nil).length + x > x)
        assert(unknownsOf(prop).isEmpty)
    }

    private def assumedOf(prop: Prop): List[Clause] =
        CallsAsUnknowns
            .rewrite(prop, CallsAsUnknowns.inLibrary)
            .fold(reason => fail(reason), _.assumed)

    // A contract stated with `ensuring` is partial: it says nothing where the call fails, and the
    // unknown of a call may be `None`. So these statements are of the calls that return.
    private val countIsNotNegative =
        forAll[prelude.List[BigInt]](xs =>
            succeeds(CallsAsUnknownsContracts.count(xs)) ==>:
                (CallsAsUnknownsContracts.count(xs) >= BigInt(0))
        )

    test("a call of a function with a contract is an unknown, of which the contract is assumed") {
        assert(
          unknownsOf(countIsNotNegative) ==
              List("scalus.verify.uplcblaster.CallsAsUnknownsContracts$.count")
        )
        val assumed = assumedOf(countIsNotNegative)
        assert(assumed.map(_.kind) == List(Clause.Kind.Ensuring), assumed)
        assert(assumed.head.file.endsWith("CallsAsUnknownsTest.scala"), assumed)
    }

    test("a function whose code calls one with a contract has its contract") {
        val prop =
            forAll[prelude.List[BigInt]](xs => CallsAsUnknownsContracts.counted(xs) >= BigInt(0))
        assert(assumedOf(prop) == assumedOf(countIsNotNegative))
    }

    test("a call with a contract in the arguments of another gets a premise of its own") {
        // The unknown of `count(prepend(x, xs))` stands for the call of `prepend` too: its
        // premise binds the argument, and so makes an unknown of `prepend` with its premise, which
        // speaks of the walks `length` of both lists.
        val prop = forAll[BigInt, prelude.List[BigInt]]((x, xs) =>
            CallsAsUnknownsContracts.count(CallsAsUnknownsContracts.prepend(x, xs)) >= BigInt(0)
        )
        assert(assumedOf(prop).size == 2, assumedOf(prop))
        assert(unknownsOf(prop).size == 4, unknownsOf(prop))
    }

    test("a premise does not make the statement hold where the arguments of its call fail") {
        // `count({ require(x > 0); xs })`: the argument fails where `x` is not positive, and the
        // call with it. The premise reads it only where the call returned, and holds elsewhere: so
        // the statement that the call fails there is not proved by a premise that fails there.
        val prop = forAll[BigInt, prelude.List[BigInt]]((x, xs) =>
            CallsAsUnknownsContracts.count { require(x > BigInt(0)); xs } >= BigInt(0)
        )
        val rewritten = CallsAsUnknowns.rewrite(prop, CallsAsUnknowns.inLibrary).toOption.get
        def premises(p: Prop): List[SIR] = p match
            case Prop.Forall(_, body) => premises(body)
            case Prop.Implies(Prop.Bool(PropExpr.SIRExpr(premise)), body) =>
                premise :: premises(body)
            case _ => Nil
        val List(premise) = premises(rewritten.prop): @unchecked
        def unwrapped(sir: SIR): SIR = sir match
            case SIR.Decl(_, term) => unwrapped(term)
            case SIR.Let(bindings, body, _, _) if bindings.forall(_.name.contains("$.")) =>
                unwrapped(body)
            case other => other
        unwrapped(premise) match
            case SIR.Match(SIR.Var(unknown, _, _), _, _, _) =>
                assert(unknown == rewritten.unknowns.head.ident.name)
            case other => fail(s"the premise is no match of its unknown: $other")
    }

    test("a contract's precondition is a premise of its own, under which the guarantee is") {
        // So where the precondition fails, as on the empty list here, the guarantee is not
        // assumed, and the premise does not fail with it.
        val prop = forAll[prelude.List[BigInt]](xs =>
            succeeds(CallsAsUnknownsContracts.lengthOfSome(xs)) ==>:
                (CallsAsUnknownsContracts.lengthOfSome(xs) >= BigInt(1))
        )
        def body(p: Prop): Prop = p match
            case Prop.Forall(_, inner) => body(inner)
            case other                 => other
        body(CallsAsUnknowns.rewrite(prop, CallsAsUnknowns.inLibrary).toOption.get.prop) match
            case Prop.Implies(Prop.Implies(Prop.Bool(_), Prop.Bool(_)), _) => ()
            case other => fail(s"the premise is no implication of two tests: $other")
    }

    test("a `let` that may fail stays in the key of a call written out into its code") {
        // `one` does not read its parameter: the key of `{ val x = ...; one(x) }` is that of its
        // code, a constant, and keeps the `let`, which `one(0)` has not. So the two walks of lists
        // that begin with them are two unknowns, as one of them fails where `c` is not positive.
        val prop = forAll[BigInt, prelude.List[BigInt]]((c, xs) =>
            (prelude.List
                .Cons(
                  {
                      val x = { require(c > BigInt(0)); BigInt(0) }
                      CallsAsUnknownsHelpers.one(x)
                  },
                  xs
                )
                .length >= BigInt(0)) ==>:
                (prelude.List.Cons(CallsAsUnknownsHelpers.one(BigInt(0)), xs).length >= BigInt(0))
        )
        assert(unknownsOf(prop).size == 2, unknownsOf(prop))
    }

    test("a `let` that may fail stays in the key of a call of a name for a definition") {
        // As the test above, where the function is called by a name the program binds to it, as
        // an inline instance's `self`: the name is the definition, whose code drops the argument.
        val prop = forAll[BigInt, prelude.List[BigInt]]((c, xs) =>
            (CallsAsUnknownsHelpers.lengthWithOne(c, xs) >= BigInt(0)) ==>: (prelude.List
                .Cons(CallsAsUnknownsHelpers.constantOne(BigInt(0)), xs)
                .length >= BigInt(0))
        )
        assert(unknownsOf(prop).size == 2, unknownsOf(prop))
    }

    test("a call whose arguments read a lambda's parameter gets no premise") {
        // `one(p)` drops `p` from the key of the call of `count`, which is of the statement's
        // variable only, and an unknown; its arguments read `p`, which a premise does not bind.
        val prop = forAll[prelude.List[BigInt]](xs => {
            val f = (p: BigInt) =>
                CallsAsUnknownsContracts.count(prelude.List.Cons(CallsAsUnknownsHelpers.one(p), xs))
            f(BigInt(0)) >= BigInt(0)
        })
        assert(
          unknownsOf(prop) == List("scalus.verify.uplcblaster.CallsAsUnknownsContracts$.count"),
          unknownsOf(prop)
        )
        assert(assumedOf(prop).isEmpty, assumedOf(prop))
    }

    test("without contracts, a statement assumes none") {
        val rewritten = CallsAsUnknowns
            .rewrite(countIsNotNegative, CallsAsUnknowns.inLibrary, contracts = false)
            .fold(reason => fail(reason), identity)
        assert(rewritten.assumed.isEmpty, rewritten.assumed)
        assert(
          rewritten.unknowns.map(_.written) ==
              List("scalus.cardano.onchain.plutus.prelude.List$.length")
        )
    }

    test("a postcondition that is no function literal is no contract") {
        val prop =
            forAll[prelude.List[BigInt]](xs => CallsAsUnknownsContracts.countedBy(xs) >= BigInt(0))
        assert(assumedOf(prop).isEmpty)
        assert(unknownsOf(prop) == List("scalus.cardano.onchain.plutus.prelude.List$.length"))
    }

    test("a statement with no walk is left as it is") {
        val prop = forAll[BigInt](x => x + BigInt(0) == x)
        assert(CallsAsUnknowns.rewrite(prop, CallsAsUnknowns.inLibrary).map(_.prop) == Right(prop))
    }

    private val unknowns = new CallsAsUnknowns(UplcBlaster(Budget.Auto, lean, 5.minutes))

    private val longerThanNothingStatement =
        verifier.statement("calls_as_unknowns.longer_than_nothing", longerThanNothing)

    test("with its walk as an unknown, a statement of every list is proved") {
        val result = verify(VerificationRequest(longerThanNothingStatement, unknowns))
        assert(
          result.meets(longerThanNothingStatement),
          explain(longerThanNothingStatement, result)
        )
    }

    test("a statement proved with calls as unknowns is written of its programs, of any run") {
        // The proof is of the programs with unknowns, at a budget. The statement's own programs
        // walk lists longer than any budget reaches, so the module states it of runs of any
        // number of steps, and a proof by hand can cite it.
        val lowered =
            UplcBlaster.lower(longerThanNothing, FunctionTable.empty).fold(fail(_), identity)
        val module = "Scalus.CallsAsUnknownsTest.longer_than_nothing"
        // In a workspace the programs are at paths from its directory; here, in a directory of
        // the test, by their whole paths.
        val directory = Files.createTempDirectory("scalus-unknowns-module")
        val written = UplcBlaster.statementModule(
          lowered,
          120,
          Some(ProofKind.BlasterOfUnknowns),
          400000,
          module,
          directory.toString
        )
        written.programs.foreach((path, program) =>
            Files.writeString(Path.of(path), UplcBlaster.programHex(program))
        )
        assert(written.programs.map(_._2) == lowered.leaves, "the programs as written")
        assert(written.text.contains("at any number of steps of the machine"), written.text)
        assert(!written.text.contains("#prep_uplc_run"), written.text)
        assert(written.text.contains("(ScalusProofs.Runs.returns "), written.text)
        assert(written.text.contains("\nimport ScalusProofs.Runs\n"), written.text)
        assert(UplcBlaster.statementModuleOf(written.text).isEmpty, "no budget in the header")
        assert(written.text.contains("\naxiom proved : statement\n"), written.text)
        assert(written.text.contains("a proof of the kind `BlasterOfUnknowns`"), written.text)

        requireLean()
        val cited = s"theorem longer_than_nothing_cited : $module.statement := $module.proved\n" +
            "#print axioms longer_than_nothing_cited\n"
        leanServer.check(written.text + "\n" + cited, Some(5.minutes)) match
            case Result.Finished(messages) =>
                val errors = messages.filter(_.severity == Severity.Error)
                assert(errors.isEmpty, errors.map((m: Message) => s"${m.line}: ${m.text}"))
                assert(
                  messages.exists(m =>
                      m.text.contains("longer_than_nothing_cited' depends on axioms") &&
                          m.text.contains(s"$module.proved")
                  ),
                  messages.map((m: Message) => s"${m.line}: ${m.text}")
                )
            case other => fail(s"Lean did not check the statement: $other")
    }

    private val countIsNotNegativeStatement =
        verifier.statement("calls_as_unknowns.count_not_negative", countIsNotNegative)

    test("with the contract of its call, a statement of every list is proved, and names it") {
        // Without its contract, the unknown of `count` could be negative, as that of `length` is
        // below: the contract is what the proof rests on, and no proof here proves it.
        val result = verify(VerificationRequest(countIsNotNegativeStatement, unknowns))
        assert(
          result.meets(countIsNotNegativeStatement),
          explain(countIsNotNegativeStatement, result)
        )
        val premises = proofOf(result).premises
        assert(premises == assumedOf(countIsNotNegative), premises)
        assert(explain(result).contains("assuming the contracts of ensuring at"), explain(result))
    }

    private val twice = FunctionDef(CallsAsUnknownsContracts.twice)
    verifier.addFunction(twice)

    private val twiceContract = verifier.statement(
      "calls_as_unknowns.twice_contract",
      FunctionContract.inSource(twice).getOrElse(fail("twice states no contract"))
    )

    private val twiceIsEven = verifier.statement(
      "calls_as_unknowns.twice_is_even",
      forAll[BigInt](x =>
          succeeds(CallsAsUnknownsContracts.twice(x)) ==>:
              (CallsAsUnknownsContracts.twice(x) - x == x)
      )
    )

    test("a proof cites the contract it assumes as a lemma, where the verifier has proved it") {
        val contract = verify(twiceContract)
        assert(contract.meets(twiceContract), explain(twiceContract, contract))
        val result = verify(VerificationRequest(twiceIsEven, unknowns))
        assert(result.meets(twiceIsEven), explain(twiceIsEven, result))
        val proof = proofOf(result)
        assert(proof.premises.isEmpty, proof.premises)
        assert(proof.usedLemmas.map(_.statement.name) == List(twiceContract.name))
    }

    private val lengthOfSomeIsPositive = verifier.refute(
      "calls_as_unknowns.length_of_some_is_positive",
      forAll[prelude.List[BigInt]](xs =>
          succeeds(CallsAsUnknownsContracts.lengthOfSome(xs)) ==>:
              (CallsAsUnknownsContracts.lengthOfSome(xs) >= BigInt(1))
      )
    )

    test("where a contract's precondition fails, the statement is refuted, not assumed") {
        // The guarantee holds where the precondition does, and the precondition fails on the
        // empty list, where the call returns 0: the statement is false there, and its premise
        // does not hide it.
        val result = verifyWithoutKeeping(lengthOfSomeIsPositive, unknowns)
        assert(result.meets(lengthOfSomeIsPositive), explain(lengthOfSomeIsPositive, result))
    }

    private val outerIsTheDifference = verifier.statement(
      "calls_as_unknowns.outer_is_the_difference",
      forAll[BigInt, BigInt]((x, y) =>
          succeeds(CallsAsUnknownsContracts.outer(x, y)) ==>:
              (CallsAsUnknownsContracts.outer(x, y) == y - x)
      )
    )

    test("a contract through two functions whose code calls the next binds each one's arguments") {
        val result = verify(VerificationRequest(outerIsTheDifference, unknowns))
        assert(result.meets(outerIsTheDifference), explain(outerIsTheDifference, result))
        assert(proofOf(result).premises == assumedOf(outerIsTheDifference.prop))
    }

    private val ninefold = FunctionDef(CallsAsUnknownsContracts.ninefold)
    verifier.addFunction(ninefold)

    private val ninefoldHolds = verifier.guarantees(ninefold.ref)

    private val ninefoldMinusOne = verifier.statement(
      "calls_as_unknowns.ninefold_minus_one",
      forAll[BigInt](x =>
          succeeds(CallsAsUnknownsContracts.ninefold(x)) ==>:
              (CallsAsUnknownsContracts.ninefold(x) - x == x * BigInt(8))
      )
    )

    test("a proof rests on what the lemmas it cites rest on") {
        // `ninefold`'s guarantee is proved assuming the contract of `thrice`, which nothing here
        // proves. A statement that assumes `ninefold`'s contract cites that proof, and so rests on
        // `thrice`'s contract too.
        val List(guarantee) = ninefoldHolds.statements: @unchecked
        val proved = verify(VerificationRequest(guarantee, unknowns))
        assert(proved.meets(guarantee), explain(guarantee, proved))
        val thrice = proofOf(proved).premises
        assert(thrice.size == 1, thrice)
        val result = verify(VerificationRequest(ninefoldMinusOne, unknowns))
        assert(result.meets(ninefoldMinusOne), explain(ninefoldMinusOne, result))
        val proof = proofOf(result)
        assert(proof.premises.isEmpty, proof.premises)
        assert(proof.usedLemmas.map(_.statement.name) == List(guarantee.name))
        assert(proof.assumed == thrice, proof.assumed)
        assert(explain(result).contains(thrice.head.where), explain(result))
        // The clause of `ninefold` is checked, assuming that of `thrice`.
        val ninefolds =
            verifier.specifications.filter(_.functions.contains(ninefold.ref.displayName))
        assert(
          ninefolds.exists(_.status match
              case ClauseStatus.Checked(_, _, _, _, assuming) => assuming == thrice
              case _                                          => false),
          ninefolds.map(_.describe)
        )
    }

    private def proofOf(result: VerificationResult): Proof = result match
        case VerificationResult.Proven(proof) => proof
        case other                            => fail(s"no proof: $other")

    test("the module of a proof that assumed contracts names them by the axiom") {
        val lowered =
            UplcBlaster.lower(longerThanNothing, FunctionTable.empty).fold(fail(_), identity)
        val clause = assumedOf(countIsNotNegative).head
        val written = UplcBlaster.statementModule(
          lowered,
          120,
          Some(ProofKind.BlasterOfUnknowns),
          400000,
          "Scalus.CallsAsUnknownsTest.assuming",
          "programs",
          List(clause)
        )
        assert(
          written.text.contains(
            s"It assumes the contracts of ${clause.where}, which are not proved."
          ),
          written.text
        )
    }

    test("a refutation whose unknown is no value its walk returns is spurious") {
        // A length is never negative, but an unknown of it can be: the replay on the statement as
        // written says that the refutation is of no list.
        val result = verifyWithoutKeeping(
          verifier.statement(
            "calls_as_unknowns.length_not_negative",
            forAll[prelude.List[BigInt]](xs => xs.length >= BigInt(0))
          ),
          unknowns
        )
        assert(result.isInconclusive, explain(result))
        val reason = result.why.getOrElse("")
        assert(reason.contains("values that their walks do not return"), reason)
    }
}
