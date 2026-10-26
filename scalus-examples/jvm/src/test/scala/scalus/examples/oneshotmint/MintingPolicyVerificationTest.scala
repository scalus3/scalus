package scalus.examples.oneshotmint

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.cardano.onchain.plutus.v1.{TxId, TxOutRef, Value}
import scalus.compiler.Compile
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{LeanProofs, LeanProofsSuite}

import java.nio.file.Path

/** A minting of the shape the statements of [[MintingPolicyVerificationTest]] are about, as one
  * value a statement can quantify over: the policy's parameters, and what it reads of the
  * transaction.
  */
case class Mint(
    // The parameters: the seed output, and the one token the policy mints.
    seedId: ByteString,
    seedIdx: BigInt,
    name: ByteString,
    amount: BigInt,
    // The references of the transaction's inputs: one, and a second one where `twoInputs`.
    firstId: ByteString,
    firstIdx: BigInt,
    twoInputs: Boolean,
    secondId: ByteString,
    secondIdx: BigInt,
    // The policy the script runs as.
    ownSymbol: ByteString,
    // The transaction's mint after the entry of ADA: a policy with one token, and where
    // `twoPolicies` one with two.
    policy: ByteString,
    token: ByteString,
    quantity: BigInt,
    twoPolicies: Boolean,
    otherPolicy: ByteString,
    firstToken: ByteString,
    firstQuantity: BigInt,
    secondToken: ByteString,
    secondQuantity: BigInt
) derives FromData,
      ToData

@Compile
object Mint

/** Properties of [[MintingPolicy]], proved about its compiled UPLC with the `UplcBlaster` tactic
  * (see `docs/design/verification-details/uplc-blaster.md`).
  *
  * It is a one-shot policy: the transaction that spends its seed output mints the tokens, and any
  * other transaction can only burn them. The statements are about the policy on minting contexts,
  * as a deserializer reads them: the guarantees `mint` states in its body, that the transaction
  * that spends the seed mints the tokens, and that any other burns them.
  *
  * The minting context has the shape the ledger gives a script of version 1 or 2 (see
  * `LedgerToPlutusTranslation.getMintValueV1V2`): its mint starts with an entry of zero ADA, then
  * has a policy of one token, and possibly another of two. It has one input, or two.
  *
  * What is not proved:
  *   - a transaction of any other shape. The policy searches the inputs and the mint, and the
  *     tactic cannot follow a loop over a list of unknown length;
  *   - that the tokens are minted once. They are minted by the transaction that spends the seed
  *     output, and that the ledger lets an output be spent once is the ledger's;
  *   - the deserializers, and so the script on the ledger's script context;
  *   - the published scripts' bytes. The policy is compiled here with the tactic's options
  *     (`UplcBlaster.options`): the statements are about the same source.
  *
  * Without Lean, that is without `lake` and a built workspace, the tests pass on the results kept
  * beside this file (see [[LeanProofs]]).
  */
class MintingPolicyVerificationTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    import MintingPolicyVerificationTest.*

    /** The results of the statements below are kept beside this file: a run without Lean passes on
      * them, and sees at once where the policy's code is no longer the one they are of.
      */
    override protected def keptResultsFile: Option[Path] = Some(
      LeanProofs
          .inSources(
            "scalus-examples",
            "jvm",
            "src",
            "test",
            "scala",
            "scalus",
            "examples",
            "oneshotmint"
          )
          .resolve("MintingPolicyVerificationTest.proofs.json")
    )

    // The policy, on minting contexts

    /** The policy on the minting context of `m`. `mint` is inline, so the verifier finds its
      * clauses in the code of this function.
      */
    private val mints = FunctionDef.named(
      "mints",
      (m: Mint) => MintingPolicy.mint(context(m), m.seedId, m.seedIdx, tokens(m))
    )

    verifier.addFunction(mints)

    /** The policy on a mint whose second policy names two different tokens. The ledger's mint is a
      * map, so its names are different; the precondition asks no more of it than that. It promises
      * nothing more than to return, and is there for its precondition.
      */
    private val mintsOfDistinctTokenNames = verifier.contract(
      "mints_of_distinct_token_names",
      contract(mints)(expects = m => distinctTokenNames(m), ensures = m => accepted => true)
    )

    /** What `MintingPolicy.mint` states at its head, with `Spec.ensures`: a transaction that does
      * not spend the seed only burns, and the one that does mints exactly the tokens. They are
      * stated of a mint whose token names are different: `equalsAssets` compares the tokens one by
      * one, in order, so the policy accepts the tokens `[(t, q), (t, 0)]` for `[(t, q)]`, where `t`
      * has two quantities.
      */
    private val mintGuarantees = verifier.guarantees(mintsOfDistinctTokenNames)

    test("the guarantees stated on mint hold of every transaction the policy accepts") {
        // A clause that is not read would be missing from what is proved.
        assert(mintGuarantees.unsupported.isEmpty, mintGuarantees.unsupported)
        assert(mintGuarantees.statements.size == 2)
        // They are proved together, so that the tactic runs the policy once for both.
        check(mintGuarantees.together.getOrElse(fail("mint states no guarantee")))
    }

    private val seedMintsTheTokens = verifier.statement(
      "one_shot_mint.mint.seed_mints_the_tokens",
      forAll[Mint](m =>
          (spendsTheSeed(m) && firstIsOwn(m) && m.token == m.name && m.quantity == m.amount) ==>
              succeeds(mints, m)
      )
    )

    private val seedMintsAnyQuantity = verifier.refute(
      "one_shot_mint.mint.seed_mints_any_quantity",
      forAll[Mint](m =>
          (spendsTheSeed(m) && firstIsOwn(m) && m.token == m.name) ==> succeeds(mints, m)
      )
    )

    test("the transaction that spends the seed mints the tokens") {
        // Wherever the seed is among its inputs, and whether the mint has another policy.
        check(seedMintsTheTokens)
        // negative control: in a quantity other than the tokens', it is rejected
        check(seedMintsAnyQuantity)
    }

    private val burnsWithoutTheSeed = verifier.statement(
      "one_shot_mint.mint.burns_without_the_seed",
      forAll[Mint](m =>
          (!spendsTheSeed(m) && firstIsOwn(m) && m.quantity < BigInt(0)) ==> succeeds(mints, m)
      )
    )

    private val acceptedOnlyWithTheSeed = verifier.refute(
      "one_shot_mint.mint.accepted_only_with_the_seed",
      forAll[Mint](m => succeeds(mints, m) ==> spendsTheSeed(m))
    )

    test("a transaction that does not spend the seed burns the tokens") {
        // The tokens are not stuck once minted.
        check(burnsWithoutTheSeed)
        // negative control: so the policy accepts a transaction that does not spend the seed
        check(acceptedOnlyWithTheSeed)
    }

}

/** The values the statements of [[MintingPolicyVerificationTest]] are about. They are `inline`, so
  * that each is part of the expression it is used in, which the Scalus plugin compiles. They live
  * in an object: an `inline def` of the test class that uses another one would leave a reference to
  * the test's `this` in that expression.
  */
object MintingPolicyVerificationTest {

    /** The tokens the policy mints: one, `name`, in the quantity `amount`. */
    inline def tokens(inline m: Mint): SortedMap[ByteString, BigInt] =
        SortedMap.singleton(m.name, m.amount)

    /** The mint of `m` as the ledger gives it to a script of version 1 or 2: an entry of zero ADA,
      * `policy` with one token, and where `twoPolicies` `otherPolicy` with two.
      */
    inline def minted(inline m: Mint): Value = {
        val other: prelude.List[(ByteString, SortedMap[ByteString, BigInt])] =
            if m.twoPolicies then
                prelude.List.Cons(
                  (
                    m.otherPolicy,
                    SortedMap.unsafeFromList(
                      prelude.List.Cons(
                        (m.firstToken, m.firstQuantity),
                        prelude.List.Cons((m.secondToken, m.secondQuantity), prelude.List.Nil)
                      )
                    )
                  ),
                  prelude.List.Nil
                )
            else prelude.List.Nil
        Value.unsafeFromSortedMap(
          SortedMap.unsafeFromList(
            prelude.List.Cons(
              (Value.adaPolicyId, SortedMap.singleton(Value.adaTokenName, BigInt(0))),
              prelude.List.Cons((m.policy, SortedMap.singleton(m.token, m.quantity)), other)
            )
          )
        )
    }

    /** Whether the two tokens of the mint's second policy have different names. In the ledger's
      * mint, a map, every name is different; this asks that of the one policy where it matters
      * here, and nothing of the order of the names, the policies, or the quantities.
      */
    inline def distinctTokenNames(inline m: Mint): Boolean = m.firstToken != m.secondToken

    /** Whether the first policy of the mint after ADA is the script's, which is not ADA. */
    inline def firstIsOwn(inline m: Mint): Boolean =
        m.policy == m.ownSymbol && m.ownSymbol != Value.adaPolicyId

    inline def firstRef(inline m: Mint): TxOutRef = TxOutRef(TxId(m.firstId), m.firstIdx)

    inline def secondRef(inline m: Mint): TxOutRef = TxOutRef(TxId(m.secondId), m.secondIdx)

    /** Whether an input of the transaction is the seed output. */
    inline def spendsTheSeed(inline m: Mint): Boolean = {
        (m.firstId == m.seedId && m.firstIdx == m.seedIdx) ||
        (m.twoInputs && m.secondId == m.seedId && m.secondIdx == m.seedIdx)
    }

    /** The references of the transaction's inputs. */
    inline def inputs(inline m: Mint): prelude.List[TxOutRef] = {
        val second: prelude.List[TxOutRef] =
            if m.twoInputs then prelude.List.Cons(secondRef(m), prelude.List.Nil)
            else prelude.List.Nil
        prelude.List.Cons(firstRef(m), second)
    }

    /** The minting context of `m`, as a deserializer reads it. */
    inline def context(inline m: Mint): MintingContext =
        new MintingContext(inputs(m), minted(m), m.ownSymbol)
}
