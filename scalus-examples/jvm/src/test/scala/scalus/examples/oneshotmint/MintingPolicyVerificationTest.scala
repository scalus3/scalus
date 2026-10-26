package scalus.examples.oneshotmint

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.{AssetName, LedgerToPlutusTranslation, MultiAsset, ScriptHash}
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.{===, SortedMap}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, Interval, PubKeyHash, ScriptContext, ScriptPurpose, TxId, TxInInfo, TxInfo, TxOut, TxOutRef, Value}
import scalus.compiler.{Compile, Options}
import scalus.compiler.sir.transform.EraseSpecifications
import scalus.uplc.builtin.Builtins.{bData, constrData, listData, mkCons, mkNilData, unConstrData}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
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

/** What the optimized deserializer does not read of a script context: the outputs the inputs spend,
  * and the fields of the transaction other than its inputs and its mint. Each is any `Data`, so a
  * statement over an `Unread` holds whatever they are.
  */
case class Unread(
    firstResolved: Data,
    secondResolved: Data,
    outputs: Data,
    fee: Data,
    dcert: Data,
    withdrawals: Data,
    validRange: Data,
    signatories: Data,
    data: Data,
    id: Data
) derives FromData,
      ToData

@Compile
object Unread

/** Properties of [[MintingPolicy]], proved about its compiled UPLC with the `UplcBlaster` tactic
  * (see `docs/design/verification-details/uplc-blaster.md`).
  *
  * It is a one-shot policy: the transaction that spends its seed output mints the tokens, and any
  * other transaction can only burn them. Three groups of statements:
  *   - the policy on minting contexts, as a deserializer reads them: the guarantees `mint` states
  *     in its body, that the transaction that spends the seed mints the tokens, and that any other
  *     burns them;
  *   - the optimized deserializer: it reads the ledger's script context of a minting, whatever its
  *     other fields are, and of nothing but a minting;
  *   - the script, the policy with that deserializer, on the ledger's script context: the same
  *     guarantees, stated of the transaction and not of what the deserializer reads, and that the
  *     transaction that spends the seed mints the tokens.
  *
  * The transaction has the shape the ledger gives a script of version 1 or 2 (see
  * `LedgerToPlutusTranslation.getMintValueV1V2`): its mint starts with an entry of zero ADA, then
  * has a policy of one token, and possibly another of two. It has one input, or two.
  *
  * What is not proved:
  *   - a transaction of any other shape. The policy searches the inputs and the mint, and the
  *     tactic cannot follow a loop over a list of unknown length;
  *   - that the tokens are minted once. They are minted by the transaction that spends the seed
  *     output, and that the ledger lets an output be spent once is the ledger's;
  *   - the other deserializers: the one that reads the whole context, and the one of version 2;
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

    test("the shape is the ledger's encoding of a minting") {
        // The statements below write the script context field by field, so that what the
        // deserializer does not read can be any `Data`. With the fields of a transaction that the
        // ledger's types build, it is the context the ledger gives the script. The mint is the
        // ledger's translation of the transaction's mint for a script of version 1 or 2. Each of
        // the four shapes the statements' Booleans choose is checked.
        for
            twoInputs <- List(false, true)
            twoPolicies <- List(false, true)
        do
            val m = sample.copy(twoInputs = twoInputs, twoPolicies = twoPolicies)
            val resolved = TxOut(
              Address(Credential.PubKeyCredential(PubKeyHash(hex"11")), prelude.Option.None),
              Value.lovelace(BigInt(5)),
              prelude.Option.None
            )
            val spent = prelude.List.Cons(
              TxInInfo(firstRef(m), resolved),
              if m.twoInputs then
                  prelude.List.Cons(TxInInfo(secondRef(m), resolved), prelude.List.Nil)
              else prelude.List.Nil
            )
            val ledgerTxInfo = TxInfo(
              inputs = spent,
              outputs = prelude.List.Nil,
              fee = Value.lovelace(BigInt(188021)),
              mint = LedgerToPlutusTranslation.getMintValueV1V2(Some(ledgerMint(m))),
              dcert = prelude.List.Nil,
              withdrawals = prelude.List.Nil,
              validRange = Interval.always,
              signatories = prelude.List.Nil,
              data = prelude.List.Nil,
              id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
            )
            val fields = unConstrData(ledgerTxInfo.toData).snd.toList
            val unread = Unread(
              firstResolved = resolved.toData,
              secondResolved = resolved.toData,
              outputs = fields(1),
              fee = fields(2),
              dcert = fields(4),
              withdrawals = fields(5),
              validRange = fields(6),
              signatories = fields(7),
              data = fields(8),
              id = fields(9)
            )
            val expected = ScriptContext(ledgerTxInfo, ScriptPurpose.Minting(m.ownSymbol)).toData
            assert(scriptContext(m, unread) == expected)
            // And the policy accepts it: it spends the seed and mints the tokens.
            MintingPolicy.mintingPolicyScript(MintingPolicy.optimizedCtxDeserializer)(
              m.seedId,
              m.seedIdx,
              tokens(m)
            )((), expected)
    }

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

    test("the specification is not part of the script") {
        val compiled = MintingPolicy.compiledOptimizedMintingPolicyScript
        // The clauses are kept in the SIR of the policy, with the functions they call,
        val clauses = List("spendsSeed", "ownTokens", "burnsOnly", "mintsExactly", "quantityOf")
        assert(mints(Representation.Sir).toString.contains("Spec$.holdsOnReturn"))
        clauses.foreach(name => assert(compiled.toString.contains(s"MintingPolicy$$.$name"), name))
        // and erasing the specification, which lowering starts with, removes the clauses and the
        // functions only they call.
        assert(compiled.toString.contains("Spec$.holdsOnReturn"))
        val erased = EraseSpecifications(compiled).toString
        assert(!erased.contains("holdsOnReturn"))
        clauses.foreach(name => assert(!erased.contains(s"MintingPolicy$$.$name"), name))
        // Compiled with error traces, a program has the message of every error it can raise, as
        // the policy has its own, and none of a function that only a clause calls.
        given Options = Options.debug
        val script = compiled.toUplc().show
        assert(script.contains("\"M\""), script)
        assert(!script.contains(MintingPolicy.NoOwnTokens))
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

    // The optimized deserializer, on script contexts

    private val readsTheLedgerContext = verifier.statement(
      "one_shot_mint.optimized_deserializer.reads_the_ledger_context",
      forAll[Mint, Unread]((m, unread) =>
          reads(MintingPolicy.optimizedCtxDeserializer(scriptContext(m, unread)), m)
      )
    )

    private val readsOnlyAMinting = verifier.statement(
      "one_shot_mint.optimized_deserializer.reads_only_a_minting",
      forAll[Mint, Unread, Data]((m, unread, purpose) =>
          !Prop(unConstrData(purpose).fst == BigInt(0)) ==>
              fails(MintingPolicy.optimizedCtxDeserializer(scriptContextOf(m, unread, purpose)))
      )
    )

    test("the optimized deserializer reads the ledger's context, and only of a minting") {
        // The references of the inputs, the mint and the policy, whatever the other fields of the
        // transaction are: it finds them by their positions.
        check(readsTheLedgerContext)
        // Minting is the script purpose of tag 0. For a purpose that is not a constructor the
        // premise's test fails, so its negation holds, and the deserializer fails as well.
        check(readsOnlyAMinting)
    }

    // The script

    /** The script with the optimized deserializer, applied to its parameters and a context. */
    private val script = FunctionDef.named(
      "script",
      (m: Mint, unread: Unread) =>
          MintingPolicy.mintingPolicyScript(MintingPolicy.optimizedCtxDeserializer)(
            m.seedId,
            m.seedIdx,
            tokens(m)
          )((), scriptContext(m, unread))
    )

    verifier.addFunction(script)

    private val onlyTheSeedMints = verifier.statement(
      "one_shot_mint.script.only_the_seed_mints",
      forAll[Mint, Unread]((m, unread) =>
          succeeds(script, (m, unread)) ==>
              (spendsTheSeed(m) || MintingPolicy.burnsOnly(context(m)))
      )
    )

    private val seedMintsExactlyTheTokens = verifier.statement(
      "one_shot_mint.script.seed_mints_exactly_the_tokens",
      forAll[Mint, Unread]((m, unread) =>
          distinctTokenNames(m) ==> (succeeds(script, (m, unread)) ==>
              (!spendsTheSeed(m) || MintingPolicy.mintsExactly(context(m), tokens(m))))
      )
    )

    private val scriptSeedMintsTheTokens = verifier.statement(
      "one_shot_mint.script.seed_mints_the_tokens",
      forAll[Mint, Unread]((m, unread) =>
          (spendsTheSeed(m) && firstIsOwn(m) && m.token == m.name && m.quantity == m.amount) ==>
              succeeds(script, (m, unread))
      )
    )

    test("the script mints only with the seed, and with it exactly the tokens") {
        // The guarantees of `mint`, of the transaction the ledger gives the script: whichever of
        // the mint's policies the script's is, and whatever the fields it does not read are. The
        // second, as on `mint`, of a mint whose token names are different.
        check(onlyTheSeedMints)
        check(seedMintsExactlyTheTokens)
        // And it accepts the transaction that spends the seed and mints the tokens.
        check(scriptSeedMintsTheTokens)
    }
}

/** The values the statements of [[MintingPolicyVerificationTest]] are about. They are `inline`, so
  * that each is part of the expression it is used in, which the Scalus plugin compiles. They live
  * in an object: an `inline def` of the test class that uses another one would leave a reference to
  * the test's `this` in that expression.
  */
object MintingPolicyVerificationTest {

    /** A minting the policy accepts, for the tests that run on the JVM: its first input is the
      * seed, and the first policy of its mint is the script's, of the tokens. It has a second input
      * and a second policy.
      */
    val sample: Mint = Mint(
      seedId = hex"1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982",
      seedIdx = BigInt(0),
      name = hex"484f534b59",
      amount = BigInt(1000),
      firstId = hex"1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982",
      firstIdx = BigInt(0),
      twoInputs = true,
      secondId = hex"2222222222222222222222222222222222222222222222222222222222222222",
      secondIdx = BigInt(1),
      ownSymbol = hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
      policy = hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
      token = hex"484f534b59",
      quantity = BigInt(1000),
      twoPolicies = true,
      otherPolicy = hex"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
      firstToken = hex"01",
      firstQuantity = BigInt(1),
      secondToken = hex"02",
      secondQuantity = BigInt(-2)
    )

    /** The mint of `m` in the ledger's types, before it is translated for the script. */
    def ledgerMint(m: Mint): scalus.cardano.ledger.Mint = {
        // The ledger's types hold less than a `Mint`: quantities are `Long`s, and a policy is a key
        // of a map. Of another `Mint` they would describe another mint than `minted(m)`.
        val quantities = List(m.quantity) ++
            (if m.twoPolicies then List(m.firstQuantity, m.secondQuantity) else Nil)
        require(quantities.forall(_.isValidLong), s"a quantity is not a Long: $quantities")
        require(!m.twoPolicies || m.policy != m.otherPolicy, "the two policies are the same")
        val first = Map(
          ScriptHash.fromByteString(m.policy) -> Map(AssetName(m.token) -> m.quantity.toLong)
        )
        val other =
            if m.twoPolicies then
                Map(
                  ScriptHash.fromByteString(m.otherPolicy) -> Map(
                    AssetName(m.firstToken) -> m.firstQuantity.toLong,
                    AssetName(m.secondToken) -> m.secondQuantity.toLong
                  )
                )
            else Map.empty
        scalus.cardano.ledger.Mint(MultiAsset.fromAssets(first ++ other))
    }

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

    /** Whether `read` is what the script context of `m` holds: the references of the inputs, the
      * mint, and the policy the script runs as.
      */
    inline def reads(inline read: MintingContext, inline m: Mint): Boolean = read match
        case MintingContext(refs, mint, own) =>
            refs === inputs(m) && mint === minted(m) && own == m.ownSymbol

    /** An input of the transaction, as the ledger encodes it. */
    inline def input(inline ref: TxOutRef, inline resolved: Data): Data =
        constrData(0, mkCons(ref.toData, mkCons(resolved, mkNilData())))

    /** The transaction of `m`, as the ledger encodes it: the inputs and the mint of `m`, and the
      * other fields of `unread`.
      */
    inline def txInfo(inline m: Mint, inline unread: Unread): Data = {
        val second =
            if m.twoInputs then mkCons(input(secondRef(m), unread.secondResolved), mkNilData())
            else mkNilData()
        val spent = listData(mkCons(input(firstRef(m), unread.firstResolved), second))
        val fields = mkCons(
          unread.dcert,
          mkCons(
            unread.withdrawals,
            mkCons(
              unread.validRange,
              mkCons(unread.signatories, mkCons(unread.data, mkCons(unread.id, mkNilData())))
            )
          )
        )
        constrData(
          0,
          mkCons(
            spent,
            mkCons(unread.outputs, mkCons(unread.fee, mkCons(minted(m).toData, fields)))
          )
        )
    }

    /** The script context of the transaction of `m`, for a script of the purpose `purpose`. */
    inline def scriptContextOf(
        inline m: Mint,
        inline unread: Unread,
        inline purpose: Data
    ): Data = {
        constrData(0, mkCons(txInfo(m, unread), mkCons(purpose, mkNilData())))
    }

    /** The script context of the transaction of `m`, for the policy `m.ownSymbol`. */
    inline def scriptContext(inline m: Mint, inline unread: Unread): Data = {
        scriptContextOf(m, unread, constrData(0, mkCons(bData(m.ownSymbol), mkNilData())))
    }
}
