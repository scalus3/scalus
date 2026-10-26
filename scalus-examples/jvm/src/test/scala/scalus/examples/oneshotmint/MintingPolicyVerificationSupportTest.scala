package scalus.examples.oneshotmint

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.{AssetName, LedgerToPlutusTranslation, MultiAsset, ScriptHash}
import scalus.compiler.Options
import scalus.compiler.sir.transform.EraseSpecifications
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.verify.{FunctionDef, Representation}

/** What [[MintingPolicyVerificationTest]] rests on, checked without Lean: the mint of its minting
  * contexts is the ledger's, and the specification is no part of the script.
  */
class MintingPolicyVerificationSupportTest extends AnyFunSuite {
    import MintingPolicyVerificationTest.*
    import MintingPolicyVerificationSupportTest.*

    test("the mint of the minting contexts is the ledger's translation of a mint") {
        // The statements build the mint of a minting context from a `Mint`. For each of the four
        // shapes their Booleans choose, it is what the ledger gives a script of version 1 or 2.
        for
            twoInputs <- List(false, true)
            twoPolicies <- List(false, true)
        do
            val m = sample.copy(twoInputs = twoInputs, twoPolicies = twoPolicies)
            val ledger = LedgerToPlutusTranslation.getMintValueV1V2(Some(ledgerMint(m)))
            assert(minted(m).toData == ledger.toData, m)
            assert(inputs(m).length == (if twoInputs then BigInt(2) else BigInt(1)), m)
    }

    test("the specification is not part of the script") {
        val compiled = MintingPolicy.compiledOptimizedMintingPolicyScript
        // The clauses are kept in the SIR of the function the verifier reads,
        val mints = FunctionDef.named(
          "mints",
          (m: Mint) => MintingPolicy.mint(context(m), m.seedId, m.seedIdx, tokens(m))
        )
        assert(mints(Representation.Sir).toString.contains("Spec$.holdsOnReturn"))
        // and in the policy's, with the functions they call,
        val clauses = List("spendsSeed", "ownTokens", "burnsOnly", "mintsExactly", "quantityOf")
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
}

object MintingPolicyVerificationSupportTest {

    /** A minting for the tests here: its first input is the seed, and the first policy of its mint
      * is the script's, of the tokens. It has a second input and a second policy.
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
}
