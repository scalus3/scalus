package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.Script
import scalus.cardano.onchain.RequirementError
import scalus.examples.{MarketplaceBaseProgram, ParameterValidationUsage}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.testing.kit.EvalTestKit
import scalus.uplc.PlutusV3

class ParameterValidationTest
    extends AnyFunSuite
    with EvalTestKit
    with scalus.ledger.api.v3.ArbitraryInstances {

    // --- Off-chain Tests ---

    test("computeScriptHashV3 produces correct hash for parameterized script") {
        // Given a creator public key hash
        val creatorPkh = PubKeyHash(ByteString.fromHex("deadbeef" * 7))

        // Compute hash using the pattern helper
        val computedHash = ParameterValidation.computeScriptHashV3(
          MarketplaceBaseProgram.program.deBruijnedProgram,
          creatorPkh.toData
        )

        // Manually apply parameter and compute hash
        val parameterized = MarketplaceBaseProgram.program.deBruijnedProgram $ creatorPkh.toData
        val manualHash =
            Script.PlutusV3(ByteString.unsafeFromArray(parameterized.cborEncoded)).scriptHash

        assert(computedHash == manualHash)
    }

    test("computeScriptHashV3 with multiple parameters applies in order") {
        // Create a simple two-parameter script for testing
        val twoParamScript = PlutusV3
            .compile { (a: BigInt, b: BigInt) =>
                a + b
            }
            .program
            .deBruijnedProgram

        // Apply parameters using helper
        val hash1 = ParameterValidation.computeScriptHashV3(
          twoParamScript,
          BigInt(1).toData,
          BigInt(2).toData
        )

        // Apply in different order should produce different hash
        val hash2 = ParameterValidation.computeScriptHashV3(
          twoParamScript,
          BigInt(2).toData,
          BigInt(1).toData
        )

        assert(hash1 != hash2, "Different parameter order should produce different hashes")
    }

    test("different parameters produce different hashes") {
        val creator1 = PubKeyHash(ByteString.fromHex("11111111" * 7))
        val creator2 = PubKeyHash(ByteString.fromHex("22222222" * 7))

        val hash1 = ParameterValidationUsage.computeMarketplaceHash(creator1)
        val hash2 = ParameterValidationUsage.computeMarketplaceHash(creator2)

        assert(hash1 != hash2, "Different creators should produce different marketplace hashes")
    }

    test("computeScriptHashV2 and computeScriptHashV1 produce different hashes than V3") {
        val creatorPkh = PubKeyHash(ByteString.fromHex("deadbeef" * 7))
        val program = MarketplaceBaseProgram.program.deBruijnedProgram

        val hashV3 = ParameterValidation.computeScriptHashV3(program, creatorPkh.toData)
        val hashV2 = ParameterValidation.computeScriptHashV2(program, creatorPkh.toData)
        val hashV1 = ParameterValidation.computeScriptHashV1(program, creatorPkh.toData)

        // Different language versions should produce different hashes (different prefix bytes)
        assert(hashV3 != hashV2)
        assert(hashV3 != hashV1)
        assert(hashV2 != hashV1)
    }

    // --- On-chain Tests ---
    // Note: ByteString.fromHex requires literal strings in Scalus compilation,
    // so we use actual 28-byte hex strings (56 hex chars)

    test("verifyScriptCredential succeeds for matching script hash") {
        assertEvalSuccess {
            val expectedHash =
                ByteString.fromHex("abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
            val credential = Credential.ScriptCredential(expectedHash)

            ParameterValidationOnChain.verifyScriptCredential(credential, expectedHash)
        }
    }

    test("verifyScriptCredential fails for non-matching script hash") {
        assertEvalFailsWithMessage[RequirementError](
          ParameterValidationOnChain.ScriptHashMismatch
        ) {
            val expectedHash =
                ByteString.fromHex("abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
            val wrongHash =
                ByteString.fromHex("12341234123412341234123412341234123412341234123412341234")
            val credential = Credential.ScriptCredential(wrongHash)

            ParameterValidationOnChain.verifyScriptCredential(credential, expectedHash)
        }
    }

    test("verifyScriptCredential fails for pub key credential") {
        assertEvalFailsWithMessage[NoSuchElementException](
          ParameterValidationOnChain.ExpectedScriptCredential
        ) {
            val expectedHash =
                ByteString.fromHex("abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
            val pubKeyHash = PubKeyHash(
              ByteString.fromHex("beefbeefbeefbeefbeefbeefbeefbeefbeefbeefbeefbeefbeefbeef")
            )
            val credential = Credential.PubKeyCredential(pubKeyHash)

            ParameterValidationOnChain.verifyScriptCredential(credential, expectedHash)
        }
    }

    test("verifyAddressScript succeeds for address with correct script credential") {
        assertEvalSuccess {
            val expectedHash =
                ByteString.fromHex("cafecafecafecafecafecafecafecafecafecafecafecafecafecafe")
            val address = Address(
              credential = Credential.ScriptCredential(expectedHash),
              stakingCredential = Option.None
            )

            ParameterValidationOnChain.verifyAddressScript(address, expectedHash)
        }
    }

    test("verifyAddressScript fails for address with wrong script") {
        assertEvalFailsWithMessage[RequirementError](
          ParameterValidationOnChain.ScriptHashMismatch
        ) {
            val expectedHash =
                ByteString.fromHex("cafecafecafecafecafecafecafecafecafecafecafecafecafecafe")
            val wrongHash =
                ByteString.fromHex("deaddeaddeaddeaddeaddeaddeaddeaddeaddeaddeaddeaddeaddead")
            val address = Address(
              credential = Credential.ScriptCredential(wrongHash),
              stakingCredential = Option.None
            )

            ParameterValidationOnChain.verifyAddressScript(address, expectedHash)
        }
    }

    test("isExpectedScript returns true for matching script credential") {
        assertEval {
            val expectedHash =
                ByteString.fromHex("12341234123412341234123412341234123412341234123412341234")
            val credential = Credential.ScriptCredential(expectedHash)

            ParameterValidationOnChain.isExpectedScript(credential, expectedHash)
        }
    }

    test("isExpectedScript returns false for non-matching script credential") {
        assertEval {
            val expectedHash =
                ByteString.fromHex("12341234123412341234123412341234123412341234123412341234")
            val wrongHash =
                ByteString.fromHex("56785678567856785678567856785678567856785678567856785678")
            val credential = Credential.ScriptCredential(wrongHash)

            !ParameterValidationOnChain.isExpectedScript(credential, expectedHash)
        }
    }

    test("isExpectedScript returns false for pub key credential") {
        assertEval {
            val expectedHash =
                ByteString.fromHex("12341234123412341234123412341234123412341234123412341234")
            val pubKeyHash = PubKeyHash(
              ByteString.fromHex("99999999999999999999999999999999999999999999999999999999")
            )
            val credential = Credential.PubKeyCredential(pubKeyHash)

            !ParameterValidationOnChain.isExpectedScript(credential, expectedHash)
        }
    }

    test("findOutputsToScript finds outputs to specified script") {
        assertEvalEq(
          {
              val targetHash =
                  ByteString.fromHex("abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
              val otherHash =
                  ByteString.fromHex("12341234123412341234123412341234123412341234123412341234")
              val pubKeyHash = PubKeyHash(
                ByteString.fromHex("beefbeefbeefbeefbeefbeefbeefbeefbeefbeefbeefbeefbeefbeef")
              )

              val outputs = List(
                TxOut(
                  address = Address(Credential.ScriptCredential(targetHash), Option.None),
                  value = Value.lovelace(BigInt(1000000)),
                  datum = OutputDatum.NoOutputDatum,
                  referenceScript = Option.None
                ),
                TxOut(
                  address = Address(Credential.PubKeyCredential(pubKeyHash), Option.None),
                  value = Value.lovelace(BigInt(2000000)),
                  datum = OutputDatum.NoOutputDatum,
                  referenceScript = Option.None
                ),
                TxOut(
                  address = Address(Credential.ScriptCredential(otherHash), Option.None),
                  value = Value.lovelace(BigInt(3000000)),
                  datum = OutputDatum.NoOutputDatum,
                  referenceScript = Option.None
                ),
                TxOut(
                  address = Address(Credential.ScriptCredential(targetHash), Option.None),
                  value = Value.lovelace(BigInt(4000000)),
                  datum = OutputDatum.NoOutputDatum,
                  referenceScript = Option.None
                )
              )

              val found = ParameterValidationOnChain.findOutputsToScript(outputs, targetHash)
              found.length
          },
          BigInt(2)
        )
    }

    test("findOutputsToScript returns empty list when no matching outputs") {
        assertEvalEq(
          {
              val targetHash =
                  ByteString.fromHex("abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
              val otherHash =
                  ByteString.fromHex("12341234123412341234123412341234123412341234123412341234")

              val outputs = List(
                TxOut(
                  address = Address(Credential.ScriptCredential(otherHash), Option.None),
                  value = Value.lovelace(BigInt(1000000)),
                  datum = OutputDatum.NoOutputDatum,
                  referenceScript = Option.None
                )
              )

              val found = ParameterValidationOnChain.findOutputsToScript(outputs, targetHash)
              found.length
          },
          BigInt(0)
        )
    }

    // --- Integration Test: Full Workflow ---

    test("setupNFTWithMarketplace produces consistent hashes") {
        val creatorPkh = PubKeyHash(ByteString.fromHex("deadbeef" * 7))
        val tokenName = ByteString.fromHex("4e4654") // "NFT"

        val (marketplace, mintingPolicy, marketplaceHash) =
            ParameterValidationUsage.setupNFTWithMarketplace(creatorPkh, tokenName)

        // Verify the marketplace hash matches what we'd compute directly
        val directHash = ParameterValidationUsage.computeMarketplaceHash(creatorPkh)
        assert(marketplaceHash == directHash)

        // Verify the marketplace program when converted to script produces the same hash
        val marketplaceScript =
            Script.PlutusV3(ByteString.unsafeFromArray(marketplace.cborEncoded))
        assert(marketplaceScript.scriptHash == marketplaceHash)
    }
}
