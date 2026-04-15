package scalus.examples.factory

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.given
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.testing.kit.EvalTestKit

class FactoryTest
    extends AnyFunSuite
    with EvalTestKit
    with scalus.cardano.onchain.plutus.v3.ArbitraryInstances {

    // --- computeTokenName ---

    test("computeTokenName is deterministic") {
        assertEval {
            val seed = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "1111111111111111111111111111111111111111111111111111111111111111"
                )
              ),
              BigInt(0)
            )
            val tn1 = Factory.computeTokenName(seed)
            val tn2 = Factory.computeTokenName(seed)
            tn1 === tn2
        }
    }

    test("computeTokenName differs for different seed UTxOs") {
        assertEval {
            val seed1 = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "1111111111111111111111111111111111111111111111111111111111111111"
                )
              ),
              BigInt(0)
            )
            val seed2 = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "2222222222222222222222222222222222222222222222222222222222222222"
                )
              ),
              BigInt(0)
            )
            !(Factory.computeTokenName(seed1) === Factory.computeTokenName(seed2))
        }
    }

    test("computeTokenName differs for different output indices") {
        assertEval {
            val seed1 = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "1111111111111111111111111111111111111111111111111111111111111111"
                )
              ),
              BigInt(0)
            )
            val seed2 = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "1111111111111111111111111111111111111111111111111111111111111111"
                )
              ),
              BigInt(1)
            )
            !(Factory.computeTokenName(seed1) === Factory.computeTokenName(seed2))
        }
    }

    // --- validateCreate ---

    test("validateCreate succeeds with correct mint, datum, output, and seed UTxO") {
        assertEvalSuccess {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val tag = ByteString.fromHex("cafebabe")
            val policyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val seedUtxo = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "1111111111111111111111111111111111111111111111111111111111111111"
                )
              ),
              BigInt(0)
            )
            val expectedTokenName = Factory.computeTokenName(seedUtxo)
            val productDatum = ProductDatum(tag, creator)

            val tx = TxInfo(
              inputs = List(
                TxInInfo(
                  outRef = seedUtxo,
                  resolved = TxOut(
                    address = Address(
                      Credential.PubKeyCredential(creator),
                      Option.None
                    ),
                    value = Value.lovelace(BigInt(5000000))
                  )
                )
              ),
              outputs = List(
                TxOut(
                  address = Address(
                    Credential.ScriptCredential(policyId),
                    Option.None
                  ),
                  value = Value
                      .lovelace(BigInt(2000000)) + Value(policyId, expectedTokenName, BigInt(1)),
                  datum = OutputDatum.OutputDatum(productDatum.toData),
                  referenceScript = Option.None
                )
              ),
              mint = Value(policyId, expectedTokenName, BigInt(1)),
              signatories = List(creator),
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateCreate(creator, tag, seedUtxo, policyId, policyId, tx)
        }
    }

    test("validateCreate fails when creator has not signed") {
        assertEvalFailsWithMessage[RequirementError](Factory.CreatorMustSign) {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val tag = ByteString.fromHex("cafebabe")
            val policyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val seedUtxo = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "1111111111111111111111111111111111111111111111111111111111111111"
                )
              ),
              BigInt(0)
            )
            val expectedTokenName = Factory.computeTokenName(seedUtxo)
            val productDatum = ProductDatum(tag, creator)

            val tx = TxInfo(
              inputs = List(
                TxInInfo(
                  outRef = seedUtxo,
                  resolved = TxOut(
                    address = Address(
                      Credential.PubKeyCredential(creator),
                      Option.None
                    ),
                    value = Value.lovelace(BigInt(5000000))
                  )
                )
              ),
              outputs = List(
                TxOut(
                  address = Address(Credential.ScriptCredential(policyId), Option.None),
                  value = Value
                      .lovelace(BigInt(2000000)) + Value(policyId, expectedTokenName, BigInt(1)),
                  datum = OutputDatum.OutputDatum(productDatum.toData),
                  referenceScript = Option.None
                )
              ),
              mint = Value(policyId, expectedTokenName, BigInt(1)),
              signatories = List.empty, // no signatories
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateCreate(creator, tag, seedUtxo, policyId, policyId, tx)
        }
    }

    test("validateCreate fails when seed UTxO is not consumed") {
        assertEvalFailsWithMessage[RequirementError](Factory.SeedUtxoMustBeConsumed) {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val tag = ByteString.fromHex("cafebabe")
            val policyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val seedUtxo = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "1111111111111111111111111111111111111111111111111111111111111111"
                )
              ),
              BigInt(0)
            )
            val expectedTokenName = Factory.computeTokenName(seedUtxo)
            val productDatum = ProductDatum(tag, creator)

            val tx = TxInfo(
              inputs = List.empty, // seed UTxO NOT in inputs
              outputs = List(
                TxOut(
                  address = Address(Credential.ScriptCredential(policyId), Option.None),
                  value = Value
                      .lovelace(BigInt(2000000)) + Value(policyId, expectedTokenName, BigInt(1)),
                  datum = OutputDatum.OutputDatum(productDatum.toData),
                  referenceScript = Option.None
                )
              ),
              mint = Value(policyId, expectedTokenName, BigInt(1)),
              signatories = List(creator),
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateCreate(creator, tag, seedUtxo, policyId, policyId, tx)
        }
    }

    test("validateCreate fails with wrong token name") {
        assertEvalFailsWithMessage[RequirementError](Factory.WrongTokenName) {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val tag = ByteString.fromHex("cafebabe")
            val policyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val seedUtxo = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "1111111111111111111111111111111111111111111111111111111111111111"
                )
              ),
              BigInt(0)
            )
            val wrongTokenName = ByteString.fromHex("1234567890abcdef")
            val productDatum = ProductDatum(tag, creator)

            val tx = TxInfo(
              inputs = List(
                TxInInfo(
                  outRef = seedUtxo,
                  resolved = TxOut(
                    address = Address(
                      Credential.PubKeyCredential(creator),
                      Option.None
                    ),
                    value = Value.lovelace(BigInt(5000000))
                  )
                )
              ),
              outputs = List(
                TxOut(
                  address = Address(Credential.ScriptCredential(policyId), Option.None),
                  value =
                      Value.lovelace(BigInt(2000000)) + Value(policyId, wrongTokenName, BigInt(1)),
                  datum = OutputDatum.OutputDatum(productDatum.toData),
                  referenceScript = Option.None
                )
              ),
              mint = Value(policyId, wrongTokenName, BigInt(1)),
              signatories = List(creator),
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateCreate(creator, tag, seedUtxo, policyId, policyId, tx)
        }
    }

    test("validateCreate fails with missing product output") {
        assertEvalFailsWithMessage[NoSuchElementException](Factory.MissingProductOutput) {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val tag = ByteString.fromHex("cafebabe")
            val policyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val seedUtxo = TxOutRef(
              TxId(
                ByteString.fromHex(
                  "1111111111111111111111111111111111111111111111111111111111111111"
                )
              ),
              BigInt(0)
            )
            val expectedTokenName = Factory.computeTokenName(seedUtxo)

            val tx = TxInfo(
              inputs = List(
                TxInInfo(
                  outRef = seedUtxo,
                  resolved = TxOut(
                    address = Address(
                      Credential.PubKeyCredential(creator),
                      Option.None
                    ),
                    value = Value.lovelace(BigInt(5000000))
                  )
                )
              ),
              outputs = List.empty, // no outputs
              mint = Value(policyId, expectedTokenName, BigInt(1)),
              signatories = List(creator),
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateCreate(creator, tag, seedUtxo, policyId, policyId, tx)
        }
    }

    // --- validateDestroy ---

    test("validateDestroy succeeds with correct burn and signature") {
        assertEvalSuccess {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val policyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val someTokenName = ByteString.fromHex("aabbccdd")

            val tx = TxInfo(
              inputs = List.empty,
              mint = Value(policyId, someTokenName, BigInt(-1)),
              signatories = List(creator),
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateDestroy(creator, policyId, tx)
        }
    }

    test("validateDestroy fails when creator has not signed") {
        assertEvalFailsWithMessage[RequirementError](Factory.CreatorMustSign) {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val policyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val someTokenName = ByteString.fromHex("aabbccdd")

            val tx = TxInfo(
              inputs = List.empty,
              mint = Value(policyId, someTokenName, BigInt(-1)),
              signatories = List.empty, // no signatories
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateDestroy(creator, policyId, tx)
        }
    }

    // --- validateSpend ---

    test("validateSpend succeeds when NFT is burned and creator signs") {
        assertEvalSuccess {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val tag = ByteString.fromHex("cafebabe")
            val factoryPolicyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val datum = ProductDatum(tag, creator)
            val tokenName = ByteString.fromHex(
              "aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd"
            )
            val ownInputValue =
                Value.lovelace(BigInt(2000000)) + Value(factoryPolicyId, tokenName, BigInt(1))

            val tx = TxInfo(
              inputs = List.empty,
              mint = Value(factoryPolicyId, tokenName, BigInt(-1)),
              signatories = List(creator),
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateSpend(datum, factoryPolicyId, ownInputValue, tx)
        }
    }

    test("validateSpend fails when creator has not signed") {
        assertEvalFailsWithMessage[RequirementError](Factory.CreatorMustSign) {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val tag = ByteString.fromHex("cafebabe")
            val factoryPolicyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val datum = ProductDatum(tag, creator)
            val tokenName = ByteString.fromHex(
              "aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd"
            )
            val ownInputValue =
                Value.lovelace(BigInt(2000000)) + Value(factoryPolicyId, tokenName, BigInt(1))

            val tx = TxInfo(
              inputs = List.empty,
              mint = Value(factoryPolicyId, tokenName, BigInt(-1)),
              signatories = List.empty, // no signatories
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateSpend(datum, factoryPolicyId, ownInputValue, tx)
        }
    }

    test("validateSpend fails when NFT is not burned") {
        assertEvalFailsWithMessage[RequirementError](Factory.ProductNFTMustBeBurned) {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val tag = ByteString.fromHex("cafebabe")
            val factoryPolicyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val datum = ProductDatum(tag, creator)
            val tokenName = ByteString.fromHex(
              "aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd"
            )
            val ownInputValue =
                Value.lovelace(BigInt(2000000)) + Value(factoryPolicyId, tokenName, BigInt(1))

            val tx = TxInfo(
              inputs = List.empty,
              mint = Value.zero, // no burn
              signatories = List(creator),
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateSpend(datum, factoryPolicyId, ownInputValue, tx)
        }
    }

    test("validateSpend fails when no factory token in input") {
        assertEvalFailsWithMessage[NoSuchElementException](Factory.NoFactoryToken) {
            val creator = PubKeyHash(
              ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
            )
            val tag = ByteString.fromHex("cafebabe")
            val factoryPolicyId = ByteString.fromHex(
              "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            )
            val datum = ProductDatum(tag, creator)
            val ownInputValue = Value.lovelace(BigInt(2000000)) // no factory NFT

            val tx = TxInfo(
              inputs = List.empty,
              mint = Value.zero,
              signatories = List(creator),
              id = TxId(
                ByteString.fromHex(
                  "0000000000000000000000000000000000000000000000000000000000000000"
                )
              )
            )

            Factory.validateSpend(datum, factoryPolicyId, ownInputValue, tx)
        }
    }
}
