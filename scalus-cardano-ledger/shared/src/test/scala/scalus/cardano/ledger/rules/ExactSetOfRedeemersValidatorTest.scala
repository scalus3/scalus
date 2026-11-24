package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.testing.kit.Party.Alice
import scalus.uplc.builtin.{ByteString, Data}

import TransactionWitnessSet.given
import scala.collection.immutable.SortedMap

// The verdict of the rule does not depend on the units of a redeemer, so each test that sets a
// redeemer is a `forAll` over the wide `genExUnits`, spec `[kit.exunits.wide-forall]`.
class ExactSetOfRedeemersValidatorTest
    extends AnyFunSuite,
      ScalaCheckPropertyChecks,
      ValidatorRulesTestKit {

    // The rule only hashes the scripts and never runs them, so any bytes serve.
    private val v1Script = Script.PlutusV1(ByteString.fromHex("01" * 32))
    private val v2Script = Script.PlutusV2(ByteString.fromHex("02" * 32))
    private val datum = Data.I(1)

    // A UTxO at a key address: spending it needs no redeemer.
    private val keyUtxo: Utxos = Map(input(0) -> TransactionOutput(keyAddress(0), Value.ada(1)))

    private def witnesses(script: Script, redeemers: Option[Redeemers]): TransactionWitnessSet =
        TransactionWitnessSet(
          scripts = Seq(script),
          redeemers = redeemers,
          vkeyWitnesses = Set.empty,
          plutusData = Seq(datum)
        )

    private def redeemer(tag: RedeemerTag, exUnits: ExUnits): Option[Redeemers] =
        Some(Redeemers(Redeemer(tag, 0, datum, exUnits)))

    private def withdrawalOf(hash: ScriptHash): Option[Withdrawals] =
        Some(Withdrawals(SortedMap(scriptRewardAccount(hash) -> Coin.zero)))

    private def votingOf(hash: ScriptHash): Option[VotingProcedures] =
        Some(VotingProcedures(SortedMap(Voter.DRepScript(hash) -> SortedMap.empty)))

    private def proposalOf(hash: ScriptHash): TaggedOrderedSet[ProposalProcedure] =
        TaggedOrderedSet(
          proposal(GovAction.TreasuryWithdrawals(Map.empty, Some(hash)), rewardAccount(1))
        )

    test("ExactSetOfRedeemersValidator success with no scripts") {
        val transaction = txSpending(input(0))(TransactionWitnessSet.empty)
        val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator success with matching spend redeemer") {
        val utxo = Map(scriptUtxo(input(0), v1Script.scriptHash))
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Spend, exUnits)))
            val result = validate(ExactSetOfRedeemersValidator, utxo, transaction)
            assert(result.isRight)
        }
    }

    test("ExactSetOfRedeemersValidator failure with missing spend redeemer") {
        val utxo = Map(scriptUtxo(input(0), v1Script.scriptHash))
        // Create a redeemer for a different index to test missing redeemer
        val wrongRedeemer = Redeemer(
          tag = RedeemerTag.Spend,
          index = 1, // Wrong index!
          data = datum,
          exUnits = ExUnits(100000, 100000)
        )
        val transaction =
            txSpending(input(0))(witnesses(v1Script, Some(Redeemers.from(Seq(wrongRedeemer)))))
        val result = validate(ExactSetOfRedeemersValidator, utxo, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra spend redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Spend, exUnits)))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isLeft)
        }
    }

    test("ExactSetOfRedeemersValidator success with matching mint redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v2Script, redeemer(RedeemerTag.Mint, exUnits)))
                    .withBody(_.copy(mint = Some(mintOne(v2Script.scriptHash))))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isRight)
        }
    }

    test("ExactSetOfRedeemersValidator failure with missing mint redeemer") {
        val transaction =
            txSpending(input(0))(witnesses(v2Script, None))
                .withBody(_.copy(mint = Some(mintOne(v2Script.scriptHash))))
        val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra mint redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v2Script, redeemer(RedeemerTag.Mint, exUnits)))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isLeft)
        }
    }

    test("ExactSetOfRedeemersValidator success with matching Cert redeemer") {
        val certificates = TaggedOrderedStrictSet[Certificate](
          Certificate.UnregCert(Credential.ScriptHash(v1Script.scriptHash), None)
        )
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Cert, exUnits)))
                    .withBody(_.copy(certificates = certificates))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isRight)
        }
    }

    test("ExactSetOfRedeemersValidator failure with missing Cert redeemer") {
        val certificates = TaggedOrderedStrictSet[Certificate](
          Certificate.UnregCert(Credential.ScriptHash(v1Script.scriptHash), None)
        )
        val transaction =
            txSpending(input(0))(witnesses(v1Script, None))
                .withBody(_.copy(certificates = certificates))
        val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra Cert redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Cert, exUnits)))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isLeft)
        }
    }

    test("ExactSetOfRedeemersValidator success with matching Reward redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Reward, exUnits)))
                    .withBody(_.copy(withdrawals = withdrawalOf(v1Script.scriptHash)))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isRight)
        }
    }

    test("ExactSetOfRedeemersValidator failure with missing Reward redeemer") {
        val transaction =
            txSpending(input(0))(witnesses(v1Script, None))
                .withBody(_.copy(withdrawals = withdrawalOf(v1Script.scriptHash)))
        val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra Reward redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Reward, exUnits)))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isLeft)
        }
    }

    test("ExactSetOfRedeemersValidator success with matching Voting redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Voting, exUnits)))
                    .withBody(_.copy(votingProcedures = votingOf(v1Script.scriptHash)))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isRight)
        }
    }

    test("ExactSetOfRedeemersValidator failure with missing Voting redeemer") {
        val transaction =
            txSpending(input(0))(witnesses(v1Script, None))
                .withBody(_.copy(votingProcedures = votingOf(v1Script.scriptHash)))
        val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra Voting redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Voting, exUnits)))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isLeft)
        }
    }

    test("ExactSetOfRedeemersValidator success with matching Proposing redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Proposing, exUnits)))
                    .withBody(_.copy(proposalProcedures = proposalOf(v1Script.scriptHash)))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isRight)
        }
    }

    test("ExactSetOfRedeemersValidator failure with missing Proposing redeemer") {
        val transaction =
            txSpending(input(0))(witnesses(v1Script, None))
                .withBody(_.copy(proposalProcedures = proposalOf(v1Script.scriptHash)))
        val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra Proposing redeemer") {
        forAll(genExUnits) { exUnits =>
            val transaction =
                txSpending(input(0))(witnesses(v1Script, redeemer(RedeemerTag.Proposing, exUnits)))
            val result = validate(ExactSetOfRedeemersValidator, keyUtxo, transaction)
            assert(result.isLeft)
        }
    }

    test("ExactSetOfRedeemersValidator success with native scripts (no redeemers needed)") {
        val nativeScript = Timelock.Signature(Alice.addrKeyHash)
        val utxo = Map(scriptUtxo(input(0), nativeScript.scriptHash))
        val transaction = txSpending(input(0))(
          TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript)),
            redeemers = None
          )
        ).withBody(_.copy(mint = Some(mintOne(nativeScript.scriptHash))))
        val result = validate(ExactSetOfRedeemersValidator, utxo, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator success with mixed native and Plutus scripts") {
        val nativeScript = Timelock.Signature(Alice.addrKeyHash)
        val utxo = Map(
          scriptUtxo(input(1), nativeScript.scriptHash),
          scriptUtxo(input(2), v2Script.scriptHash)
        )
        // The inputs are sorted: input(2), at the Plutus script, has index 1.
        val plutusInputIndex = 1
        forAll(genExUnits) { exUnits =>
            val transaction = txSpending(input(1), input(2))(
              TransactionWitnessSet(
                scripts = Seq(Script.Native(nativeScript), v2Script),
                redeemers = Some(
                  Redeemers(Redeemer(RedeemerTag.Spend, plutusInputIndex, datum, exUnits))
                ),
                vkeyWitnesses = Set.empty,
                plutusData = Seq(datum)
              )
            )
            val result = validate(ExactSetOfRedeemersValidator, utxo, transaction)
            assert(result.isRight)
        }
    }
}
