package scalus.testing.conformance

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.conformance.CardanoLedgerVectors.*
import scalus.utils.Hex

/** Cardano Ledger Conformance Test Suite
  *
  * Runs conformance tests from cardano-ledger test vectors to validate Scalus ledger implementation
  * against reference implementation.
  */
class CardanoLedgerConformanceTest extends AnyFunSuite {
    
    val TestTag = Tag("conformance")

    test("MetadataValidator Conway.Imp.AllegraImpSpec.UTXOW.InvalidMetadata", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName = "Conway.Imp.AllegraImpSpec.UTXOW.InvalidMetadata"
        val vectorPath = conformanceVectorsPath.resolve("conway/impl/dump").resolve(vectorName)
        for case (path, vector) <- loadAllVectors(vectorPath) do {
            val ledgerState = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
            println(pprint(ledgerState))
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            println(pprint(transaction))
            val validation = MetadataValidator.validate(
              Context.testMainnet(),
              ledgerState.ruleState,
              transaction
            )
            println(pprint(validation))
            assert(vector.success === validation.isRight)
        }
    }

    test(
      "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.Voting.Active voting stake.StakePool.Proposal deposits contribute to active voting stake.After switching delegations/9",
      TestTag
    ) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName =
            "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.Voting.Active voting stake.StakePool.Proposal deposits contribute to active voting stake.After switching delegations/9"
        val vectorPath = conformanceVectorsPath.resolve("conway/impl/dump").resolve(vectorName)
        for case (path, vector) <- loadAllVectors(vectorPath) do {
            val ledgerState = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
            println(pprint(ledgerState))
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            println(pprint(transaction))
        }
    }

}
