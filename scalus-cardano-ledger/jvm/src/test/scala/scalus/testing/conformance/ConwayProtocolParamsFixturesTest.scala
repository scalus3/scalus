package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite

class ConwayProtocolParamsFixturesTest extends AnyFunSuite {
    private val current = "11" * 32
    private val previous = "22" * 32

    private def array(fields: String*): String = {
        require(fields.size < 24)
        (0x80 + fields.size).toHexString + fields.mkString
    }

    private def ledger(govState: String): String =
        array("80", array("a0", "00", "00", govState, "a0", "00"))

    private val conway = ledger(
      array(
        "80", // proposals
        "80", // committee
        "80", // constitution
        "5820" + current,
        "5820" + previous,
        "80", // future pparams
        "80" // DRep pulser
      )
    )

    for (hashes, order) <- List(
          (List(previous, current), "previous first"),
          (List(current, previous), "current first")
        )
    do
        test(s"current pparams wins with equal versions and $order") {
            assert(
              ConwayProtocolParamsFixtures.selectPparamsHash(conway, hashes, _ => Some(10)) ==
                  Some(current)
            )
        }

    test("current pparams does not depend on protocol version") {
        assert(
          ConwayProtocolParamsFixtures.selectPparamsHash(
            conway,
            List(previous, current),
            _ => fail("Conway selection must not inspect protocol versions")
          ) == Some(current)
        )
    }

    test("a missing current hash does not select the previous pparams") {
        assert(
          ConwayProtocolParamsFixtures
              .selectPparamsHash(conway, List(previous), _ => Some(10))
              .isEmpty
        )
    }

    test("states without a Conway GovState retain the highest-version search") {
        val older = ledger(array("5820" + current, "5820" + previous))
        assert(
          ConwayProtocolParamsFixtures.selectPparamsHash(
            older,
            List(previous, current),
            hash => Some(if hash == current then 10 else 9)
          ) == Some(current)
        )
    }

    test("the current hash of the SPO security-parameter vector resolves in the fixtures") {
        val name = "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.Voting." +
            "SPO needs to vote on security-relevant parameter changes"
        val (_, vector) = CardanoLedgerVectors.loadVector(name).find(_._1.toString == "16").get
        assert(vector.pparamsHash.isEmpty)
        assert(
          ConwayProtocolParamsFixtures.extractPparamsHash(
            vector.oldLedgerState,
            CardanoLedgerVectors.pparamsDir
          ) == Some("ff1d46602921ceae1224b46860441fc930921b0bca59dab496818fc7d05d8c54")
        )
    }
}
