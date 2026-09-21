package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.{Constant, Program, Term}

/** Which protocol version introduces each Plutus ledger language. Pinned to Plutus
  * `ledgerLanguageIntroducedIn` (plutus-ledger-api `Versions.hs`): PlutusV4 arrives with the
  * Dijkstra hard fork (PV12), not with van Rossem (PV11).
  */
class LanguageTest extends AnyFunSuite {

    test("introducedInVersion follows ledgerLanguageIntroducedIn") {
        assert(Language.PlutusV1.introducedInVersion == MajorProtocolVersion.alonzoPV)
        assert(Language.PlutusV2.introducedInVersion == MajorProtocolVersion.vasilPV)
        assert(Language.PlutusV3.introducedInVersion == MajorProtocolVersion.changPV)
        assert(Language.PlutusV4.introducedInVersion == MajorProtocolVersion.dijkstraPV)
    }

    test("a PlutusV4 script is well-formed from Dijkstra on, not before") {
        val script = Program((1, 1, 0), Term.Const(Constant.Unit)).cborByteString
        assert(
          !PlutusScript.isWellFormed(script, Language.PlutusV4, MajorProtocolVersion.vanRossemPV)
        )
        assert(
          PlutusScript.isWellFormed(script, Language.PlutusV4, MajorProtocolVersion.dijkstraPV)
        )
    }

    test("PlutusV4 builtins are introduced at Dijkstra") {
        assert(
          Builtins
              .findBuiltinsIntroducedIn(Language.PlutusV4, MajorProtocolVersion.vanRossemPV)
              .isEmpty
        )
        val atDijkstra =
            Builtins.findBuiltinsIntroducedIn(Language.PlutusV4, MajorProtocolVersion.dijkstraPV)
        assert(
          atDijkstra == Builtins.findBuiltinsIntroducedIn(
            Language.PlutusV3,
            MajorProtocolVersion.vanRossemPV
          )
        )
    }
}
