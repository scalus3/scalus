package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** A V3 `UpdateCommittee` must reach a script as the ledger builds it in `transGovAction`
  * (Conway/TxInfo.hs): the removed members through `Set.toList`, so in the ledger's
  * `Ord Credential` (scripts before keys, then by hash), the added members through `Map.toList`,
  * and the quorum through `transBoundedRational`, a Haskell `Ratio` and so in lowest terms.
  */
class UpdateCommitteeEncodingTest extends AnyFunSuite {
    private def hash(byte: String) = ByteString.fromHex(byte * 28)
    private def key(byte: String): Credential = Credential.KeyHash(Hash(hash(byte)))
    private def script(byte: String): Credential = Credential.ScriptHash(Hash(hash(byte)))

    private val none = Data.Constr(1, PList.Nil)
    private def keyData(byte: String) = Data.Constr(0, PList(Data.B(hash(byte))))
    private def scriptData(byte: String) = Data.Constr(1, PList(Data.B(hash(byte))))

    private def translate(
        removed: Set[Credential],
        added: Map[Credential, Long],
        threshold: UnitInterval
    ): Data = {
        LedgerToPlutusTranslation
            .getGovernanceActionV3(GovAction.UpdateCommittee(None, removed, added, threshold))
            .toData
    }

    /** `UpdateCommittee Nothing removed added quorum` as a node encodes it. */
    private def updateCommittee(
        removed: List[Data],
        added: List[(Data, Data)],
        quorum: (Int, Int)
    ): Data = {
        Data.Constr(
          4,
          PList(
            none,
            Data.List(PList.from(removed)),
            Data.Map(PList.from(added)),
            Data.Constr(0, PList(Data.I(quorum._1), Data.I(quorum._2)))
          )
        )
    }

    test("committee members are listed scripts first, then by hash, as the ledger iterates them") {
        // A Scala Set or Map of up to 4 elements iterates in insertion order, which here is not
        // the ledger's order. Hashes compare as unsigned bytes, so 0x81.. sorts after 0x01..
        val removed = Set(key("81"), script("81"), key("01"), script("01"))
        val added = Map(key("04") -> 10L, script("03") -> 20L)
        assert(
          translate(removed, added, UnitInterval(1, 2)) == updateCommittee(
            List(scriptData("01"), scriptData("81"), keyData("01"), keyData("81")),
            List(scriptData("03") -> Data.I(20), keyData("04") -> Data.I(10)),
            (1, 2)
          )
        )
    }

    test("the quorum is in lowest terms, however the transaction encodes it") {
        for (encoded, reduced) <- Seq((6, 10) -> (3, 5), (0, 5) -> (0, 1), (7, 7) -> (1, 1)) do
            val threshold = UnitInterval(encoded._1, encoded._2)
            assert(
              translate(Set.empty, Map.empty, threshold) == updateCommittee(Nil, Nil, reduced),
              s"$encoded"
            )
    }
}
