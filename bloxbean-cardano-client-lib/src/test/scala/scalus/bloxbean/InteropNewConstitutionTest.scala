package scalus.bloxbean

import com.bloxbean.cardano.client.transaction.spec.governance.Constitution
import com.bloxbean.cardano.client.transaction.spec.governance.actions.NewConstitution
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** `Interop` must give a `NewConstitution` the encoding a node gives it: the script hash is wrapped
  * in the `Constitution` constructor, `Constr 0 [Maybe ScriptHash]`
  * (PlutusLedgerApi/V3/Contexts.hs).
  */
class InteropNewConstitutionTest extends AnyFunSuite {
    private val hash = "07" * 28
    private val none = Data.Constr(1, PList.Nil)
    private def just(d: Data) = Data.Constr(0, PList(d))

    /** `NewConstitution Nothing (Constitution script)` as a node encodes it. */
    private def newConstitution(script: Data) =
        Data.Constr(5, PList(none, Data.Constr(0, PList(script))))

    private def action(constitution: Constitution) =
        NewConstitution.builder().constitution(constitution).build()

    test("NewConstitution wraps the script hash in the Constitution constructor") {
        val withScript = action(Constitution.builder().scripthash(hash).build())
        val withoutScript = action(Constitution.builder().build())
        assert(
          Interop.getGovernanceActionV3(withScript).toData ==
              newConstitution(just(Data.B(ByteString.fromHex(hash))))
        )
        assert(Interop.getGovernanceActionV3(withoutScript).toData == newConstitution(none))
    }
}
