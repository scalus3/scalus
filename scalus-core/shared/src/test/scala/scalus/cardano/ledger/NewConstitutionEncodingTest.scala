package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.{<=>, ===, List as PList, Option as POption}
import scalus.cardano.onchain.plutus.v3
import scalus.compiler.{compile, Compile}
import scalus.toUplc
import scalus.uplc.{Constant, Term}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.{PlutusVM, Result}

@Compile
object NewConstitutionScript {

    /** The guardrails script hash a script reads from a `NewConstitution`, empty if it has none. */
    def guardrails(action: Data): ByteString =
        action.to[v3.GovernanceAction] match
            case v3.GovernanceAction.NewConstitution(_, constitution) =>
                constitution.constitutionScript.getOrElse(ByteString.empty)
            case _ => prelude.fail("not a NewConstitution")

    /** Whether a node-encoded action is the `NewConstitution` a script builds for `hash`. */
    def isNewConstitution(action: Data, hash: ByteString): Boolean = {
        val expected: v3.GovernanceAction = v3.GovernanceAction.NewConstitution(
          prelude.Option.None,
          v3.Constitution(prelude.Option.Some(hash))
        )
        val actual = action.to[v3.GovernanceAction]
        actual === expected && (actual <=> expected).isEqual && expected.toData === action
    }
}

/** A V3 `NewConstitution` carries the `Constitution` data type, a one-field constructor around
  * `Maybe ScriptHash`, not the `Maybe` itself: PlutusLedgerApi/V3/Contexts.hs declares
  * `newtype Constitution` with `makeIsDataSchemaIndexed ''Constitution [('Constitution, 0)]`, and
  * the ledger builds `PV3.Constitution (transGovPolicy govPolicy)` (Conway/TxInfo.hs).
  */
class NewConstitutionEncodingTest extends AnyFunSuite {
    private val hash = ByteString.fromHex("07" * 28)
    private val none = Data.Constr(1, PList.Nil)
    private def just(d: Data) = Data.Constr(0, PList(d))

    /** `NewConstitution Nothing (Constitution script)` as a node encodes it. */
    private def newConstitution(script: Data) =
        Data.Constr(5, PList(none, Data.Constr(0, PList(script))))

    private val anchor = Anchor("https://example.org", Hash(ByteString.fromHex("00" * 32)))

    test("NewConstitution wraps the script hash in the Constitution constructor") {
        val withScript: v3.GovernanceAction =
            v3.GovernanceAction.NewConstitution(POption.None, v3.Constitution(POption.Some(hash)))
        val withoutScript: v3.GovernanceAction =
            v3.GovernanceAction.NewConstitution(POption.None, v3.Constitution(POption.None))
        assert(withScript.toData == newConstitution(just(Data.B(hash))))
        assert(withoutScript.toData == newConstitution(none))
        assert(newConstitution(just(Data.B(hash))).to[v3.GovernanceAction] == withScript)
        assert(newConstitution(none).to[v3.GovernanceAction] == withoutScript)
    }

    test("the ledger translation wraps the script hash in the Constitution constructor") {
        val withScript = GovAction.NewConstitution(None, Constitution(anchor, Some(Hash(hash))))
        val withoutScript = GovAction.NewConstitution(None, Constitution(anchor, None))
        assert(
          LedgerToPlutusTranslation.getGovernanceActionV3(withScript).toData ==
              newConstitution(just(Data.B(hash)))
        )
        assert(
          LedgerToPlutusTranslation.getGovernanceActionV3(withoutScript).toData ==
              newConstitution(none)
        )
    }

    test("a compiled script reads the script hash of a node-encoded NewConstitution") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val script = compile { NewConstitutionScript.guardrails }.toUplc()
        def guardrails(action: Data): ByteString =
            (script $ Term.Const(Constant.Data(action))).evaluateDebug match
                case Result.Success(Term.Const(Constant.ByteString(bytes), _), _, _, _) => bytes
                case other => fail(s"unexpected result: $other")
        assert(guardrails(newConstitution(just(Data.B(hash)))) == hash)
        assert(guardrails(newConstitution(none)) == ByteString.empty)
    }

    test("a NewConstitution built by a compiled script equals the node-encoded one") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val script = compile { NewConstitutionScript.isNewConstitution }.toUplc()
        def isNewConstitution(action: Data, hash: ByteString): Boolean =
            val applied =
                script $ Term.Const(Constant.Data(action)) $ Term.Const(Constant.ByteString(hash))
            applied.evaluateDebug match
                case Result.Success(Term.Const(Constant.Bool(value), _), _, _, _) => value
                case other => fail(s"unexpected result: $other")
        val otherHash = ByteString.fromHex("08" * 28)
        assert(isNewConstitution(newConstitution(just(Data.B(hash))), hash))
        assert(!isNewConstitution(newConstitution(just(Data.B(hash))), otherHash))
        assert(!isNewConstitution(newConstitution(none), hash))
    }
}
