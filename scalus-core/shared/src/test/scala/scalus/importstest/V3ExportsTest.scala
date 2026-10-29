package scalus.importstest

// Reaches every v3 re-export through `import v3.*` only: OutputDatum, ByteString, Data,
// FromData, ToData, Compile and the exported Credential. Removing one fails compilation.
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.builtin.ByteString.hex

case class Lock(owner: PubKeyHash, tag: ByteString) derives FromData, ToData

@Compile
object ExportsUse {
    def datumHash(d: OutputDatum): Option[ByteString] = d match
        case OutputDatum.OutputDatumHash(h) => Option.Some(h)
        case _                              => Option.None

    def owner(c: Credential): Option[PubKeyHash] = c match
        case Credential.PubKeyCredential(pkh) => Option.Some(pkh)
        case Credential.ScriptCredential(_)   => Option.None

    def tagOf(d: Data): ByteString = d.to[Lock].tag

    // A case type written through the v3 path needs its companion's Eq. Through an `export`
    // forwarder, the given search finds that Eq twice and fails as ambiguous.
    def samePayee(a: ByteString, b: ByteString): Boolean = {
        val x: Credential.PubKeyCredential = Credential.PubKeyCredential(PubKeyHash(a))
        val y: Credential.PubKeyCredential = Credential.PubKeyCredential(PubKeyHash(b))
        AssocMap.empty[Credential.PubKeyCredential, BigInt].insert(x, 1).get(y).isDefined
    }

    def sameDatumHash(a: ByteString, b: ByteString): Boolean = {
        val x: OutputDatum.OutputDatumHash = OutputDatum.OutputDatumHash(a)
        val y: OutputDatum.OutputDatumHash = OutputDatum.OutputDatumHash(b)
        AssocMap.empty[OutputDatum.OutputDatumHash, BigInt].insert(x, 1).get(y).isDefined
    }

    def sameBound(a: BigInt, b: BigInt): Boolean = {
        val x: IntervalBoundType.Finite = IntervalBoundType.Finite(a)
        val y: IntervalBoundType.Finite = IntervalBoundType.Finite(b)
        AssocMap.empty[IntervalBoundType.Finite, BigInt].insert(x, 1).get(y).isDefined
    }

    def sameStake(a: ByteString, b: ByteString): Boolean = {
        val x: StakingCredential.StakingHash =
            StakingCredential.StakingHash(Credential.PubKeyCredential(PubKeyHash(a)))
        val y: StakingCredential.StakingHash =
            StakingCredential.StakingHash(Credential.PubKeyCredential(PubKeyHash(b)))
        AssocMap.empty[StakingCredential.StakingHash, BigInt].insert(x, 1).get(y).isDefined
    }

    def sameRetire(a: BigInt, b: BigInt): Boolean = {
        val x: DCert.PoolRetire = DCert.PoolRetire(PubKeyHash(hex"bb"), a)
        val y: DCert.PoolRetire = DCert.PoolRetire(PubKeyHash(hex"bb"), b)
        AssocMap.empty[DCert.PoolRetire, BigInt].insert(x, 1).get(y).isDefined
    }

    def check(lock: Data): Boolean =
        datumHash(OutputDatum.OutputDatumHash(hex"aa")) === Option.Some(hex"aa") &&
            datumHash(OutputDatum.NoOutputDatum).isEmpty &&
            owner(Credential.PubKeyCredential(PubKeyHash(hex"bb"))) === Option.Some(
              PubKeyHash(hex"bb")
            ) &&
            tagOf(lock) === hex"cc" &&
            samePayee(hex"bb", hex"bb") && !samePayee(hex"bb", hex"dd") &&
            sameDatumHash(hex"aa", hex"aa") && !sameDatumHash(hex"aa", hex"dd") &&
            sameBound(1, 1) && !sameBound(1, 2) &&
            sameStake(hex"bb", hex"bb") && !sameStake(hex"bb", hex"dd") &&
            sameRetire(1, 1) && !sameRetire(1, 2)
}

class V3ExportsTest extends org.scalatest.funsuite.AnyFunSuite {
    import scalus.toUplc
    import scalus.compiler.{compile, Options}
    import scalus.compiler.sir.TargetLoweringBackend
    import scalus.uplc.Term
    import scalus.uplc.Term.asTerm
    import scalus.uplc.builtin.Data.toData
    import scalus.uplc.eval.{PlutusVM, Result}

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    private val lock = Lock(PubKeyHash(hex"bb"), hex"cc").toData

    test("v3 re-exports give the same result on the JVM and on-chain") {
        assert(ExportsUse.check(lock))
        val term = compile(ExportsUse.check).toUplc() $ Term.Const(
          scalus.uplc.Constant.Data(lock)
        )
        term.evaluateDebug match
            case Result.Success(result, _, _, _) => assert(result == true.asTerm)
            case other                           => fail(s"unexpected result: $other")
    }
}
