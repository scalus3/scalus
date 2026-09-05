package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Language
import scalus.compiler.{Compile, Options}
import scalus.compiler.sir.{SIR, TargetLoweringBackend}
import scalus.compiler.sir.lowering.UplcPipeline
import scalus.cardano.onchain.plutus.prelude.List
import scalus.uplc.builtin.Builtins.equalsInteger
import scalus.uplc.{PlutusV3, Program}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths

/** Whole-pipeline determinism on the real example corpus.
  *
  * Every example validator is compiled in this JVM and in a child JVM whose identity-hash algorithm
  * is different (`-XX:hashCode=2`: every identity hash is 1). Identity hashes are the only
  * realistic source of run-to-run drift in a single-threaded, non-random compiler, and the flag
  * perturbs every identity-keyed container at once, so one comparison covers the linker,
  * MutualRecursionElimination, BooleanOptimizer, StaticArgumentTransformation, the three lowerings
  * and both optimizer pipelines without knowing where the containers are.
  *
  * The synthetic-term guard for the pass that actually had this bug is
  * `scalus.uplc.transform.CseDeterminismCrossJvmTest`; the story is in
  * docs/internal/UPLC_OPTIMIZER_DETERMINISM.md.
  */
class PipelineDeterminismCrossJvmTest extends AnyFunSuite {

    test(
      "every example validator compiles to the same bytes in a JVM with different identity hashes"
    ) {
        val expected = PipelineDeterminismChild.render()
        assert(expected.linesIterator.size > 40, "corpus unexpectedly small")

        val javaBin = Paths.get(System.getProperty("java.home"), "bin", "java").toString
        val cmd = java.util.List.of(
          javaBin,
          "-XX:+UnlockExperimentalVMOptions",
          "-XX:hashCode=2",
          "-Xss64m",
          "-cp",
          System.getProperty("java.class.path"),
          classOf[PipelineDeterminismChild.type].getName.stripSuffix("$")
        )
        val process = new ProcessBuilder(cmd).start()
        val out = new String(process.getInputStream.readAllBytes(), UTF_8)
        val err = new String(process.getErrorStream.readAllBytes(), UTF_8)
        val exit = process.waitFor()
        if exit != 0 && (err.contains("Unrecognized VM option") || err.contains("Could not create"))
        then cancel(s"this JVM does not support -XX:hashCode=2: ${err.linesIterator.next()}")
        assert(exit == 0, s"child JVM failed: $err")

        val differing = expected.linesIterator
            .zip(out.linesIterator)
            .collect {
                case (a, b) if a != b => a.split(' ').take(2).mkString(" ")
            }
            .toList
        assert(
          out == expected,
          s"compiled bytes differ in a JVM with -XX:hashCode=2 for: ${differing.mkString(", ")}"
        )
    }
}

/** A pure-ADT program for the legacy backends, which cannot lower the `Data` conversions the
  * example validators use but do encode sum types, pattern matching and recursion themselves.
  */
@Compile
object LegacyBackendSample {
    enum Shape:
        case Circle(r: BigInt)
        case Rect(w: BigInt, h: BigInt)

    def area(s: Shape): BigInt = s match
        case Shape.Circle(r)  => r * r * 3
        case Shape.Rect(w, h) => w * h

    def sumAreas(shapes: List[Shape], acc: BigInt): BigInt = shapes match
        case List.Nil           => acc
        case List.Cons(s, rest) => sumAreas(rest, acc + area(s))

    def validate(a: BigInt, b: BigInt): Boolean =
        equalsInteger(
          sumAreas(List.Cons(Shape.Circle(a), List.Cons(Shape.Rect(a, b), List.Nil)), 0),
          area(Shape.Rect(b, a)) + a * a * 3
        )
}

/** Compiles the corpus under several configurations and prints `name config hex` per program. Used
  * in-process and as the main class of the child JVM, so both sides run identical code.
  */
object PipelineDeterminismChild {

    /** The same list as BooleanOptimizerImpactTest, plus the CSE tie reproduction and the
      * legacy-backend sample.
      */
    private def corpus(using Options): Seq[(String, SIR)] = Seq(
      "HelloCardano" -> PlutusV3.compile(HelloCardano.validate).sir,
      "MembershipToken" -> PlutusV3.compile(MembershipTokenValidator.validate).sir,
      "Auction" -> PlutusV3.compile(scalus.examples.auction.AuctionValidator.validate).sir,
      "Allowlist" -> PlutusV3
          .compile(scalus.examples.bilinearAccumulator.AllowlistValidator.validate)
          .sir,
      "Amm" -> PlutusV3.compile(scalus.examples.amm.AmmValidator.validate).sir,
      "Betting" -> PlutusV3.compile(scalus.examples.betting.BettingValidator.validate).sir,
      "Crowdfunding" -> PlutusV3
          .compile(scalus.examples.crowdfunding.CrowdfundingValidator.validate)
          .sir,
      "DecentralizedIdentity" -> PlutusV3
          .compile(scalus.examples.decentralizedidentity.DecentralizedIdentityValidator.validate)
          .sir,
      "EditableNft" -> PlutusV3
          .compile(scalus.examples.editablenft.EditableNftValidator.validate)
          .sir,
      "Escrow" -> PlutusV3.compile(scalus.examples.escrow.EscrowValidator.validate).sir,
      "Htlc" -> PlutusV3.compile(scalus.examples.htlc.HtlcValidator.validate).sir,
      "LinearVesting" -> PlutusV3
          .compile(scalus.examples.cape.linearvesting.LinearVestingValidator.validate)
          .sir,
      "LinkedList" -> PlutusV3
          .compile(scalus.examples.linkedlist.LinkedListValidator.validate)
          .sir,
      "Lottery" -> PlutusV3.compile(scalus.examples.lottery.LotteryValidator.validate).sir,
      "NaivePaymentSplitter" -> PlutusV3
          .compile(scalus.examples.paymentsplitter.NaivePaymentSplitterValidator.validate)
          .sir,
      "OptimizedPaymentSplitter" -> PlutusV3
          .compile(scalus.examples.paymentsplitter.OptimizedPaymentSplitterValidator.validate)
          .sir,
      "Pricebet" -> PlutusV3.compile(scalus.examples.pricebet.PricebetValidator.validate).sir,
      "SimpleTransfer" -> PlutusV3
          .compile(scalus.examples.simpletransfer.SimpleTransferValidator.validate)
          .sir,
      "TwoPartyEscrow" -> PlutusV3
          .compile(scalus.examples.cape.twopartyescrow.TwoPartyEscrowValidator.validate)
          .sir,
      "Vault" -> PlutusV3.compile(scalus.examples.vault.VaultValidator.validate).sir,
      "Vesting" -> PlutusV3.compile(scalus.examples.vesting.VestingValidator.validate).sir,
      "UpgradeableProxy" -> PlutusV3
          .compile(scalus.examples.upgradeableproxy.ProxyValidator.validate)
          .sir,
      "SameScopeFieldChains" -> PlutusV3.compile(SameScopeFieldChains.validate).sir,
      "LegacyBackendSample" -> PlutusV3.compile(LegacyBackendSample.validate).sir
    )

    /** Run over the whole corpus: the production configuration, and one that turns on the passes
      * production leaves off (CCE, extra CSE rounds).
      */
    private val fullConfigs: Seq[(String, Options)] = Seq(
      "release" -> Options.releaseUntagged,
      "cce" -> Options.releaseUntagged.copy(cseIterations = 4, cceEnabled = true)
    )

    /** The V3 lowering targeting Plutus V2, which selects `V1V2Optimizer`. */
    private val v2Names =
        Set("HelloCardano", "Htlc", "Vesting", "SimpleTransfer", "LegacyBackendSample")

    /** The legacy backends, on the programs that do not need the V3-only intrinsics. */
    private val legacyNames = Set("SameScopeFieldChains", "LegacyBackendSample")
    private val legacyConfigs: Seq[(String, Options)] = Seq(
      "scott" -> Options.releaseUntagged.copy(
        targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering
      ),
      "sop" -> Options.releaseUntagged.copy(
        targetLoweringBackend = TargetLoweringBackend.SumOfProductsLowering
      )
    )

    private def hex(program: Program): String =
        program.cborEncoded.map(b => f"$b%02x").mkString

    private def lower(sir: SIR, opts: Options, language: Language): Program =
        Program.plutusV3(
          UplcPipeline.run(sir, opts, language, UplcPipeline.defaultOptimizer(language, opts))
        )

    def render(): String = {
        val sb = new StringBuilder
        val all = corpus(using Options.releaseUntagged)
        for (name, sir) <- all do
            for (cfg, opts) <- fullConfigs do
                sb.append(s"$name $cfg ${hex(lower(sir, opts, opts.targetLanguage))}\n")
            if v2Names.contains(name) then
                val opts = Options.releaseUntagged
                sb.append(s"$name v2 ${hex(lower(sir, opts, Language.PlutusV2))}\n")
            if legacyNames.contains(name) then
                for (cfg, opts) <- legacyConfigs do
                    sb.append(s"$name $cfg ${hex(lower(sir, opts, opts.targetLanguage))}\n")
        sb.toString
    }

    def main(args: Array[String]): Unit = {
        print(render())
        System.out.flush()
    }
}
