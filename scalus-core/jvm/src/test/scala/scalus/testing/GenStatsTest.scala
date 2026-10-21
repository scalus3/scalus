package scalus.testing

import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

import java.nio.file.{Files, Path}
import scala.collection.mutable

/** Measures the ScalaCheck generators: which constructors they produce, how big and deep the values
  * are, and how often edge values appear (spec `[P0-12]` to `[P0-16]` of
  * docs/superpowers/specs/2026-10-20-test-refactoring-design.md).
  *
  * Not a correctness test: it asserts nothing and writes
  * `docs/internal/test-refactor/gen-stats.md`. Tagged `scalus.testing.Benchmark`, so the default
  * test run leaves it out. Run it with:
  * `scalusJVM/testOnly scalus.testing.GenStatsTest -- -n scalus.testing.Benchmark`
  */
class GenStatsTest extends AnyFunSuite {
    import GenStatsTest.*

    test("generator statistics", Benchmark) {
        val rows = uplcRows ++ ledgerRows ++ onchainRows
        val out = Path.of("docs/internal/test-refactor/gen-stats.md")
        Files.createDirectories(out.getParent)
        Files.writeString(out, render(rows))
    }
}

object GenStatsTest {
    private object Benchmark extends Tag("scalus.testing.Benchmark")

    val Samples = 10_000

    final case class Row(
        group: String,
        name: String,
        samples: Int,
        discarded: Int,
        roots: Map[String, Int],
        kinds: Map[String, Int],
        nodes: Map[Int, Int],
        depths: Map[Int, Int],
        maxNodes: Int,
        maxDepth: Int,
        edges: Map[String, Int],
        millis: Long
    )

    /** Samples `Samples` values with the sizes that `forAll` uses (0 to 100) and a fixed seed. */
    def measure[A](group: String, name: String)(using arb: Arbitrary[A]): Row = {
        val roots, kinds, edges = mutable.Map.empty[String, Int].withDefaultValue(0)
        val nodes, depths = mutable.Map.empty[Int, Int].withDefaultValue(0)
        var discarded, maxNodes, maxDepth = 0
        val start = System.nanoTime()
        for i <- 0 until Samples do
            val params = Gen.Parameters.default.withSize(i % 101)
            arb.arbitrary(params, Seed(i.toLong)) match
                case None => discarded += 1
                case Some(value) =>
                    roots(constructor(value)) += 1
                    val seenKinds = mutable.Set.empty[String]
                    val seenEdges = mutable.Set.empty[String]
                    val (n, d) = walk(value, seenKinds, seenEdges)
                    seenKinds.foreach(k => kinds(k) += 1)
                    seenEdges.foreach(e => edges(e) += 1)
                    nodes(bucket(n)) += 1
                    depths(bucket(d)) += 1
                    maxNodes = maxNodes.max(n)
                    maxDepth = maxDepth.max(d)
        val millis = (System.nanoTime() - start) / 1_000_000
        Row(
          group,
          name,
          Samples,
          discarded,
          roots.toMap,
          kinds.toMap,
          nodes.toMap,
          depths.toMap,
          maxNodes,
          maxDepth,
          edges.toMap,
          millis
        )
    }

    /** The case name of an enum or case class value, else the runtime class name. */
    private def constructor(x: Any): String = x match
        case null       => "null"
        case p: Product => p.productPrefix
        case other      => other.getClass.getSimpleName

    /** Power-of-two bucket: 1, 2, 4, 8, ... (bucket b holds values in [b, 2b)). */
    private def bucket(n: Int): Int = if n <= 1 then n else Integer.highestOneBit(n)

    /** Returns (node count, depth) of a value and records the constructors and edge values in it.
      */
    private def walk(x: Any, kinds: mutable.Set[String], edges: mutable.Set[String]): (Int, Int) = {
        def children(xs: Iterator[Any]): (Int, Int) =
            xs.foldLeft((1, 1)) { case ((n, d), c) =>
                val (cn, cd) = walk(c, kinds, edges)
                (n + cn, d.max(cd + 1))
            }
        x match
            case null => edges += "null"; (1, 1)
            case b: BigInt =>
                if b == 0 then edges += "int 0"
                if b == 1 then edges += "int 1"
                if b == -1 then edges += "int -1"
                if b < 0 then edges += "int <0"
                if b.bitLength > 63 then edges += "int >64bit"
                (1, 1)
            case l: Long =>
                if l == 0 then edges += "long 0"
                if l < 0 then edges += "long <0"
                if l == Long.MaxValue || l == Long.MinValue then edges += "long min/max"
                (1, 1)
            case i: Int =>
                if i == 0 then edges += "int32 0"
                if i < 0 then edges += "int32 <0"
                if i == Int.MaxValue || i == Int.MinValue then edges += "int32 min/max"
                (1, 1)
            case bs: ByteString => bytes(bs.size, edges); (1, 1)
            case a: Array[Byte] => bytes(a.length, edges); (1, 1)
            case s: String      => if s.isEmpty then edges += "string empty"; (1, 1)
            case _: Boolean     => (1, 1)
            case it: Iterable[?] =>
                if it.isEmpty then edges += "collection empty"
                children(it.iterator)
            case p: Product =>
                kinds += p.productPrefix
                children(p.productIterator)
            case other =>
                kinds += other.getClass.getSimpleName
                (1, 1)
    }

    private def bytes(size: Int, edges: mutable.Set[String]): Unit =
        if size == 0 then edges += "bytes empty"
        if size == 28 then edges += "bytes 28"
        if size == 32 then edges += "bytes 32"
        if size == 64 then edges += "bytes 64"

    private def pct(n: Int, total: Int): String = f"${100.0 * n / total}%.1f%%"

    private def topCounts(m: Map[String, Int], total: Int, limit: Int): String = {
        val sorted = m.toSeq.sortBy(-_._2)
        val shown = sorted.take(limit).map((k, v) => s"$k ${pct(v, total)}").mkString(", ")
        if sorted.size > limit then s"$shown, … (${sorted.size} kinds)" else shown
    }

    private def histogram(m: Map[Int, Int], total: Int): String =
        m.toSeq.sortBy(_._1).map((b, v) => s"$b+: ${pct(v, total)}").mkString(", ")

    def render(rows: Seq[Row]): String = {
        val sb = new StringBuilder
        sb ++= "# Generator statistics\n\n"
        sb ++= s"Generated by `GenStatsTest`. $Samples samples per generator, sizes 0 to 100, seeds 0 to ${Samples - 1}.\n\n"
        sb ++= "- **roots**: share of each top-level constructor.\n"
        sb ++= "- **kinds**: share of samples that contain the constructor anywhere.\n"
        sb ++= "- **nodes / depth**: power-of-two histograms; `8+` means 8 to 15.\n"
        sb ++= "- **edges**: share of samples that contain the edge value anywhere.\n\n"
        for (group, groupRows) <- rows.groupBy(_.group).toSeq.sortBy(_._1) do
            sb ++= s"## $group\n\n"
            for r <- groupRows do
                val kept = r.samples - r.discarded
                sb ++= s"### ${r.name}\n\n"
                sb ++= s"- discarded: ${r.discarded}; time: ${r.millis} ms\n"
                if kept > 0 then
                    sb ++= s"- roots: ${topCounts(r.roots, kept, 12)}\n"
                    sb ++= s"- kinds: ${topCounts(r.kinds, kept, 25)}\n"
                    sb ++= s"- nodes: ${histogram(r.nodes, kept)}; max ${r.maxNodes}\n"
                    sb ++= s"- depth: ${histogram(r.depths, kept)}; max ${r.maxDepth}\n"
                    sb ++= s"- edges: ${topCounts(r.edges, kept, 20)}\n"
                sb ++= "\n"
        sb.toString
    }

    def uplcRows: Seq[Row] = {
        import scalus.uplc.{Constant, DefaultUni, Program, Term}
        import scalus.uplc.builtin.Data
        // uplc.test.ArbitraryInstances is a trait only; cardano.ledger.ArbitraryInstances extends it.
        import scalus.cardano.ledger.ArbitraryInstances.given
        val g = "uplc.test.ArbitraryInstances"
        Seq(
          measure[ByteString](g, "ByteString"),
          measure[Data](g, "Data"),
          measure[DefaultUni](g, "DefaultUni"),
          measure[Constant](g, "Constant"),
          measure[Term](g, "Term"),
          measure[Program](g, "Program")
        )
    }

    def ledgerRows: Seq[Row] = {
        import scalus.cardano.address.{Address, ShelleyAddress, StakeAddress}
        import scalus.cardano.ledger.*
        import scalus.cardano.ledger.ArbitraryInstances.given
        val g = "cardano.ledger.ArbitraryInstances"
        Seq(
          measure[Address](g, "Address"),
          measure[ShelleyAddress](g, "ShelleyAddress"),
          measure[StakeAddress](g, "StakeAddress"),
          measure[Credential](g, "Credential"),
          measure[Coin](g, "Coin"),
          measure[AssetName](g, "AssetName"),
          measure[MultiAsset](g, "MultiAsset"),
          measure[Value](g, "Value"),
          measure[ExUnits](g, "ExUnits"),
          measure[Script](g, "Script"),
          measure[Timelock](g, "Timelock"),
          measure[DatumOption](g, "DatumOption"),
          measure[TransactionOutput](g, "TransactionOutput"),
          measure[Metadatum](g, "Metadatum"),
          measure[Redeemers](g, "Redeemers"),
          measure[Certificate](g, "Certificate"),
          measure[GovAction](g, "GovAction"),
          measure[TransactionBody](g, "TransactionBody"),
          measure[Transaction](g, "Transaction")
        )
    }

    def onchainRows: Seq[Row] = {
        import scalus.cardano.onchain.plutus.v1.{Address, Interval, Value}
        import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
        import scalus.cardano.onchain.plutus.v3.{ScriptPurpose, TxInInfo, TxInfo, TxOutRef}
        import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.given
        val g = "onchain.plutus.v3.ArbitraryInstances"
        Seq(
          measure[Interval](g, "Interval"),
          measure[Address](g, "v1.Address"),
          measure[Value](g, "v1.Value"),
          measure[OutputDatum](g, "v2.OutputDatum"),
          measure[TxOut](g, "v2.TxOut"),
          measure[TxOutRef](g, "TxOutRef"),
          measure[TxInInfo](g, "TxInInfo"),
          measure[ScriptPurpose](g, "ScriptPurpose"),
          measure[TxInfo](g, "TxInfo")
        )
    }
}
