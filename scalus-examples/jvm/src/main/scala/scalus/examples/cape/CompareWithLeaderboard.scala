package scalus.examples.cape

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Ranks every submission per scenario by the total fee its metrics imply at mainnet prices.
  *
  * Total fee is what a script actually costs to use on-chain, and it is what CAPE's own
  * `metrics.json` reports (`lib/Cape/Protocol/Parameters.hs`):
  *
  *   - `execution_fee = ceil(memSum * 0.0577 + cpuSum * 0.0000721)` — one ceiling over the sums of
  *     the included evaluations (NOT a sum of per-case ceilings),
  *   - `reference_script_fee = floor(tiered(size))` — Conway tiered pricing, 15 lovelace/byte for
  *     the first 25 KiB tier, each next tier at 1.2x,
  *   - `total_fee = execution_fee + reference_script_fee`.
  *
  * Ranking by execution units alone is misleading in both directions: mainnet prices memory far
  * more heavily per unit than steps (so a CPU ranking lies about execution cost), and a statically
  * unrolled script can buy low ExUnits with hundreds of bytes of script that cost more in
  * reference-script fees than they save (so an execution-fee ranking lies about total cost). The
  * fees are recomputed from the raw per-evaluation units and `script_size_bytes` rather than
  * trusting the `*_fee_lovelace` fields, so stale metrics can't skew the ranking; exec/mem ranks
  * are shown alongside.
  */
@main def CompareWithLeaderboard(args: String*): Unit = {
    val capeRepo = Path.of(
      args.headOption.getOrElse(
        sys.error("usage: CompareWithLeaderboard <cape-repo-dir> [--strict]")
      )
    )
    val strict = args.contains("--strict")
    // Which rows are OURS. Other contributors also submit Scalus-compiled programs, so matching on
    // the compiler name alone would credit us with their rankings (and hide ours behind them).
    val ourHandle = args
        .sliding(2)
        .collectFirst { case Seq("--ours", h) => h }
        .getOrElse("_nau")
    val subs = capeRepo.resolve("submissions")
    var scalusBehind = false

    /** CAPE's execution fee: one ceiling over the summed units. */
    def executionFee(mem: Long, cpu: Long): Long =
        (BigDecimal(mem) * BigDecimal("0.0577") + BigDecimal(cpu) * BigDecimal("0.0000721"))
            .setScale(0, BigDecimal.RoundingMode.CEILING)
            .toLong

    /** CAPE's Conway tiered reference-script fee: 15 lovelace/byte, 25 KiB tiers, 1.2x per tier. */
    def referenceScriptFee(size: Long): Long = {
        val tierSize = 25L * 1024
        @annotation.tailrec
        def go(acc: BigDecimal, price: BigDecimal, remaining: Long): BigDecimal =
            if remaining < tierSize then acc + price * remaining
            else go(acc + price * tierSize, price * BigDecimal("1.2"), remaining - tierSize)
        go(BigDecimal(0), BigDecimal(15), size).setScale(0, BigDecimal.RoundingMode.FLOOR).toLong
    }

    final case class Row(name: String, total: Long, exec: Long, ref: Long, mem: Long, cpu: Long)

    val scenarios = Files
        .list(subs)
        .iterator
        .asScala
        .toSeq
        .filter(p => Files.isDirectory(p) && p.getFileName.toString != "TEMPLATE")
        .sortBy(_.getFileName.toString)

    for scenario <- scenarios do {
        val rows = Files
            .list(scenario)
            .iterator
            .asScala
            .toSeq
            .map(_.resolve("metrics.json"))
            .filter(Files.exists(_))
            .map { mf =>
                val m = ujson.read(Files.readString(mf))
                val evs = m("evaluations").arr.filter(_("included_in_aggregates").bool)
                val mem = evs.map(_("memory_units").num.toLong).sum
                val cpu = evs.map(_("cpu_units").num.toLong).sum
                val size = m("measurements")("script_size_bytes").num.toLong
                val exec = executionFee(mem, cpu)
                val ref = referenceScriptFee(size)
                Row(
                  name = mf.getParent.getFileName.toString,
                  total = exec + ref,
                  exec = exec,
                  ref = ref,
                  mem = mem,
                  cpu = cpu
                )
            }
            .sortBy(r => (r.total, r.exec))

        // Execution-fee and memory ranks per submission, so the total-fee-ordered table also shows
        // who is leanest to execute and on the unit mainnet prices most heavily.
        val execRank = rows.sortBy(_.exec).map(_.name).zipWithIndex.toMap
        val memRank = rows.sortBy(_.mem).map(_.name).zipWithIndex.toMap

        println(s"== ${scenario.getFileName}")
        for (row, i) <- rows.zipWithIndex do
            println(
              f"  ${i + 1}%2d. ${row.name}%-45s total=${row.total}%,9d (exec=${row.exec}%,8d" +
                  f" #${execRank(row.name) + 1}%d + ref=${row.ref}%,7d) mem=${row.mem}%,12d " +
                  f"(mem #${memRank(row.name) + 1}%d) cpu=${row.cpu}%,15d"
            )

        rows.headOption.foreach { leader =>
            rows.find(_.name.contains(ourHandle)) match
                case Some(us) if leader.total < us.total =>
                    scalusBehind = true
                    val feePct = (us.total - leader.total) * 100.0 / leader.total
                    val execPct = (us.exec - leader.exec) * 100.0 / leader.exec
                    val exec =
                        if execPct <= 0 then f"${-execPct}%.1f%% less exec"
                        else f"$execPct%.1f%% more exec"
                    println(f"  -> BEHIND ${leader.name} by $feePct%.1f%% total fee ($exec)")
                case Some(us) =>
                    val margin = rows
                        .find(_.total > us.total)
                        .map(r => f" by ${(r.total - us.total) * 100.0 / r.total}%.1f%% total fee")
                        .getOrElse("")
                    val tied =
                        if leader.name != us.name then s" (tied with ${leader.name})" else ""
                    println(s"  -> LEADS$margin$tied (exec #${execRank(us.name) + 1})")
                case None =>
                    scalusBehind = true
                    println(s"  -> NO submission matching '$ourHandle'")
        }
    }
    if strict && scalusBehind then sys.exit(1)
}
