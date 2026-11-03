package scalus.testing.conformance

import io.bullet.borer.*
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{CardanoMutator, Context, UtxoEnv}
import scalus.testing.conformance.LedgerState.*
import scalus.utils.Hex

import java.io.{BufferedInputStream, FileInputStream}
import java.nio.file.Path
import java.security.MessageDigest
import java.time.Instant
import scala.collection.mutable
import scala.util.{Failure, Success, Try, Using}

/** The Dugite LEDGER dumps, spec [CV-21] to [CV-25]: each case is one LEDGER transition of the
  * Conway Imp tests at PV11, the environment (`env`), the `[certState, utxoState]` before it
  * (`st`), the tx (`sig`) and, when the ledger accepts the tx, the state after it (`st_out`). The
  * provenance is in `ledger-vectors-pv11.README.md` beside the vendored sample.
  */
object DugiteLedgerVectors {

    /** The files of one case; `stOut` is absent when the ledger rejects the tx. */
    final case class Case(
        name: String,
        env: Array[Byte],
        st: Array[Byte],
        sig: Array[Byte],
        stOut: Option[Array[Byte]]
    )

    /** The result of a run over an archive.
      *
      * @param names
      *   the case names
      * @param rejected
      *   the number of cases without `st_out`
      * @param distinct
      *   the number of distinct `(env, st, sig)` triples
      * @param differing
      *   the cases where Scalus differs from the ledger, with the difference
      * @param rejectedBy
      *   the cases that Scalus rejects as the ledger does, with the ledger name of the first
      *   failure Scalus reports
      */
    final case class Summary(
        names: Set[String],
        rejected: Int,
        distinct: Int,
        differing: Map[String, String],
        rejectedBy: Map[String, String]
    ) {
        def cases: Int = names.size
    }

    /** The `SlotConfig` of Imp's genesis, spec [CV-23a]: system start 2017-09-23T21:44:51Z, slot
      * length 1 s, epoch length 4 320 (shelley testlib
      * Test/Cardano/Ledger/Shelley/ImpTest.hs:806-814).
      */
    val impGenesisSlotConfig: SlotConfig = SlotConfig(
      zeroTime = Instant.parse("2017-09-23T21:44:51Z").toEpochMilli,
      zeroSlot = 0,
      slotLength = 1000,
      epochLength = 4320,
      zeroEpoch = 0
    )

    /** `[slot, [epoch], txIx, pparams, [treasury, reserves]]`, the `LedgerEnv` record (shelley
      * Rules/Ledger.hs:101-125), with the epoch a `Maybe`.
      */
    final case class Env(slot: SlotNo, epoch: Option[Long], params: ProtocolParams, treasury: Coin)

    def readEnv(cbor: Array[Byte]): Env = {
        given Decoder[Env] = r => {
            r.readArrayHeader(5)
            val slot = r.readLong()
            val epoch = r.readArrayHeader() match
                case 0 => None
                case 1 => Some(r.readLong())
                case n => r.validationFailure(s"expected a Maybe EpochNo, got $n elements")
            r.readInt() // txIx
            val params = r.read[ConwayProtocolParams]().toProtocolParams
            r.readArrayHeader(2) // ChainAccountState, K/State/ChainAccount.hs:46-48
            val treasury = r.read[Coin]()
            r.read[Coin]() // reserves
            Env(slot, epoch, params, treasury)
        }
        Cbor.decode(cbor).to[Env].value
    }

    /** Runs every LEDGER case of `archive` and checks it, spec [CV-22] to [CV-24]. */
    def run(archive: Path): Summary = {
        val names = mutable.Set.empty[String]
        val triples = mutable.Set.empty[String]
        val differing = mutable.Map.empty[String, String]
        val rejectedBy = mutable.Map.empty[String, String]
        var rejected = 0
        foreachCase(archive) { c =>
            require(names.add(c.name), s"case ${c.name} is split in the archive")
            triples += tripleKey(c)
            if c.stOut.isEmpty then rejected += 1
            Try(check(c)) match
                case Success(Right(Some(failure))) => rejectedBy(c.name) = failure
                case Success(Right(None))          => ()
                case Success(Left(difference))     => differing(c.name) = difference
                case Failure(e)                    => differing(c.name) = s"harness error: $e"
        }
        Summary(names.toSet, rejected, triples.size, differing.toMap, rejectedBy.toMap)
    }

    /** Checks one case, spec [CV-23a], [CV-24]: `Left` with the difference between Scalus and the
      * ledger, else `Right` with the ledger name of the first failure when both reject the tx.
      */
    def check(c: Case): Either[String, Option[String]] = {
        val env = readEnv(c.env)
        val epoch = impGenesisSlotConfig.epochOf(env.slot)
        if !env.epoch.contains(epoch) then
            return Left(s"slot ${env.slot} is in epoch $epoch, env says ${env.epoch}")
        val state = LedgerState.fromCbor(c.st).ruleState
        val tx = Transaction.fromCbor(c.sig)
        // The default evaluator mode, Validate: the dumps come from a ledger run, which enforces
        // the script budgets. ImpTest.hs:808 sets the network to Testnet.
        val context = Context(
          env = UtxoEnv(env.slot, env.params, state.certState, Network.Testnet, env.treasury),
          slotConfig = impGenesisSlotConfig
        )
        (CardanoMutator.transit(context, state, tx), c.stOut) match
            case (Left(e), None)  => Right(Some(TransactionException.ledgerNames(e).head))
            case (Right(_), None) => Left("the ledger rejects the tx, Scalus accepts it")
            case (Left(e), Some(_)) =>
                Left(s"the ledger accepts the tx, Scalus rejects it: $e")
            case (Right(actual), Some(stOut)) =>
                val mismatches = LedgerStateComparison.withoutExcluded(
                  c.name,
                  LedgerStateComparison.compare(LedgerState.fromCbor(stOut), actual),
                  LedgerStateComparison.exclusions
                )
                if mismatches.isEmpty then Right(None)
                else Left(mismatches.map(m => s"${m.field}: ${m.detail}").mkString("; "))
    }

    /** The sha256 of the `env`, `st` and `sig` hashes, the identity of a case, spec [CV-22a]. */
    private def tripleKey(c: Case): String = {
        def sha(bytes: Array[Byte]) = MessageDigest.getInstance("SHA-256").digest(bytes)
        Hex.bytesToHex(sha(sha(c.env) ++ sha(c.st) ++ sha(c.sig)))
    }

    /** Streams the LEDGER cases of a gzip tar, one case in memory at a time, spec [CV-22]. The
      * files of a case are adjacent in the tar. A `ctx` entry is skipped unread, and the other rule
      * directories too.
      */
    def foreachCase(archive: Path)(f: Case => Unit): Unit =
        Using.resource(
          TarArchiveInputStream(
            GzipCompressorInputStream(BufferedInputStream(FileInputStream(archive.toFile)))
          )
        ) { tar =>
            var name = ""
            val files = mutable.Map.empty[String, Array[Byte]]
            def flush(): Unit =
                if name.nonEmpty then
                    def file(kind: String) =
                        files.getOrElse(kind, sys.error(s"case $name has no $kind file"))
                    f(Case(name, file("env"), file("st"), file("sig"), files.get("st_out")))
                    files.clear()
            var entry = tar.getNextEntry
            while entry != null do {
                entry.getName.stripPrefix("./").split('/') match
                    case Array("LEDGER", caseName, s"conformance_dump_$kind.cbor")
                        if entry.isFile && kind != "ctx" =>
                        if caseName != name then
                            flush()
                            name = caseName
                        files(kind) = tar.readAllBytes()
                    case _ => ()
                entry = tar.getNextEntry
            }
            flush()
        }
}
