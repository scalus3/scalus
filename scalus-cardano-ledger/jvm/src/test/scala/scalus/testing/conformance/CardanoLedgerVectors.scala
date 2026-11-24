package scalus.testing.conformance

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.utils.Hex

import java.io.{FileInputStream, FileOutputStream}
import java.nio.file.{FileSystemException, Files, Path, Paths, StandardCopyOption}
import java.security.MessageDigest
import java.util.Comparator
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}

/** Importer for Cardano Ledger conformance test vectors
  *
  * Supports importing test vectors from the cardano-ledger dump format as described in
  * https://github.com/SundaeSwap-finance/cardano-ledger-conformance-tests
  */
object CardanoLedgerVectors {

    /** Slot/epoch environment replicating cardano-ledger's Imp test framework, which generated the
      * test vectors. The dump format carries no slot/epoch, but the Imp environment is
      * deterministic:
      *   - Imp builds its globals from the Shelley genesis, `fixedEpochInfo (sgEpochLength
      *     shelleyGenesis)` (shelley testlib Test/Cardano/Ledger/Shelley/ImpTest.hs:664-668): the
      *     genesis of the Dugite runner, `DugiteLedgerVectors.impGenesisSlotConfig`.
      *   - Conway tests start at `pred impEraStartEpochNo = majProtVer * 100 - 1 = 899`
      *     (Test.Cardano.Ledger.Shelley.ImpTest).
      */
    val impSlotConfig: SlotConfig = DugiteLedgerVectors.impGenesisSlotConfig
    val impEpoch: Long = 899

    /** One test case of a vector: the expected outcome, the Scalus result, the expected new ledger
      * state (present when `success` is true), and the epoch the case ran at.
      */
    case class VectorRun(
        file: String,
        success: Boolean,
        result: Try[CardanoMutator.Result],
        newLedgerState: Option[VectorLedgerState],
        epoch: Long
    )

    /** The harness could not decode a ledger state of a case, so the case has no verdict. The
      * conformance test reports it as a failure, never as the ledger's "fail" verdict.
      */
    final class UndecodableState(cause: Throwable)
        extends Exception(s"cannot decode the ledger state: ${cause.getMessage}", cause)

    def runVector(
        vectorName: String,
        evaluatorMode: EvaluatorMode = EvaluatorMode.EvaluateAndComputeCost
    ): List[VectorRun] = {
        // spec [CV-8]: the epoch of a case is at least the epoch of every earlier case, D43
        var floor = impEpoch
        for case (path, vector) <- loadVector(vectorName) yield
            val file = path.getFileName.toFile.getName
            val transaction = Try(Transaction.fromCbor(Hex.hexToBytes(vector.cbor)))
            val states = Try {
                val old = VectorLedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
                val next =
                    vector.newLedgerState.map(hex =>
                        VectorLedgerState.fromCbor(Hex.hexToBytes(hex))
                    )
                (old, next)
            }
            // The fixtures may fail to give a case's pparams; that case then has no verdict, and
            // the other cases of the vector keep theirs.
            val caseParams = Try(vectorParams(vector))
                .map(_.getOrElse(UtxoEnv.default.params))
            (states, caseParams) match
                case (Failure(e), _) =>
                    VectorRun(
                      file,
                      vector.success,
                      Failure(UndecodableState(e)),
                      None,
                      floor
                    )
                case (_, Failure(e)) =>
                    VectorRun(file, vector.success, Failure(e), None, floor)
                case (Success((oldState, newState)), Success(params)) =>
                    val state = oldState.ruleState
                    val epoch =
                        (floor :: oldState.utxos.govState.proposals.map(_.proposedIn).toList ++
                            transaction.toOption.flatMap(tx =>
                                newState.flatMap(caseEpoch(tx, oldState, _, params.dRepActivity))
                            )).max
                    floor = epoch
                    val context = Context(
                      env = rules.UtxoEnv(
                        impSlotConfig.firstSlotOfEpoch(epoch),
                        params,
                        state.certState,
                        scalus.cardano.address.Network.Testnet,
                        // the vectors carry no ChainAccountState, so the treasury is unknown
                        scalus.cardano.ledger.Coin.zero
                      ),
                      slotConfig = impSlotConfig,
                      evaluatorMode = evaluatorMode
                    )
                    VectorRun(
                      file,
                      vector.success,
                      transaction.flatMap(tx => Try(CardanoMutator.transit(context, state, tx))),
                      newState,
                      epoch
                    )
    }

    /** The epoch that a case's `newLedgerState` records, spec [CV-8]: the `proposedIn` of a
      * proposal of the case tx ([GV-5]); else the epoch that the new expiry of a DRep the tx
      * registers, updates or votes with inverts ([DR-1], [DR-4]: `epoch + dRepActivity -
      * numDormantEpochs`, with the counter of `oldState`); else `None`.
      */
    private def caseEpoch(
        tx: Transaction,
        oldState: VectorLedgerState,
        newState: VectorLedgerState,
        dRepActivity: Long
    ): Option[Long] = {
        val proposed = newState.utxos.govState.proposals
            .find(_.id.transactionId == tx.id)
            .map(_.proposedIn)
        def touchedDReps: Seq[Credential] =
            tx.body.value.certificates.toSeq.collect {
                case Certificate.RegDRepCert(credential, _, _) => credential
                case Certificate.UpdateDRepCert(credential, _) => credential
            } ++ tx.body.value.votingProcedures.toSeq.flatMap(_.procedures.keys).collect {
                case Voter.DRepKey(hash)    => Credential.KeyHash(hash)
                case Voter.DRepScript(hash) => Credential.ScriptHash(hash)
            }
        def fromDRep = touchedDReps.iterator
            .flatMap(newState.certs.vstate.dreps.get)
            .map(_.expiry - dRepActivity + oldState.certs.vstate.numDormantEpochs)
            .nextOption()
        proposed.orElse(fromDRep)
    }

    /** The protocol parameters of a vector: the `pparamsHash` field (new format), else the hash in
      * the old ledger state.
      */
    private def vectorParams(vector: RawTestVector): Option[ProtocolParams] =
        vector.pparamsHash
            .flatMap(hash => ConwayProtocolParamsFixtures.loadFromHash(pparamsDir, hash))
            .orElse(
              ConwayProtocolParamsFixtures
                  .extractPparamsHash(vector.oldLedgerState, pparamsDir)
                  .flatMap(hash => ConwayProtocolParamsFixtures.loadFromHash(pparamsDir, hash))
            )
            .map(_.toProtocolParams)

    // Extract vectors.tar.gz to a temporary directory, shared by all test runs
    lazy val conformanceVectorsPath: Path = {
        val vectorsTarGz = getClass.getClassLoader.getResource("vectors.tar.gz")
        if vectorsTarGz == null then {
            null
        } else {
            val archive = Paths.get(vectorsTarGz.toURI)
            extractOnce(archive, cacheDirectory(archive)).resolve("eras/conway/impl/dump")
        }
    }

    /** `<tmpdir>/cardano-ledger-vectors-<hash of archive>`, shared by every test JVM on the machine
      * that runs the same archive.
      */
    private[conformance] def cacheDirectory(archive: Path): Path = {
        val digest = MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(archive))
        Paths.get(
          System.getProperty("java.io.tmpdir"),
          s"cardano-ledger-vectors-${Hex.bytesToHex(digest.take(8))}"
        )
    }

    /** Extracts `archive` into `target` and reuses that directory in later runs. A fresh temp
      * directory per run leaked ~10 MB per test JVM.
      *
      * The extraction goes to a staging directory first, then an atomic rename publishes it. So a
      * run never sees a half-extracted directory, and two JVMs that extract at the same time do not
      * corrupt each other: the rename fails on a directory the other JVM published first, which is
      * then used, and a published directory is never deleted.
      *
      * A published directory is reused only if it still holds every file it was extracted with:
      * macOS deletes old files under the temp directory and keeps the directories, which left the
      * vectors empty and every conformance test passing without a case. Such a stale directory is
      * renamed aside and deleted before the extraction starts, so a directory that another JVM
      * publishes during the extraction is kept. Only a JVM that publishes between the completeness
      * check and that rename, a few milliseconds, loses its directory.
      */
    private[conformance] def extractOnce(archive: Path, target: Path): Path = {
        if !isComplete(target) then {
            retireStale(target)
            val staging = Files.createTempDirectory("cardano-ledger-vectors-staging")
            try {
                extractTarGz(archive, staging)
                Files.writeString(staging.resolve(stampFile), regularFileCount(staging).toString)
                publish(staging, target)
            } finally deleteRecursively(staging)
        }
        target
    }

    /** Publishes `staging` as `target` with one atomic rename, and deletes nothing: a rename onto a
      * non-empty directory fails, so a complete `target` that another JVM published first is kept,
      * and that JVM may be reading it.
      */
    private[conformance] def publish(staging: Path, target: Path): Unit =
        try Files.move(staging, target, StandardCopyOption.ATOMIC_MOVE)
        catch {
            // Another JVM published the same archive first
            case _: FileSystemException if isComplete(target) => ()
        }

    /** Renames a stale `target` aside and deletes the copy. Does nothing when `target` does not
      * exist, or when another JVM renamed it first.
      */
    private def retireStale(target: Path): Unit = {
        val aside = target.resolveSibling(s"${target.getFileName}-stale-${System.nanoTime}")
        if Try(Files.move(target, aside, StandardCopyOption.ATOMIC_MOVE)).isSuccess then
            deleteRecursively(aside)
    }

    /** Holds the number of extracted files, written last into the staging directory. */
    private[conformance] val stampFile = ".extracted-file-count"

    /** Whether `dir` holds its stamp and the number of files the stamp records. */
    private[conformance] def isComplete(dir: Path): Boolean = {
        val stamp = dir.resolve(stampFile)
        Files.isRegularFile(stamp) &&
        Try(Files.readString(stamp).trim.toLong).toOption.contains(regularFileCount(dir))
    }

    /** The number of regular files under `dir`, without the stamp. */
    private def regularFileCount(dir: Path): Long =
        Using.resource(Files.walk(dir)) { paths =>
            paths
                .filter(p => Files.isRegularFile(p) && p.getFileName.toString != stampFile)
                .count()
        }

    private def extractTarGz(archive: Path, dir: Path): Unit = {
        Using.resource(new FileInputStream(archive.toFile)) { fis =>
            Using.resource(new GzipCompressorInputStream(fis)) { gzis =>
                Using.resource(new TarArchiveInputStream(gzis)) { tis =>
                    var entry = tis.getNextEntry
                    while entry != null do {
                        val outputFile = dir.resolve(entry.getName).toFile
                        if entry.isDirectory then {
                            outputFile.mkdirs()
                        } else {
                            outputFile.getParentFile.mkdirs()
                            Using.resource(new FileOutputStream(outputFile)) { fos =>
                                val buffer = new Array[Byte](8192)
                                var len = tis.read(buffer)
                                while len != -1 do {
                                    fos.write(buffer, 0, len)
                                    len = tis.read(buffer)
                                }
                            }
                        }
                        entry = tis.getNextEntry
                    }
                }
            }
        }
    }

    /** Deletes `dir` and its contents. Does nothing if `dir` does not exist. */
    private def deleteRecursively(dir: Path): Unit = {
        if Files.exists(dir) then
            Using.resource(Files.walk(dir)) { paths =>
                paths.sorted(Comparator.reverseOrder()).forEach(p => Files.delete(p))
            }
    }

    // Skip tests if vectors directory doesn't exist
    val vectorsExist: Boolean =
        conformanceVectorsPath != null && Files.exists(conformanceVectorsPath)

    // Path to pparams-by-hash directory
    lazy val pparamsDir: Path =
        if conformanceVectorsPath != null then conformanceVectorsPath.resolve("pparams-by-hash")
        else null

    if !vectorsExist then {
        println(
          s"⚠ Conformance test vectors not found at $conformanceVectorsPath - skipping conformance tests"
        )
    }

    /** Raw test vector from cardano-ledger dump */
    case class RawTestVector(
        /** Transaction CBOR (hex-encoded) */
        cbor: String,

        /** Old (before) ledger state CBOR (hex-encoded) */
        oldLedgerState: String,

        /** New (after) ledger state CBOR (hex-encoded) - only present if success=true */
        newLedgerState: Option[String] = None,

        /** Whether the transaction should succeed */
        success: Boolean,

        /** Test category/name - optional for backwards compatibility */
        testState: Option[String] = None,

        /** Protocol parameters hash (Blake2b-256 of pparams CBOR) - used to look up pparams file */
        pparamsHash: Option[String] = None
    )

    given JsonValueCodec[RawTestVector] = JsonCodecMaker.make

    /** Load a single test vector from a file */
    def loadRawVector(path: Path): Try[RawTestVector] = Try {
        val json = Files.readString(path)
        readFromString[RawTestVector](json)
    }

    // Each directory stream is closed: the tests run in the sbt server JVM, which ran out of file
    // descriptors on the leaked ones.
    def vectorNames(): List[String] =
        // No vectors: no tests, not a NullPointerException in the suite. Each suite checks that
        // it found vectors.
        if !vectorsExist then Nil
        else
            Using.resource(Files.list(conformanceVectorsPath)) { paths =>
                paths
                    .map(_.getFileName.toFile.getName)
                    .iterator()
                    .asScala
                    .filterNot(_.startsWith("._"))
                    .filterNot(_ == "pparams-by-hash")
                    .toList
                    .sorted
            }

    def loadVector(name: String): List[(Path, RawTestVector)] = {
        val rootPath = conformanceVectorsPath.resolve(name)
        if !Files.exists(rootPath) then {
            println(s"Warning: Test vector directory not found: $rootPath")
            return List.empty
        }

        val files = Using.resource(Files.walk(rootPath))(_.iterator().asScala.toList)
        files
            .filter(p => Files.isRegularFile(p) && !p.getFileName.toString.startsWith("."))
            .filter(p => !p.toString.contains("pparams-by-hash")) // Skip protocol params files
            .filter(p => !p.getFileName.toString.endsWith(".md")) // Skip markdown files
            .filter(p => !p.getFileName.toString.endsWith(".tar.gz")) // Skip tar.gz files
            .flatMap { path =>
                loadRawVector(path) match {
                    case Success(vector) => Some((path.getFileName, vector))
                    case Failure(e)      =>
                        // Skip files that aren't valid JSON vectors
                        None
                }
            }
            .toList
            // numeric order, the order the Imp test made the cases in
            .sortBy((path, _) => (path.toString.toIntOption, path.toString))
    }

    // The conformance helpers below are shared by the JVM and the IT conformance suites, spec
    // `[kit.vectors.both-suites]`.

    private def summarizeResult(result: Try[CardanoMutator.Result]): String =
        result match
            case Failure(e) => s"Failure(${e.getClass.getSimpleName}: ${e.getMessage})"
            case Success(Left(e)) =>
                s"Left(${e.getClass.getSimpleName}: ${e.getMessage})"
            case Success(Right(_)) => "Right(...)"

    val conway = "Conway.Imp.ConwayImpSpec - Version 10."

    /** The cases that the ledger and Scalus decide differently, keyed by vector name, each with its
      * cause (D37). Each listed case must still fail, so a fix must remove it from this list.
      */
    val expectedFailures: Map[String, Set[String]] = Map(
      // A Word64 above Long.MaxValue that `Coin(value: Long)` cannot hold: withdrawal amounts of
      // a TreasuryWithdrawals in the state (EnactSpec.hs:129-137). The harness cannot decode the
      // old state, spec section 20 Q9.
      s"${conway}ENACT.Treasury withdrawals.Withdrawals exceeding maxBound Word64 submitted in a single proposal" ->
          Set("10", "11", "12", "13"),
      // A threshold denominator of 10^19 that `UnitInterval(Long, Long)` cannot hold, in an
      // UpdateCommittee in the state (GovSpec.hs:807-820), spec section 20 Q9.
      s"${conway}GOV.Voting.committee member can not vote on UpdateCommittee action" ->
          Set("2", "3"),
      // The vote comes three epochs after the proposal (`passNEpochs 3`, GovSpec.hs:788-798), and
      // no field of the dump records the passed epochs, so the harness runs the vote at the
      // proposal's epoch, where the action has not expired. The ledger disables this test in its
      // own conformance run (`disableInConformanceIt`). GovMutatorTest covers
      // VotingOnExpiredGovAction, spec [CV-8], D43.
      s"${conway}GOV.Voting.expired gov-actions" -> Set("5")
    )

    /** The problems of one vector against the ledger's verdicts, one line each, spec
      * `[kit.vectors.check-vector]`: a case whose verdict differs (unless `expectedFailures` lists
      * it), an expected failure that now matches, and a passing case whose new state differs. An
      * empty list means the vector conforms. A vector with no cases is a problem too: it would pass
      * without checking anything, for example when the extracted vectors lost their files.
      */
    def checkVector(name: String, mode: EvaluatorMode): List[String] = {
        val runs = runVector(name, mode)
        if runs.isEmpty then return List(s"vector $name has no test cases")
        val expectedToFail = expectedFailures.getOrElse(name, Set.empty)
        // A thrown exception is no verdict: it matches neither a pass nor a fail.
        val mismatched = runs.filter(run =>
            run.result match
                case Success(result) => run.success != result.isRight
                case Failure(_)      => true
        )
        val failures = for
            run <- mismatched
            if !expectedToFail.contains(run.file)
        yield s"  [${run.file}] expected=${if run.success then "pass" else "fail"}, got=${summarizeResult(run.result)}"
        val fixed = (expectedToFail -- mismatched.map(_.file)).toSeq.sorted.map(file =>
            s"  [$file] is an expected failure but now matches the ledger: remove it from expectedFailures"
        )
        // spec [SC-14]: a vector that expects success must also reach its newLedgerState
        val stateMismatches = runs.flatMap { run =>
            (run.success, run.result, run.newLedgerState) match
                case (true, Success(Right(actual)), Some(expected)) =>
                    LedgerStateComparison
                        .withoutExcluded(
                          name,
                          LedgerStateComparison.compare(expected, actual),
                          LedgerStateComparison.exclusions
                        )
                        .map(m => s"  [${run.file}] state field ${m.field} differs: ${m.detail}")
                case (true, Success(_), None) =>
                    List(s"  [${run.file}] has no newLedgerState")
                case _ => Nil
        }
        failures ++ fixed ++ stateMismatches
    }
}
