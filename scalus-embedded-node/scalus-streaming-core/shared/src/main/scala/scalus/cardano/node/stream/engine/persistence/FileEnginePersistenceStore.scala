package scalus.cardano.node.stream.engine.persistence

import io.bullet.borer.Cbor

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executors, ScheduledExecutorService, ScheduledFuture, TimeUnit}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

import PersistenceCodecs.given

/** File-backed [[EnginePersistenceStore]].
  *
  * Layout for `appId` = "com.foo.bar":
  *
  * {{{
  * <dataRoot>/scalus-stream/com.foo.bar/
  *   com.foo.bar.snapshot        ← last compacted state (absent on first run)
  *   com.foo.bar.log             ← append-only journal since the last compaction
  *   com.foo.bar.lock            ← process-exclusion `FileLock`
  * }}}
  *
  * See `docs/local/claude/indexer/engine-persistence-minimal.md` for the durability contract.
  */
final class FileEnginePersistenceStore private (
    private val dir: Path,
    private val baseName: String,
    private val fsyncInterval: FiniteDuration,
    private val bufferCapacityBytes: Int
)(using ec: ExecutionContext)
    extends EnginePersistenceStore {

    private val snapshotPath: Path = dir.resolve(s"$baseName.snapshot")
    private val logPath: Path = dir.resolve(s"$baseName.log")
    private val lockPath: Path = dir.resolve(s"$baseName.lock")

    private val lockChannel: FileChannel =
        FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    private val fileLock: FileLock =
        try
            Option(lockChannel.tryLock()).getOrElse {
                lockChannel.close()
                throw EnginePersistenceError.Locked(lockPath.toString)
            }
        catch {
            case _: OverlappingFileLockException =>
                lockChannel.close()
                throw EnginePersistenceError.Locked(lockPath.toString)
            case NonFatal(t) =>
                lockChannel.close()
                throw EnginePersistenceError.Io(t)
        }

    private val logChannel: FileChannel = FileChannel.open(
      logPath,
      StandardOpenOption.CREATE,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE
    )
    // Position at end so appends go after any existing records.
    logChannel.position(logChannel.size())

    private val buffer: ByteBuffer = ByteBuffer.allocate(bufferCapacityBytes)
    private val closed: AtomicBoolean = new AtomicBoolean(false)

    // Scheduled fsync ticker. One thread per store — cheap because the store's lifetime matches
    // the engine's.
    private val scheduler: ScheduledExecutorService =
        Executors.newSingleThreadScheduledExecutor(r => {
            val t = new Thread(r, s"scalus-stream-persistence-ticker-$baseName")
            t.setDaemon(true)
            t
        })
    private val tickerHandle: ScheduledFuture[?] = scheduler.scheduleAtFixedRate(
      () => { val _ = flushInternal() },
      fsyncInterval.toMillis,
      fsyncInterval.toMillis,
      TimeUnit.MILLISECONDS
    )

    // ------------------------------------------------------------------

    def load(): Future[Option[PersistedEngineState]] = Future {
        val snap: Option[EngineSnapshotFile] =
            if Files.exists(snapshotPath) then Some(loadSnapshot())
            else None
        val records: Seq[JournalRecord] = loadJournal()
        if snap.isEmpty && records.isEmpty then None
        else Some(PersistedEngineState(snap, records))
    }

    def appendSync(record: JournalRecord): Unit = {
        if closed.get() then return
        val bytes = Cbor.encode(record).toByteArray
        val required = 4 + bytes.length
        try
            this.synchronized {
                if buffer.remaining() < required then writeBuffer()
                if required > buffer.capacity() then {
                    // Oversized record — flush any buffered data first, then write directly.
                    writeDirect(bytes)
                } else {
                    buffer.putInt(bytes.length)
                    buffer.put(bytes)
                }
            }
        catch {
            case NonFatal(t) =>
                // Swallow — by contract, transient append errors are logged and surface via the
                // next flush/compact. Here we have no logger in this scope; a System.err line is
                // acceptable for M6 and keeps the "never silently eat exceptions" invariant.
                System.err.println(
                  s"scalus-stream: appendSync failed on $logPath — ${t.getClass.getSimpleName}: ${t.getMessage}"
                )
        }
    }

    def flush(): Future[Unit] = Future(flushInternal())

    def compact(snap: EngineSnapshotFile): Future[Unit] = Future {
        this.synchronized {
            flushInternal()
            val tmp = dir.resolve(s"$baseName.snapshot.tmp")
            val bytes = Cbor.encode(snap).toByteArray
            Files.write(
              tmp,
              bytes,
              StandardOpenOption.CREATE,
              StandardOpenOption.TRUNCATE_EXISTING,
              StandardOpenOption.WRITE
            )
            // Atomic rename over the old snapshot.
            Files.move(
              tmp,
              snapshotPath,
              StandardCopyOption.REPLACE_EXISTING,
              StandardCopyOption.ATOMIC_MOVE
            )
            // Zero out the log — everything in it is now subsumed by the snapshot.
            logChannel.truncate(0)
            logChannel.position(0)
            logChannel.force(true)
        }
    }

    def close(): Future[Unit] = Future {
        if closed.compareAndSet(false, true) then {
            try tickerHandle.cancel(false)
            catch { case NonFatal(_) => () }
            try scheduler.shutdownNow()
            catch { case NonFatal(_) => () }
            this.synchronized {
                try flushInternal()
                catch { case NonFatal(_) => () }
                try logChannel.close()
                catch { case NonFatal(_) => () }
                try fileLock.release()
                catch { case NonFatal(_) => () }
                try lockChannel.close()
                catch { case NonFatal(_) => () }
            }
        }
    }

    // ------------------------------------------------------------------

    private def loadSnapshot(): EngineSnapshotFile = {
        val bytes = Files.readAllBytes(snapshotPath)
        try Cbor.decode(bytes).to[EngineSnapshotFile].value
        catch {
            case NonFatal(t) => throw EnginePersistenceError.Corrupt(0L, t)
        }
    } match {
        case snap if snap.schemaVersion == EngineSnapshotFile.CurrentSchemaVersion => snap
        case snap =>
            throw EnginePersistenceError.SchemaMismatch(
              snap.schemaVersion,
              EngineSnapshotFile.CurrentSchemaVersion
            )
    }

    private def loadJournal(): Seq[JournalRecord] = {
        // Read existing records, truncating at the first malformed tail record. Runs before the
        // append path opens the channel for writing in this method's caller — we read via a
        // separate ReadOnly channel to avoid fighting with our write-mode `logChannel`.
        if !Files.exists(logPath) then return Seq.empty
        val bytes = Files.readAllBytes(logPath)
        val builder = Seq.newBuilder[JournalRecord]
        var pos = 0
        var stopPos = bytes.length
        while pos < bytes.length do {
            if pos + 4 > bytes.length then {
                // Partial length prefix — truncate here.
                stopPos = pos
                pos = bytes.length
            } else {
                val len = ByteBuffer.wrap(bytes, pos, 4).getInt
                if len < 0 || len > bytes.length - pos - 4 then {
                    // Implausible length — treat as corrupt tail, truncate here.
                    System.err.println(
                      s"scalus-stream: truncating corrupt journal tail at byte $pos (len=$len)"
                    )
                    stopPos = pos
                    pos = bytes.length
                } else {
                    val recBytes = java.util.Arrays.copyOfRange(bytes, pos + 4, pos + 4 + len)
                    Try(Cbor.decode(recBytes).to[JournalRecord].value).toOption match {
                        case Some(rec) =>
                            builder += rec
                            pos += 4 + len
                        case None =>
                            System.err.println(
                              s"scalus-stream: truncating malformed journal record at byte $pos"
                            )
                            stopPos = pos
                            pos = bytes.length
                    }
                }
            }
        }
        if stopPos < bytes.length then {
            // Physically truncate the file so subsequent appends append after the last good
            // record, not after the corrupt tail. Open a short-lived channel so we don't mess
            // with our existing write-mode channel's position.
            val trunc = FileChannel.open(logPath, StandardOpenOption.WRITE)
            try
                trunc.truncate(stopPos.toLong)
                trunc.force(true)
            finally trunc.close()
            // Reposition our write channel to the new end.
            logChannel.position(stopPos.toLong)
        }
        builder.result()
    }

    private def writeBuffer(): Unit = {
        if buffer.position() == 0 then return
        buffer.flip()
        while buffer.hasRemaining do {
            val _ = logChannel.write(buffer)
        }
        buffer.clear()
    }

    private def writeDirect(bytes: Array[Byte]): Unit = {
        val bb = ByteBuffer.allocate(4 + bytes.length)
        bb.putInt(bytes.length)
        bb.put(bytes)
        bb.flip()
        while bb.hasRemaining do {
            val _ = logChannel.write(bb)
        }
    }

    private def flushInternal(): Unit = this.synchronized {
        if closed.get() then return
        try {
            writeBuffer()
            logChannel.force(false)
        } catch {
            case NonFatal(t) =>
                System.err.println(
                  s"scalus-stream: flush failed on $logPath — ${t.getClass.getSimpleName}: ${t.getMessage}"
                )
                throw EnginePersistenceError.Io(t)
        }
    }
}

object FileEnginePersistenceStore {

    /** Default fsync cadence. Trade-off: shorter → more syscalls, less data lost on SIGKILL; longer
      * → cheaper, up to this much data lost on SIGKILL.
      */
    val DefaultFsyncInterval: FiniteDuration = 1.second

    /** 16 KiB — idle blocks batch naturally; oversized records fall through to direct writes. */
    val DefaultBufferBytes: Int = 16 * 1024

    /** File-backed store using the platform-default data directory for `appId`. Creates the
      * directory on first write.
      */
    def fileForApp(appId: String)(using ExecutionContext): EnginePersistenceStore =
        fileForApp(appId, DefaultFsyncInterval, DefaultBufferBytes)

    def fileForApp(appId: String, fsyncInterval: FiniteDuration, bufferBytes: Int)(using
        ExecutionContext
    ): EnginePersistenceStore = {
        val dir = PlatformPaths.ensureDir(PlatformPaths.dataDirFor(appId))
        openAt(dir, appId, fsyncInterval, bufferBytes)
    }

    /** Explicit path override. `dir` is created on demand. */
    def file(dir: Path, appId: String)(using ExecutionContext): EnginePersistenceStore =
        file(dir, appId, DefaultFsyncInterval, DefaultBufferBytes)

    def file(dir: Path, appId: String, fsyncInterval: FiniteDuration, bufferBytes: Int)(using
        ExecutionContext
    ): EnginePersistenceStore = {
        Files.createDirectories(dir)
        openAt(dir, appId, fsyncInterval, bufferBytes)
    }

    private def openAt(
        dir: Path,
        appId: String,
        fsyncInterval: FiniteDuration,
        bufferBytes: Int
    )(using ExecutionContext): FileEnginePersistenceStore = {
        require(appId.nonEmpty, "appId must be non-empty")
        new FileEnginePersistenceStore(dir, appId, fsyncInterval, bufferBytes)
    }

    /** Delete the persistence files for `appId` in the platform-default directory. Non-fatal if
      * they do not exist. Used by apps that want to explicitly cold-start after catching an
      * [[EnginePersistenceError.Mismatched]] or [[EnginePersistenceError.SchemaMismatch]].
      */
    def reset(appId: String)(using ExecutionContext): Future[Unit] = Future {
        val dir = PlatformPaths.dataDirFor(appId)
        if Files.exists(dir) then {
            Seq(
              dir.resolve(s"$appId.log"),
              dir.resolve(s"$appId.snapshot"),
              dir.resolve(s"$appId.snapshot.tmp"),
              dir.resolve(s"$appId.lock")
            ).foreach(p => Files.deleteIfExists(p))
        }
    }
}
