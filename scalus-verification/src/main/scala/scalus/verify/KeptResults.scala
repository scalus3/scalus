package scalus.verify

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalus.utils.{Hex, Utils}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.collection.immutable.TreeMap

/** The hashes of everything a result rests on, part by part (docs/design/verification-overview.md
  * §5.6). Two fingerprints are the same where every part is, and a part that differs says what
  * changed.
  *
  * @param environment
  *   the backend's side, by the names of its parts: what every result of the tactic rests on, as
  *   its toolchain and its libraries. A file of kept results says it once.
  * @param statement
  *   the statement as the backend is given it
  * @param programs
  *   the program of each test of the statement
  */
final case class Fingerprint(
    environment: Map[String, String],
    statement: String,
    programs: List[String]
)

object Fingerprint {

    /** The hash a fingerprint has of `bytes`: the first 16 hexadecimal digits of their SHA-256. A
      * kept result is not guarded against a file written by hand, only against a change, so the
      * whole hash would say no more, and reads worse.
      */
    def hash(bytes: Array[Byte]): String =
        Hex.bytesToHex(Utils.sha2_256(bytes)).toLowerCase.take(16)

    def hash(text: String): String = hash(text.getBytes(StandardCharsets.UTF_8))
}

/** What is kept of a result: that the statement was proved or refuted, and what the tactic keeps
  * with that.
  *
  * @param notes
  *   the tactic's own, as the budget a search came to
  * @param counterexample
  *   for a refutation, the value of each variable by its name, as JSON. A later run replays it, and
  *   trusts nothing that was kept.
  */
final case class Kept(
    result: Kept.Result,
    notes: Map[String, String],
    counterexample: Map[String, String]
)

object Kept {
    enum Result {
        case Proved, Refuted
    }
}

/** The results kept for the statements of a suite, in a file, and the way a run uses them
  * (docs/design/verification-overview.md §5.6).
  *
  * A result is kept under the name of its statement, with the fingerprint it had. It counts only
  * where that is the fingerprint the statement has now. A statement that changed gets a new entry
  * in place of the old one.
  *
  * The file is JSON written out in lines, with the statements in the order of their names, to be
  * read and compared as text. It is written whole on every change, to a file beside it that then
  * takes its place.
  */
final class KeptResults private (
    file: Path,
    val mode: KeptResults.Mode,
    context: String,
    lock: AnyRef
) {
    import KeptResults.*

    /** The same results, for statements whose names are those of `context`: a test's name, for the
      * statements of the test.
      */
    def under(context: String): KeptResults = new KeptResults(file, mode, context, lock)

    private def named(statement: String): String =
        if context.isEmpty then statement else s"$context $statement"

    /** What is kept for the statement `name`, where it was kept for `fingerprint`. */
    private[verify] def lookup(
        name: String,
        tactic: String,
        fingerprint: Fingerprint
    ): Option[Kept] =
        lock.synchronized {
            val stored = read()
            stored.results.get(named(name)).filter { entry =>
                entry.tactic == tactic && entry.statement == fingerprint.statement &&
                entry.programs == fingerprint.programs &&
                stored.environment.get(tactic).contains(sorted(fingerprint.environment))
            }
        }.map(entry => Kept(result(entry.result), entry.notes, raw(entry.counterexample)))

    /** Keeps `kept` for the statement `name`, in place of what was kept for it.
      *
      * A tactic's side is said once for the file. Where it is another than the file has, every
      * other result of the tactic in the file was kept for a side that is gone, and goes with it.
      */
    private[verify] def keep(
        name: String,
        tactic: String,
        fingerprint: Fingerprint,
        kept: Kept
    ): Unit = lock.synchronized {
        val stored = read()
        val environment = sorted(fingerprint.environment)
        val current =
            if stored.environment.get(tactic).contains(environment) then stored.results
            else stored.results.filter((_, entry) => entry.tactic != tactic)
        val entry = Entry(
          tactic,
          word(kept.result),
          fingerprint.statement,
          fingerprint.programs,
          sorted(kept.notes),
          TreeMap.from(kept.counterexample.view.mapValues(json => new Raw(bytes(json))))
        )
        write(
          Stored(
            stored.environment.updated(tactic, environment),
            current.updated(named(name), entry)
          )
        )
    }

    /** Why nothing that is kept counts for the statement `name` with `fingerprint`: what the entry
      * under its name no longer agrees with, part by part.
      */
    private[verify] def stale(name: String, tactic: String, fingerprint: Fingerprint): String =
        lock.synchronized {
            val stored = read()
            stored.results.get(named(name)).filter(_.tactic == tactic) match
                case None => s"no result of $tactic is kept for it in $file"
                case Some(entry) =>
                    val before = stored.environment.getOrElse(tactic, TreeMap.empty[String, String])
                    val side = (before.keySet ++ fingerprint.environment.keySet).toList.sorted
                        .filter(part => before.get(part) != fingerprint.environment.get(part))
                    val programs =
                        if entry.programs.sizeIs != fingerprint.programs.size then
                            List("the number of its tests")
                        else
                            entry.programs.zip(fingerprint.programs).zipWithIndex.collect {
                                case ((was, is), index) if was != is =>
                                    s"the program of test ${index + 1}"
                            }
                    val statement =
                        if entry.statement != fingerprint.statement then List("the statement")
                        else Nil
                    val changed = statement ++ programs ++ side.map(part => s"$part of the backend")
                    s"what is kept for it in $file was ${entry.result} with another " +
                        changed.mkString(", ")
        }

    private def read(): Stored =
        if Files.isRegularFile(file) then readFromArray[Stored](Files.readAllBytes(file))
        else Stored(TreeMap.empty, TreeMap.empty)

    private def write(stored: Stored): Unit = {
        val text = writeToString(stored, WriterConfig.withIndentionStep(2)) + "\n"
        Option(file.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
        val beside = file.resolveSibling(s"${file.getFileName}.new")
        Files.writeString(beside, text)
        Files.move(
          beside,
          file,
          StandardCopyOption.REPLACE_EXISTING,
          StandardCopyOption.ATOMIC_MOVE
        )
    }
}

object KeptResults {

    /** The way a run uses what is kept. */
    enum Mode {

        /** A kept result is the result, and a refutation is replayed. Where none is kept the tactic
          * runs, and its result is kept.
          */
        case Use

        /** The tactic runs, whatever is kept. A result that differs from the kept one is a failure.
          * This is what keeps the kept proofs honest.
          */
        case Recalculate

        /** As [[Use]], where no backend runs: a statement for which nothing is kept is stale, and
          * has no result.
          */
        case Frozen
    }

    /** The results kept in `file`, used in the way `mode` says. The file need not be there yet. */
    def in(file: Path, mode: Mode): KeptResults = new KeptResults(file, mode, "", new AnyRef)

    /** How the reason of a statement begins that is stale: nothing kept counts for it, and no
      * backend ran ([[Mode.Frozen]]).
      */
    val stale: String = "the kept result is stale"

    /** A JSON value, kept as the bytes it was written with. */
    private final class Raw(val bytes: Array[Byte])

    private given JsonValueCodec[Raw] = new JsonValueCodec[Raw] {
        // The bytes of a value in a file that is written out in lines have the blanks before it.
        override def decodeValue(in: JsonReader, default: Raw): Raw =
            new Raw(bytes(new String(in.readRawValAsBytes(), StandardCharsets.UTF_8).trim))
        override def encodeValue(value: Raw, out: JsonWriter): Unit = out.writeRawVal(value.bytes)
        override def nullValue: Raw = null
    }

    /** An entry of the file. Collections that are empty are not written. */
    private final case class Entry(
        tactic: String,
        result: String,
        statement: String,
        programs: List[String],
        notes: TreeMap[String, String],
        counterexample: TreeMap[String, Raw]
    )

    /** The file: the side of each tactic's backend, by the tactic's name, and the entries by the
      * names of their statements.
      */
    private final case class Stored(
        environment: TreeMap[String, TreeMap[String, String]],
        results: TreeMap[String, Entry]
    )

    private given JsonValueCodec[Stored] = JsonCodecMaker.make

    private def sorted(parts: Map[String, String]): TreeMap[String, String] = TreeMap.from(parts)

    private def bytes(json: String): Array[Byte] = json.getBytes(StandardCharsets.UTF_8)

    private def raw(values: TreeMap[String, Raw]): Map[String, String] =
        values.view.mapValues(value => new String(value.bytes, StandardCharsets.UTF_8)).toMap

    private def word(result: Kept.Result): String = result match
        case Kept.Result.Proved  => "proved"
        case Kept.Result.Refuted => "refuted"

    private def result(word: String): Kept.Result = word match
        case "proved"  => Kept.Result.Proved
        case "refuted" => Kept.Result.Refuted
        case other     => throw new IllegalArgumentException(s"no result of a statement: $other")
}
