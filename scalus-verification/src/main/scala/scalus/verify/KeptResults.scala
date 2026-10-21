package scalus.verify

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
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
  * in place of the old one. A statement named by its content that changed is another statement: its
  * old entry is left, and [[prune]] removes it once no test states it.
  *
  * An entry says which tests state its statement, as `asking` says them where the statement is
  * looked up or kept: one statement may be stated by several tests.
  *
  * The file is JSON written out in lines, with the statements in the order of their names, to be
  * read and compared as text. It is written whole on every change, to a file beside it that then
  * takes its place.
  */
final class KeptResults private (
    file: Path,
    val mode: KeptResults.Mode,
    lock: AnyRef,
    asking: () => String
) {
    import KeptResults.*

    /** The statements this run looked up or kept, by their names, with the tests that did: the
      * empty name for a statement that no test asked for.
      */
    private val used = new java.util.concurrent.ConcurrentHashMap[String, Set[String]]

    /** The programs of each statement this run looked up, by its name and its tactic's. */
    private val looked =
        new java.util.concurrent.ConcurrentHashMap[(String, String), List[String]]

    /** Says that the test that runs uses the statement `name`. */
    private def use(name: String): Unit = {
        val test = asking()
        used.merge(name, Set(test), _ ++ _)
    }

    /** The tests that state the statement `name`: those its entry has, and those that used it in
      * this run.
      */
    private def testsOf(name: String, stored: Stored): Set[String] =
        stored.results
            .get(name)
            .orElse(stored.former.get(name))
            .fold(Set.empty[String])(_.tests.toSet) ++ used.getOrDefault(name, Set.empty) - ""

    /** Takes out what no test states any longer, and writes into each entry the tests that state
      * it, where the run is one that keeps ([[KeptResults.Mode.Use]],
      * [[KeptResults.Mode.Recalculate]]).
      *
      * Of the tests of an entry ([[testsOf]]), one goes that is among `completed`, the tests that
      * ran to their end, and did not use it: the test no longer states it. A test that failed, was
      * canceled or did not run keeps its entries. So does one go that is not among `existing`,
      * where that is given: all the tests there are, of which a test renamed or removed is not. An
      * entry goes when no test is left of those it had, and this run did not use it outside a test.
      * One that never had a test goes too where `existing` is given and this run did not use it: a
      * suite uses what it states outside its tests, as in its constructor, in every run. Without
      * `existing`, as for a program that runs no tests, it stays.
      */
    def prune(completed: Set[String], existing: Option[Set[String]]): Unit = {
        if mode != Mode.Frozen then
            lock.synchronized {
                if Files.isRegularFile(file) then
                    val stored = read()
                    // The former results go as the results do: with the tests that state them.
                    def pruned(entries: TreeMap[String, Entry]): TreeMap[String, Entry] =
                        entries.flatMap { (name, entry) =>
                            val now = used.getOrDefault(name, Set.empty)
                            val had = entry.tests.toSet ++ now - ""
                            val tests = had.filter(test =>
                                (now.contains(test) || !completed.contains(test)) &&
                                    existing.forall(_.contains(test))
                            )
                            val of = had.nonEmpty || existing.isDefined
                            if of && tests.isEmpty && !now.contains("") then None
                            else Some(name -> entry.copy(tests = tests.toList.sorted))
                        }
                    val results = pruned(stored.results)
                    val former = pruned(stored.former)
                    if results != stored.results || former != stored.former then
                        write(stored.copy(results = results, former = former))
            }
    }

    /** What is kept for the statement `name`, where it was kept for `fingerprint`.
      *
      * A statement names the functions it calls by their names ([[PropIdentity]]). It fails where a
      * statement was looked up in this run, for the same tactic, with other programs: two functions
      * of one name, one kept result for both of them, which each would take from the other.
      */
    private[verify] def lookup(
        name: String,
        tactic: String,
        fingerprint: Fingerprint
    ): Option[Kept] = {
        use(name)
        val before = looked.putIfAbsent((name, tactic), fingerprint.programs)
        if before != null && before != fingerprint.programs then
            throw new IllegalStateException(
              s"the statement $name is stated with other programs than before in this run: a " +
                  "function it calls is another one of the same name. A statement names a " +
                  "function by its name, so functions that are not the same need other names"
            )
        lock.synchronized {
            val stored = read()
            stored.results.get(name).filter { entry =>
                entry.tactic == tactic && entry.statement == fingerprint.statement &&
                entry.programs == fingerprint.programs &&
                stored.environment.get(tactic).contains(sorted(fingerprint.environment))
            }
        }.flatMap(kept)
    }

    /** What was kept for the statement `name` as it is now, with its statement and programs, but
      * with another side of the backend, as before the backend's library was updated: a result of
      * the file kept for its old side, or a former result ([[keep]]). It is no result, but what the
      * tactic came to there is a start for it ([[Tactic.run]]), as the budget a search found, and a
      * refutation is one the tactic checks again without the backend ([[Tactic.restore]]).
      */
    private[verify] def keptOnAnotherSide(
        name: String,
        tactic: String,
        fingerprint: Fingerprint
    ): Option[Kept] = {
        lock.synchronized {
            val stored = read()
            val onOldSide = stored.results
                .get(name)
                .filter(_ =>
                    !stored.environment.get(tactic).contains(sorted(fingerprint.environment))
                )
            onOldSide.orElse(stored.former.get(name)).filter { entry =>
                entry.tactic == tactic && entry.statement == fingerprint.statement &&
                entry.programs == fingerprint.programs
            }
        }.flatMap(kept)
    }

    /** Keeps `kept` for the statement `name`, in place of what was kept for it.
      *
      * A tactic's side is said once for the file. Where it is another than the file has, every
      * other result of the tactic in the file was kept for a side that is gone. It goes from the
      * results to the former results, which are none, but a start for their statements, until each
      * is kept again or no test states it ([[keptOnAnotherSide]], [[prune]]).
      */
    private[verify] def keep(
        name: String,
        tactic: String,
        fingerprint: Fingerprint,
        kept: Kept
    ): Unit = {
        use(name)
        lock.synchronized {
            val stored = read()
            val environment = sorted(fingerprint.environment)
            val (current, gone) =
                if stored.environment.get(tactic).contains(environment) then
                    stored.results -> TreeMap.empty[String, Entry]
                else stored.results.partition((_, entry) => entry.tactic != tactic)
            val entry = Entry(
              tactic,
              word(kept.result),
              fingerprint.statement,
              fingerprint.programs,
              sorted(kept.notes),
              TreeMap.from(kept.counterexample.view.mapValues(json => new Raw(bytes(json)))),
              testsOf(name, stored).toList.sorted
            )
            write(
              Stored(
                stored.environment.updated(tactic, environment),
                current.updated(name, entry),
                (stored.former ++ gone).removed(name)
              )
            )
        }
    }

    /** Why nothing that is kept counts for the statement `name` with `fingerprint`: what the entry
      * under its name no longer agrees with, part by part.
      */
    private[verify] def stale(name: String, tactic: String, fingerprint: Fingerprint): String = {
        use(name)
        lock.synchronized {
            val stored = read()
            // What of `entry` is another than the statement and its programs are now.
            def changedOf(entry: Entry): List[String] = {
                val programs =
                    if entry.programs.sizeIs != fingerprint.programs.size then
                        List("the number of its tests")
                    else
                        entry.programs.zip(fingerprint.programs).zipWithIndex.collect {
                            case ((was, is), index) if was != is =>
                                s"the program of test ${index + 1}"
                        }
                val statement =
                    if entry.statement != fingerprint.statement then List("the statement") else Nil
                statement ++ programs
            }
            stored.results.get(name).filter(_.tactic == tactic) match
                case None =>
                    stored.former.get(name).filter(_.tactic == tactic) match
                        case Some(entry) =>
                            val changed = changedOf(entry)
                            s"what is kept for it in $file was ${entry.result} with an earlier " +
                                "side of the backend" +
                                (if changed.isEmpty then ""
                                 else s", and another ${changed.mkString(", ")}")
                        case None => s"no result of $tactic is kept for it in $file"
                case Some(entry) if result(entry.result).isEmpty =>
                    s"what is kept for it in $file has the result \"${entry.result}\", which is none"
                case Some(entry) =>
                    val before = stored.environment.getOrElse(tactic, TreeMap.empty[String, String])
                    val side = (before.keySet ++ fingerprint.environment.keySet).toList.sorted
                        .filter(part => before.get(part) != fingerprint.environment.get(part))
                    val changed = changedOf(entry) ++ side.map(part => s"$part of the backend")
                    // With every part as it was, it is what was kept that the tactic does not
                    // stand for: a counterexample that is none of the statement as it is now.
                    if changed.isEmpty then
                        s"what is kept for it in $file, ${entry.result}, does not stand for the " +
                            "statement as it is now"
                    else
                        s"what is kept for it in $file was ${entry.result} with another " +
                            changed.mkString(", ")
        }
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

    /** The lock of each file, by its path. A change writes the whole file, so whoever keeps results
      * in one file of a JVM takes turns: each test of a suite may have results of its own.
      */
    private val locks = new java.util.concurrent.ConcurrentHashMap[Path, AnyRef]

    /** The results kept in `file`, used in the way `mode` says, by no test. The file need not be
      * there yet.
      */
    def in(file: Path, mode: Mode): KeptResults = in(file, mode, () => "")

    /** The results kept in `file`, used in the way `mode` says, by the tests `asking` says: the
      * test that runs where a statement is looked up or kept, or the empty name for none. Where
      * tests run at the same time, it says which of them asks.
      */
    def in(file: Path, mode: Mode, asking: () => String): KeptResults = {
        val lock = locks.computeIfAbsent(file.toAbsolutePath.normalize, _ => new AnyRef)
        new KeptResults(file, mode, lock, asking)
    }

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

    /** An entry of the file, with the tests that state its statement. Collections that are empty
      * are not written.
      */
    private final case class Entry(
        tactic: String,
        result: String,
        statement: String,
        programs: List[String],
        notes: TreeMap[String, String],
        counterexample: TreeMap[String, Raw],
        tests: List[String] = Nil
    )

    /** The file: the side of each tactic's backend, by the tactic's name, the entries by the names
      * of their statements, and the former entries, kept with an earlier side ([[keep]]). A file
      * without former entries does not say them.
      */
    private final case class Stored(
        environment: TreeMap[String, TreeMap[String, String]],
        results: TreeMap[String, Entry],
        former: TreeMap[String, Entry] = TreeMap.empty
    )

    // A file has an entry for every statement of a suite, and the reader's own limit on the
    // entries of a map is lower than a large suite's.
    private given JsonValueCodec[Stored] =
        JsonCodecMaker.make(CodecMakerConfig.withMapMaxInsertNumber(1000000))

    private def sorted(parts: Map[String, String]): TreeMap[String, String] = TreeMap.from(parts)

    private def bytes(json: String): Array[Byte] = json.getBytes(StandardCharsets.UTF_8)

    private def raw(values: TreeMap[String, Raw]): Map[String, String] =
        values.view.mapValues(value => new String(value.bytes, StandardCharsets.UTF_8)).toMap

    private def word(result: Kept.Result): String = result match
        case Kept.Result.Proved  => "proved"
        case Kept.Result.Refuted => "refuted"

    /** The result `word` says, or none for a word that says none, as in a file that was edited. */
    private def result(word: String): Option[Kept.Result] = word match
        case "proved"  => Some(Kept.Result.Proved)
        case "refuted" => Some(Kept.Result.Refuted)
        case _         => None

    /** What `entry` keeps, where its result is one. */
    private def kept(entry: Entry): Option[Kept] =
        result(entry.result).map(Kept(_, entry.notes, raw(entry.counterexample)))
}
