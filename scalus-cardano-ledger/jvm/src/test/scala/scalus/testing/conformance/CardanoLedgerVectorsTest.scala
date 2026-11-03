package scalus.testing.conformance

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream}
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The cache of the extracted conformance vectors, shared by every test JVM on a machine. */
class CardanoLedgerVectorsTest extends AnyFunSuite {

    /** Runs `body` on a fresh temp directory and deletes it afterwards. */
    private def withTempDir[A](body: Path => A): A = {
        val dir = Files.createTempDirectory("cardano-ledger-vectors-test")
        try body(dir)
        finally
            Using.resource(Files.walk(dir)) { paths =>
                paths.sorted(Comparator.reverseOrder()).forEach(p => Files.delete(p))
            }
    }

    /** A directory `name` under `parent` of `files`, each holding its name, with the stamp of a
      * complete extraction.
      */
    private def published(parent: Path, name: String, files: String*): Path = {
        val dir = Files.createDirectory(parent.resolve(name))
        files.foreach(file => Files.writeString(dir.resolve(file), file))
        Files.writeString(dir.resolve(CardanoLedgerVectors.stampFile), files.size.toString)
        dir
    }

    /** The names of the files under `dir`, the stamp included. */
    private def names(dir: Path): Set[String] =
        Using.resource(Files.list(dir))(_.iterator().asScala.map(_.getFileName.toString).toSet)

    /** A tar.gz under `parent` of `files`, each holding its name. */
    private def archive(parent: Path, files: String*): Path = {
        val path = parent.resolve("vectors.tar.gz")
        Using.resource(
          new TarArchiveOutputStream(new GzipCompressorOutputStream(Files.newOutputStream(path)))
        ) { tar =>
            files.foreach { file =>
                val bytes = file.getBytes
                val entry = new TarArchiveEntry(file)
                entry.setSize(bytes.length)
                tar.putArchiveEntry(entry)
                tar.write(bytes)
                tar.closeArchiveEntry()
            }
        }
        path
    }

    test("publish keeps a complete directory that another JVM published first") {
        // That JVM may be reading it, so it is never deleted or replaced
        withTempDir { dir =>
            val target = published(dir, "target", "a", "b")
            val staging = published(dir, "staging", "c")
            CardanoLedgerVectors.publish(staging, target)
            assert(names(target) == Set("a", "b", CardanoLedgerVectors.stampFile))
            assert(CardanoLedgerVectors.isComplete(target))
        }
    }

    test("extractOnce replaces a stale directory with a complete one") {
        withTempDir { dir =>
            val target = published(dir, "target", "a", "b")
            // What macOS does to an old temp directory: the files go, the directory stays
            Files.delete(target.resolve("b"))
            assert(!CardanoLedgerVectors.isComplete(target))
            assert(CardanoLedgerVectors.extractOnce(archive(dir, "x", "y"), target) == target)
            assert(names(target) == Set("x", "y", CardanoLedgerVectors.stampFile))
            assert(CardanoLedgerVectors.isComplete(target))
            assert(names(dir) == Set("target", "vectors.tar.gz"), "no stale copy is left behind")
        }
    }

    test("extractOnce reuses a complete directory as it is") {
        withTempDir { dir =>
            val target = published(dir, "target", "a")
            assert(CardanoLedgerVectors.extractOnce(archive(dir, "x"), target) == target)
            assert(names(target) == Set("a", CardanoLedgerVectors.stampFile))
        }
    }

    test("extractOnce extracts into a directory that does not exist yet") {
        withTempDir { dir =>
            val target = dir.resolve("target")
            assert(CardanoLedgerVectors.extractOnce(archive(dir, "x"), target) == target)
            assert(names(target) == Set("x", CardanoLedgerVectors.stampFile))
            assert(CardanoLedgerVectors.isComplete(target))
        }
    }
}
