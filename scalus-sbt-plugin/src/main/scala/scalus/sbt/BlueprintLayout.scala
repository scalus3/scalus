package scalus.sbt

import java.io.File
import java.nio.file.Files
import java.security.MessageDigest

/** Pure helpers for the blueprint layout, cache fingerprint and pruning.
  *
  * Deliberately sbt-free (plain `java.io`/`java.nio`) so it unit-tests without an sbt harness and
  * compiles unchanged under Scala 2.12 (sbt 1) and Scala 3 (sbt 2).
  */
object BlueprintLayout {

    /** Layout/format scheme; part of the cache fingerprint header. Bump whenever the generated
      * file layout or JSON shape changes, so stale caches regenerate.
      */
    val Scheme: Int = 1

    /** Derives the package-nested output path for a Contract's blueprint:
      * `scalus.examples.auction.AuctionContract$` ->
      * `scalus/examples/auction/AuctionContract.json`. Nesting by package prevents same-simple-name
      * validators from overwriting each other.
      */
    def contractRelativePath(className: String): String = {
        val parts = className.stripSuffix("$").split('.')
        (parts.init :+ (parts.last + ".json")).mkString("/")
    }

    /** All regular files under `dir` (recursively), sorted by absolute path for determinism. Empty
      * when `dir` does not exist or is not a directory.
      */
    def listFilesRecursively(dir: File): Seq[File] = {
        val buf = scala.collection.mutable.ArrayBuffer.empty[File]
        def walk(f: File): Unit = {
            val children = f.listFiles()
            if (children != null) children.foreach { c =>
                if (c.isDirectory) walk(c) else if (c.isFile) buf += c
            }
        }
        if (dir.isDirectory) walk(dir)
        buf.sortBy(_.getAbsolutePath).toList
    }

    /** SHA-256 hex digest over `header` plus every file's root-relative path and content.
      * Content-based (not mtime-based), so a rebuild that rewrites identical class files still
      * counts as unchanged.
      */
    def fingerprint(header: String, root: File, files: Seq[File]): String = {
        val md = MessageDigest.getInstance("SHA-256")
        md.update(header.getBytes("UTF-8"))
        files.foreach { f =>
            val rel = root.toPath.relativize(f.toPath).toString.replace('\\', '/')
            md.update(0.toByte)
            md.update(rel.getBytes("UTF-8"))
            md.update(Files.readAllBytes(f.toPath))
        }
        md.digest().map(b => "%02x".format(b)).mkString
    }

    /** Deletes every `.json` file under `root` that is not in `keep` (compared canonically), then
      * removes directories left empty. Never touches non-json files. Returns the deleted files.
      * No-op when `root` does not exist.
      */
    def pruneStale(root: File, keep: Set[File]): Seq[File] = {
        val keepCanonical = keep.map(_.getCanonicalFile)
        val stale = listFilesRecursively(root).filter { f =>
            f.getName.endsWith(".json") && !keepCanonical.contains(f.getCanonicalFile)
        }
        stale.foreach(f => Files.deleteIfExists(f.toPath))
        // remove emptied directories bottom-up, keeping root itself
        def dirs(f: File): Seq[File] = {
            val children = f.listFiles()
            if (children == null) Seq.empty
            else children.filter(_.isDirectory).toSeq.flatMap(d => dirs(d) :+ d)
        }
        if (root.isDirectory) {
            dirs(root)
                .sortBy(d => -d.getAbsolutePath.length)
                .foreach { d =>
                    val children = d.listFiles()
                    if (children != null && children.isEmpty) Files.deleteIfExists(d.toPath)
                }
        }
        stale
    }
}
