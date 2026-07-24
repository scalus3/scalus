package scalus.sbt

import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.Files

class BlueprintLayoutTest extends AnyFunSuite {

    private def tempDir(): File = {
        val dir = Files.createTempDirectory("blueprint-layout-test").toFile
        dir.deleteOnExit()
        dir
    }

    private def write(dir: File, relPath: String, content: String): File = {
        val f = new File(dir, relPath)
        f.getParentFile.mkdirs()
        Files.write(f.toPath, content.getBytes("UTF-8"))
        f
    }

    test("contractRelativePath nests by package and strips the module suffix") {
        assert(
          BlueprintLayout.contractRelativePath("scalus.examples.auction.AuctionContract$") ==
              "scalus/examples/auction/AuctionContract.json"
        )
        assert(
          BlueprintLayout.contractRelativePath("scalus.examples.HelloCardanoContract$") ==
              "scalus/examples/HelloCardanoContract.json"
        )
    }

    test("contractRelativePath handles classes without a package") {
        assert(BlueprintLayout.contractRelativePath("RootContract$") == "RootContract.json")
        assert(BlueprintLayout.contractRelativePath("RootContract") == "RootContract.json")
    }

    test("listFilesRecursively returns regular files sorted by path, empty for non-dirs") {
        val dir = tempDir()
        write(dir, "b/two.class", "2")
        write(dir, "a/one.class", "1")
        write(dir, "top.class", "0")
        val listed = BlueprintLayout.listFilesRecursively(dir).map(_.getName)
        // sorted by absolute path: <dir>/a/one.class < <dir>/b/two.class < <dir>/top.class
        assert(listed == Seq("one.class", "two.class", "top.class"))
        assert(BlueprintLayout.listFilesRecursively(new File(dir, "missing")).isEmpty)
    }

    test("fingerprint is stable for identical content and changes with content/header/path") {
        val dir1 = tempDir()
        write(dir1, "a/X.class", "same")
        val dir2 = tempDir()
        write(dir2, "a/X.class", "same")
        def fp(dir: File, header: String): String =
            BlueprintLayout.fingerprint(header, dir, BlueprintLayout.listFilesRecursively(dir))
        assert(fp(dir1, "h") == fp(dir2, "h"))
        assert(fp(dir1, "h") != fp(dir1, "other-header"))
        write(dir2, "a/X.class", "changed")
        assert(fp(dir1, "h") != fp(dir2, "h"))
        val dir3 = tempDir()
        write(dir3, "a/Y.class", "same") // same content, different path
        assert(fp(dir1, "h") != fp(dir3, "h"))
    }

    test("pruneStale deletes unkept json files and emptied dirs, keeps everything else") {
        val root = tempDir()
        val keep = write(root, "scalus/examples/Keep.json", "{}")
        write(root, "scalus/old/Stale.json", "{}")
        val notJson = write(root, "scalus/old/notes.txt", "keep me")
        val deleted = BlueprintLayout.pruneStale(root, Set(keep))
        assert(deleted.map(_.getName) == Seq("Stale.json"))
        assert(keep.isFile)
        assert(notJson.isFile) // non-json files are never deleted
        assert(new File(root, "scalus/examples").isDirectory)
    }

    test("pruneStale removes directories left empty and is a no-op on a missing root") {
        val root = tempDir()
        write(root, "a/b/OnlyOne.json", "{}")
        BlueprintLayout.pruneStale(root, Set.empty[File])
        assert(!new File(root, "a").exists()) // a/b/OnlyOne.json deleted, a/b and a emptied
        assert(root.isDirectory) // the root itself survives
        assert(BlueprintLayout.pruneStale(new File(root, "missing"), Set.empty[File]).isEmpty)
    }
}
