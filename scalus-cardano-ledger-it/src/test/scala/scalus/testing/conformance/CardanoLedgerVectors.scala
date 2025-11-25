package scalus.testing.conformance

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream

import java.io.{FileInputStream, FileOutputStream}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

/** Importer for Cardano Ledger conformance test vectors
  *
  * Supports importing test vectors from the cardano-ledger dump format as described in
  * https://github.com/SundaeSwap-finance/cardano-ledger-conformance-tests
  */
object CardanoLedgerVectors {

    // Extract vectors.tar.gz to a temporary directory
    lazy val conformanceVectorsPath: Path = {
        val vectorsTarGz = getClass.getClassLoader.getResource("vectors.tar.gz")
        if vectorsTarGz == null then {
            null
        } else {
            val tempDir = Files.createTempDirectory("cardano-ledger-vectors")

            Using.resource(new FileInputStream(Paths.get(vectorsTarGz.toURI).toFile)) { fis =>
                Using.resource(new GzipCompressorInputStream(fis)) { gzis =>
                    Using.resource(new TarArchiveInputStream(gzis)) { tis =>
                        var entry = tis.getNextTarEntry
                        while entry != null do {
                            val outputFile = tempDir.resolve(entry.getName).toFile
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
                            entry = tis.getNextTarEntry
                        }
                    }
                }
            }

            tempDir.resolve("eras/conway/impl/dump")
        }
    }

    // Skip tests if vectors directory doesn't exist
    val vectorsExist: Boolean =
        conformanceVectorsPath != null && Files.exists(conformanceVectorsPath)

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

        /** Test category/name */
        testState: String
    )

    given JsonValueCodec[RawTestVector] = JsonCodecMaker.make

    /** Load a single test vector from a file */
    def loadRawVector(path: Path): Try[RawTestVector] = Try {
        val json = Files.readString(path)
        readFromString[RawTestVector](json)
    }

    def vectorNames(): List[String] =
        Files
            .list(conformanceVectorsPath)
            .map(_.getFileName.toFile.getName)
            .iterator()
            .asScala
            .toList.sorted

    /** Load all test vectors from a directory tree */
    def loadAllVectors(name: String): List[(Path, RawTestVector)] = {
        val rootPath = conformanceVectorsPath.resolve(name)
        if !Files.exists(rootPath) then {
            println(s"Warning: Test vector directory not found: $rootPath")
            return List.empty
        }

        Files
            .walk(rootPath)
            .iterator()
            .asScala
            .filter(p => Files.isRegularFile(p) && !p.getFileName.toString.startsWith("."))
            .filter(p => !p.toString.contains("pparams-by-hash")) // Skip protocol params files
            .filter(p => !p.getFileName.toString.endsWith(".md")) // Skip markdown files
            .filter(p => !p.getFileName.toString.endsWith(".tar.gz")) // Skip tar.gz files
            .flatMap { path =>
                loadRawVector(path) match {
                    case scala.util.Success(vector) => Some((path, vector))
                    case scala.util.Failure(e)      =>
                        // Skip files that aren't valid JSON vectors
                        None
                }
            }
            .toList
    }
}
