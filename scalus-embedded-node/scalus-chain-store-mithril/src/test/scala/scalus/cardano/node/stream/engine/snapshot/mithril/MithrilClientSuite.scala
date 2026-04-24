package scalus.cardano.node.stream.engine.snapshot.mithril

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

/** End-to-end exercise of the typed facade against the live testing-preview aggregator. Combines
  * the WASM path (metadata listing via `mithrilclient_list_*`) with the JVM path (HTTP download +
  * tar+zstd extraction). Tagged `[network]` so CI skips it.
  */
final class MithrilClientSuite extends AnyFunSuite {

    private val aggregatorUrl =
        "https://aggregator.testing-preview.api.mithril.network/aggregator"
    private val genesisVerificationKey =
        "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38" +
            "352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234" +
            "332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"

    test("[network] facade: list → metadata → download one immutable end-to-end") {
        given ExecutionContext = ExecutionContext.global
        val client = MithrilClient.create(aggregatorUrl, genesisVerificationKey)
        try {
            val certs = Await.result(client.listCertificates(), 30.seconds)
            assert(certs.nonEmpty)
            info(s"${certs.size} certificates; latest hash=${certs.head.hash}")

            val snapshots = Await.result(client.listCardanoDatabaseV2Snapshots(), 30.seconds)
            assert(snapshots.nonEmpty)
            val latest = snapshots.head
            info(
              s"${snapshots.size} v2 snapshots; latest hash=${latest.hash} " +
                  s"immutable=${latest.beacon.immutableFileNumber}"
            )

            val metaOpt = Await.result(client.getCardanoDatabaseV2Snapshot(latest.hash), 30.seconds)
            val meta = metaOpt.getOrElse(fail(s"metadata not found for ${latest.hash}"))
            info(
              s"merkle_root=${meta.merkleRoot} " +
                  s"immutables.avg=${meta.immutables.averageSizeUncompressed}B"
            )

            val workDir = Files.createTempDirectory("scalus-mithril-client-test-")
            try {
                val files = Await.result(
                  client.downloadImmutable(meta, meta.beacon.immutableFileNumber, workDir),
                  120.seconds
                )
                val names = files.map(_.getFileName.toString).toSet
                info(s"extracted ${files.size} files: ${names.mkString(", ")}")
                val n = meta.beacon.immutableFileNumber
                assert(names.contains(s"$n.chunk"))
                assert(names.contains(s"$n.primary"))
                assert(names.contains(s"$n.secondary"))
            } finally
                scala.util.Try {
                    import scala.jdk.CollectionConverters.*
                    Files.walk(workDir).iterator.asScala.toSeq.reverse.foreach(Files.deleteIfExists)
                }
        } finally client.close()
    }
}
