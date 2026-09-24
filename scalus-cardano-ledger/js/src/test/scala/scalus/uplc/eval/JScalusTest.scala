package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.platform
import scalus.cardano.ledger.{CardanoInfo, JsSlotConfig, MajorProtocolVersion}
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

class JScalusTest extends AnyFunSuite {

    test("exported methods work detached from the Scalus object") {
        val script = "545301010023357389210753756363657373004981"
        val detached = JScalus
            .asInstanceOf[js.Dynamic]
            .applyDataArgToScript
            .asInstanceOf[js.Function2[String, String, String]]
        assert(
          detached(script, "{\"int\":42}") == JScalus.applyDataArgToScript(script, "{\"int\":42}")
        )
    }

    test("JScalus.evalPlutusScripts with CBOR files") {
        // Read transaction CBOR bytes using platform-specific file I/O
        val txBytes = platform
            .readFile(
              "scalus-examples/js/src/main/ts/tx-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"
            )
        val tx = txBytes.toUint8Array

        // Read UTxO CBOR bytes using platform-specific file I/O
        val utxoBytes = platform
            .readFile(
              "scalus-examples/js/src/main/ts/utxo-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"
            )
        val utxo = utxoBytes.toUint8Array

        val costModels =
            CardanoInfo.mainnet.protocolParams.costModels.models.values
                .map(_.map(_.toDouble).toJSArray)
                .toJSArray

        // Evaluate Plutus scripts
        val redeemers = JScalus.evalPlutusScripts(tx, utxo, JsSlotConfig.mainnet, costModels)

        // Verify results
        assert(redeemers.length == 2, "Should have 2 redeemers evaluated")

        val redeemersWithProtocolVersion = JScalus.evalPlutusScripts(
          tx,
          utxo,
          JsSlotConfig.mainnet,
          costModels,
          MajorProtocolVersion.vanRossemPV.version
        )
        assert(redeemersWithProtocolVersion.length == 2, "Should accept protocol major version")
    }
}
