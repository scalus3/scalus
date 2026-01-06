package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.platform
import scalus.cardano.ledger.{CardanoInfo, SlotConfig}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.{byteArray2Int8Array, Uint8Array}

class JScalusTest extends AnyFunSuite {

    test("JScalus.evalPlutusScripts with CBOR files") {
        // Read transaction CBOR bytes using platform-specific file I/O
        val txBytes = platform
            .readFile(
              "scalus-examples/js/src/main/ts/tx-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"
            )
        val tx = new Uint8Array(byteArray2Int8Array(txBytes).buffer)

        // Read UTxO CBOR bytes using platform-specific file I/O
        val utxoBytes = platform
            .readFile(
              "scalus-examples/js/src/main/ts/utxo-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"
            )
        val utxo = new Uint8Array(byteArray2Int8Array(utxoBytes).buffer)

        val costModels =
            CardanoInfo.mainnet.protocolParams.costModels.models.values
                .map(_.map(_.toDouble).toJSArray)
                .toJSArray

        // Evaluate Plutus scripts
        val redeemers = JScalus.evalPlutusScripts(tx, utxo, SlotConfig.mainnet, costModels)

        // Verify results
        assert(redeemers.length == 2, "Should have 2 redeemers evaluated")
    }
}
