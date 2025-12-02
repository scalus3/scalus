package scalus.uplc.eval

import scalus.builtin.platform
import scalus.cardano.ledger.{CardanoInfo, Language}

/** Tests for the Plutus Conformance Test Suite.
  *
  * @note
  *   This tests run only on JVM right now.
  */
class PlutusConformanceNativeTest extends PlutusConformanceTest:

    override protected def createPlutusVM: PlutusVM = {
        // Read the cost model from file system instead of classpath resources
        val costModelJson =
            new String(
              platform.readFile("scalus-core/shared/src/main/resources/builtinCostModelC.json"),
              "UTF-8"
            )
        val builtinCostModel = BuiltinCostModel.fromJsonString(costModelJson)
        // Get machine costs from protocol params (same as makePlutusV3VM does)
        val baseParams = MachineParams.fromProtocolParams(
          CardanoInfo.mainnet.protocolParams,
          Language.PlutusV3
        )
        val params = MachineParams(baseParams.machineCosts, builtinCostModel)
        PlutusVM.makePlutusV3VM(params)
    }
