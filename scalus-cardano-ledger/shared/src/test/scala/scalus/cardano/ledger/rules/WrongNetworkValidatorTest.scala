package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, ByronAddress}
import scalus.cardano.address.Network.Mainnet
import scalus.uplc.builtin.ByteString

class WrongNetworkValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    // Create a mainnet Byron address (no network magic = mainnet)
    // Context uses Network.Testnet by default, so this address should fail validation
    private val mainnetByronAddress = ByronAddress.create(
      addrRoot = ByteString.fromHex("aa5372095aaa680d19d4ca496983a145709c3be18b0d4c83cb7bdc5e"),
      addrType = 0,
      networkMagic = None // No magic = mainnet
    )

    // The rule reads only the addresses of the outputs and of the collateral return output.
    private def withOutput(address: Address): Transaction =
        Transaction(
          emptyBody.copy(outputs = IndexedSeq(Sized(Output(address, Value.zero)))),
          TransactionWitnessSet.empty
        )

    private def withCollateralReturn(address: Address): Transaction =
        Transaction(
          emptyBody.copy(collateralReturnOutput = Some(Sized(Output(address, Value.zero)))),
          TransactionWitnessSet.empty
        )

    test(
      "WrongNetworkValidator rule success for outputs for ShelleyAddress with matching network"
    ) {
        val transaction = withOutput(keyAddress(0))
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isRight, result)
    }

    test(
      "WrongNetworkValidator rule failure for outputs for ShelleyAddress with non-matching network"
    ) {
        val transaction = withOutput(keyAddress(0, Mainnet))
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isLeft, result)
    }

    test("WrongNetworkValidator rule success for outputs for StakeAddress with matching network") {
        val transaction = withOutput(rewardAccount(0).address)
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isRight, result)
    }

    test(
      "WrongNetworkValidator rule failure for outputs for StakeAddress with non-matching network"
    ) {
        val transaction = withOutput(rewardAccount(0, Mainnet).address)
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isLeft, result)
    }

    test("WrongNetworkValidator rule failure for outputs for ByronAddress") {
        val transaction = withOutput(mainnetByronAddress)
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isLeft, result)
    }

    test(
      "WrongNetworkValidator rule success for collateralReturnOutput for ShelleyAddress with matching network"
    ) {
        val transaction = withCollateralReturn(keyAddress(0))
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isRight, result)
    }

    test(
      "WrongNetworkValidator rule failure for collateralReturnOutput for ShelleyAddress with non-matching network"
    ) {
        val transaction = withCollateralReturn(keyAddress(0, Mainnet))
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isLeft, result)
    }

    test(
      "WrongNetworkValidator rule success for collateralReturnOutput for StakeAddress with matching network"
    ) {
        val transaction = withCollateralReturn(rewardAccount(0).address)
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isRight, result)
    }

    test(
      "WrongNetworkValidator rule failure for collateralReturnOutput for StakeAddress with non-matching network"
    ) {
        val transaction = withCollateralReturn(rewardAccount(0, Mainnet).address)
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isLeft, result)
    }

    test("WrongNetworkValidator rule failure for collateralReturnOutput for ByronAddress") {
        val transaction = withCollateralReturn(mainnetByronAddress)
        val result = validate(WrongNetworkValidator, Map.empty, transaction)
        assert(result.isLeft, result)
    }
}
