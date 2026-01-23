package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

class UtxoQueryMacrosSpec extends AnyFunSuite {

    // Test fixtures - using valid mainnet addresses from CIP-19 test vectors
    private val testAddress = Address.fromBech32(
      "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"
    )

    private val testAddress2 = Address.fromBech32(
      "addr1z8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs9yc0hh"
    )

    private val testTxHash = TransactionHash.fromHex(
      "0000000000000000000000000000000000000000000000000000000000000001"
    )

    // PolicyId is a type alias for ScriptHash (28 bytes = 56 hex chars)
    private val testPolicyId: PolicyId = ScriptHash.fromHex(
      "00000000000000000000000000000000000000000000000000000002"
    )

    private val testAssetName = AssetName(ByteString.fromString("TestToken"))

    private val testDatumHash = DataHash.fromHex(
      "0000000000000000000000000000000000000000000000000000000000000003"
    )

    test("buildQuery: simple address query") {
        val addr = testAddress
        val query = UtxoQueryMacros.buildQuery { u =>
            u.output.address == addr
        }

        query match
            case UtxoQuery.Simple(UtxoSource.FromAddress(a), None, None, None, None) =>
                assert(a == addr)
            case _ => fail(s"Unexpected query structure: $query")
    }

    test("buildQuery: address query with reversed operands") {
        val addr = testAddress
        val query = UtxoQueryMacros.buildQuery { u =>
            addr == u.output.address
        }

        query match
            case UtxoQuery.Simple(UtxoSource.FromAddress(a), None, None, None, None) =>
                assert(a == addr)
            case _ => fail(s"Unexpected query structure: $query")
    }

    test("buildQuery: transaction id query") {
        val txId = testTxHash
        val query = UtxoQueryMacros.buildQuery { u =>
            u.input.transactionId == txId
        }

        query match
            case UtxoQuery.Simple(UtxoSource.FromTransaction(t), None, None, None, None) =>
                assert(t == txId)
            case _ => fail(s"Unexpected query structure: $query")
    }

    test("buildQuery: standalone asset query (becomes source)") {
        val policyId = testPolicyId
        val assetName = testAssetName
        val query = UtxoQueryMacros.buildQuery { u =>
            u.output.value.hasAsset(policyId, assetName)
        }

        query match
            case UtxoQuery.Simple(UtxoSource.FromAsset(p, n), None, None, None, None) =>
                assert(p == policyId)
                assert(n == assetName)
            case _ => fail(s"Unexpected query structure: $query")
    }

    test("buildQuery: address with asset filter") {
        val addr = testAddress
        val policyId = testPolicyId
        val assetName = testAssetName
        val query = UtxoQueryMacros.buildQuery { u =>
            u.output.address == addr && u.output.value.hasAsset(policyId, assetName)
        }

        query match
            case UtxoQuery.Simple(
                  UtxoSource.FromAddress(a),
                  Some(UtxoFilter.HasAsset(p, n)),
                  None,
                  None,
                  None
                ) =>
                assert(a == addr)
                assert(p == policyId)
                assert(n == assetName)
            case _ => fail(s"Unexpected query structure: $query")
    }

    test("buildQuery: address with min lovelace filter") {
        val addr = testAddress
        val minAmount = Coin.ada(5)
        val query = UtxoQueryMacros.buildQuery { u =>
            u.output.address == addr && u.output.value.coin >= minAmount
        }

        query match
            case UtxoQuery.Simple(
                  UtxoSource.FromAddress(a),
                  Some(UtxoFilter.MinLovelace(amt)),
                  None,
                  None,
                  None
                ) =>
                assert(a == addr)
                assert(amt == minAmount)
            case _ => fail(s"Unexpected query structure: $query")
    }

    test("buildQuery: address with datum hash filter (extension method)") {
        val addr = testAddress
        val hash = testDatumHash
        val query = UtxoQueryMacros.buildQuery { u =>
            u.output.address == addr && u.output.hasDatumHash(hash)
        }

        query match
            case UtxoQuery.Simple(
                  UtxoSource.FromAddress(a),
                  Some(UtxoFilter.HasDatumHash(h)),
                  None,
                  None,
                  None
                ) =>
                assert(a == addr)
                assert(h == hash)
            case _ => fail(s"Unexpected query structure: $query")
    }

    test("buildQuery: OR of two addresses") {
        val addr1 = testAddress
        val addr2 = testAddress2
        val query = UtxoQueryMacros.buildQuery { u =>
            u.output.address == addr1 || u.output.address == addr2
        }

        query match
            case UtxoQuery.Or(
                  UtxoQuery.Simple(UtxoSource.FromAddress(a1), None, None, None, None),
                  UtxoQuery.Simple(UtxoSource.FromAddress(a2), None, None, None, None),
                  None,
                  None,
                  None
                ) =>
                assert(a1 == addr1)
                assert(a2 == addr2)
            case _ => fail(s"Unexpected query structure: $query")
    }

    test("buildQuery: multiple filters with AND") {
        val addr = testAddress
        val policyId = testPolicyId
        val assetName = testAssetName
        val minAmount = Coin.ada(10)
        val query = UtxoQueryMacros.buildQuery { u =>
            u.output.address == addr &&
            u.output.value.hasAsset(policyId, assetName) &&
            u.output.value.coin >= minAmount
        }

        query match
            case UtxoQuery.Simple(
                  UtxoSource.FromAddress(a),
                  Some(UtxoFilter.And(UtxoFilter.HasAsset(p, n), UtxoFilter.MinLovelace(amt))),
                  None,
                  None,
                  None
                ) =>
                assert(a == addr)
                assert(p == policyId)
                assert(n == assetName)
                assert(amt == minAmount)
            case _ => fail(s"Unexpected query structure: $query")
    }

    test("buildQuery: asset as filter when combined with address source") {
        val addr = testAddress
        val policyId = testPolicyId
        val assetName = testAssetName
        // When hasAsset comes after an address, it becomes a filter
        val query = UtxoQueryMacros.buildQuery { u =>
            u.output.address == addr && u.output.value.hasAsset(policyId, assetName)
        }

        query match
            case UtxoQuery.Simple(
                  UtxoSource.FromAddress(_),
                  Some(UtxoFilter.HasAsset(_, _)),
                  _,
                  _,
                  _
                ) =>
                succeed
            case _ => fail(s"Expected address source with asset filter, got: $query")
    }

    test("buildQuery: asset as source when standalone") {
        val policyId = testPolicyId
        val assetName = testAssetName
        val query = UtxoQueryMacros.buildQuery { u =>
            u.output.value.hasAsset(policyId, assetName)
        }

        query match
            case UtxoQuery.Simple(UtxoSource.FromAsset(_, _), None, _, _, _) =>
                succeed
            case _ => fail(s"Expected asset source, got: $query")
    }
}
