package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.*
import scalus.cardano.onchain.plutus.v1 as pv1
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.utils.Hex

import scala.collection.immutable.TreeMap

class LedgerToPlutusTranslationTest extends AnyFunSuite {
    private val address = ShelleyAddress(
      Network.Mainnet,
      ShelleyPaymentPart.Key(AddrKeyHash.fromHex("11" * 28)),
      ShelleyDelegationPart.Null
    )
    private val input = TransactionInput(
      TransactionHash.fromByteString(ByteString.fromHex("22" * 32)),
      0
    )
    private val referenceInput = input.copy(index = 1)
    private val pv = MajorProtocolVersion.plominPV

    private val firstPolicy = ScriptHash.fromHex("22" * 28)
    private val secondPolicy = ScriptHash.fromHex("33" * 28)
    private val tokenAmounts = Seq("01" -> 7L, "0100" -> 8L, "02" -> 9L)
    private val nativeAssetCbor =
        "581c" + "22" * 28 + "a341011142010012410213" +
            "581c" + "33" * 28 + "a341010742010008410209"

    private def nativeAssets(reverse: Boolean): MultiAsset = {
        val tokenOrder = summon[Ordering[AssetName]]
        val ordering = if reverse then tokenOrder.reverse else tokenOrder
        def tokens(offset: Long) = TreeMap.from(
          tokenAmounts.map { (name, amount) =>
              AssetName(ByteString.fromHex(name)) -> (amount + offset)
          }
        )(using ordering)
        val assets = MultiAsset(
          TreeMap(firstPolicy -> tokens(10), secondPolicy -> tokens(0))(using
            summon[Ordering[PolicyId]].reverse
          )
        )
        val expectedOrder =
            if reverse then Seq("02", "0100", "01") else Seq("01", "0100", "02")
        assert(assets.assets(secondPolicy).keysIterator.map(_.bytes.toHex).toSeq == expectedOrder)
        assets
    }

    // cardano-ledger transValue uses transCoinToValue's nested singleton, including zero.
    // The value field has the same Data encoding in V1, V2 and V3.
    for
        (amount, expectedAdaCbor, expectedMultiCbor) <- Seq(
          (0L, "a140a14000", "a340a14000" + nativeAssetCbor),
          (1L, "a140a14001", "a340a14001" + nativeAssetCbor)
        )
        (assetLabel, assetOrder) <- Seq(
          "no native assets" -> None,
          "multi-policy default" -> Some(false),
          "multi-policy reverse" -> Some(true)
        )
        babbage <- Seq(false, true)
        version <- Seq(1, 2, 3)
    do {
        val shape = if babbage then "Babbage" else "Shelley"
        test(s"V$version $shape output Data retains $amount lovelace with $assetLabel") {
            val value =
                Value(Coin(amount), assetOrder.map(nativeAssets).getOrElse(MultiAsset.empty))
            val expectedCbor = if assetOrder.isDefined then expectedMultiCbor else expectedAdaCbor
            val output: TransactionOutput =
                if babbage then TransactionOutput.Babbage(address, value)
                else TransactionOutput.Shelley(address, value)
            val tx = Transaction(
              TransactionBody(
                inputs = TaggedSortedSet.from(Set(input)),
                referenceInputs = TaggedSortedSet.from(Set(referenceInput)),
                outputs = IndexedSeq(Sized(output)),
                fee = Coin(amount)
              )
            )
            val utxos = Map(input -> output, referenceInput -> output)
            val expectedValue = Data.fromCbor(Hex.hexToBytes(expectedCbor))

            def check(value: pv1.Value): Unit = {
                if assetOrder.isDefined then
                    for (name, amount) <- tokenAmounts do
                        val token = ByteString.fromHex(name)
                        assert(value.quantityOf(secondPolicy, token) == BigInt(amount))
                        assert(value.quantityOf(firstPolicy, token) == BigInt(amount + 10))
                assert(value.toData == expectedValue)
                assert(Hex.bytesToHex(value.toData.toCbor) == expectedCbor)
            }

            version match
                case 1 =>
                    check(LedgerToPlutusTranslation.getTxOutV1(Sized(output)).value)
                    val info =
                        LedgerToPlutusTranslation.getTxInfoV1(tx, utxos, SlotConfig.mainnet, pv)
                    assert(Hex.bytesToHex(info.fee.toData.toCbor) == expectedAdaCbor)
                    check(info.outputs.head.value)
                    check(info.inputs.head.resolved.value)
                case 2 =>
                    check(LedgerToPlutusTranslation.getTxOutV2(Sized(output)).value)
                    val info =
                        LedgerToPlutusTranslation.getTxInfoV2(tx, utxos, SlotConfig.mainnet, pv)
                    assert(Hex.bytesToHex(info.fee.toData.toCbor) == expectedAdaCbor)
                    check(info.outputs.head.value)
                    check(info.inputs.head.resolved.value)
                    check(info.referenceInputs.head.resolved.value)
                case _ =>
                    val info =
                        LedgerToPlutusTranslation.getTxInfoV3(tx, utxos, SlotConfig.mainnet, pv)
                    check(info.outputs.head.value)
                    check(info.inputs.head.resolved.value)
                    check(info.referenceInputs.head.resolved.value)
        }
    }

    test("zero ADA precedes native assets in output Data") {
        val policy = ScriptHash.fromHex("33" * 28)
        val token = AssetName(ByteString.fromHex("44"))
        val value = Value(Coin.zero, MultiAsset.asset(policy, token, 7L))
        val expected = "a240a14000581c" + "33" * 28 + "a1414407"
        val translated = LedgerToPlutusTranslation.getValue(value)
        assert(Hex.bytesToHex(translated.toData.toCbor) == expected)
    }

    for
        (amount, expectedCbor) <- Seq(0L -> "a140a14000", 1L -> "a140a14001")
        version <- Seq(1, 2)
    do {
        test(s"V$version fee Data retains $amount lovelace") {
            val tx = Transaction(
              TransactionBody(
                inputs = TaggedSortedSet.from(Set.empty[TransactionInput]),
                outputs = IndexedSeq.empty,
                fee = Coin(amount)
              )
            )
            val fee =
                if version == 1 then
                    LedgerToPlutusTranslation.getTxInfoV1(tx, Map.empty, SlotConfig.mainnet, pv).fee
                else
                    LedgerToPlutusTranslation.getTxInfoV2(tx, Map.empty, SlotConfig.mainnet, pv).fee
            assert(fee.toData == Data.fromCbor(Hex.hexToBytes(expectedCbor)))
            assert(Hex.bytesToHex(fee.toData.toCbor) == expectedCbor)
        }
    }

    test("general-purpose Plutus Value still normalizes zero lovelace") {
        assert(Hex.bytesToHex(pv1.Value.lovelace(0).toData.toCbor) == "a0")
        assert(Hex.bytesToHex(pv1.Value.zero.toData.toCbor) == "a0")
    }
}
