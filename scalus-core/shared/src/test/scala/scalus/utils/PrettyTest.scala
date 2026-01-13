package scalus.utils

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.cardano.address.*
import scalus.cardano.ledger.*

class PrettyTest extends AnyFunSuite {

    // Common test fixtures
    private val testPolicyId =
        ScriptHash.fromHex("c0ffee0123456789abcdef0123456789abcdef0123456789abcdef01")
    private val testScriptHash =
        ScriptHash.fromHex("0123456789abcdef0123456789abcdef0123456789abcdef01234567")
    private val testHash28 =
        AddrKeyHash.fromHex("0123456789abcdef0123456789abcdef0123456789abcdef01234567")
    private val testHash32 =
        DataHash.fromHex("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
    private val testTxHash =
        TransactionHash.fromHex("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

    // === Core Pretty Typeclass Tests ===

    test("Pretty.formatAda formats lovelace correctly") {
        assert(Pretty.formatAda(0) == "0.000000")
        assert(Pretty.formatAda(1) == "0.000001")
        assert(Pretty.formatAda(1000000) == "1.000000")
        assert(Pretty.formatAda(1500000) == "1.500000")
        assert(Pretty.formatAda(123456789) == "123.456789")
        assert(Pretty.formatAda(-1000000) == "-1.000000")
        assert(Pretty.formatAda(-500000) == "-0.500000")
    }

    test("Pretty primitive instances work") {
        assert("hello".show == "hello")
        assert(42.show == "42")
        assert(123456789L.show == "123456789")
        assert(true.show == "true")
        assert(false.show == "false")
    }

    test("Pretty ByteString shows hex") {
        val bs = ByteString.fromHex("deadbeef")
        assert(bs.show == "deadbeef")
    }

    test("Pretty extension methods work") {
        assert("test".show == "test")
        assert("test".pretty.render(80) == "test")
        assert("test".showHighlighted.nonEmpty)
    }

    // === Hash Tests ===

    test("Pretty[Hash] shows full hex") {
        assert(
          testHash32.show == "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
        )
    }

    test("Pretty[ScriptHash] shows full hex") {
        assert(testScriptHash.show == "0123456789abcdef0123456789abcdef0123456789abcdef01234567")
    }

    // === Coin Tests ===

    test("Pretty[Coin] formats as ADA decimal") {
        assert(Coin(0).show == "0.000000")
        assert(Coin(1000000).show == "1.000000")
        assert(Coin(2500000).show == "2.500000")
        assert(Coin(123456789).show == "123.456789")
    }

    // === Value Tests ===

    test("Pretty[Value] ADA-only shows Lucid flat style") {
        val value = Value.lovelace(2500000)
        val result = value.show
        assert(result.contains("ada: 2.500000"))
        assert(result.contains("{"))
        assert(result.contains("}"))
    }

    test("Pretty[Value] with assets shows vertical layout") {
        val value = Value.asset(
          testPolicyId,
          AssetName(ByteString.fromString("HOSKY")),
          1000000,
          Coin(5000000)
        )
        val result = value.show
        assert(result.contains("ada: 5.000000"))
        assert(result.contains("HOSKY"))
        assert(result.contains("1000000"))
    }

    // === MultiAsset Tests ===

    test("Pretty[MultiAsset] empty shows {}") {
        val ma = MultiAsset.empty
        assert(ma.show == "{}")
    }

    test("Pretty[MultiAsset] single asset shows inline") {
        val ma = MultiAsset(
          scala.collection.immutable.TreeMap(
            testPolicyId -> scala.collection.immutable.TreeMap(
              AssetName(ByteString.fromString("MIN")) -> 500L
            )
          )
        )
        val result = ma.show
        assert(result.contains("MIN"))
        assert(result.contains("500"))
    }

    // === Credential Tests ===

    test("Pretty[Credential] KeyHash") {
        val cred = Credential.KeyHash(testHash28)
        val result = cred.show
        assert(result.startsWith("KeyHash("))
        assert(result.contains("0123456789abcdef"))
    }

    test("Pretty[Credential] ScriptHash") {
        val cred = Credential.ScriptHash(testScriptHash)
        val result = cred.show
        assert(result.startsWith("ScriptHash("))
        assert(result.contains("0123456789abcdef"))
    }

    // === Address Tests ===

    test("Pretty[ShelleyAddress] concise shows bech32") {
        val addr = Address.fromBech32(
          "addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp"
        )
        addr match
            case shelley: ShelleyAddress =>
                val result = shelley.show
                assert(result.startsWith("addr_test1"))
            case _ => fail("Expected ShelleyAddress")
    }

    test("Pretty[ShelleyAddress] detailed shows structure") {
        val addr = Address.fromBech32(
          "addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp"
        )
        addr match
            case shelley: ShelleyAddress =>
                val result = shelley.showDetailed
                assert(result.contains("ShelleyAddress"))
                assert(result.contains("network:"))
                assert(result.contains("payment:"))
                assert(result.contains("delegation:"))
            case _ => fail("Expected ShelleyAddress")
    }

    test("Pretty[StakeAddress] concise shows bech32") {
        val addr = Address.fromBech32(
          "stake_test1uqfu74w3wh4gfzu8m6e7j987h4lq9r3t7ef5gaw497uu85qsqfy27"
        )
        addr match
            case stake: StakeAddress =>
                val result = stake.show
                assert(result.startsWith("stake_test1"))
            case _ => fail("Expected StakeAddress")
    }

    // === TransactionInput Tests ===

    test("Pretty[TransactionInput] shows txHash#index format") {
        val input = TransactionInput(testTxHash, 5)
        val result = input.show
        assert(result.contains("#5"))
        assert(result.contains("0123456789abcdef"))
    }

    // === DatumOption Tests ===

    test("Pretty[DatumOption] Hash") {
        val datum = DatumOption.Hash(testHash32)
        val result = datum.show
        assert(result.startsWith("Hash("))
        assert(result.contains("0123456789abcdef"))
    }

    test("Pretty[DatumOption] Inline") {
        val data = scalus.builtin.Data.I(BigInt(42))
        val datum = DatumOption.Inline(data)
        val result = datum.show
        assert(result.startsWith("Inline("))
    }

    // === Script Tests ===

    test("Pretty[Script] PlutusV3") {
        val scriptBytes = ByteString.fromHex("4e4d0100003322222005")
        val script: Script = Script.PlutusV3(scriptBytes)
        val result = script.show
        assert(result.startsWith("PlutusV3("))
    }

    // === VKeyWitness Tests ===

    test("Pretty[VKeyWitness] shows vkey hash and truncated signature") {
        val vkey =
            ByteString.fromHex("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
        val sig = ByteString.fromHex(
          "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
        )
        val witness = VKeyWitness(vkey, sig)
        val result = witness.show
        assert(result.contains("VKeyWitness"))
        assert(result.contains("vkeyHash"))
        assert(result.contains("sig="))
    }

    // === Redeemer Tests ===

    test("Pretty[Redeemer] shows tag, index, and exUnits") {
        val data = scalus.builtin.Data.I(BigInt(42))
        val redeemer = Redeemer(RedeemerTag.Spend, 0, data, ExUnits(100000, 200000))
        val result = redeemer.show
        assert(result.contains("Spend"))
        assert(result.contains("#0"))
        assert(result.contains("100000"))
        assert(result.contains("200000"))
    }

    // === DRep Tests ===

    test("Pretty[DRep] AlwaysAbstain") {
        val drep = DRep.AlwaysAbstain
        assert(drep.show == "DRep.AlwaysAbstain")
    }

    test("Pretty[DRep] AlwaysNoConfidence") {
        val drep = DRep.AlwaysNoConfidence
        assert(drep.show == "DRep.AlwaysNoConfidence")
    }

    test("Pretty[DRep] KeyHash") {
        val drep = DRep.KeyHash(testHash28)
        val result = drep.show
        assert(result.startsWith("DRep.KeyHash("))
        assert(result.contains("0123456789abcdef"))
    }

    // === Anchor Tests ===

    test("Pretty[Anchor] shows url and full hash") {
        val anchor = Anchor("https://example.com/metadata.json", testHash32)
        val result = anchor.show
        assert(result.contains("Anchor"))
        assert(result.contains("https://example.com/metadata.json"))
        assert(result.contains("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
    }

    // === Certificate Tests ===

    test("Pretty[Certificate] RegCert") {
        val cred = Credential.KeyHash(testHash28)
        val cert = Certificate.RegCert(cred, Some(Coin(2000000)))
        val result = cert.show
        assert(result.contains("RegCert"))
        assert(result.contains("deposit=2.000000"))
    }

    test("Pretty[Certificate] VoteDelegCert") {
        val cred = Credential.KeyHash(testHash28)
        val cert = Certificate.VoteDelegCert(cred, DRep.AlwaysAbstain)
        val result = cert.show
        assert(result.contains("VoteDelegCert"))
        assert(result.contains("AlwaysAbstain"))
    }

    // === Language Tests ===

    test("Pretty[Language] shows language name") {
        assert(Language.PlutusV1.show == "PlutusV1")
        assert(Language.PlutusV2.show == "PlutusV2")
        assert(Language.PlutusV3.show == "PlutusV3")
    }

    // === AssetName Tests ===

    test("Pretty[AssetName] ASCII printable") {
        val name = AssetName(ByteString.fromString("HOSKY"))
        assert(name.show == "HOSKY")
    }

    test("Pretty[AssetName] non-ASCII shows hex") {
        val name = AssetName(ByteString.fromHex("ff00ff"))
        assert(name.show == "ff00ff")
    }

    // === XTerm Styling Tests ===

    test("Pretty XTerm styling applies ANSI escape codes to Coin") {
        val coin = Coin(1000000)
        val highlighted = coin.showHighlighted
        // XTerm styling adds ANSI escape codes (starts with ESC[)
        assert(highlighted.contains("\u001b["), s"Expected ANSI codes in: $highlighted")
    }

    test("Pretty XTerm styling applies colors to Value") {
        val value = Value.lovelace(2500000)
        val highlighted = value.showHighlighted
        assert(highlighted.contains("\u001b["), s"Expected ANSI codes in: $highlighted")
    }

    test("Pretty XTerm styling applies colors to Credential") {
        val cred = Credential.KeyHash(testHash28)
        val highlighted = cred.showHighlighted
        assert(highlighted.contains("\u001b["), s"Expected ANSI codes in: $highlighted")
    }

    test("Pretty XTerm styling applies colors to TransactionInput") {
        val input = TransactionInput(testTxHash, 0)
        val highlighted = input.showHighlighted
        assert(highlighted.contains("\u001b["), s"Expected ANSI codes in: $highlighted")
    }

    // === Pretty[Option[A]] Tests ===

    test("Pretty[Option[A]] Some shows value") {
        val opt: Option[Coin] = Some(Coin(1000000))
        val result = opt.show
        assert(result == "1.000000")
    }

    test("Pretty[Option[A]] None shows empty") {
        val opt: Option[Coin] = None
        val result = opt.show
        assert(result == "")
    }

    // === Pretty[Seq[A]] Tests ===

    test("Pretty[Seq[A]] empty shows []") {
        val seq: Seq[Coin] = Seq.empty
        val result = seq.show
        assert(result == "[]")
    }

    test("Pretty[Seq[A]] non-empty shows comma-separated values") {
        val seq: Seq[Coin] = Seq(Coin(1000000), Coin(2000000))
        val result = seq.show
        assert(result.contains("1.000000"))
        assert(result.contains("2.000000"))
    }
}
