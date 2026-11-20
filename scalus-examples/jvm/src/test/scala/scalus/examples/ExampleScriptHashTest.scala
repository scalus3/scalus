package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.ScalaCompilerVersion
import scalus.cardano.blueprint.Contract

import scala.io.Source
import scala.jdk.CollectionConverters.*

/** The script hash of every example contract must not change by accident. A new hash is a new
  * script address, even when the size and the budget stay the same, and budget pins do not see it:
  * batched CSE gave two of these contracts new bytes with the same size and budget.
  *
  * A compiler or optimizer change that alters a script updates the table, so the change and the
  * release notes can name the scripts. The failure message prints the new table. Scala 3.8 and
  * later compile other scripts, so the table holds for the Scala 3.3 LTS only.
  */
class ExampleScriptHashTest extends AnyFunSuite {

    // contract (under scalus.examples), script hash, script size in bytes
    private val expected: Map[String, (String, Int)] = """
        HelloCardanoContract ec767659c8e7432290657b05be0c61d56eab3c543e716fa432ad7062 149
        amm.AmmContract e49473c6be76cd5437663a72ea1055a755a5f34c3213855f15bc2991 2901
        auction.AuctionContract cab6ccd7fb488f3d68b101b13ac97caa793c2607f33c9658cae14123 2190
        betting.BettingContract 6e6115a35dd16b3a39ca4cbc56f270e43e6a64d7cea9dae4e038310b 2015
        bilinearAccumulator.AllowlistContract e13d1a8a51135e0043a40b03a816975ab1a3767b68d8915519c2960b 588
        cape.twopartyescrow.TwoPartyEscrowContract 25c577da3c1e958d44fa72284b0a194e9517e49d94001249b830cac3 1040
        crowdfunding.CrowdfundingContract 4fafddcb7bb85503a5dd3ee6d8af38987e48ff6294582f8c0f1ccbae 3031
        crowdfunding.DonationMintingContract 5c6c2fa1e519548473a49b5e274ac08284201eb6017fdcaba06038f9 749
        decentralizedidentity.DecentralizedIdentityContract 77253ee698c5b13e7a2c3a304ec7f195881c30fe52670105f5681aec 2289
        editablenft.EditableNftContract 5e00fd1f6922eabaebd594a9e4c0158bf1664b20db4d014aeecf7bb9 1033
        escrow.EscrowContract 5acfb2b7df64ec6aca9e9af9a7d2794521f1f3bf65cb8115fa0756d7 1019
        factory.FactoryContract 82cfb006a3f199d496acb2f164a0c1366332523c4ef09171f94c93f1 911
        htlc.HtlcContract ab3c87e431f747800398333b321ab53639fa268c5eead27ce538c710 310
        linkedlist.LinkedListContract 453ad5c4dec8895effcc315ead46dd38e60a0467d06c62d06d5a4b9f 3853
        lottery.LotteryContract ce65d6c3e05a99497aaf8d465d1366c71edf7687187b853c6c82a770 1817
        paymentsplitter.NaivePaymentSplitterContract 3cbb2fa3f2072fec8dfd28c4288aec316494858fdacec6b44c90e82b 1533
        paymentsplitter.OptimizedPaymentSplitterContract ddeacc26df24b2190e8d099cc0475b8556dba16fa9bea81a60876757 1259
        pricebet.PricebetContract 43cf8cc805b354fcec7dc3ca469a8d559afcb5553a0b3bfb8f434455 933
        pricebet.PricebetOracleContract 2ea2be7b663f4e5b65a27a6fef17843f4bdfb0e7ad98b53a7bfae8b1 1012
        simpletransfer.SimpleTransferContract 905582a24deaf01a2099bb94b6e36bfea77f0624491cb5f14b7dd7b5 826
        upgradeableproxy.UpgradeableProxyContract 7d1fa6ef9f01e47b834263d9c2f0717d1fce4db5c53e9cd7711ae80e 641
        vault.VaultContract ff89f6b2ce3d9a15049d8518988d8ed8340b86ee1d33c8bab2249566 1321
        vesting.VestingContract b3f6d8a65f4de978e93b754b6b3e47ec55ef7d0cbf3c2c6ceb324504 837
        """.linesIterator
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(_.split(' '))
        .map(a => a(0) -> (a(1), a(2).toInt))
        .toMap

    /** The contracts that the Scalus compiler plugin listed for blueprint generation. Every class
      * directory compiled with the plugin has this manifest, most of them empty, so read them all.
      */
    private def contracts: Vector[(String, Contract)] = {
        val manifests =
            getClass.getClassLoader.getResources("META-INF/scalus/blueprint-modules").asScala
        val classNames = manifests.toVector.flatMap { url =>
            val source = Source.fromURL(url, "UTF-8")
            try source.getLines().filter(_.nonEmpty).map(_.takeWhile(_ != '\t')).toVector
            finally source.close()
        }.distinct
        classNames.map { className =>
            val name = className.stripPrefix("scalus.examples.").stripSuffix("$")
            name -> Class.forName(className).getField("MODULE$").get(null).asInstanceOf[Contract]
        }
    }

    test("every example contract keeps its script hash") {
        assume(!ScalaCompilerVersion.hasLeanDesugaring, "the table is for the Scala 3.3 LTS")
        val actual = contracts
            .flatMap { (name, contract) =>
                val validators = contract.blueprint.validators
                validators.zipWithIndex.map { (v, i) =>
                    val key = if validators.size == 1 then name else s"$name#$i"
                    val code = v.compiledCode.getOrElse(fail(s"$key has no compiled code"))
                    key -> (v.hash.getOrElse(fail(s"$key has no hash")), code.length / 2)
                }
            }
            .sortBy(_._1)
        assert(actual.size >= 20, s"found ${actual.size} contracts")

        val changes = actual.collect {
            case (key, (hash, size)) if !expected.get(key).exists(_._1 == hash) =>
                expected.get(key) match
                    case Some((_, old)) => s"$key: new hash, $old -> $size bytes"
                    case None           => s"$key: not in the table"
        } ++ expected.keySet.diff(actual.map(_._1).toSet).toVector.sorted.map(k => s"$k: removed")
        val table = actual.map { case (key, (hash, size)) => s"$key $hash $size" }.mkString("\n")
        assert(
          changes.isEmpty,
          s"Script hashes changed:\n${changes.mkString("\n")}\nIf on purpose, the table is:\n$table"
        )
    }
}
