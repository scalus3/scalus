package scalus.testing.integration

import co.nstant.in.cbor.{model as cbor, CborException}
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.spec.Era
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.yaci.core.model.serializers.util.WitnessUtil.getArrayBytes
import com.bloxbean.cardano.yaci.core.model.serializers.util.{TransactionBodyExtractor, WitnessUtil}
import com.bloxbean.cardano.yaci.core.util.CborSerializationUtil
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.bloxbean.*
import scalus.bloxbean.Interop.??
import scalus.bloxbean.TxEvaluator.ScriptHash
import scalus.builtin.{platform, ByteString}
import scalus.cardano.ledger
import scalus.cardano.ledger.{AddrKeyHash, BlockFile, CardanoInfo, ExUnits, OriginalCborByteArray, PlutusScriptEvaluationException, PlutusScriptEvaluator, ProtocolParams, RedeemerTag, Redeemers, Script, ScriptDataHashGenerator, SlotConfig, ValidityInterval}
import scalus.ledger.api.v1.ScriptPurpose
import scalus.ledger.api.v2.ScriptPurpose
import scalus.ledger.api.v3.ScriptInfo
import scalus.ledger.api.{v3, ScriptContext, *}
import scalus.uplc.eval.ExBudget
import scalus.utils.Utils

import java.math.BigInteger
import java.nio.channels.FileChannel
import java.nio.file.*
import java.util
import java.util.stream.Collectors
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.math.Ordering.Implicits.*
import scala.util.boundary.{break, Break}
import scala.util.{boundary, Using}

/** Setup BLOCKFROST_API_KEY environment variable before running this test. In SBT shell:
  *   - set `scalus-bloxbean-cardano-client-lib`/envVars := Map("BLOCKFROST_API_KEY" -> "apikey")
  *   - scalus-bloxbean-cardano-client-lib/Test/runMain scalus.bloxbean.BlocksValidation
  *   - cat script-1.flat | uplc evaluate --input-format flat --counting --trace-mode
  *     LogsWithBudgets --builtin-semantics-variant B
  */
class BlocksValidation extends AnyFunSuite {

    case class BlockTx(tx: Transaction, datums: util.List[ByteString], txHash: String)

    private lazy val apiKey = System.getenv("BLOCKFROST_API_KEY") ?? {
        println("BLOCKFROST_API_KEY is not set, please set it before running the test")
        ""
    }

    private lazy val dataPath = System.getenv("SCALUS_IT_DATA_PATH") ?? sys.error(
      "SCALUS_IT_DATA_PATH is not set, please set it before running the test"
    )

    private lazy val resourcesPath = Paths.get(dataPath)

    private lazy val pprinter = pprint.copy(defaultHeight = Int.MaxValue)

    private def showScriptContext(sc: ScriptContext): String =
        ScriptContext.foldMap(sc)(
          "v1." + pprinter(_),
          "v2." + pprinter(_),
          "v3." + pprinter(_),
        )

    private def validateBlocksOfEpoch(epoch: Int): Unit = {
        import com.bloxbean.cardano.yaci.core.config.YaciConfig
        YaciConfig.INSTANCE.setReturnBlockCbor(true) // needed to get the block cbor
        YaciConfig.INSTANCE.setReturnTxBodyCbor(true) // needed to get the tx body cbor

        val backendService = new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, apiKey)
        val utxoSupplier = CachedUtxoSupplier(
          resourcesPath.resolve("utxos"),
          DefaultUtxoSupplier(backendService.getUtxoService)
        )
        // memory and file cached script supplier using the script service
        val scriptSupplier = InMemoryCachedScriptSupplier(
          FileScriptSupplier(
            resourcesPath.resolve("scripts"),
            ScriptServiceSupplier(backendService.getScriptService)
          )
        )
        val protocolParamsSupplier = CachedEpochParamsSupplier(
          resourcesPath.resolve("epochs"),
          backendService.getEpochService
        )

        val protocolParams = protocolParamsSupplier.getProtocolParameters(epoch).getValue
        val utxoResolver = CclUtxoResolver(utxoSupplier, scriptSupplier)
        val evaluator = ScalusTransactionEvaluator(
          SlotConfig.Mainnet,
          protocolParams,
          utxoSupplier,
          scriptSupplier,
          EvaluatorMode.VALIDATE,
          debugDumpFilesForTesting = false
        )

        val (r, e) = plutusScriptEvaluator(utxoSupplier, scriptSupplier, epoch)
        val evalScalus = evalBlockWithScalus(r, e)

        var totalBlocks = 0
        var totalTx = 0
        val errors = mutable.ArrayBuffer[(String, String, String)]()
        val v1Scripts = mutable.HashSet.empty[String]
        var v1ScriptsExecuted = 0
        val v2Scripts = mutable.HashSet.empty[String]
        var v2ScriptsExecuted = 0
        val v3Scripts = mutable.HashSet.empty[String]
        var v3ScriptsExecuted = 0

        val blocks = getAllBlocksPaths()
        println(s"Validating scripts of ${blocks.size} blocks")
        boundary:
            for blockPath <- getAllBlocksPaths() do
                totalBlocks += 1
                val fileName = blockPath.getFileName.toString
                try {
                    val (txs, blockBytes) = readTransactionsFromBlockCbor(blockPath)
                    val txsWithScripts =
                        val r = mutable.Buffer.empty[
                          (Transaction, util.List[ByteString], String, Map[ScriptHash, Script])
                        ]
                        for BlockTx(tx, datums, txhash) <- txs do
                            try
                                val utxos = utxoResolver.resolveUtxos(tx)
                                val scripts = TxEvaluator.getAllResolvedScripts(tx, utxos)
                                if scripts.nonEmpty then r.addOne((tx, datums, txhash, scripts))
                            catch
                                case e: Exception =>
                                    if e.getMessage.endsWith("No value present") then {
                                        println(s"$fileName: $e")
                                        println("Cannot resolve UTXO, stopping test ...")
                                        break()
                                    } else {
                                        errors += ((e.getMessage, fileName, txhash))
                                        println(
                                          s"Error in block $fileName, tx $txhash: ${e.getMessage}"
                                        )
                                    }
                        r.toSeq
                    //                println(s"\rBlock $blockNum, num txs to validate: ${txsWithScripts.size}")
                    //            println(s"Block txs:\n${txsWithScripts.map(_._3).sorted.mkString("\n")}")

                    for (tx, datums, txhash, scripts) <- txsWithScripts do {
                        //                println(s"Validating tx $txhash")
                        //                println(tx)
                        if tx.isValid
                        && (datums.size() == tx.getWitnessSet.getPlutusDataList
                            .size()) // FIXME: remove this check when we have the correct datums
                        then
                            totalTx += 1
                            val result =
                                evaluator.evaluateTxWithContexts(tx, util.Set.of(), datums, txhash)
                            result match {
                                case Left(error) =>
                                    errors += ((error.toString, fileName, txhash))
                                    println(
                                      s"${Console.RED}[error# ${errors.size}] AAAA!!!! block $fileName $txhash ${error}${Console.RESET}"
                                    )
                                    println(
                                      error.scriptContext.map(showScriptContext).mkString("\n")
                                    )
                                    for case (
                                          tx,
                                          key,
                                          actualExUnits,
                                          expectedExUnits,
                                          newSc,
                                          newSh
                                        ) <-
                                            evalScalus(blockBytes)
                                    do
                                        if tx.id.toHex == txhash then {
                                            val oldSc = error.scriptContext.head
                                            val p = getPurpose(oldSc)
                                            if p == key._1 then {
                                                println(
                                                  s"\nScalus script context (key $key, actual $actualExUnits, expected $expectedExUnits):"
                                                )
                                                println(newSh)
                                                // println(showScriptContext(newSc))
                                            }
                                        }
                                    println()
                                case Right(_) =>
                                    for script <- scripts.values do
                                        script match
                                            case _: Script.PlutusV1 =>
                                                v1Scripts += script.scriptHash.toHex
                                                v1ScriptsExecuted += 1
                                            case _: Script.PlutusV2 =>
                                                v2Scripts += script.scriptHash.toHex
                                                v2ScriptsExecuted += 1
                                            case _: Script.PlutusV3 =>
                                                v3Scripts += script.scriptHash.toHex
                                                v3ScriptsExecuted += 1
                                            case _ =>
                            }
                        else
                            println(
                              s"${Console.RED}[error# ${errors.size}] AAAAA invalid!!! $txhash ${Console.RESET}"
                            )
                            errors += (("Invalid tx", fileName, txhash))
                    }
                } catch {
                    case e: Break[?]  => throw e
                    case e: Exception => println(s"$fileName ${e.getMessage}")
                }

        println(s"""Total blocks: $totalBlocks,
               |Total txs: $totalTx,
               |errors: ${errors.size},
               |v1: $v1ScriptsExecuted of ${v1Scripts.size},
               |v2: $v2ScriptsExecuted of ${v2Scripts.size}
               |v3: $v3ScriptsExecuted of ${v3Scripts.size}
               |""".stripMargin)

        assert(errors.isEmpty)
    }

    private def getPurpose(oldSc: ScriptContext): RedeemerTag = {
        ScriptContext.foldMap(oldSc)(
          _.purpose match {
              case ScriptPurpose.Minting(curSymbol)     => RedeemerTag.Mint
              case ScriptPurpose.Spending(txOutRef)     => RedeemerTag.Spend
              case ScriptPurpose.Rewarding(stakingCred) => RedeemerTag.Reward
              case ScriptPurpose.Certifying(cert)       => RedeemerTag.Cert
          },
          _.purpose match {
              case ScriptPurpose.Minting(curSymbol)     => RedeemerTag.Mint
              case ScriptPurpose.Spending(txOutRef)     => RedeemerTag.Spend
              case ScriptPurpose.Rewarding(stakingCred) => RedeemerTag.Reward
              case ScriptPurpose.Certifying(cert)       => RedeemerTag.Cert
          },
          _.scriptInfo match {
              case ScriptInfo.MintingScript(policyId)           => RedeemerTag.Mint
              case ScriptInfo.SpendingScript(txOutRef, datum)   => RedeemerTag.Spend
              case ScriptInfo.RewardingScript(credential)       => RedeemerTag.Reward
              case ScriptInfo.CertifyingScript(index, cert)     => RedeemerTag.Cert
              case ScriptInfo.VotingScript(voter)               => RedeemerTag.Voting
              case ScriptInfo.ProposingScript(index, procedure) => RedeemerTag.Proposing
          }
        )
    }

    def plutusScriptEvaluator(
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier,
        epoch: Int
    ): (ScalusUtxoResolver, PlutusScriptEvaluator) = {
        val params: ProtocolParams = CardanoInfo.mainnet.protocolParams
        val utxoResolver = ScalusUtxoResolver(utxoSupplier, scriptSupplier)
        utxoResolver -> PlutusScriptEvaluator(
          CardanoInfo.mainnet.slotConfig,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = CardanoInfo.mainnet.majorProtocolVersion,
          costModels = params.costModels
        )
    }

    def evalBlockWithScalus(utxoResolver: ScalusUtxoResolver, evaluator: PlutusScriptEvaluator)(
        blockBytes: Array[Byte]
    ): Seq[
      (ledger.Transaction, (RedeemerTag, Int), ExUnits, ExUnits, ScriptContext, ScriptHash)
    ] = {
        given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
        val block = BlockFile.fromCborArray(blockBytes).block
        val txs = block.transactions.filter(t => t.witnessSet.redeemers.nonEmpty && t.isValid)
        for
            (tx, idx) <- txs.zipWithIndex
            if tx.isValid && tx.witnessSet.redeemers.nonEmpty
            utxos = utxoResolver.resolveUtxos(tx)
            results = evaluator.evalPlutusScriptsWithContexts(tx, utxos)
            actualRedeemers = Redeemers.from(results.map(_._1)).toMap
            expectedRedeemers = tx.witnessSet.redeemers.get.value.toMap
            (key, (_, actualExUnits)) <- actualRedeemers
            case (_, sc, sh) <- results.find(r => (r._1.tag, r._1.index) == key)
        yield (tx, key, actualExUnits, expectedRedeemers(key)._2, sc, sh)
    }

    private def validateBlocksOfEpochWithScalus(epoch: Int): Int = {
        val errors = mutable.ArrayBuffer[String]()
        val stats = (
          mutable.Map[ScriptHash, Int](),
          mutable.Map[ScriptHash, Int](),
          mutable.Map[ScriptHash, Int]()
        )

        val backendService = new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, apiKey)
        val utxoSupplier = CachedUtxoSupplier(
          resourcesPath.resolve("utxos"),
          DefaultUtxoSupplier(backendService.getUtxoService)
        )
        // memory and file cached script supplier using the script service
        val scriptSupplier = InMemoryCachedScriptSupplier(
          FileScriptSupplier(
            resourcesPath.resolve("scripts"),
            ScriptServiceSupplier(backendService.getScriptService)
          )
        )

        val (r, e) = plutusScriptEvaluator(utxoSupplier, scriptSupplier, epoch)
        val evalScalus = evalBlockWithScalus(r, e)

        var totalTx = 0
        val blocks = getAllBlocksPaths()
        println(s"Validating scripts of ${blocks.size} blocks")
        boundary:
            for path <- blocks do
                try {
                    val blockBytes = Files.readAllBytes(path)
                    for case (tx, key, actualExUnits, expectedExUnits, sc, sh) <- evalScalus(
                          blockBytes
                        )
                    do
                        totalTx += 1
                        ScriptContext.foldMap(sc)(
                          _ => stats._1.updateWith(sh)(n => Some(n.fold(1)(_ + 1))),
                          _ => stats._2.updateWith(sh)(n => Some(n.fold(1)(_ + 1))),
                          _ => stats._3.updateWith(sh)(n => Some(n.fold(1)(_ + 1))),
                        )
                        if actualExUnits > expectedExUnits then {
                            val error =
                                s"AAAA!!!! block $path, tx ${tx.id} ${key._1} budget: $actualExUnits > $expectedExUnits"
                            errors += error
                            println(
                              s"\n${Console.RED}[error# ${errors.size}] ${error}${Console.RESET}"
                            )
                            println(showScriptContext(sc))
                        }
                } catch {
                    case e: PlutusScriptEvaluationException =>
                        val error = s"${path.getFileName}: $e"
                        errors += error
                        println(s"\n${Console.RED}[error# ${errors.size}] ${error}${Console.RESET}")
                    case e: IllegalStateException =>
                        if e.getMessage.startsWith("Reference UTXO not found for input") then {
                            println(s"${path.getFileName}: $e")
                            println("Cannot resolve UTXO, stopping test ...")
                            break()
                        } else println(s"${path.getFileName}: $e")
                    case e: Exception => println(s"${path.getFileName}: $e")
                }
        println(
          s"\n${Console.GREEN}Total txs: $totalTx, errors ${errors.size}, blocks: ${blocks.size}, epoch: $epoch${Console.RESET}"
        )
        showScriptsStats(1, stats._1.toMap)
        showScriptsStats(2, stats._2.toMap)
        showScriptsStats(3, stats._3.toMap)
        errors.size
    }

    def showScriptsStats(ver: Int, stats: Map[ScriptHash, Int]): Unit = {
        val scripts = stats.size
        val runs = stats.values.sum
        println(s"Scripts v$ver: $runs runs of $scripts scripts")
        val top = stats.toSeq.sortBy(_._2)(using Ordering[Int].reverse)
        println(
            s"  Top 1: ${top.head._2} (${100 * top.head._2 / runs}%) runs by 1 script "
        )
        val top10 = top.take(10).map(_._2).sum
        println(
          s"  Top 10: ${top10} (${100 * top10 / runs}%) runs by 10 (${100 * 10 / scripts}%) scripts "
        )
        val top100 = top.take(100).map(_._2).sum
        println(
            s"  Top 100: ${top100} (${100 * top100 / runs}%) runs by 100 (${100 * 100 / scripts}%) scripts "
        )
    }

    def readTransactionsFromBlockCbor(path: Path): (collection.Seq[BlockTx], Array[Byte]) = {
        // read block cbor from file using mmap
        val channel = FileChannel.open(path, StandardOpenOption.READ)
        try
            val size = channel.size()
            val buffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, size)
            val output = new Array[Byte](size.toInt)
            buffer.get(output)
            readTransactionsFromBlockCbor(output) -> output
        finally channel.close()
    }

    def readTransactionsFromBlockCbor(
        blockCbor: Array[Byte]
    ): collection.Seq[BlockTx] = {
        val array = CborSerializationUtil.deserializeOne(blockCbor).asInstanceOf[cbor.Array]
        val blockArray = array.getDataItems.get(1).asInstanceOf[cbor.Array]
        val witnessesListArr = blockArray.getDataItems.get(2).asInstanceOf[cbor.Array]
        val auxiliaryDataMap = blockArray.getDataItems.get(3).asInstanceOf[cbor.Map]
        val invalidTxs = blockArray.getDataItems
            .get(4)
            .asInstanceOf[cbor.Array]
            .getDataItems
            .stream()
            .map(_.asInstanceOf[cbor.Number].getValue.intValue())
            .collect(Collectors.toSet())
        val txBodyTuples = TransactionBodyExtractor.getTxBodiesFromBlock(blockCbor)
        val witnesses = WitnessUtil.getWitnessRawData(blockCbor)
        val txBodyDatumsCbor = witnesses.asScala
            .map { witness =>
                try
                    val fields = WitnessUtil.getWitnessFields(witness)
                    val datums = fields.get(BigInteger.valueOf(4L))
                    if datums != null then
                        getArrayBytes(datums)
                            .stream()
                            .map(ByteString.fromArray)
                            .collect(Collectors.toList())
                    else util.Collections.emptyList()
                catch
                    case e: CborException =>
                        e.printStackTrace()
                        util.Collections.emptyList();
            }
        for ((tuple, datumsCbor), idx) <- txBodyTuples.asScala.zip(txBodyDatumsCbor).zipWithIndex
        yield
            val txbody = TransactionBody.deserialize(tuple._1.asInstanceOf[cbor.Map])
            val witnessSetDataItem = witnessesListArr.getDataItems.get(idx).asInstanceOf[cbor.Map]
            val witnessSet = TransactionWitnessSet.deserialize(witnessSetDataItem)
            // val auxiliaryData =
            //     if auxiliaryDataMap.get(new UnsignedInteger(idx)) != null then
            //         AuxiliaryData.deserialize(
            //           auxiliaryDataMap.get(new UnsignedInteger(idx)).asInstanceOf[cbor.Map]
            //         )
            //     else null
            val isValid = !invalidTxs.contains(idx)
//            val txbytes =
//                val array = new cbor.Array()
//                array.add(tuple._1)
//                array.add(witnessSetDataItem)
//                array.add(if isValid then SimpleValue.TRUE else SimpleValue.FALSE)
//                array.add(SimpleValue.NULL)
//                CborSerializationUtil.serialize(array)
//            Files.write(Paths.get(s"tx-${TransactionUtil.getTxHash(txbytes)}.cbor"), txbytes)
//            println(Utils.bytesToHex(txbytes))
            /*val witnessMap = witnessesListArr.getDataItems.get(idx).asInstanceOf[cbor.Map]
            val plutusDataArray = witnessMap.get(new UnsignedInteger(4))
            if plutusDataArray != null then
                val plutusData = plutusDataArray.asInstanceOf[cbor.Array]
//                    println(s"size ${plutusData.getDataItems.size()}")
                plutusData.getDataItems.asScala.foreach { item =>
                    // get cbor hex
//                        println(s"${Utils.bytesToHex(CborSerializationUtil.serialize(item))}")
                }*/

            val transaction =
                Transaction
                    .builder()
                    .era(Era.Conway)
                    .body(txbody)
                    .witnessSet(witnessSet)
                    // .auxiliaryData(auxiliaryData)
                    .isValid(isValid)
                    .build()
//            println(s"tx: ${transaction.toJson()}")
            val txBodyBytes = tuple._2
            // get tx hash from bytes
            // because Cardano changed TxBody serialization in Conway era, hash(tx.serialized) != hash(txBodyBytes)
            val txHashFromBytes = Utils.bytesToHex(Blake2bUtil.blake2bHash256(txBodyBytes))
            BlockTx(transaction, datumsCbor, txHashFromBytes)
    }

    private def filterBlocks(min: Int = 11544518, max: Int = 11550000): IndexedSeq[Path] = {
        if min == max then IndexedSeq(resourcesPath.resolve("blocks").resolve(s"block-$min.cbor"))
        else {
            val from = s"block-${Math.min(min, max)}.cbor"
            val to = s"block-${Math.max(min, max)}.cbor"

            inline def f(s: String) = from <= s && s <= to

            getAllBlocksPaths().filter(path => f(path.getFileName.toString))
        }
    }

    private def getAllBlocksPaths(): IndexedSeq[Path] = {
        val blocksDir = resourcesPath.resolve("blocks")
        if !Files.exists(blocksDir) then
            sys.error(
              s"Blocks directory $blocksDir does not exist. Please run `sbt bloxbean-cardano-client-lib/test` first."
            )
        Using(Files.list(blocksDir)) {
            _.filter(f => f.getFileName.toString.endsWith(".cbor"))
                .iterator()
                .asScala
                .toIndexedSeq
                .sorted
        }.get
    }

    def validateNativeScriptEvaluation(): Unit = {
        case class Res(
            var succ: Int,
            var fail: Int,
            blocks: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty
        )

        val stats = mutable.HashMap.empty[ByteString, Res].withDefaultValue(Res(0, 0))
        val start = System.currentTimeMillis()

        val blocks = getAllBlocksPaths()

        println(s"Validating native scripts of ${blocks.size} blocks")
        for path <- blocks do
            try
                val blockBytes = Files.readAllBytes(path)
                val block = BlockFile.fromCborArray(blockBytes).block
                for
                    case (txb, w) <- block.transactionBodies.view
                        .map(_.value)
                        .zip(block.transactionWitnessSets)
                    case (scriptHash, native) <- w.nativeScripts.toMap
                do
                    val keyHashes = w.vkeyWitnesses.toSortedSet.map { w =>
                        val key = w.vkey
                        AddrKeyHash(platform.blake2b_224(key))
                    }

                    if native.script.evaluate(
                          keyHashes,
                          ValidityInterval(txb.validityStartSlot, txb.ttl)
                        )
                    then stats.getOrElseUpdate(scriptHash, Res(0, 0)).succ += 1
                    else stats.getOrElseUpdate(scriptHash, Res(0, 0)).fail += 1

            catch
                case e: Exception =>
                // println(s"Error reading block $path: ${e.getMessage}")
        end for
        println()
        println(s"Time taken: ${System.currentTimeMillis() - start} ms")
        println(
          s"Stats: num scripts ${stats.size}, succ: ${stats.values.map(_.succ).sum}, failed: ${stats.values.map(_.fail).sum}"
        )
        assert(stats.values.map(_.fail).sum == 0)
    }

    def validateScriptDataHashEvaluation(): Unit = {
        import com.bloxbean.cardano.yaci.core.config.YaciConfig
        YaciConfig.INSTANCE.setReturnBlockCbor(true) // needed to get the block cbor
        YaciConfig.INSTANCE.setReturnTxBodyCbor(true) // needed to get the tx body cbor
        case class Res(
            var succ: Int,
            var fail: Int,
            blocks: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty
        )

        val blocksDir = resourcesPath.resolve("blocks")
        val stats = mutable.HashMap.empty[ByteString, Res].withDefaultValue(Res(0, 0))
        val start = System.currentTimeMillis()

        val backendService = new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, apiKey)
        val utxoSupplier = CachedUtxoSupplier(
          resourcesPath.resolve("utxos"),
          DefaultUtxoSupplier(backendService.getUtxoService)
        )
        // memory and file cached script supplier using the script service
        val scriptSupplier = InMemoryCachedScriptSupplier(
          FileScriptSupplier(
            resourcesPath.resolve("scripts"),
            ScriptServiceSupplier(backendService.getScriptService)
          )
        )

        val utxoResolver = ScalusUtxoResolver(utxoSupplier, scriptSupplier)

        val params = CardanoInfo.mainnet.protocolParams

        val blocks = filterBlocks()
        println(s"Validating script data hashes of ${blocks.size} blocks")
        for path <- blocks do
            try
                val blockBytes = Files.readAllBytes(path)
                given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
                val block = BlockFile.fromCborArray(blockBytes).block
                for (tx, idx) <- block.transactions.zipWithIndex do
                    val utxos = utxoResolver.resolveUtxos(tx)
                    val (txb, w) = (tx.body.value, tx.witnessSet)
//                    pprint.pprintln(txb)
//                    pprint.pprintln(w)
                    txb.scriptDataHash match

                        case Some(scriptDataHash) =>
                            val calculatedHash =
                                ScriptDataHashGenerator
                                    .computeScriptDataHash(tx, utxos, params)
                                    .toOption
                                    .flatten
                                    .get

                            val desc =
                                (if w.plutusV1Scripts.toMap.nonEmpty then "v1" else "")
                                    ++ (if w.plutusV2Scripts.toMap.nonEmpty then "v2" else "")
                                    ++ (if w.plutusV3Scripts.toMap.nonEmpty then "v3" else "")
                                    ++ (if w.plutusData.value.toMap.nonEmpty then "D"
                                        else "")
                                    ++ (if w.redeemers.nonEmpty then "R" else "")

                            val scalusHasCorrectHash =
                                scriptDataHash.toHex == calculatedHash.toHex // mine is correct
                            val color = if scalusHasCorrectHash then Console.GREEN else Console.RED

                            if !scalusHasCorrectHash then
                                println(
                                  s"$idx: $desc ${color}data hash: ${scriptDataHash.toHex}, calculated: ${calculatedHash.toHex}${Console.RESET}"
                                )
                        case _ =>

            catch
                case e: Exception =>
//                    println(s"Error reading block $path: ${e.getMessage}")
//                    e.printStackTrace()
//            print(s"\rBlock $path")
        end for
        println()
        println(s"Time taken: ${System.currentTimeMillis() - start} ms")
        println(
          s"Stats: num scripts ${stats.size}, succ: ${stats.values.map(_.succ).sum}, failed: ${stats.values.map(_.fail).sum}"
        )
        assert(stats.values.map(_.fail).sum == 0)
    }

    def findInterestingBlocks(): Unit = {
        val blocks = getAllBlocksPaths()
        println(s"Found ${blocks.size} blocks")
        val interestingBlocks = blocks.filter { path =>
            val blockBytes = Files.readAllBytes(path)
            val block = BlockFile.fromCborArray(blockBytes).block
            block.transactionWitnessSets.exists { _.plutusV1Scripts.toMap.nonEmpty } &&
            block.transactionWitnessSets.exists { _.plutusV2Scripts.toMap.nonEmpty } &&
            block.transactionWitnessSets.exists { _.plutusV3Scripts.toMap.nonEmpty } &&
            block.transactionWitnessSets.exists { _.nativeScripts.toMap.nonEmpty } &&
            block.transactionWitnessSets.exists { _.vkeyWitnesses.toSortedSet.nonEmpty } &&
            block.transactionWitnessSets.exists { _.plutusData.value.toMap.nonEmpty }
        }
        println(s"Interesting blocks ${interestingBlocks.size} of ${blocks.size}")
        interestingBlocks.foreach { p =>
            // copy  to resources
            Files.copy(
              p,
              Paths.get("src", "test", "resources", p.getFileName.toString),
              StandardCopyOption.REPLACE_EXISTING
            )
        }
    }

    test("validateBlocksOfEpochWithScalus(543)"):
        assert(validateBlocksOfEpochWithScalus(543) == 0)

    ignore("validateBlocksOfEpoch(543)"):
        validateBlocksOfEpoch(543)

    test("validateNativeScriptEvaluation()"):
        validateNativeScriptEvaluation()

    test("validateScriptDataHashEvaluation()"):
        validateScriptDataHashEvaluation()

}
