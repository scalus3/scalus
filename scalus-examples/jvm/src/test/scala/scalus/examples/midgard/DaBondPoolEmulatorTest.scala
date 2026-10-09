package scalus.examples.midgard

import org.scalatest.funsuite.AnyFunSuite
import scalus.ScalaCompilerVersion
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.node.{Emulator, EmulatorBase}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.ByteString
import scalus.utils.await

import scala.collection.mutable

/** Real transactions through the Emulator, once with the Aiken script and once with the Scalus
  * script, each deployed as a reference script. Records the fee the builder sets (size fee +
  * ExUnits fee + reference-script fee), the redeemer ExUnits, and the tx size.
  *
  * `Slash` is not run here: it needs a hub oracle, a state-queue mint, and a correction lock. It is
  * covered by [[DaBondPoolDifferentialTest]].
  */
class DaBondPoolEmulatorTest extends AnyFunSuite {
    import DaBondPoolConstants.*

    private given env: CardanoInfo = CardanoInfo.mainnet

    private val network = env.network
    private val builder = TxBuilder(env)

    private val hubPolicy = ScriptHash.fromHex("e0" * 28)
    private val daParamsPolicy = ScriptHash.fromHex("e1" * 28)

    private val poolLovelace = 605_000_000L
    private val topUp = 50_000_000L
    private val withdrawAmount = 100_000_000L

    private val parameters = DaBondPoolFixtures.parameters

    private val seedHash = TransactionHash.fromByteString(ByteString.fromHex("11" * 32))
    private val aikenInitInput = Input(seedHash, 0)
    private val scalusInitInput = Input(seedHash, 1)
    private val daParamsInput = Input(seedHash, 2)

    /** A pre-funded DA params UTxO: `Script(daParamsPolicy)`, its NFT, and owners Alice, Bob and
      * Charles with update threshold 2. It is only read, so no script has to exist behind it.
      */
    private val daParamsOutput: TransactionOutput = {
        val committee = ByteString.fromHex("a1" * 32)
        val datum = DaParamsDatum(
          committee = committee,
          committeeSignersHash = blake2b_256(committee),
          daThreshold = 1,
          owners = PList(Alice, Bob, Charles).map(p => PubKeyHash(p.addrKeyHash)),
          updateThreshold = 2
        )
        TransactionOutput(
          scriptAddress(daParamsPolicy),
          Value.asset(daParamsPolicy, AssetName(daParamsAssetName), 1, Coin.ada(2)),
          datum.toData
        )
    }

    private val quorumSigner =
        new TransactionSigner(Set(Alice.account.paymentKeyPair, Charles.account.paymentKeyPair))
    private val quorumKeys = Set(Alice.addrKeyHash, Charles.addrKeyHash)

    private def scriptAddress(hash: ScriptHash): Address =
        ShelleyAddress(network, ShelleyPaymentPart.Script(hash), ShelleyDelegationPart.Null)

    private def onchainRef(input: TransactionInput): TxOutRef =
        TxOutRef(TxId(input.transactionId), input.index)

    private def newEmulator(): Emulator = {
        val alice = Alice.address(network)
        val genesis = EmulatorBase.createInitialUtxos(Seq(alice, Bob.address(network)))
        Emulator(
          initialUtxos = genesis ++ Map(
            aikenInitInput -> TransactionOutput(alice, Value.ada(20)),
            scalusInitInput -> TransactionOutput(alice, Value.ada(20)),
            daParamsInput -> daParamsOutput
          )
        )
    }

    case class Step(name: String, fee: Long, exUnits: ExUnits, txSize: Int)

    private def metrics(name: String, tx: Transaction): Step = {
        val exUnits = tx.witnessSet.redeemers
            .map(_.value.toSeq.map(_.exUnits).foldLeft(ExUnits(0, 0))(_ + _))
            .getOrElse(ExUnits(0, 0))
        Step(name, tx.body.value.fee.value, exUnits, tx.toCbor.length)
    }

    /** Runs publish, InitPool, TopUp, BeginWithdraw, CancelWithdraw, BeginWithdraw and
      * CompleteWithdraw on a fresh Emulator.
      */
    private def lifecycle(
        initInput: TransactionInput,
        script: DaBondPoolParams => Script.PlutusV3
    ): (Int, Seq[Step]) = {
        val emulator = newEmulator()
        val alice = Alice.address(network)
        val reserved = Set(aikenInitInput, scalusInitInput)
        def aliceUtxos: Utxos =
            emulator.findUtxos(alice).await().toOption.get.filterNot((in, _) => reserved(in))

        val params = DaBondPoolParams(
          onchainRef(initInput),
          hubPolicy,
          daParamsPolicy,
          parameters
        )
        val pool = script(params)
        val policy = pool.scriptHash
        val poolAddress = scriptAddress(policy)
        val nftName = AssetName(poolAssetName)
        def poolValue(lovelace: Long) = Value.asset(policy, nftName, 1, Coin(lovelace))
        val steps = mutable.ArrayBuffer.empty[Step]

        def submit(name: String, tx: Transaction): Transaction = {
            val result = emulator.submit(tx).await()
            assert(result.isRight, s"$name failed: $result")
            steps += metrics(name, tx)
            tx
        }
        def poolUtxo(tx: Transaction): Utxo =
            Utxo(tx.utxos.find((_, out) => out.address == poolAddress).get)
        def poolOutputIndex(tx: Transaction): BigInt =
            BigInt(tx.body.value.outputs.indexWhere(_.value.address == poolAddress))
        def daParamsIndex(tx: Transaction): BigInt =
            BigInt(tx.body.value.referenceInputs.toSeq.indexOf(daParamsInput))

        // Publish the reference script.
        val publishTx = builder
            .output(
              TransactionOutput(Bob.address(network), Value.ada(40), None, Some(ScriptRef(pool)))
            )
            .complete(availableUtxos = aliceUtxos, sponsor = alice)
            .sign(Alice.signer)
            .transaction
        submit("publish", publishTx)
        val refScript = Utxo(publishTx.utxos.find((_, out) => out.scriptRef.isDefined).get)
        val daParams = Utxo(daParamsInput, daParamsOutput)

        // InitPool.
        val initTx = submit(
          "InitPool",
          builder
              .references(refScript)
              .spend(Utxo(initInput, TransactionOutput(alice, Value.ada(20))))
              .mint(policy, Map(nftName -> 1L), DaBondPoolMintRedeemer.InitPool(0))
              .payTo(poolAddress, poolValue(poolLovelace), DaBondPoolDatum.Bonded.toData)
              .complete(availableUtxos = aliceUtxos, sponsor = alice)
              .sign(Alice.signer)
              .transaction
        )
        assert(poolOutputIndex(initTx) == 0)

        // TopUp.
        val topUpTx = submit(
          "TopUp",
          builder
              .references(refScript)
              .spend(
                poolUtxo(initTx),
                tx => DaBondPoolSpendRedeemer.TopUp(poolOutputIndex(tx)).toData
              )
              .payTo(poolAddress, poolValue(poolLovelace + topUp), DaBondPoolDatum.Bonded.toData)
              .complete(availableUtxos = aliceUtxos, sponsor = alice)
              .sign(Alice.signer)
              .transaction
        )
        val afterTopUp = poolLovelace + topUp

        def begin(from: Transaction, name: String): (Transaction, BigInt) = {
            val start = emulator.currentSlotSync
            val end = start + 300 // 300 s, inside the 480 s limit
            val unlockAt = BigInt(env.slotConfig.slotToTime(end) - 1) + daBondWithdrawDelayMs
            val tx = submit(
              name,
              builder
                  .references(refScript, daParams)
                  .spend(
                    poolUtxo(from),
                    tx =>
                        DaBondPoolSpendRedeemer
                            .BeginWithdraw(daParamsIndex(tx), poolOutputIndex(tx))
                            .toData
                  )
                  .payTo(
                    poolAddress,
                    poolValue(afterTopUp),
                    DaBondPoolDatum.Withdrawing(unlockAt).toData
                  )
                  .validDuring(ValidityInterval(Some(start), Some(end)))
                  .requireSignatures(quorumKeys)
                  .complete(availableUtxos = aliceUtxos, sponsor = alice)
                  .sign(quorumSigner)
                  .transaction
            )
            (tx, unlockAt)
        }

        val (beginTx, _) = begin(topUpTx, "BeginWithdraw")

        val cancelTx = submit(
          "CancelWithdraw",
          builder
              .references(refScript, daParams)
              .spend(
                poolUtxo(beginTx),
                tx =>
                    DaBondPoolSpendRedeemer
                        .CancelWithdraw(daParamsIndex(tx), poolOutputIndex(tx))
                        .toData
              )
              .payTo(poolAddress, poolValue(afterTopUp), DaBondPoolDatum.Bonded.toData)
              .requireSignatures(quorumKeys)
              .complete(availableUtxos = aliceUtxos, sponsor = alice)
              .sign(quorumSigner)
              .transaction
        )

        val (beginAgainTx, unlockAt) = begin(cancelTx, "BeginWithdraw (again)")

        emulator.setSlot(env.slotConfig.timeToSlot(unlockAt.toLong) + 1)
        submit(
          "CompleteWithdraw",
          builder
              .references(refScript, daParams)
              .spend(
                poolUtxo(beginAgainTx),
                tx =>
                    DaBondPoolSpendRedeemer
                        .CompleteWithdraw(withdrawAmount, daParamsIndex(tx), poolOutputIndex(tx))
                        .toData
              )
              .payTo(
                poolAddress,
                poolValue(afterTopUp - withdrawAmount),
                DaBondPoolDatum.Bonded.toData
              )
              .validDuring(ValidityInterval(Some(emulator.currentSlotSync), None))
              .requireSignatures(quorumKeys)
              .complete(availableUtxos = aliceUtxos, sponsor = alice)
              .sign(quorumSigner)
              .transaction
        )
        (pool.script.size, steps.toSeq)
    }

    test("da-bond-pool lifecycle: Aiken vs Scalus fees with reference scripts") {
        val (aikenSize, aiken) = lifecycle(
          aikenInitInput,
          p => Script.PlutusV3(AikenDaBondPool(p).cborByteString)
        )
        val (scalusSize, scalus) = lifecycle(
          scalusInitInput,
          p =>
              Script.PlutusV3(
                DaBondPoolContract
                    .applyParams(DaBondPoolContract.compiled.program, p)
                    .cborByteString
              )
        )
        val refFee = new RefScriptFee(env.protocolParams)
        val lines = mutable.ArrayBuffer(
          s"Reference script: Aiken $aikenSize B (fee ${refFee.calculate(aikenSize).value}), " +
              s"Scalus $scalusSize B (fee ${refFee.calculate(scalusSize).value})",
          "",
          "| step | Aiken fee | Scalus fee | saved | fee ratio | Aiken mem | Scalus mem | mem ratio |",
          "|---|---:|---:|---:|---:|---:|---:|---:|"
        )
        for (a, s) <- aiken.zip(scalus) do
            val memRatio =
                if a.exUnits.memory == 0 then "–"
                else f"${s.exUnits.memory.toDouble / a.exUnits.memory}%.2f"
            lines += f"| ${a.name} | ${a.fee}%,d | ${s.fee}%,d | ${a.fee - s.fee}%,d | " +
                f"${s.fee.toDouble / a.fee}%.2f | ${a.exUnits.memory}%,d | ${s.exUnits.memory}%,d | $memRatio |"
        println(lines.mkString("\n"))
        assert(aiken.map(_.name) == scalus.map(_.name))
        for s <- scalus do
            val (fee, exUnits) = scalusPins(s.name)
            assert(s.exUnits == exUnits, s"Scalus ${s.name} ExUnits")
            assert(s.fee == fee.value, s"Scalus ${s.name} fee")
    }

    /** The 3.8+ compiler emits a 16 B smaller script, so every fee has two baselines. */
    private def fee(pre38: Long, since38: Long): Coin =
        ScalaCompilerVersion.baseline(Coin(pre38), Coin(since38))

    /** Exact Scalus fee and ExUnits of every step (`scalus:contract-test` pin convention). */
    private val scalusPins: Map[String, (Coin, ExUnits)] = Map(
      "publish" -> (fee(273345, 272597), ExUnits(memory = 0, steps = 0)),
      "InitPool" -> (fee(219761, 219506), ExUnits(memory = 50711, steps = 22_525575)),
      "TopUp" -> (fee(218950, 218695), ExUnits(memory = 65975, steps = 31_410473)),
      "BeginWithdraw" -> (fee(233422, 233167), ExUnits(memory = 120326, steps = 54_990725)),
      "CancelWithdraw" -> (fee(231830, 231575), ExUnits(memory = 110015, steps = 50_924756)),
      "BeginWithdraw (again)" -> (fee(233422, 233167), ExUnits(memory = 120326, steps = 54_990725)),
      "CompleteWithdraw" -> (fee(231053, 230798), ExUnits(memory = 117150, steps = 52_132631))
    )
}
