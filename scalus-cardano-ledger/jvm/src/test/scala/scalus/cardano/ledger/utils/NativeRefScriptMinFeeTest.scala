package scalus.cardano.ledger
package utils

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.address.Address.addr
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.uplc.builtin.{platform, ByteString}

import scala.collection.immutable.SortedSet

/** Test for GitHub issue #207: Native reference scripts should count towards minFee.
  *
  * The Haskell cardano-ledger implementation counts ALL reference scripts (including native/timelock
  * scripts) towards the minimum fee calculation via `txNonDistinctRefScriptsSize`. The current
  * Scalus implementation incorrectly treats native scripts as size 0.
  *
  * @see
  *   https://github.com/scalus3/scalus/issues/207
  * @see
  *   https://github.com/IntersectMBO/cardano-ledger/blob/6ef1bf9fa1ca589e706e781fa8c9b4ad8df1e919/eras/conway/impl/src/Cardano/Ledger/Conway/UTxO.hs#L154-L175
  */
class NativeRefScriptMinFeeTest extends AnyFunSuite {

    private val params: ProtocolParams = CardanoInfo.mainnet.protocolParams

    private val testAddress: Address =
        addr"addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    /** A non-trivial native script to make the size difference meaningful */
    private val nativeScript: Script.Native = Script.Native(
      Timelock.AllOf(
        IndexedSeq(
          Timelock.Signature(Hash(platform.blake2b_224(ByteString.fromString("key1")))),
          Timelock.Signature(Hash(platform.blake2b_224(ByteString.fromString("key2")))),
          Timelock.TimeStart(1000L)
        )
      )
    )

    /** The CBOR-encoded size of the native script, as would be counted by the Haskell
      * implementation's `originalBytesSize`.
      */
    private val nativeScriptCborSize: Int = Cbor.encode(nativeScript).toByteArray.length

    test("native reference script has non-zero CBOR size") {
        assert(
          nativeScriptCborSize > 0,
          "Native script should have a non-zero CBOR-encoded size"
        )
        System.err.println(s"Native script CBOR size: $nativeScriptCborSize bytes")
    }

    test("native reference scripts in inputs should contribute to minFee") {
        // Create a UTxO with a native reference script on a regular input
        val inputWithRefScript = Input(genesisHash, 0)
        val utxos: Utxos = Map(
          inputWithRefScript -> Babbage(
            address = testAddress,
            value = Value.ada(10),
            scriptRef = Some(ScriptRef(nativeScript))
          )
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(SortedSet(inputWithRefScript)),
            outputs = IndexedSeq.empty,
            fee = Coin(1_000_000L)
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        // Compute fee with native ref script
        val feeWithNativeRef = MinTransactionFee.computeMinFee(tx, utxos, params).toOption.get

        // Now compute fee with the same UTxO but without a reference script
        val utxosNoRef: Utxos = Map(
          inputWithRefScript -> Babbage(
            address = testAddress,
            value = Value.ada(10)
          )
        )
        val feeWithoutRef = MinTransactionFee.computeMinFee(tx, utxosNoRef, params).toOption.get

        System.err.println(s"Fee with native ref script: ${feeWithNativeRef.value}")
        System.err.println(s"Fee without ref script:     ${feeWithoutRef.value}")
        System.err.println(s"Difference:                 ${feeWithNativeRef.value - feeWithoutRef.value}")

        assert(
          feeWithNativeRef > feeWithoutRef,
          s"Native reference script (${nativeScriptCborSize} bytes) should increase the minFee, " +
              s"but fee with ref script (${feeWithNativeRef.value}) == fee without (${feeWithoutRef.value}). " +
              s"See https://github.com/scalus3/scalus/issues/207"
        )
    }

    test("native reference scripts in reference inputs should contribute to minFee") {
        // Create a UTxO with a native reference script on a reference input
        val refInput = Input(genesisHash, 0)
        val spendInput = Input(genesisHash, 1)
        val utxos: Utxos = Map(
          refInput -> Babbage(
            address = testAddress,
            value = Value.ada(10),
            scriptRef = Some(ScriptRef(nativeScript))
          ),
          spendInput -> Babbage(
            address = testAddress,
            value = Value.ada(10)
          )
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(SortedSet(spendInput)),
            outputs = IndexedSeq.empty,
            fee = Coin(1_000_000L),
            referenceInputs = TaggedSortedSet(SortedSet(refInput))
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        // Compute fee with native ref script on reference input
        val feeWithNativeRef = MinTransactionFee.computeMinFee(tx, utxos, params).toOption.get

        // Compute fee without the reference input
        val txNoRef = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(SortedSet(spendInput)),
            outputs = IndexedSeq.empty,
            fee = Coin(1_000_000L)
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val feeWithoutRef =
            MinTransactionFee.computeMinFee(txNoRef, utxos, params).toOption.get

        System.err.println(s"Fee with native ref input:  ${feeWithNativeRef.value}")
        System.err.println(s"Fee without ref input:      ${feeWithoutRef.value}")

        assert(
          feeWithNativeRef > feeWithoutRef,
          s"Native reference script on reference input should increase the minFee. " +
              s"See https://github.com/scalus3/scalus/issues/207"
        )
    }

    test("native reference script fee should be proportional to script size") {
        // Create a small and large native script
        val smallScript = Script.Native(
          Timelock.Signature(Hash(platform.blake2b_224(ByteString.fromString("key1"))))
        )
        val largeScript = Script.Native(
          Timelock.AllOf(
            IndexedSeq.tabulate(10)(i =>
                Timelock.Signature(
                  Hash(platform.blake2b_224(ByteString.fromString(s"key$i")))
                )
            )
          )
        )

        val smallSize = Cbor.encode(smallScript).toByteArray.length
        val largeSize = Cbor.encode(largeScript).toByteArray.length

        System.err.println(s"Small native script size: $smallSize bytes")
        System.err.println(s"Large native script size: $largeSize bytes")

        def feeForScript(script: Script): Coin = {
            val input = Input(genesisHash, 0)
            val utxos: Utxos = Map(
              input -> Babbage(
                address = testAddress,
                value = Value.ada(10),
                scriptRef = Some(ScriptRef(script))
              )
            )
            val tx = Transaction(
              body = TransactionBody(
                inputs = TaggedSortedSet(SortedSet(input)),
                outputs = IndexedSeq.empty,
                fee = Coin(1_000_000L)
              ),
              witnessSet = TransactionWitnessSet.empty
            )
            MinTransactionFee.computeMinFee(tx, utxos, params).toOption.get
        }

        val smallFee = feeForScript(smallScript)
        val largeFee = feeForScript(largeScript)

        System.err.println(s"Fee with small native script: ${smallFee.value}")
        System.err.println(s"Fee with large native script: ${largeFee.value}")

        assert(
          largeFee > smallFee,
          s"Larger native reference script ($largeSize bytes) should produce a higher fee " +
              s"than smaller one ($smallSize bytes). " +
              s"See https://github.com/scalus3/scalus/issues/207"
        )
    }
}
