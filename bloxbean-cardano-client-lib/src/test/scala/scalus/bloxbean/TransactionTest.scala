package scalus.bloxbean

import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString, given}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.serialization.cbor.Cbor

import java.math.BigInteger

class TransactionTest extends AnyFunSuite {
    private val addr =
        "addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"

    test("CCL and Scalus transaction hash match") {
        def scalusTransactionHash = {
            val tx = Transaction(
              TransactionBody(
                inputs = TaggedSortedSet.from(
                  Set(
                    Input(Hash(ByteString.fill(32, 0)), 0)
                  )
                ),
                outputs = Vector(
                  Sized(
                    Output(
                      address = Address.fromByteString(AddressBytes.fromBech32(addr)),
                      value = Value.lovelace(2)
                    )
                  )
                ),
                fee = Coin(0)
              ),
              witnessSet = TransactionWitnessSet(),
            )
            //        println(tx)
            //        println(AddressBytes.fromBech32(addr).toHex)
            //        println(Cbor.encode(tx).toByteArray.toHex)
            val txbody = Cbor.encodeToByteString(tx.body)
            val txhash = platform.blake2b_256(txbody)
            //        println(txhash)
            txhash.toHex
        }

        def cclTransactionHash = {
            import com.bloxbean.cardano.client.transaction.spec.*
            val tx = Transaction
                .builder()
                .body(
                  TransactionBody
                      .builder()
                      .inputs(
                        java.util.List.of(
                          TransactionInput
                              .builder()
                              .transactionId(ByteString.fill(32, 0).toHex)
                              .index(0)
                              .build()
                        )
                      )
                      .outputs(
                        java.util.List.of(
                          TransactionOutput
                              .builder()
                              .address(addr)
                              .value(Value.builder().coin(BigInteger.valueOf(2)).build())
                              .build()
                        )
                      )
                      .fee(BigInteger.valueOf(0))
                      .build()
                )
                .build()
            //        println(tx)
            //        println(tx.serialize().toHex)
            val txhash = getTxHash(tx)
            //        println(txhash)
            txhash
        }

        assert(scalusTransactionHash == cclTransactionHash)
    }
    /*

    enum Intent {
        case Input()
        case Output(address: String, value: Value)
        case FromScript(redeemer: Data, script: Script)
    }

    class Tx extends Selectable {
        def apply(intents: Intent*)
    }

    val tx = Tx(Input(???), Output(addr, Value.lovelace(1000)), FeeFrom(???))

    object TxBuilder {
        def payToAddress(
            address: String,
            amount: Value
        ): Transaction = ???
        def send(tx: Transaction): Transaction = ???
        def sign(tx: Transaction) = ???
    }

    val tx = TxBuilder.payToAddress("", Value.lovelace(1000))
    TxBuilder.sign(tx)
    TxBuilder.send(tx)
     */

}
