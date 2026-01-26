package scalus.examples.escrow

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob, Eve}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

/** Tests for the EscrowValidator contract.
  *
  * Flow:
  *   1. Initialize: Alice (seller) creates escrow UTxO with initializationAmount
  *   2. Deposit: Bob (buyer) deposits escrowAmount
  *   3. Pay: Bob releases payment to Alice OR
  *   4. Refund: Alice refunds to Bob
  */
class EscrowTest extends AnyFunSuite, ScalusTest {

    private given env: CardanoInfo = TestUtil.testEnvironment
    private val contract = EscrowContract.withErrorTraces

    private val txCreator = EscrowTransactions(
      env = env,
      contract = contract
    )

    // Test amounts (in lovelace)
    private val initializationAmount: Long = 2_000_000L // 2 ADA (min UTxO)
    private val escrowAmount: Long = 10_000_000L // 10 ADA
    private val totalAmount: Long = initializationAmount + escrowAmount

    private def createProvider(): Emulator =
        Emulator.withAddresses(Seq(Alice.address, Bob.address, Eve.address))

    /** Helper: Initialize escrow contract. Returns the escrow UTxO. */
    private def initialize(provider: Emulator): Utxo = {
        val utxos = provider.findUtxos(address = Alice.address).await().toOption.get

        val initTx = txCreator.initialize(
          utxos = utxos,
          sponsor = Alice.address,
          seller = Alice.addrKeyHash,
          buyer = Bob.addrKeyHash,
          escrowAmount = escrowAmount,
          initializationAmount = initializationAmount,
          signer = Alice.signer
        )
        assert(provider.submit(initTx).await().isRight, "Initialize tx failed")

        val escrowUtxo = initTx.utxos.find { case (_, txOut) =>
            txOut.address == contract.address(env.network)
        }.get
        Utxo(escrowUtxo)
    }

    /** Helper: Initialize and deposit. Returns the funded escrow UTxO. */
    private def initializeAndDeposit(provider: Emulator): Utxo = {
        val escrowUtxo = initialize(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val depositTx = txCreator.deposit(
          utxos = utxos,
          escrowUtxo = escrowUtxo,
          buyerAddress = Bob.address,
          sponsor = Bob.address,
          buyer = Bob.addrKeyHash,
          signer = Bob.signer
        )
        assert(provider.submit(depositTx).await().isRight, "Deposit tx failed")

        // Find the new funded escrow UTxO
        val fundedUtxo = depositTx.utxos.find { case (_, txOut) =>
            txOut.address == contract.address(env.network)
        }.get
        Utxo(fundedUtxo)
    }

    // --- Contract Size ---

    test(s"Escrow validator size is ${EscrowContract.script.script.size} bytes") {
        assert(EscrowContract.script.script.size > 0)
    }

    // --- Initialize Tests ---

    test("Initialize: seller creates escrow UTxO") {
        val provider = createProvider()
        val escrowUtxo = initialize(provider)

        assert(escrowUtxo.output.value.coin.value == initializationAmount)
    }

    // --- Deposit Tests ---

    test("Deposit: buyer deposits escrow amount") {
        val provider = createProvider()
        val escrowUtxo = initialize(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val depositTx = txCreator.deposit(
          utxos = utxos,
          escrowUtxo = escrowUtxo,
          buyerAddress = Bob.address,
          sponsor = Bob.address,
          buyer = Bob.addrKeyHash,
          signer = Bob.signer
        )

        val result = provider.submit(depositTx).await()
        assert(result.isRight, s"Deposit tx failed: $result")

        // Verify the contract UTxO now has the full amount
        val fundedUtxo = depositTx.utxos.find { case (_, txOut) =>
            txOut.address == contract.address(env.network)
        }.get
        assert(fundedUtxo._2.value.coin.value == totalAmount)
    }

    test("Deposit fails: wrong signer (not buyer)") {
        val provider = createProvider()
        val escrowUtxo = initialize(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        assertScriptFail("Buyer must sign deposit transaction") {
            txCreator.deposit(
              utxos = utxos,
              escrowUtxo = escrowUtxo,
              buyerAddress = Eve.address,
              sponsor = Eve.address,
              buyer = Eve.addrKeyHash, // Wrong signer - should be Bob
              signer = Eve.signer
            )
        }
    }

    test("Deposit: already funded (known bug - should fail but passes)") {
        val provider = createProvider()
        val fundedUtxo = initializeAndDeposit(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        // BUG: This should fail but passes due to incorrect validation in handleDeposit.
        // The validator uses `contractBalance != escrowAmount` (line 91-92) instead of
        // checking `contractBalance === initializationAmount`. This allows deposits
        // to already-funded contracts. The deposit creates a contract output with
        // the same amount as the input (12 ADA), effectively doing nothing.
        val depositTx = txCreator.deposit(
          utxos = utxos,
          escrowUtxo = fundedUtxo,
          buyerAddress = Bob.address,
          sponsor = Bob.address,
          buyer = Bob.addrKeyHash,
          signer = Bob.signer
        )

        // Transaction succeeds due to the bug - documenting this as the current behavior
        val result = provider.submit(depositTx).await()
        assert(result.isRight, s"Deposit tx unexpectedly failed: $result")
    }

    // --- Pay Tests ---

    test("Pay: buyer releases payment to seller") {
        val provider = createProvider()
        val fundedUtxo = initializeAndDeposit(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val payTx = txCreator.pay(
          utxos = utxos,
          escrowUtxo = fundedUtxo,
          sellerAddress = Alice.address,
          buyerAddress = Bob.address,
          sponsor = Bob.address,
          buyer = Bob.addrKeyHash,
          signer = Bob.signer
        )

        val result = provider.submit(payTx).await()
        assert(result.isRight, s"Pay tx failed: $result")
    }

    test("Pay fails: wrong signer (not buyer)") {
        val provider = createProvider()
        val fundedUtxo = initializeAndDeposit(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        // The validator checks buyerOutputs.nonEmpty before checking isSignedBy.
        // Since Eve's address != Bob's address (the buyer), buyerOutputs is empty.
        assertScriptFail("Buyer outputs must not be empty") {
            txCreator.pay(
              utxos = utxos,
              escrowUtxo = fundedUtxo,
              sellerAddress = Alice.address,
              buyerAddress = Eve.address,
              sponsor = Eve.address,
              buyer = Eve.addrKeyHash, // Wrong signer - should be Bob
              signer = Eve.signer
            )
        }
    }

    test("Pay fails: not funded") {
        val provider = createProvider()
        val escrowUtxo = initialize(provider) // Not deposited yet
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        assertScriptFail("Contract must be fully funded before payment") {
            txCreator.pay(
              utxos = utxos,
              escrowUtxo = escrowUtxo,
              sellerAddress = Alice.address,
              buyerAddress = Bob.address,
              sponsor = Bob.address,
              buyer = Bob.addrKeyHash,
              signer = Bob.signer
            )
        }
    }

    test("Pay fails: wrong amount to seller") {
        val provider = createProvider()
        val fundedUtxo = initializeAndDeposit(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get
        val builder = TxBuilder(env)

        // Try to pay seller less than required amount
        assertScriptFail("Seller must receive exactly escrow amount plus initialization amount") {
            builder
                .spend(fundedUtxo, Action.Pay, contract.script, Set(Bob.addrKeyHash))
                .payTo(
                  Alice.address,
                  Value.ada(5)
                ) // Wrong: should be escrowAmount + initializationAmount
                .payTo(Bob.address, Value.ada(1))
                .complete(availableUtxos = utxos, sponsor = Bob.address)
                .sign(Bob.signer)
                .transaction
        }
    }

    // --- Refund Tests ---

    test("Refund: seller refunds to buyer") {
        val provider = createProvider()
        val fundedUtxo = initializeAndDeposit(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val refundTx = txCreator.refund(
          utxos = utxos,
          escrowUtxo = fundedUtxo,
          sellerAddress = Alice.address,
          buyerAddress = Bob.address,
          sponsor = Alice.address,
          seller = Alice.addrKeyHash,
          signer = Alice.signer
        )

        val result = provider.submit(refundTx).await()
        assert(result.isRight, s"Refund tx failed: $result")
    }

    test("Refund fails: wrong signer (not seller)") {
        val provider = createProvider()
        val fundedUtxo = initializeAndDeposit(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        // The validator checks sellerOutputs.nonEmpty before checking isSignedBy.
        // Since Eve's address != Alice's address (the seller), sellerOutputs is empty.
        assertScriptFail("Seller outputs must not be empty") {
            txCreator.refund(
              utxos = utxos,
              escrowUtxo = fundedUtxo,
              sellerAddress = Eve.address,
              buyerAddress = Bob.address,
              sponsor = Eve.address,
              seller = Eve.addrKeyHash, // Wrong signer - should be Alice
              signer = Eve.signer
            )
        }
    }

    test("Refund fails: not funded") {
        val provider = createProvider()
        val escrowUtxo = initialize(provider) // Not deposited yet
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        assertScriptFail("Contract must be fully funded before refund") {
            txCreator.refund(
              utxos = utxos,
              escrowUtxo = escrowUtxo,
              sellerAddress = Alice.address,
              buyerAddress = Bob.address,
              sponsor = Alice.address,
              seller = Alice.addrKeyHash,
              signer = Alice.signer
            )
        }
    }

    test("Refund fails: wrong amount to buyer") {
        val provider = createProvider()
        val fundedUtxo = initializeAndDeposit(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val builder = TxBuilder(env)

        // Try to refund buyer less than required amount
        assertScriptFail("Buyer must receive exactly the escrow amount back") {
            builder
                .spend(fundedUtxo, Action.Refund, contract.script, Set(Alice.addrKeyHash))
                .payTo(Bob.address, Value.ada(5)) // Wrong: should be escrowAmount (10 ADA)
                .payTo(Alice.address, Value.ada(1))
                .complete(availableUtxos = utxos, sponsor = Alice.address)
                .sign(Alice.signer)
                .transaction
        }
    }
}
