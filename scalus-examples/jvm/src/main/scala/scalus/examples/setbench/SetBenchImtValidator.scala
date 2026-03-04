package scalus.examples.setbench

import scalus.*
import scalus.uplc.builtin.Builtins.unBData
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.v1.Value.getLovelace
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*

@Compile
object SetBenchImtValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        import scalus.cardano.onchain.plutus.crypto.tree.IncrementalMerkleTree

        val state = datum.getOrFail("No datum").to[ImtDatum]
        val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
        val contractAddr = ownInput.address
        val K = BigInt(2_000_000)

        val action = redeemer.to[ImtRedeemer]
        val (newRoot, newSize, delta) = action match
            case ImtRedeemer.Add(key, proofData) =>
                val siblings = unBData(proofData)
                val nr = IncrementalMerkleTree.append(
                  state.root,
                  state.size,
                  state.depth,
                  key,
                  siblings
                )
                (nr, state.size + 1, BigInt(0))
            case ImtRedeemer.Deposit(key, proofData) =>
                IncrementalMerkleTree.verifyMembership(
                  state.root,
                  key,
                  state.depth,
                  unBData(proofData)
                )
                (state.root, state.size, K)
            case ImtRedeemer.Withdraw(key, proofData) =>
                require(state.remaining >= K, "Insufficient remaining for withdrawal")
                IncrementalMerkleTree.verifyMembership(
                  state.root,
                  key,
                  state.depth,
                  unBData(proofData)
                )
                (state.root, state.size, -K)

        val newRemaining = state.remaining + delta

        val outputs = txInfo.findOwnOutputsByCredential(contractAddr.credential)
        require(outputs.length === BigInt(1), "Expected one continuing output")
        val out = outputs.head
        val outDatum = out.datum match
            case OutputDatum.OutputDatum(d) => d.to[ImtDatum]
            case _                          => fail("Expected inline datum")
        require(outDatum.remaining === newRemaining, "Wrong remaining")
        require(outDatum.root === newRoot, "Wrong root")
        require(outDatum.size === newSize, "Wrong size")
        require(outDatum.depth === state.depth, "Depth must not change")
        require(out.value.getLovelace >= newRemaining, "Insufficient lovelace")
    }
}
