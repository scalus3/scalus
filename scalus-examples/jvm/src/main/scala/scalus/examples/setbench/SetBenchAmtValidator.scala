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
object SetBenchAmtValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        import scalus.cardano.onchain.plutus.amt.AppendOnlyMerkleTree

        val state = datum.getOrFail("No datum").to[AmtDatum]
        val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
        val contractAddr = ownInput.address
        val K = BigInt(2_000_000)

        val action = redeemer.to[AmtRedeemer]
        val (newRoot, newSize, delta) = action match
            case AmtRedeemer.Add(key, proofData) =>
                val siblings = unBData(proofData)
                val nr = AppendOnlyMerkleTree.append(
                  state.root,
                  state.size,
                  state.depth,
                  key,
                  siblings
                )
                (nr, state.size + 1, BigInt(0))
            case AmtRedeemer.Deposit(key, proofData) =>
                AppendOnlyMerkleTree.verifyMember(
                  state.root,
                  key,
                  state.depth,
                  unBData(proofData)
                )
                (state.root, state.size, K)
            case AmtRedeemer.Withdraw(key, proofData) =>
                AppendOnlyMerkleTree.verifyMember(
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
            case OutputDatum.OutputDatum(d) => d.to[AmtDatum]
            case _                          => fail("Expected inline datum")
        require(outDatum.remaining === newRemaining, "Wrong remaining")
        require(outDatum.root === newRoot, "Wrong root")
        require(outDatum.size === newSize, "Wrong size")
        require(outDatum.depth === state.depth, "Depth must not change")
        require(out.value.getLovelace >= newRemaining, "Insufficient lovelace")
    }
}
