package scalus.examples.setbench

import scalus.compiler.Compile

import scalus.*
import scalus.uplc.builtin.Builtins.unBData
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.v1.Value.getLovelace
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*

@Compile
object SetBenchMpf16bValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry
        import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry.*

        val state = datum.getOrFail("No datum").to[SetBenchDatum]
        val trie = FusedMerklePatriciaForestry(state.root)
        val ownInput = txInfo.findInputOrFail(txOutRef)
        val K = BigInt(2_000_000)

        val action = redeemer.to[SetBenchRedeemer]
        val newTrie = action match
            case SetBenchRedeemer.Withdraw(key, value, proofData) =>
                trie.delete(key, value, unBData(proofData))
            case SetBenchRedeemer.Deposit(key, value, proofData) =>
                trie.insert(key, value, unBData(proofData))

        val delta = action match
            case _: SetBenchRedeemer.Withdraw =>
                require(state.remaining >= K, "Insufficient remaining for withdrawal")
                -K
            case _: SetBenchRedeemer.Deposit => K

        val out = txInfo.findContinuingOutputOrFail(ownInput, "Expected one continuing output")
        val outDatum = out.datum.inlineOrFail[SetBenchDatum]("Expected inline datum")
        require(outDatum.remaining === state.remaining + delta, "Wrong remaining")
        require(outDatum.root === newTrie.root, "Wrong root")
        require(out.value.getLovelace >= state.remaining + delta, "Insufficient lovelace")
    }
}
