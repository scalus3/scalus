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
object SetBenchMpf64bValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        import scalus.cardano.onchain.plutus.mpf64b.MerklePatriciaForestry
        import scalus.cardano.onchain.plutus.mpf64b.MerklePatriciaForestry.*

        val state = datum.getOrFail("No datum").to[SetBenchDatum]
        val trie = MerklePatriciaForestry(state.root)
        val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
        val contractAddr = ownInput.address
        val K = BigInt(2_000_000)

        val action = redeemer.to[SetBenchRedeemer]
        val newTrie = action match
            case SetBenchRedeemer.Withdraw(key, value, proofData) =>
                trie.delete(key, value, unBData(proofData))
            case SetBenchRedeemer.Deposit(key, value, proofData) =>
                trie.insert(key, value, unBData(proofData))

        val delta = action match
            case _: SetBenchRedeemer.Withdraw => -K
            case _: SetBenchRedeemer.Deposit  => K

        val outputs = txInfo.findOwnOutputsByCredential(contractAddr.credential)
        require(outputs.length === BigInt(1), "Expected one continuing output")
        val out = outputs.head
        val outDatum = out.datum match
            case OutputDatum.OutputDatum(d) => d.to[SetBenchDatum]
            case _                          => fail("Expected inline datum")
        require(outDatum.remaining === state.remaining + delta, "Wrong remaining")
        require(outDatum.root === newTrie.root, "Wrong root")
        require(out.value.getLovelace >= state.remaining + delta, "Insufficient lovelace")
    }
}
