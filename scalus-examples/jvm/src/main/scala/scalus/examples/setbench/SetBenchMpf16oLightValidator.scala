package scalus.examples.setbench

import scalus.*
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*

/** Light MPF-16o validator — only validates the Merkle proof, no business logic.
  *
  * Matches the Aiken merkle-patricia-forestry validator exactly:
  * read datum root, parse redeemer, call delete/insert, done.
  * Used for fair Scalus-vs-Aiken on-chain cost comparison.
  */
@Compile
object SetBenchMpf16oLightValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry
        import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry.*

        val state = datum.getOrFail("No datum").to[SetBenchDatum]
        val trie = MerklePatriciaForestry(state.root)

        val action = redeemer.to[SetBenchRedeemer]
        action match
            case SetBenchRedeemer.Withdraw(key, value, proofData) =>
                trie.delete(key, value, proofData.to[Proof])
            case SetBenchRedeemer.Deposit(key, value, proofData) =>
                trie.insert(key, value, proofData.to[Proof])
    }
}
