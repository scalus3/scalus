use aiken/merkle_patricia_forestry.{MerklePatriciaForestry, Proof}
use aiken/merkle_patricia_forestry

pub type SetBenchDatum {
  remaining: Int,
  root: ByteArray,
}

pub type SetBenchRedeemer {
  Withdraw { key: ByteArray, value: ByteArray, proof_data: Data }
  Deposit { key: ByteArray, value: ByteArray, proof_data: Data }
}

validator mpf {
  spend(
    datum_opt: Option<SetBenchDatum>,
    redeemer: SetBenchRedeemer,
    _oref: Data,
    _self: Data,
  ) {
    expect Some(datum) = datum_opt
    let trie = merkle_patricia_forestry.from_root(datum.root)
    when redeemer is {
      Withdraw(key, value, proof_data) -> {
        expect proof: Proof = proof_data
        let new_trie = merkle_patricia_forestry.delete(trie, key, value, proof)
        merkle_patricia_forestry.root(new_trie) != datum.root
      }
      Deposit(key, value, proof_data) -> {
        expect proof: Proof = proof_data
        let new_trie = merkle_patricia_forestry.insert(trie, key, value, proof)
        merkle_patricia_forestry.root(new_trie) != datum.root
      }
    }
  }

  else(_) {
    fail
  }
}
