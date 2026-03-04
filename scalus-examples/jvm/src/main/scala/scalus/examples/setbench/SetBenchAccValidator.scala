package scalus.examples.setbench

import scalus.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.FromData
import scalus.cardano.onchain.plutus.v1.Value.getLovelace
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.cardano.onchain.plutus.crypto.accumulator.G1Accumulator
import scalus.cardano.onchain.plutus.prelude.bls12_381.G2

@Compile
object SetBenchAccValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val state = datum.getOrFail("No datum").to[SetBenchDatum]
        val acc = bls12_381_G1_uncompress(state.root)
        val K = BigInt(2_000_000)

        val action = redeemer.to[AccWithdrawRedeemer]
        val proof = bls12_381_G1_uncompress(action.compressedProof)

        val g2_0 = G2.uncompress(SetBenchCRS.g2_0)
        val g2_1 = G2.uncompress(SetBenchCRS.g2_1)
        val crs = List(g2_0, g2_1)

        require(
          G1Accumulator.verifyMembership(crs, acc, List(action.element), proof),
          "Membership proof failed"
        )

        // After deletion, membership proof IS the new accumulator
        val newRoot = bls12_381_G1_compress(proof)
        val newRemaining = state.remaining - K

        val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
        val outputs = txInfo.findOwnOutputsByCredential(ownInput.address.credential)
        require(outputs.length === BigInt(1), "Expected one continuing output")
        val out = outputs.head
        val outDatum = out.datum match
            case OutputDatum.OutputDatum(d) => d.to[SetBenchDatum]
            case _                          => fail("Expected inline datum")
        require(outDatum.remaining === newRemaining, "Wrong remaining")
        require(outDatum.root === newRoot, "Wrong root")
        require(out.value.getLovelace >= newRemaining, "Insufficient lovelace")
    }
}
