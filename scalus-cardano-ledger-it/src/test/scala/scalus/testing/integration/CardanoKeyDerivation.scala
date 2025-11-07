package scalus.testing.integration

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.crypto.cip1852.{DerivationPath, Segment}
import org.bouncycastle.crypto.digests.SHA512Digest
import scalus.builtin.ByteString
import scalus.cardano.ledger.{TaggedSortedSet, Transaction, VKeyWitness}
import scalus.testing.integration.TxSigner

def makeSignerFrom(derivation: String, mnemonic: String): TxSigner = {
    val derivationPieces = derivation.split("/").drop(1).map(_.stripSuffix("'")).map(_.toInt)

    val derivationPath = DerivationPath
        .builder()
        .purpose(new Segment(derivationPieces(0), true))
        .coinType(new Segment(derivationPieces(1), true))
        .account(new Segment(derivationPieces(2), true))
        .role(new Segment(derivationPieces(3), false))
        .index(new Segment(derivationPieces(4), false))
        .build()
    val account = Account.createFromMnemonic(Networks.testnet(), mnemonic, derivationPath)
    val publicKeyData = account.publicKeyBytes()
    val privateKeyData = account.privateKeyBytes()
    (unsigned: Transaction) => {
        val signature = signEd25519(
          privateKeyData,
          publicKeyData,
          unsigned.id.bytes
        )
        val ws = unsigned.witnessSet
            .copy(vkeyWitnesses =
                TaggedSortedSet(
                  VKeyWitness(
                    ByteString.fromArray(publicKeyData),
                    ByteString.fromArray(signature)
                  )
                )
            )
        unsigned.copy(witnessSet = ws)
    }

}

/*
 * This method implements slip-001 ed25519 signatures, which is the way to sign transactions on
 * cardano, hence why we cannot just use the public bouncycastle ed25519 API.
 *
 * In an ideal world, we use the public API, for which we need bouncycastle to expose this method.
 */
def signEd25519(
    cardanoExtendedPrivKey: Array[Byte],
    publicKey: Array[Byte],
    data: Array[Byte]
): Array[Byte] = {
    // private static Unit implSign(d: Digest, h: Array[Byte], s: Array[Byte], pk: Array[Byte], pkOff: Int, ctx: Array[Byte], phflag: Byte, m: Array[Byte], mOff: Int, mLen: Int, sig: Array[Byte], sigOff: Int)
    val ed25519Class = classOf[org.bouncycastle.math.ec.rfc8032.Ed25519]
    val method = ed25519Class.getDeclaredMethod(
      "implSign",
      classOf[org.bouncycastle.crypto.Digest],
      classOf[Array[Byte]],
      classOf[Array[Byte]],
      classOf[Array[Byte]],
      classOf[Int],
      classOf[Array[Byte]],
      classOf[Byte],
      classOf[Array[Byte]],
      classOf[Int],
      classOf[Int],
      classOf[Array[Byte]],
      classOf[Int]
    )
    method.setAccessible(true)
    val d = new SHA512Digest
    val h = new Array[Byte](64)
    Array.copy(cardanoExtendedPrivKey, 0, h, 0, 64)
    val s = new Array[Byte](32)
    Array.copy(cardanoExtendedPrivKey, 0, s, 0, 32)
    val pk = new Array[Byte](32)
    Array.copy(publicKey, 0, pk, 0, 32)
    val ctx = null
    val phflag: Byte = 0
    val m = data
    val mOff = 0
    val mLen = data.length
    val sig = new Array[Byte](64)
    val sigOff = 0
    method.invoke(null, d, h, s, pk, 0, ctx, phflag, m, mOff, mLen, sig, sigOff)
    sig
}
