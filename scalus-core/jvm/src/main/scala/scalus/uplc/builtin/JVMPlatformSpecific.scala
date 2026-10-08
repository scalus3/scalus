package scalus.uplc.builtin

import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.jcajce.provider.digest.{Keccak, RIPEMD160, SHA3}
import scalus.crypto.ed25519.{JvmEd25519Signer, SigningKey}
import scalus.crypto.jni.{Blst, Secp256k1, Sodium}
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element, MLResult}
import scalus.utils.Utils

import java.nio.file.{Files, Paths}

object Builtins extends Builtins(using JVMPlatformSpecific)
class Builtins(using ps: PlatformSpecific) extends AbstractBuiltins(using ps)

object JVMPlatformSpecific extends JVMPlatformSpecific
trait JVMPlatformSpecific extends PlatformSpecific {
    // Unused. Kept: a trait without fields has no $init$, which breaks subclasses compiled
    // against 1.2.0. It must keep this name and type: a new private val adds abstract accessors
    // that those subclasses do not implement.
    private val SECP256K1_ORDER =
        BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141", 16)

    override def sha2_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Utils.sha2_256(bs.bytes))

    override def sha2_512(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Utils.sha2_512(bs.bytes))

    override def sha3_256(bs: ByteString): ByteString =
        val digestSHA3 = new SHA3.Digest256()
        ByteString.unsafeFromArray(digestSHA3.digest(bs.bytes))

    override def blake2b_224(bs: ByteString): ByteString =
        val digest = new Blake2bDigest(224)
        digest.update(bs.bytes, 0, bs.size)
        val hash = new Array[Byte](digest.getDigestSize)
        digest.doFinal(hash, 0)
        ByteString.unsafeFromArray(hash)

    override def blake2b_256(bs: ByteString): ByteString =
        val digest = new Blake2bDigest(256)
        digest.update(bs.bytes, 0, bs.size)
        val hash = new Array[Byte](digest.getDigestSize)
        digest.doFinal(hash, 0)
        ByteString.unsafeFromArray(hash)

    override def verifySchnorrSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = {
        require(pk.size == 32, s"Invalid public key length ${pk.size}")
        require(sig.size == 64, s"Invalid signature length ${sig.size}")
        // A key that does not parse throws IllegalArgumentException, as cardano-crypto-class errors.
        Secp256k1.schnorrVerify(sig.bytes, msg.bytes, pk.bytes)
    }

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        require(pk.size == 32, s"Invalid public key length ${pk.size}")
        require(sig.size == 64, s"Invalid signature length ${sig.size}")
        Sodium.ed25519VerifyDetached(sig.bytes, msg.bytes, pk.bytes)

    override def signEd25519(privateKey: ByteString, message: ByteString): ByteString =
        require(privateKey.size == 32, s"Invalid private key length ${privateKey.size}")
        val signingKey = SigningKey.unsafeFromByteString(privateKey)
        JvmEd25519Signer.sign(signingKey, message)

    override def verifyEcdsaSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = {
        require(
          pk.size == 33,
          s"Invalid public key length ${pk.size}, expected 33, ${pk.toHex}"
        )
        require(msg.size == 32, s"Invalid message length ${msg.size}, expected 32")
        require(sig.size == 64, s"Invalid signature length ${sig.size}, expected 64")
        // A key that does not parse, or r or s not below the group order, throws
        // IllegalArgumentException, as cardano-crypto-class errors. Zero r or s parses and then
        // fails verification, so the result is False.
        Secp256k1.ecdsaVerify(msg.bytes, sig.bytes, pk.bytes)
    }

    // BLS12_381 operations, through the blst build cardano-node links (scalus-crypto-jni).

    /** The `(points, scalars)` buffers for a blst MSM, truncated to the shorter of the two inputs.
      * Each point is `pointSize` raw bytes (144 for `blst_p1`, 288 for `blst_p2`); each scalar is
      * 32 big-endian bytes. `value` reads a point's bytes on the fly, so no list of all the points
      * is built.
      */
    private def msmInputs[P](
        scalars: Seq[BigInt],
        points: Seq[P],
        pointSize: Int
    )(value: P => Array[Byte]): (Array[Byte], Array[Byte]) = {
        val n = math.min(scalars.size, points.size)
        val pointBytes = new Array[Byte](n * pointSize)
        val scalarBytes = new Array[Byte](n * 32)
        var i = 0
        val ps = points.iterator.map(value)
        val ss = scalars.iterator
        while i < n do
            System.arraycopy(ps.next(), 0, pointBytes, i * pointSize, pointSize)
            System.arraycopy(PlatformSpecific.blsScalar(ss.next()), 0, scalarBytes, i * 32, 32)
            i += 1
        (pointBytes, scalarBytes)
    }

    override def bls12_381_G1_equal(p1: G1Element, p2: G1Element): Boolean =
        p1 == p2

    override def bls12_381_G1_add(p1: G1Element, p2: G1Element): G1Element =
        new G1Element(Blst.p1AddOrDouble(p1.value, p2.value))

    override def bls12_381_G1_scalarMul(s: BigInt, p: G1Element): G1Element =
        new G1Element(Blst.p1Mult(p.value, PlatformSpecific.blsScalar(s)))

    override def bls12_381_G1_neg(p: G1Element): G1Element =
        new G1Element(Blst.p1Neg(p.value))

    override def bls12_381_G1_compress(p: G1Element): ByteString =
        p.toCompressedByteString

    override def bls12_381_G1_uncompress(bs: ByteString): G1Element = {
        require(
          bs.size == 48,
          s"Invalid length of bytes for compressed point of G1: expected 48, actual: ${bs.size}, byteString: $bs"
        )
        require(
          (bs.bytes(0) & 0x80) != 0,
          s"Compressed bit isn't set for point in G1, byteString: $bs"
        )
        new G1Element(Blst.p1Uncompress(bs.bytes))
    }

    override def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): G1Element = {
        require(
          dst.size <= 255,
          s"Invalid length of bytes for dst parameter of hashToGroup of G1, expected: <= 255, actual: ${dst.size}"
        )
        new G1Element(Blst.p1HashTo(bs.bytes, dst.bytes))
    }

    override def bls12_381_G1_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[G1Element]
    ): G1Element = {
        val (pointBytes, scalarBytes) = msmInputs(scalars, points, pointSize = 144)(_.value)
        new G1Element(Blst.p1Msm(pointBytes, scalarBytes))
    }

    override def bls12_381_G2_equal(p1: G2Element, p2: G2Element): Boolean =
        p1 == p2

    override def bls12_381_G2_add(p1: G2Element, p2: G2Element): G2Element =
        new G2Element(Blst.p2AddOrDouble(p1.value, p2.value))

    override def bls12_381_G2_scalarMul(s: BigInt, p: G2Element): G2Element =
        new G2Element(Blst.p2Mult(p.value, PlatformSpecific.blsScalar(s)))

    override def bls12_381_G2_neg(p: G2Element): G2Element =
        new G2Element(Blst.p2Neg(p.value))

    override def bls12_381_G2_compress(p: G2Element): ByteString =
        p.toCompressedByteString

    override def bls12_381_G2_uncompress(bs: ByteString): G2Element = {
        require(
          bs.size == 96,
          s"Invalid length of bytes for compressed point of G2: expected 96, actual: ${bs.size}, byteString: $bs"
        )
        require(
          (bs.bytes(0) & 0x80) != 0,
          s"Compressed bit isn't set for point in G2, byteString: $bs"
        )
        new G2Element(Blst.p2Uncompress(bs.bytes))
    }

    override def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): G2Element = {
        require(
          dst.size <= 255,
          s"Invalid length of bytes for dst parameter of hashToGroup of G2, expected: <= 255, actual: ${dst.size}"
        )
        new G2Element(Blst.p2HashTo(bs.bytes, dst.bytes))
    }

    override def bls12_381_G2_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[G2Element]
    ): G2Element = {
        val (pointBytes, scalarBytes) = msmInputs(scalars, points, pointSize = 288)(_.value)
        new G2Element(Blst.p2Msm(pointBytes, scalarBytes))
    }

    override def bls12_381_millerLoop(p1: G1Element, p2: G2Element): MLResult =
        new MLResult(Blst.millerLoop(p1.value, p2.value))

    override def bls12_381_mulMlResult(r1: MLResult, r2: MLResult): MLResult =
        new MLResult(Blst.fp12Mul(r1.value, r2.value))

    override def bls12_381_finalVerify(p1: MLResult, p2: MLResult): Boolean =
        Blst.finalVerify(p1.value, p2.value)

    override def keccak_256(bs: ByteString): ByteString = {
        val digest = new Keccak.Digest256()
        ByteString.unsafeFromArray(digest.digest(bs.bytes))
    }

    override def ripemd_160(byteString: ByteString): ByteString = {
        val digest = new RIPEMD160.Digest()
        ByteString.unsafeFromArray(digest.digest(byteString.bytes))
    }

    override def modPow(base: BigInt, exp: BigInt, modulus: BigInt): BigInt =
        base.modPow(exp, modulus)

    override def readFile(path: String): Array[Byte] = {
        Files.readAllBytes(Paths.get(path))
    }

    override def writeFile(path: String, bytes: Array[Byte]): Unit = {
        Files.write(Paths.get(path), bytes)
        ()
    }

    override def appendFile(path: String, bytes: Array[Byte]): Unit = {
        Files.write(
          Paths.get(path),
          bytes,
          java.nio.file.StandardOpenOption.CREATE,
          java.nio.file.StandardOpenOption.APPEND
        )
        ()
    }

    override def createDirectories(path: String): Unit = {
        Files.createDirectories(Paths.get(path))
        ()
    }

    override def fileExists(path: String): Boolean = Files.isRegularFile(Paths.get(path))
}

given PlatformSpecific = JVMPlatformSpecific
