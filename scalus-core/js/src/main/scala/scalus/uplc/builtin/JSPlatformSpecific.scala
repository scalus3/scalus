package scalus.uplc.builtin

import scalus.uplc.builtin.bls12_381.{G1Element, G2Element, MLResult}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Int8Array
import scala.scalajs.js.typedarray.Uint8Array

@JSImport("@noble/hashes/sha2", JSImport.Namespace)
@js.native
private object Sha2 extends js.Object {
    def sha256(msg: Uint8Array): Uint8Array = js.native
    def sha512(msg: Uint8Array): Uint8Array = js.native
}

@JSImport("@noble/hashes/sha3", JSImport.Namespace)
@js.native
private object Sha3 extends js.Object {
    def sha3_256(msg: Uint8Array): Uint8Array = js.native
    def keccak_256(msg: Uint8Array): Uint8Array = js.native
}

@JSImport("@noble/hashes/blake2b", "blake2b")
@js.native
private object Blake2b extends js.Object {
    def create(opts: BlakeOpts): Hash = js.native
}

private class BlakeOpts(val dkLen: Int) extends js.Object

@js.native
private trait Hash extends js.Object {
    def update(data: Uint8Array): Hash = js.native
    def digest(): Uint8Array = js.native
}

@JSImport("@noble/hashes/ripemd160", JSImport.Namespace)
@js.native
private object Ripemd160 extends js.Object {
    def ripemd160(msg: Uint8Array): Uint8Array = js.native
}

@JSImport("@noble/curves/secp256k1", JSImport.Namespace)
@js.native
private object Secp256k1Curve extends js.Object {
    val secp256k1: Secp256k1 = js.native
    val schnorr: Secp256k1Schnorr = js.native
}

@JSImport("@noble/curves/ed25519", JSImport.Namespace)
@js.native
private object Ed25519Curves extends js.Object {
    val ed25519: Ed25519 = js.native
}

@js.native
private trait Ed25519 extends js.Object {
    def verify(signature: Uint8Array, message: Uint8Array, publicKey: Uint8Array): Boolean =
        js.native
}

@js.native
private trait Secp256k1 extends js.Object {
    def verify(signature: Uint8Array, message: Uint8Array, publicKey: Uint8Array): Boolean =
        js.native

    def ProjectivePoint: ProjectivePointModule = js.native
}

@js.native
trait ProjectivePointModule extends js.Object:
    def fromHex(bytes: Uint8Array): ProjectivePoint = js.native

@js.native
trait ProjectivePoint extends js.Object:
    def toAffine(): js.Object = js.native

@js.native
private trait Secp256k1Schnorr extends js.Object {
    def verify(signature: Uint8Array, message: Uint8Array, publicKey: Uint8Array): Boolean =
        js.native
}

object Builtins extends Builtins(using NodeJsPlatformSpecific)
class Builtins(using ps: PlatformSpecific) extends AbstractBuiltins(using ps)

trait NodeJsPlatformSpecific extends PlatformSpecific {
    extension (bs: ByteString)
        def toUint8Array: Uint8Array =
            val int8Array = new Int8Array(bs.bytes.toJSArray)
            new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)

    extension (arr: Uint8Array)
        def toByteString: ByteString =
            ByteString.unsafeFromArray(
              new Int8Array(arr.buffer, arr.byteOffset, arr.length).toArray
            )

    extension (bigInt: BigInt) def toJsBigInt: js.BigInt = js.BigInt(bigInt.toString())

    override def sha2_256(bs: ByteString): ByteString =
        Sha2.sha256(bs.toUint8Array).toByteString

    override def sha2_512(bs: ByteString): ByteString =
        Sha2.sha512(bs.toUint8Array).toByteString

    override def sha3_256(bs: ByteString): ByteString =
        Sha3.sha3_256(bs.toUint8Array).toByteString

    override def blake2b_224(bs: ByteString): ByteString =
        val hash = Blake2b.create(BlakeOpts(dkLen = 28))
        hash.update(bs.toUint8Array).digest().toByteString

    override def blake2b_256(bs: ByteString): ByteString =
        val hash = Blake2b.create(BlakeOpts(dkLen = 32))
        hash.update(bs.toUint8Array).digest().toByteString

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        require(pk.size == 32, s"Invalid public key length ${pk.size}")
        require(sig.size == 64, s"Invalid signature length ${sig.size}")
        Ed25519Curves.ed25519.verify(sig.toUint8Array, msg.toUint8Array, pk.toUint8Array)

    override def signEd25519(privateKey: ByteString, msg: ByteString): ByteString =
        require(privateKey.size == 32, s"Invalid private key length ${privateKey.size}")
        val signingKey = scalus.crypto.ed25519.SigningKey.unsafeFromByteString(privateKey)
        scalus.crypto.ed25519.JsEd25519Signer.sign(signingKey, msg)

    private def isValidPublicKey(pubKey: ByteString): Boolean =
        try
            val point = Secp256k1Curve.secp256k1.ProjectivePoint.fromHex(
              pubKey.toUint8Array
            ) // Use fromBytes instead of fromHex
            point.toAffine() // Ensures key is valid
            true
        catch case e: Throwable => false

    // secp256k1 group order n
    private val SECP256K1_ORDER =
        BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141", 16)

    override def verifyEcdsaSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = {
        require(pk.size == 33, s"Invalid public key length ${pk.size}, expected 33")
        require(isValidPublicKey(pk), s"Invalid public key ${pk}")
        require(msg.size == 32, s"Invalid message length ${msg.size}, expected 32")
        require(sig.size == 64, s"Invalid signature length ${sig.size}, expected 64")
        // Validate signature components r and s are in valid range [1, n-1]
        val r = BigInt(1, sig.bytes.slice(0, 32))
        val s = BigInt(1, sig.bytes.slice(32, 64))
        require(r > 0 && r < SECP256K1_ORDER, s"Invalid signature: r out of range")
        require(s > 0 && s < SECP256K1_ORDER, s"Invalid signature: s out of range")

        Secp256k1Curve.secp256k1.verify(sig.toUint8Array, msg.toUint8Array, pk.toUint8Array)
    }

    override def verifySchnorrSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean =
        require(pk.size == 32, s"Invalid public key length ${pk.size}, expected 32")
        require(
          isValidPublicKey(ByteString.fromArray(0x02 +: pk.bytes)),
          s"Invalid public key ${pk}"
        )
        require(sig.size == 64, s"Invalid signature length ${sig.size}, expected 64")
        Secp256k1Curve.schnorr.verify(sig.toUint8Array, msg.toUint8Array, pk.toUint8Array)

    // BLS12_381 operations
    override def bls12_381_G1_equal(
        elem1: G1Element,
        elem2: G1Element
    ): Boolean = elem1 == elem2

    override def bls12_381_G1_add(
        elem1: G1Element,
        elem2: G1Element
    ): G1Element = elem1 + elem2

    override def bls12_381_G1_scalarMul(
        scalar: BigInt,
        elem: G1Element
    ): G1Element = elem * scalar

    override def bls12_381_G1_neg(elem: G1Element): G1Element = -elem

    override def bls12_381_G1_compress(elem: G1Element): ByteString =
        elem.toCompressedByteString

    override def bls12_381_G1_uncompress(byteString: ByteString): G1Element =
        G1Element.fromCompressedByteString(byteString)

    override def bls12_381_G1_hashToGroup(
        byteString: ByteString,
        dst: ByteString
    ): G1Element = G1Element.hashToGroup(byteString, dst)

    override def bls12_381_G2_equal(
        elem1: G2Element,
        elem2: G2Element
    ): Boolean = elem1 == elem2

    override def bls12_381_G2_add(
        elem1: G2Element,
        elem2: G2Element
    ): G2Element = elem1 + elem2

    override def bls12_381_G2_scalarMul(
        scalar: BigInt,
        elem: G2Element
    ): G2Element = elem * scalar

    override def bls12_381_G2_neg(
        elem: G2Element
    ): G2Element = -elem

    override def bls12_381_G2_compress(elem: G2Element): ByteString =
        elem.toCompressedByteString

    override def bls12_381_G2_uncompress(byteString: ByteString): G2Element =
        G2Element.fromCompressedByteString(byteString)

    override def bls12_381_G2_hashToGroup(
        byteString: ByteString,
        dst: ByteString
    ): G2Element = G2Element.hashToGroup(byteString, dst)

    override def bls12_381_millerLoop(
        elemG1: G1Element,
        elemG2: G2Element
    ): MLResult =
        MLResult(elemG1, elemG2)

    override def bls12_381_mulMlResult(
        lhs: MLResult,
        rhs: MLResult
    ): MLResult =
        lhs * rhs

    override def bls12_381_finalVerify(lhs: MLResult, rhs: MLResult): Boolean =
        lhs == rhs

    override def bls12_381_G1_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[G1Element]
    ): G1Element = {
        // Use zip behavior: take minimum length, return identity for empty
        scalars.zip(points).foldLeft(G1Element.zero) { case (acc, (scalar, point)) =>
            acc + (point * scalar)
        }
    }

    override def bls12_381_G2_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[G2Element]
    ): G2Element = {
        // Use zip behavior: take minimum length, return identity for empty
        scalars.zip(points).foldLeft(G2Element.zero) { case (acc, (scalar, point)) =>
            acc + (point * scalar)
        }
    }

    override def keccak_256(bs: ByteString): ByteString =
        Sha3.keccak_256(bs.toUint8Array).toByteString

    override def ripemd_160(byteString: ByteString): ByteString =
        Ripemd160.ripemd160(byteString.toUint8Array).toByteString

    /** Custom modular exponentiation using square-and-multiply algorithm.
      *
      * This implementation avoids relying on BigInt.modPow which has bugs in Scala.js for very
      * large numbers.
      */
    override def modPow(base: BigInt, exp: BigInt, modulus: BigInt): BigInt =
        if exp == 0 then BigInt(1)
        else
            var result = BigInt(1)
            var b = base mod modulus
            var e = exp
            while e > 0 do
                if (e mod 2) == 1 then result = (result * b) mod modulus
                e = e >> 1
                b = (b * b) mod modulus
            // Ensure result is non-negative
            if result < 0 then result + modulus else result

    override def readFile(path: String): Array[Byte] = {
        val fs = js.Dynamic.global.require("fs")
        val buffer = fs.readFileSync(path).asInstanceOf[Uint8Array]
        new Int8Array(buffer.buffer, buffer.byteOffset, buffer.length).toArray
    }

    override def writeFile(path: String, bytes: Array[Byte]): Unit = {
        val fs = js.Dynamic.global.require("fs")
        val int8Array = new Int8Array(bytes.toJSArray)
        val uint8Array = new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)
        fs.writeFileSync(path, uint8Array)
    }

    override def appendFile(path: String, bytes: Array[Byte]): Unit = {
        val fs = js.Dynamic.global.require("fs")
        val int8Array = new Int8Array(bytes.toJSArray)
        val uint8Array = new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)
        fs.appendFileSync(path, uint8Array)
    }
}

object NodeJsPlatformSpecific extends NodeJsPlatformSpecific

given PlatformSpecific = NodeJsPlatformSpecific
