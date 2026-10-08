package scalus.uplc.builtin.bls12_381

import scalus.crypto.jni.Blst

import scala.compiletime.asMatchable

/** A Miller-loop result, held as a raw `blst_fp12` (576 bytes) from scalus-crypto-jni. */
class MLResult private[builtin] (private[builtin] val value: Array[Byte]):
    override def equals(that: Any): Boolean = that.asMatchable match
        case that: MLResult => Blst.fp12IsEqual(value, that.value)
        case _              => false

    // blst_fp12_is_equal compares the raw bytes, so hashing them is consistent with equals.
    override def hashCode(): Int = java.util.Arrays.hashCode(value)
