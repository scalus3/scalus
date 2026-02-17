package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import upickle.ReadWriter

import scala.collection.immutable.SortedSet

/** Represents a Cardano protocol version */
case class ProtocolVersion(
    /** Major version (1-10) */
    major: Int,

    /** Minor version */
    minor: Int
) extends Ordered[ProtocolVersion] derives Codec, ReadWriter {
    require(major >= 1, s"Major version must be positive, got $major")
    require(minor >= 0, s"Minor version must be non-negative, got $minor")
    def compare(that: ProtocolVersion): Int = {
        val majorComp = this.major.compareTo(that.major)
        if majorComp != 0 then majorComp else this.minor.compareTo(that.minor)
    }

    def toMajor: MajorProtocolVersion = MajorProtocolVersion(major)
}

object ProtocolVersion {
    // Constants
    val shelleyPV = ProtocolVersion(2, 0)
    val allegraPV = ProtocolVersion(3, 0)
    val maryPV = ProtocolVersion(4, 0)
    val alonzoPV = ProtocolVersion(5, 0)
    val vasilPV = ProtocolVersion(7, 0)
    val valentinePV = ProtocolVersion(8, 0)
    val conwayPV = ProtocolVersion(9, 0)
    val vanRossemPV = ProtocolVersion(11, 0)
    val futurePV = ProtocolVersion(Int.MaxValue, 0)

    // Known protocol versions
    val knownPVs: SortedSet[ProtocolVersion] =
        SortedSet(
          shelleyPV,
          allegraPV,
          maryPV,
          alonzoPV,
          vasilPV,
          valentinePV,
          conwayPV,
          vanRossemPV
        )
}

case class MajorProtocolVersion(version: Int) extends Ordered[MajorProtocolVersion]
    derives ReadWriter {
    def compare(that: MajorProtocolVersion): Int = this.version.compareTo(that.version)
}

object MajorProtocolVersion {
    // Constants
    val shelleyPV = MajorProtocolVersion(2)
    val allegraPV = MajorProtocolVersion(3)
    val maryPV = MajorProtocolVersion(4)
    val alonzoPV = MajorProtocolVersion(5)
    val vasilPV = MajorProtocolVersion(7)
    val valentinePV = MajorProtocolVersion(8)
    val changPV = MajorProtocolVersion(9)
    val plominPV = MajorProtocolVersion(10)
    val vanRossemPV = MajorProtocolVersion(11)
    @deprecated("Use vanRossemPV instead", "0.8.0")
    val dijkstraPV: MajorProtocolVersion = vanRossemPV
    val futurePV = MajorProtocolVersion(Int.MaxValue)

    // Known protocol versions
    val knownPVs: SortedSet[MajorProtocolVersion] =
        SortedSet(
          shelleyPV,
          allegraPV,
          maryPV,
          alonzoPV,
          vasilPV,
          valentinePV,
          changPV,
          plominPV,
          vanRossemPV
        )
}
