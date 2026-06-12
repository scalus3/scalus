package scalus.uplc

import scalus.cardano.ledger.*

enum BuiltinSemanticsVariant:
    case A, B, C, D, E

object BuiltinSemanticsVariant:

    def fromProtocolAndPlutusVersion(
        protocolVersion: ProtocolVersion,
        plutusLedgerLanguage: Language
    ): BuiltinSemanticsVariant = fromProtocolAndPlutusVersion(
      MajorProtocolVersion(protocolVersion.major),
      plutusLedgerLanguage
    )

    def fromProtocolAndPlutusVersion(
        protocolVersion: MajorProtocolVersion,
        plutusLedgerLanguage: Language
    ): BuiltinSemanticsVariant =
        (protocolVersion, plutusLedgerLanguage) match
            case (pv, Language.PlutusV1 | Language.PlutusV2) =>
                if pv < MajorProtocolVersion.changPV then BuiltinSemanticsVariant.A
                else if pv < MajorProtocolVersion.vanRossemPV then BuiltinSemanticsVariant.B
                else BuiltinSemanticsVariant.D
            case (pv, Language.PlutusV3) if pv >= MajorProtocolVersion.vanRossemPV =>
                BuiltinSemanticsVariant.E
            case (pv, Language.PlutusV3) if pv >= MajorProtocolVersion.changPV =>
                BuiltinSemanticsVariant.C
            case (pv, Language.PlutusV4) if pv >= MajorProtocolVersion.vanRossemPV =>
                BuiltinSemanticsVariant.E
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported protocol version and Plutus language combination $protocolVersion $plutusLedgerLanguage"
                )
