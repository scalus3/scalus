package scalus.uplc

import scalus.cardano.ledger.*

enum BuiltinSemanticsVariant:
    case A, B, C

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
                else BuiltinSemanticsVariant.B
            case (pv, Language.PlutusV3) if pv >= MajorProtocolVersion.changPV =>
                BuiltinSemanticsVariant.C
            case (pv, Language.PlutusV4) if pv >= MajorProtocolVersion.dijkstraPV =>
                BuiltinSemanticsVariant.C
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported protocol version and Plutus language combination $protocolVersion $plutusLedgerLanguage"
                )
