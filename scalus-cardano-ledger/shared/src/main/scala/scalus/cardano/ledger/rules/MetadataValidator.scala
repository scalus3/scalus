package scalus.cardano.ledger
package rules

import scalus.builtin.{platform, ByteString}

import java.nio.charset.StandardCharsets

// It's part of Shelley.validateMetadata in cardano-ledger
object MetadataValidator extends STS.Validator {
    override final type Error = TransactionException.MetadataException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val body = event.body.value

        val auxiliaryDataOption = event.auxiliaryData
        val auxiliaryDataHashOption = body.auxiliaryDataHash
        val majorProtocolVersion = MajorProtocolVersion(context.env.params.protocolVersion.major)

        (auxiliaryDataHashOption, auxiliaryDataOption) match
            case (None, None) => success

            case (Some(auxiliaryDataHash), None) =>
                failure(
                  TransactionException.MetadataException.MissingAuxiliaryDataException(
                    transactionId,
                    auxiliaryDataHash
                  )
                )

            case (None, Some(keepRawAuxiliaryData)) =>
                failure(
                  TransactionException.MetadataException.MissingAuxiliaryDataHashException(
                    transactionId,
                    keepRawAuxiliaryData.value
                  )
                )

            case (Some(expectedAuxiliaryDataHash), Some(keepRawAuxiliaryData)) =>
                val auxiliaryData = keepRawAuxiliaryData.value
                val cborAuxiliaryData = keepRawAuxiliaryData.raw

                val actualAuxiliaryDataHash = AuxiliaryDataHash.fromByteString(
                  platform.blake2b_256(ByteString.unsafeFromArray(cborAuxiliaryData))
                )

                if actualAuxiliaryDataHash != expectedAuxiliaryDataHash then
                    return failure(
                      TransactionException.MetadataException.InvalidAuxiliaryDataHashException(
                        transactionId,
                        actualAuxiliaryDataHash,
                        expectedAuxiliaryDataHash
                      )
                    )

                if majorProtocolVersion <= MajorProtocolVersion.shelleyPV then return success

                if isValidAuxiliaryData(auxiliaryData, majorProtocolVersion) then success
                else
                    failure(
                      TransactionException.MetadataException.InvalidAuxiliaryDataException(
                        transactionId,
                        auxiliaryData
                      )
                    )
    }

    private def isValidAuxiliaryData(
        auxiliaryData: AuxiliaryData,
        majorProtocolVersion: MajorProtocolVersion
    ): Boolean = {
        isValidAuxiliaryMetadata(auxiliaryData) && isValidAuxiliaryPlutusScripts(
          auxiliaryData,
          majorProtocolVersion
        )
    }

    private def isValidAuxiliaryMetadata(auxiliaryData: AuxiliaryData): Boolean = {
        auxiliaryData.getMetadata.view.values.forall(isValidMetadatum)
    }

    private def isValidMetadatum(metadatum: Metadatum): Boolean = {
        metadatum match {
            case Metadatum.Int(_)       => true
            case Metadatum.Bytes(bytes) => bytes.length <= MaxSize
            case Metadatum.Text(str) =>
                str.getBytes(StandardCharsets.UTF_8).length <= MaxSize
            case Metadatum.List(items) => items.forall(isValidMetadatum)
            case Metadatum.Map(entries) =>
                entries.forall { (key, value) =>
                    isValidMetadatum(key) && isValidMetadatum(value)
                }
        }
    }

    private def isValidAuxiliaryPlutusScripts(
        auxiliaryData: AuxiliaryData,
        majorProtocolVersion: MajorProtocolVersion
    ): Boolean = {
        auxiliaryData.getPlutusV1Scripts.forall {
            PlutusScript.isWellFormed(_, Language.PlutusV1, majorProtocolVersion)
        } && auxiliaryData.getPlutusV2Scripts.forall {
            PlutusScript.isWellFormed(_, Language.PlutusV2, majorProtocolVersion)
        } && auxiliaryData.getPlutusV3Scripts.forall {
            PlutusScript.isWellFormed(_, Language.PlutusV3, majorProtocolVersion)
        }
    }

    private val MaxSize = 64
}
