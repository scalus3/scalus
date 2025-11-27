package scalus.testing.integration

import com.bloxbean.cardano.client.api.model as bloxbean
import com.fasterxml.jackson.databind.ObjectMapper
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.BloxbeanToLedgerTranslation.*
import scalus.cardano.node.Provider
import scalus.utils.Hex.hexToBytes

import scala.util.Try

class BlockfrostClient(apiKey: String, baseUrl: String = BlockfrostClient.PreviewUrl)
    extends Provider
    with ProtocolParamFetcher {

    private val mapper = new ObjectMapper()

    override def fetchLatestParams(): ProtocolParams = {
        val url = s"$baseUrl/epochs/latest/parameters"
        val response = requests.get(url, headers = Map("project_id" -> apiKey))
        if response.statusCode == 200 then {
            ProtocolParams.fromBlockfrostJson(response.text())
        } else {
            throw RuntimeException(
              s"Failed to fetch protocol parameters. Status: ${response.statusCode}, Body: ${response.text()}"
            )
        }
    }

    override def fetchParamsOfEpoch(epoch: Long): ProtocolParams = {
        val url = s"$baseUrl/epochs/$epoch/parameters"
        val response = requests.get(url, headers = Map("project_id" -> apiKey))
        if response.statusCode == 200 then {
            ProtocolParams.fromBlockfrostJson(response.text())
        } else {
            throw RuntimeException(
              s"Failed to fetch protocol parameters for epoch $epoch. Status: ${response.statusCode}, Body: ${response.text()}"
            )
        }
    }

    def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None,
        minRequiredTotalAmount: Option[Coin] = None
    ): Either[RuntimeException, Utxos] = {
        val bech32 = address match {
            case sh @ ShelleyAddress(network, payment, delegation) => sh.toBech32.get
            case _ => return Left(new RuntimeException("Shelley addresses only"))
        }
        val url = s"$baseUrl/addresses/$bech32/utxos"
        val response = requests.get(url, headers = Map("project_id" -> apiKey))
        if response.is2xx then {
            Right(BlockfrostClient.parseUtxos(mapper, response.text()))
        } else {
            Left(
              RuntimeException(
                s"Failed to fetch UTXOs for address $address. Status: ${response.statusCode}, Body: ${response.text()}"
              )
            )
        }
    }

    override def submit(tx: Transaction): Either[SubmitError, Unit] = {
        val url = s"$baseUrl/tx/submit"
        val txCbor = tx.toCbor

        Try {
            val response = requests.post(
              url,
              data = txCbor,
              headers = Map("project_id" -> apiKey, "Content-Type" -> "application/cbor")
            )
            if response.is2xx then {
                Right(())
            } else if response.is4xx then {
                Left(response.text())
            } else Left(new RuntimeException(s"unexpected blockfrost error: ${response.text()}"))
        }.toEither.left.map(new RuntimeException(_)).flatten
    }

    override def findUtxo(
        input: TransactionInput
    ): Either[RuntimeException, Utxo] =
        Left(new RuntimeException("Unimplemented, use `findUtxos(address)`"))

    override def findUtxos(inputs: Set[TransactionInput]): Either[RuntimeException, Utxos] = Left(
      new RuntimeException("Unimplemented, use `findUtxos(address)`")
    )

    override def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash],
        datum: Option[DatumOption],
        minAmount: Option[Coin]
    ): Either[RuntimeException, Utxo] = Left(
      new RuntimeException("Unimplemented, use `findUtxos(address)`")
    )
}

object BlockfrostClient {
    val MainnetUrl = "https://cardano-mainnet.blockfrost.io/api/v0"
    val PreviewUrl = "https://cardano-preview.blockfrost.io/api/v0"
    val PreprodUrl = "https://cardano-preprod.blockfrost.io/api/v0"
    val LocalUrl = "http://localhost:8080/api/v1"

    def localYaci = BlockfrostClient("", LocalUrl)

    enum BlockfrostError:
        case NetworkError(underlying: Throwable)
        case LedgerError(description: String)

    def parseUtxos(mapper: ObjectMapper, json: String): Utxos = {
        val utxos = mapper.readValue(json, classOf[Array[bloxbean.Utxo]])
        utxos.map { utxo =>
            val txInput = TransactionInput(
              TransactionHash.fromHex(utxo.getTxHash),
              utxo.getOutputIndex
            )

            val address = Address.fromBech32(utxo.getAddress)
            val value = utxo.toValue.toLedgerValue

            // Parse datum if present
            val datumOption: Option[DatumOption] =
                Option(utxo.getDataHash) -> Option(utxo.getInlineDatum) match
                    case (_, Some(inlineDatum)) =>
                        Some(DatumOption.Inline(Data.fromCbor(inlineDatum.hexToBytes)))
                    case (Some(dataHash), None) =>
                        Some(DatumOption.Hash(Hash(ByteString.fromHex(dataHash))))
                    case (None, None) => None

            val txOutput = TransactionOutput(
              address = address,
              value = value,
              datumOption = datumOption,
              scriptRef = None
            )

            txInput -> txOutput
        }.toMap
    }
}
