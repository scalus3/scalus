package scalus.testing.integration

import scalus.cardano.ledger.{ProtocolParams, Transaction}
import scalus.testing.integration.YaciDevKitClient.YaciError
import scalus.testing.integration.YaciDevKitClient.YaciError.{LedgerError, NetworkError}

class YaciDevKitClient(baseUrl: String = "http://localhost:8080/api/v1")
    extends TxSubmitter
    with ProtocolParamFetcher {

    override type Error = YaciError

    override def fetchLatestParams(): ProtocolParams = {
        val url = s"$baseUrl/epochs/latest/parameters"

        val response = requests.get(url)
        if response.statusCode == 200 then {
            ProtocolParams.fromBlockfrostJson(response.text())
        } else
            throw RuntimeException(
              s"Failed to fetch protocol parameters. Status: ${response.statusCode}, Body: ${response.text()}"
            )
    }

    override def submit(tx: Transaction): Either[YaciError, Unit] = {
        val url = s"$baseUrl/tx/submit"

        val txCbor = tx.toCbor
        try {
            val response = requests.post(
              url,
              data = txCbor,
              headers = Map("Content-Type" -> "application/cbor")
            )
            if response.is2xx then Right(())
            else {
                Left(YaciError.LedgerError(response.text()))
            }
        } catch {
            case e: Throwable => Left(NetworkError(e))
        }
    }

    override def fetchParamsOfEpoch(epoch: Long): ProtocolParams = ???
}

object YaciDevKitClient {
    enum YaciError:
        case NetworkError(underlying: Throwable)
        case LedgerError(description: String)
}
