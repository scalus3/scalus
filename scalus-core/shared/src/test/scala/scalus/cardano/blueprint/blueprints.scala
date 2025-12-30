package scalus.cardano.blueprint

import scalus.builtin.ByteString
import scalus.ledger.api.v1.PosixTime

enum Interval {
    case Finite(value: Int)
    case Infinite
}
object Interval {
    def schema: String =
        """{
          |  "title": "Interval",
          |  "anyOf": [
          |    {
          |      "dataType": "constructor",
          |      "title": "Finite",
          |      "index": 0,
          |      "fields": [
          |        {
          |          "dataType": "integer",
          |          "title": "value"
          |        }
          |      ]
          |    },
          |    {
          |      "dataType": "constructor",
          |      "title": "Infinite",
          |      "index": 1,
          |      "fields": [
          |        
          |      ]
          |    }
          |  ]
          |}""".stripMargin
}

// Copied from examples
object HtlcValidatorInputs {
    type Preimage = ByteString
    type Image = ByteString
    type PubKeyHash = ByteString

    // Contract Datum
    case class ContractDatum(
        committer: PubKeyHash,
        receiver: PubKeyHash,
        image: Image,
        timeout: PosixTime
    )
    object ContractDatum {
        def schema: String =
            """{
              |  "dataType": "constructor",
              |  "title": "ContractDatum",
              |  "fields": [
              |    {
              |      "dataType": "bytes",
              |      "title": "committer"
              |    },
              |    {
              |      "dataType": "bytes",
              |      "title": "receiver"
              |    },
              |    {
              |      "dataType": "bytes",
              |      "title": "image"
              |    },
              |    {
              |      "dataType": "integer",
              |      "title": "timeout"
              |    }
              |  ]
              |}""".stripMargin
    }

    // Redeemer
    enum Action:
        case Timeout
        case Reveal(preimage: Preimage)

    object Action {
        def schema: String = """{
                                      |  "title": "Action",
                                      |  "anyOf": [
                                      |    {
                                      |      "dataType": "constructor",
                                      |      "title": "Timeout",
                                      |      "index": 0,
                                      |      "fields": [
                                      |
                                      |      ]
                                      |    },
                                      |    {
                                      |      "dataType": "constructor",
                                      |      "title": "Reveal",
                                      |      "index": 1,
                                      |      "fields": [
                                      |        {
                                      |          "dataType": "bytes",
                                      |          "title": "preimage"
                                      |        }
                                      |      ]
                                      |    }
                                      |  ]
                                      |}""".stripMargin
    }

}
