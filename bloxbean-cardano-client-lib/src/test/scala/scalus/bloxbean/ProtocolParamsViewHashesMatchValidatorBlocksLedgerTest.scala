package scalus.bloxbean

import scalus.cardano.ledger.rules.*

class ProtocolParamsViewHashesMatchValidatorBlocksLedgerTest
    extends BlocksLedgerRulesValidator(ProtocolParamsViewHashesMatchValidator)
