class HtlcApplication extends BluepringApplication {

    override val plutusVersion = PlutusVersion.V3

    override def endpoints = Seq(lock, unlock, timeout)

    override def validators = Seq(htlc)

    val lock = endpoint(
      input = Value transaction = payToValidator(htlc).withDatum(ContractDatum(input))
      // hight-level transaction build
    )

    val unlock = endpoint(
      input = Value,
      transaction = spendFromValidator(htlc).withRedeemer(Action.Reveal(input))
      // hight-level transaction build
    )

    val htlc = validator(
      datum[ContractDatum]
          redeemer [Action] script[HtlcValidator]
    )

}

// transction wich include scripot, which we can sent
//  to the blockchain or out test enging.
val valTransaction = HtlcApplication.lock(500).transaction

val scriptCbor = HtlcApplication.htlc.compiled(debug = true).toCbor

val testRunner = createTestRunner[HtlcApplication]
