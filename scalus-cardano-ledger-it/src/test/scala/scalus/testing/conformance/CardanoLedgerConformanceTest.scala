package scalus.testing.conformance

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules
import scalus.cardano.ledger.rules.*
import scalus.testing.conformance.CardanoLedgerVectors.*
import scalus.utils.Hex

import scala.util.Try

/** Cardano Ledger Conformance Test Suite
  *
  * Runs conformance tests from cardano-ledger test vectors to validate Scalus ledger implementation
  * against reference implementation.
  */
class CardanoLedgerConformanceTest extends AnyFunSuite {

    val TestTag = Tag("conformance")

    test("MetadataValidator Conway.Imp.AllegraImpSpec.UTXOW.InvalidMetadata", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName = "Conway.Imp.AllegraImpSpec.UTXOW.InvalidMetadata"
        for case (path, vector) <- loadAllVectors(vectorName) do {
            val ledgerState = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
            println(pprint(ledgerState))
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            println(pprint(transaction))
            val validation = MetadataValidator.validate(
              Context.testMainnet(),
              ledgerState.ruleState,
              transaction
            )
            println(pprint(validation))
            assert(vector.success === validation.isRight)
        }
    }

    test(
      "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.Voting.Active voting stake.StakePool.Proposal deposits contribute to active voting stake.After switching delegations/9",
      TestTag
    ) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName =
            "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.Voting.Active voting stake.StakePool.Proposal deposits contribute to active voting stake.After switching delegations/9"
        for case (path, vector) <- loadAllVectors(vectorName) do {
            val ledgerState = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
            println(pprint(ledgerState))
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            println(pprint(transaction))
        }
    }

    val validators = STS.Validator(
      List(
        AllInputsMustBeInUtxoValidator,
        EmptyInputsValidator,
        ExactSetOfRedeemersValidator,
        ExUnitsTooBigValidator,
        FeesOkValidator,
        InputsAndReferenceInputsDisjointValidator,
        MetadataValidator,
        MissingKeyHashesValidator,
        MissingOrExtraScriptHashesValidator,
        MissingRequiredDatumsValidator,
        NativeScriptsValidator,
        OutsideForecastValidator,
        OutsideValidityIntervalValidator,
        OutputBootAddrAttrsSizeValidator,
        OutputsHaveNotEnoughCoinsValidator,
        OutputsHaveTooBigValueStorageSizeValidator,
        ProtocolParamsViewHashesMatchValidator,
        ScriptsWellFormedValidator,
        TooManyCollateralInputsValidator,
        TransactionSizeValidator,
        ValueNotConservedUTxOValidator,
        VerifiedSignaturesInWitnessesValidator,
        WrongNetworkInTxBodyValidator,
        WrongNetworkValidator,
        WrongNetworkWithdrawalValidator
      )
    )

    private var pparamsHits = 0
    private var pparamsMisses = 0

    // Set to true to use extracted pparams, false to use default
    private val useExtractedPParams = true

    private def validateVector(vectorName: String) =
        val pparamsDir = CardanoLedgerVectors.pparamsDir
        for case (path, vector) <- loadAllVectors(vectorName) yield
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            val state = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState)).ruleState
            // Extract protocol parameters from test vector
            val extractedParams =
                if useExtractedPParams then
                    ConwayProtocolParams
                        .extractPparamsHash(vector.oldLedgerState, pparamsDir)
                        .flatMap(hash => ConwayProtocolParams.loadFromHash(pparamsDir, hash))
                        .map(_.toProtocolParams)
                else None

            if extractedParams.isDefined then pparamsHits += 1 else pparamsMisses += 1

            val params = extractedParams.getOrElse(Context().env.params) // Fallback to default params
            val context =
                Context(env = rules.UtxoEnv(0, params, state.certState, scalus.cardano.address.Network.Testnet))
            (
              path.getFileName.toFile.getName,
              vector.success,
              validators.validate(context, state, transaction)
            )

    private def failureVectors(
        results: List[(String, Try[List[(String, Boolean, validators.Result)]])]
    ) = for
        case (vectorName, result) <- results
        x <- result.toOption.toList
        case (n, success, result) <- x if success != result.isRight
    yield (vectorName, n, success, result)

    test("Conformance ledger rules test") {
        // Reset counters for this test
        pparamsHits = 0
        pparamsMisses = 0

        val results =
            for vectorName <- vectorNames()
            yield vectorName -> Try(validateVector(vectorName))

        val failed = failureVectors(results)

        for case (name, index, success, result) <- failed do
            println(s"$name/$index ($success) ${pprint(result)}")

        println(s"\n=== Summary ===")
        println(s"Vector names: ${results.length}")
        println(s"Total vectors: ${results.map(_._2.map(_.length).getOrElse(0)).sum}")
        println(s"Failed vectors: ${failed.length}")
        println(s"Success rate: ${100.0 * (results.map(_._2.map(_.length).getOrElse(0)).sum - failed.length) / results.map(_._2.map(_.length).getOrElse(0)).sum}%")
        println(s"PParams hits: $pparamsHits, misses: $pparamsMisses")

        // Group by exception type
        val byException = failed.groupBy {
            case (_, _, _, Left(ex)) => ex.getClass.getSimpleName
            case _ => "Unexpected success"
        }
        println(s"\nFailures by exception type:")
        byException.toSeq.sortBy(-_._2.length).foreach { case (exType, failures) =>
            println(s"  $exType: ${failures.length}")
        }

        // assert(failed.isEmpty)
    }

    test("Conway.Imp.AlonzoImpSpec.UTXO.PlutusV1.Insufficient collateral") {
        val vectorName = "Conway.Imp.AlonzoImpSpec.UTXO.PlutusV1.Insufficient collateral"
        val result = validateVector(vectorName)
        println(pprint(result))
    }

    test("Conway.Imp.AlonzoImpSpec.UTXOS.PlutusV1.Invalid plutus script fails in phase 2") {
        val vectorName =
            "Conway.Imp.AlonzoImpSpec.UTXOS.PlutusV1.Invalid plutus script fails in phase 2"
        val result = validateVector(vectorName)
        println(pprint(result))
    }

    test(
      "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.When CC expired.SPOs alone can't enact hard-fork"
    ) {
        val vectorName =
            "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.When CC expired.SPOs alone can't enact hard-fork"
        val result = validateVector(vectorName)
        println(pprint(result))
    }

    test("Debug CertState CBOR structure", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        // Just try to decode the first test vector to see what structure we have
        val vectorName = "Conway.Imp.ConwayImpSpec - Version 10.DELEG.Unregister stake credentials.deregistering returns the deposit"
        for case (path, vector) <- loadAllVectors(vectorName).take(1) do {
            println(s"\n=== Debugging CBOR structure: ${path.getFileName} ===")
            val cbor = Hex.hexToBytes(vector.oldLedgerState)
            // First 20 bytes as hex
            println(s"First 20 bytes: ${cbor.take(20).map(b => f"${b & 0xff}%02x").mkString(" ")}")

            // Parse as DOM to see structure
            import io.bullet.borer.{Cbor, Dom}
            val dom = Cbor.decode(cbor).to[Dom.Element].value

            // Find the pparams hash by searching for it in the hex string
            val stateHex = vector.oldLedgerState
            // The pparams hash is c4359a5ed626b4b0693cad1d3330c3b5f55a55c0199d725519f3d051ce282799
            val pparamsHash = "c4359a5ed626b4b0693cad1d3330c3b5f55a55c0199d725519f3d051ce282799"
            val pos = stateHex.indexOf(pparamsHash)
            if pos >= 0 then
                println(s"Found pparams hash at hex position: $pos")
                println(s"Context: ...${stateHex.substring(Math.max(0, pos-20), Math.min(stateHex.length, pos+70))}...")
            else
                println("pparams hash not found")

            // Search for any hash that exists in pparams-by-hash directory
            val pparamsDir = CardanoLedgerVectors.pparamsDir
            if pparamsDir != null && java.nio.file.Files.exists(pparamsDir) then
                import scala.jdk.CollectionConverters.*
                val pparamsFiles = java.nio.file.Files.list(pparamsDir).iterator().asScala.toList
                println(s"Found ${pparamsFiles.size} pparams files")
                pparamsFiles.take(5).foreach { f =>
                    val hash = f.getFileName.toString
                    if stateHex.contains(hash) then
                        println(s"  Found pparams hash in state: $hash")
                }

            // Try to parse and show the result
            try
                val ls = LedgerState.fromCbor(cbor)
                println(s"\nParsed VState dreps: ${ls.certs.vstate.dreps.size}")
                println(s"Parsed DState accounts: ${ls.certs.dstate.accounts.size}")
            catch case e: Exception => println(s"Parse error: ${e.getMessage}")
        }
    }

    test("Debug pparams CBOR structure", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        import io.bullet.borer.{Cbor, Dom}
        import java.nio.file.Files

        // Load a pparams file
        val pparamsDir = CardanoLedgerVectors.pparamsDir
        if pparamsDir != null && Files.exists(pparamsDir) then
            import scala.jdk.CollectionConverters.*
            val pparamsFile = Files.list(pparamsDir).iterator().asScala.toList.head
            val pparamsCbor = Files.readAllBytes(pparamsFile)
            println(s"=== Parsing pparams from ${pparamsFile.getFileName} ===")
            println(s"CBOR length: ${pparamsCbor.length} bytes")

            // Parse as DOM to see structure
            val dom = Cbor.decode(pparamsCbor).to[Dom.Element].value
            dom match
                case arr: Dom.ArrayElem =>
                    println(s"Array of ${arr.elems.size} elements")
                    arr.elems.zipWithIndex.foreach { case (elem, idx) =>
                        val elemStr = elem match
                            case Dom.IntElem(v)     => s"Int($v)"
                            case Dom.LongElem(v)    => s"Long($v)"
                            case Dom.DoubleElem(v)  => s"Double($v)"
                            case Dom.ByteArrayElem(v) => s"Bytes(${v.length})"
                            case a: Dom.ArrayElem   => s"Array(${a.elems.size})"
                            case m: Dom.MapElem     => "Map"
                            case Dom.NullElem       => "Null"
                            case Dom.BooleanElem(v) => s"Bool($v)"
                            case Dom.StringElem(v)  => s"String($v)"
                            case t: Dom.TaggedElem  => s"Tagged(${t.tag.code})"
                            case _                  => elem.getClass.getSimpleName
                        println(s"  [$idx]: $elemStr")
                    }
                case _ => println(s"Unexpected: ${dom.getClass}")

            // Try to parse protocol parameters
            println("\n=== Parsing as ConwayProtocolParams ===")
            try
                val pparams = ConwayProtocolParams.fromCbor(pparamsCbor)
                println(s"keyDeposit (stakeAddressDeposit): ${pparams.keyDeposit}")
                println(s"poolDeposit: ${pparams.poolDeposit}")
                println(s"dRepDeposit: ${pparams.dRepDeposit}")
                println(s"minFeeA: ${pparams.minFeeA}")
                println(s"minFeeB: ${pparams.minFeeB}")
                println(s"protocolVersion: ${pparams.protocolVersionMajor}.${pparams.protocolVersionMinor}")

                // Check all pparams files for protocol versions
                println("\n=== Checking all pparams protocol versions ===")
                val allVersions = Files.list(pparamsDir).iterator().asScala.map { pparamsFile =>
                    val cbor = Files.readAllBytes(pparamsFile)
                    try
                        val pp = ConwayProtocolParams.fromCbor(cbor)
                        s"${pp.protocolVersionMajor}.${pp.protocolVersionMinor}"
                    catch case _: Exception => "error"
                }.toList
                val versionCounts = allVersions.groupBy(identity).view.mapValues(_.size).toMap
                println(s"Protocol versions: $versionCounts")
            catch case e: Exception =>
                println(s"Parse error: ${e.getMessage}")
                e.printStackTrace()
        else
            println(s"pparams-by-hash directory not found at $pparamsDir")
    }

    test("Debug ValueNotConserved failures", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        // This vector expects success=true but we get ValueNotConservedUTxOException
        val vectorName =
            "Conway.Imp.ConwayImpSpec - Version 10.DELEG.Unregister stake credentials.deregistering returns the deposit"
        for case (path, vector) <- loadAllVectors(vectorName) do {
            if path.getFileName.toString == "1" then
                println(s"\n=== Analyzing ValueNotConserved: ${path.getFileName} ===")
                val ledgerState = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
                val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
                val state = ledgerState.ruleState

                // Show CertState (should now be populated!)
                println(s"certState.dstate.deposits: ${state.certState.dstate.deposits}")
                println(s"certState.vstate.dreps: ${state.certState.vstate.dreps}")

                // Show certificates in the transaction
                println(s"\nCertificates in transaction:")
                transaction.body.value.certificates.toSeq.foreach { cert =>
                    println(s"  ${pprint(cert)}")
                }

                // Show expected result
                println(s"\nExpected success: ${vector.success}")

                // Show parsed accounts count
                println(s"\nParsed accounts count: ${ledgerState.certs.dstate.accounts.size}")

                // Show consumed and produced values
                import scalus.cardano.ledger.utils.TxBalance
                val params = Context().env.params
                val consumed = TxBalance.consumed(transaction, state.certState, state.utxos, params)
                val produced = TxBalance.produced(transaction, params)
                println(s"\nConsumed: ${pprint(consumed)}")
                println(s"Produced: ${pprint(produced)}")

                // Check deposits refund and deposit calculation
                val certs = transaction.body.value.certificates.toSeq
                def lookupStakingDeposit(cred: Credential): Option[Coin] = state.certState.dstate.deposits.get(cred)
                val refunds = Certificate.shelleyTotalRefundsTxCerts(lookupStakingDeposit, params, certs)
                val deposits = Certificate.shelleyTotalDeposits(params, certs)
                println(s"\nRefunds calculated: $refunds")
                println(s"Deposits calculated: $deposits")
                println(s"stakeAddressDeposit param: ${params.stakeAddressDeposit}")
        }
    }


    test("Addr28Extra mempack parsing uses little-endian byte order", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName =
            "Conway.Imp.AlonzoImpSpec.UTXOW.Valid transactions.PlutusV1.Validating MINT script"
        for case (path, vector) <- loadAllVectors(vectorName) do {
            if path.getFileName.toString == "1" then
                val ledgerState = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
                val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))

                // Get all witness key hashes
                val witnessKeyHashes = transaction.witnessSet.vkeyWitnesses.toSet.map(_.vkeyHash.toHex)

                // Verify that input addresses have matching witnesses
                transaction.body.value.inputs.toSeq.foreach { input =>
                    ledgerState.ruleState.utxos.get(input) match {
                        case Some(output) =>
                            output.address.keyHashOption match {
                                case Some(kh) =>
                                    val khHex = kh.toHex
                                    // This test verifies the Addr28Extra parsing fix:
                                    // With correct little-endian parsing, the payment key hash
                                    // should match one of the witness key hashes
                                    assert(
                                      witnessKeyHashes.contains(khHex),
                                      s"Payment key hash $khHex should have a witness"
                                    )
                                case None => // Script address, no witness needed
                            }
                        case None =>
                            fail(s"Input $input not found in UTxO set")
                    }
                }
        }
    }
}
