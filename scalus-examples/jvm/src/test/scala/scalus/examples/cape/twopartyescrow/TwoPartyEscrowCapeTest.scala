package scalus.examples.cape.twopartyescrow

import scalus.compiler.Options
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.{CardanoInfo, Coin, ExUnits}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.{List as SList, Option as SOption}
import scalus.testing.kit.ScalusTest
import scalus.uplc.*
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.*

/** CAPE test harness for the Two-Party Escrow benchmark.
  *
  * Parses cape-tests.json from the UPLC-CAPE repository and runs all test cases against the
  * compiled TwoPartyEscrowValidator. Uses the Scalus CEK machine (same as Cardano nodes) so ExUnits
  * results match the CAPE measurement tool exactly.
  */
class TwoPartyEscrowCapeTest extends AnyFunSuite with ScalusTest {

    private given CardanoInfo = CardanoInfo.mainnet
    private val compiled = TwoPartyEscrowContract.compiled
    private val program = compiled.program

    // Parse the test file
    private val testsJson: ujson.Value = {
        val stream =
            getClass.getResourceAsStream("/cape/two_party_escrow/cape-tests.json")
        assert(stream != null, "cape-tests.json not found in test resources")
        ujson.read(stream)
    }

    // Resolve data structures references
    private val dataStructures: Map[String, ujson.Value] = testsJson("data_structures").obj.toMap

    // Script hash used for the escrow contract address
    private val scriptHash: ByteString =
        hex"1111111111111111111111111111111111111111111111111111111111"

    // Default spending TxOutRef
    private val defaultTxOutRef =
        TxOutRef(TxId(hex"0000000000000000000000000000000000000000000000000000000000000000"), 0)

    // Default TxId
    private val defaultTxId = TxId(
      hex"0000000000000000000000000000000000000000000000000000000000000000"
    )

    test(s"Script size: ${compiled.script.script.size} bytes") {
        // TODO: review after changing PairData representations
        // assert(compiled.script.script.size == 1485)
        assert(
          compiled.script.script.size ==
              1387
        )
    }

    // Expected execution budgets for success tests
    // TODO: review after changing PairData representations
    private val expectedBudgets: Map[String, ExUnits] = Map(
      "deposit_successful" -> (ExUnits(memory = 55178L, steps = 23322865L)),
      // "accept_successful" -> ExUnits(memory = 73023, steps = 27249620),
      "accept_successful" -> (ExUnits(memory = 66443L, steps = 25044594L)),
      // "accept_with_multiple_inputs" -> ExUnits(memory = 78845, steps = 30325979),
      "accept_with_multiple_inputs" -> (ExUnits(memory = 72265L, steps = 28120953L)),
      // "accept_with_datum_attached" -> ExUnits(memory = 73023, steps = 27249620),
      "accept_with_datum_attached" -> (ExUnits(memory = 66443L, steps = 25044594L)),
      // "accept_with_multiple_outputs_to_seller" -> ExUnits(memory = 97969, steps = 37424519),
      "accept_with_multiple_outputs_to_seller" -> (ExUnits(memory = 86761L, steps = 34067205L)),
      // "refund_successful" -> ExUnits(memory = 87790, steps = 32729039),
      "refund_successful" -> (ExUnits(memory = 80118L, steps = 29880553L)),
      // "refund_after_exact_deadline" -> ExUnits(memory = 87790, steps = 32729039),
      "refund_after_exact_deadline" -> (ExUnits(memory = 80118L, steps = 29880553L)),
      // "refund_with_multiple_inputs" -> ExUnits(memory = 93612, steps = 35805398),
      "refund_with_multiple_inputs" -> (ExUnits(memory = 85940L, steps = 32956912L)),
      // "refund_with_datum_attached" -> ExUnits(memory = 87790, steps = 32729039),
      "refund_with_datum_attached" -> (ExUnits(memory = 80118L, steps = 29880553L)),
      // "refund_with_multiple_outputs_to_buyer" -> ExUnits(memory = 112736, steps = 42903938)
      "refund_with_multiple_outputs_to_buyer" -> (ExUnits(memory = 100436L, steps = 38903164L))
    )

    // TODO: review after changing PairData representations
    private val expectedFees: Map[String, Coin] = Map(
      "deposit_successful" -> Coin(4866),
      "accept_successful" -> Coin(5640),
      "accept_with_multiple_inputs" -> Coin(6198),
      "accept_with_datum_attached" -> Coin(5640),
      "accept_with_multiple_outputs_to_seller" -> Coin(7463),
      "refund_successful" -> Coin(6778),
      "refund_after_exact_deadline" -> Coin(6778),
      "refund_with_multiple_inputs" -> Coin(7335),
      "refund_with_datum_attached" -> Coin(6778),
      "refund_with_multiple_outputs_to_buyer" -> Coin(8601)
    )

    // Generate test cases from the JSON
    private val tests: Seq[ujson.Value] = testsJson("tests").arr.toSeq

    for testCase <- tests do {
        val testName = testCase("name").str
        val description = testCase("description").str
        val expectedType = testCase("expected")("type").str
        val inputs = testCase("inputs").arr.toSeq

        test(s"CAPE: $testName") {
            val input = inputs.head // Each test has exactly one input

            val inputType = input("type").str
            val scriptContextData: Data = inputType match {
                case "builtin_data" =>
                    // Direct Data input (not a valid ScriptContext)
                    CapeDataParser.parse(input("value").str)
                case "script_context" =>
                    buildScriptContext(input("script_context"))
            }

            // Apply program to the input
            val applied = program $ scriptContextData
            val result = applied.evaluateDebug

            expectedType match {
                case "error" =>
                    assert(
                      result.isFailure,
                      s"$testName: Expected error but got success. Logs: ${result.logs.mkString(", ")}"
                    )

                case "value" =>
                    result match {
                        case Result.Success(term, budget, _, logs) =>
                            val actual = ExUnits(memory = budget.memory, steps = budget.steps)
                            val fee = actual.fee
                            expectedBudgets.get(testName).foreach { expected =>
                                assert(
                                  actual == expected,
                                  s"$testName: expected $expected but got $actual"
                                )
                            }
                            expectedFees.get(testName).foreach { expected =>
                                assert(
                                  fee == expected,
                                  s"$testName: expected fee $expected but got $fee"
                                )
                            }
                        case Result.Failure(ex, budget, _, logs) =>
                            fail(
                              s"$testName: Expected success but got error: $ex\nLogs: ${logs.mkString(", ")}"
                            )
                    }
            }
        }
    }

    // --- Helper methods ---

    /** Parser for CAPE builtin_data notation using cats-parse.
      *
      * Format: `#hex` (bytestring), `[a b c]` (list), `{k:v,k:v}` (map), `tag(fields...)` (constr),
      * or plain integer.
      */
    private object CapeDataParser {
        import cats.parse.{Parser as P, Parser0}

        private val ws0: Parser0[Unit] = P.charIn(" \t\r\n").rep0.void
        private def lexeme[A](p: P[A]): P[A] = p <* ws0

        val data: P[Data] = P.recursive[Data] { self =>
            val bsData: P[Data] =
                lexeme((P.char('#') *> UplcParser.hexByte.rep0).map { bs =>
                    Builtins.bData(ByteString(bs*))
                })

            val listData: P[Data] =
                (lexeme(P.char('[')) *> self.rep0 <* lexeme(P.char(']'))).map { items =>
                    val list = items.toList.foldRight(Builtins.mkNilData())(Builtins.mkCons(_, _))
                    Builtins.listData(list)
                }

            val mapPair: P[(Data, Data)] = self ~ (lexeme(P.char(':')) *> self)
            val mapData: P[Data] =
                (lexeme(P.char('{')) *> mapPair.repSep0(lexeme(P.char(','))) <* lexeme(P.char('}')))
                    .map { pairs =>
                        val pairsList = pairs.toList.foldRight(Builtins.mkNilPairData()) {
                            (pair, acc) =>
                                Builtins.mkCons(Builtins.mkPairData(pair._1, pair._2), acc)
                        }
                        Builtins.mapData(pairsList)
                    }

            val intOrConstr: P[Data] = lexeme(UplcParser.integer).flatMap { n =>
                val constr = (lexeme(P.char('(')) *> self.rep0 <* lexeme(P.char(')'))).map {
                    fields =>
                        val fieldsList =
                            fields.toList.foldRight(Builtins.mkNilData())(Builtins.mkCons(_, _))
                        Builtins.constrData(n, fieldsList)
                }
                constr.backtrack.orElse(P.pure(Builtins.iData(n)))
            }

            bsData | listData | mapData | intOrConstr
        }

        def parse(s: String): Data =
            data.parseAll(s.trim) match
                case Right(result) => result
                case Left(err) =>
                    throw RuntimeException(s"Failed to parse CAPE builtin data '$s': $err")
    }

    /** Resolve a data value that may be a @reference or inline */
    private def resolveDataValue(v: ujson.Value): Data = {
        v match {
            case ujson.Str(s) if s.startsWith("@") =>
                val resolved = dataStructures(s.drop(1))
                resolved("type").str match {
                    case "builtin_data" => CapeDataParser.parse(resolved("value").str)
                    case other =>
                        throw new RuntimeException(s"Cannot resolve data ref of type $other")
                }
            case ujson.Str(s) => CapeDataParser.parse(s)
            case _            => throw new RuntimeException(s"Unexpected data value: $v")
        }
    }

    /** Resolve a pubkey hash that may be a @reference */
    private def resolvePubKeyHash(v: ujson.Value): ByteString = {
        v match {
            case ujson.Str(s) if s.startsWith("@") =>
                val resolved = dataStructures(s.drop(1))
                ByteString.fromHex(resolved("value").str.drop(1)) // drop leading #
            case ujson.Str(s) if s.startsWith("#") =>
                ByteString.fromHex(s.drop(1))
            case _ => throw new RuntimeException(s"Unexpected pubkey hash value: $v")
        }
    }

    /** Build a ScriptContext Data from the CAPE test specification */
    private def buildScriptContext(scJson: ujson.Value): Data = {
        val baseline = scJson("baseline")
        val patches = scJson("patches").arr.toSeq

        // Start with the baseline
        val baseCtx: ScriptContextBuilder = baseline match {
            case ujson.Str(s) if s.startsWith("@") =>
                val refName = s.drop(1)
                val ref = dataStructures(refName)
                buildScriptContextBuilder(ref("script_context"))
            case ujson.Str("spending") =>
                ScriptContextBuilder.spending()
            case _ =>
                throw new RuntimeException(s"Unknown baseline: $baseline")
        }

        // Apply patches
        val patched = patches.foldLeft(baseCtx)(applyPatch)
        patched.build()
    }

    /** Internal builder for constructing ScriptContext */
    private case class ScriptContextBuilder(
        redeemer: Data = Builtins.iData(BigInt(0)),
        signatories: scala.List[PubKeyHash] = scala.Nil,
        validRange: Interval = Interval.always,
        inputs: scala.List[TxInInfo] = scala.Nil,
        outputs: scala.List[TxOut] = scala.Nil,
        scriptDatum: scala.Option[Data] = scala.None,
        txOutRef: TxOutRef = defaultTxOutRef
    ) {
        def build(): Data = {
            val sigs: SList[PubKeyHash] =
                signatories.foldRight(SList.empty[PubKeyHash])((a, b) => SList.Cons(a, b))
            // Ensure spending txOutRef has a matching input (for deposit, no is_own_input is set)
            val allInputs =
                if inputs.exists(_.outRef == txOutRef) then inputs
                else
                    TxInInfo(
                      outRef = txOutRef,
                      resolved = TxOut(
                        address = Address(Credential.ScriptCredential(scriptHash), SOption.None),
                        value = Value.lovelace(BigInt(0)),
                        datum = OutputDatum.NoOutputDatum
                      )
                    ) :: inputs
            val ins: SList[TxInInfo] =
                allInputs.foldRight(SList.empty[TxInInfo])((a, b) => SList.Cons(a, b))
            val outs: SList[TxOut] =
                outputs.foldRight(SList.empty[TxOut])((a, b) => SList.Cons(a, b))

            val txInfo = TxInfo(
              inputs = ins,
              outputs = outs,
              validRange = validRange,
              signatories = sigs,
              id = defaultTxId
            )

            val datumOpt: SOption[Data] = scriptDatum match {
                case scala.Some(d) => SOption.Some(d)
                case scala.None    => SOption.None
            }

            val sc = ScriptContext(
              txInfo = txInfo,
              redeemer = redeemer,
              scriptInfo = ScriptInfo.SpendingScript(txOutRef, datumOpt)
            )

            sc.toData
        }
    }

    private object ScriptContextBuilder {
        def spending(): ScriptContextBuilder = ScriptContextBuilder()
    }

    /** Build a ScriptContextBuilder from a nested script_context JSON */
    private def buildScriptContextBuilder(scJson: ujson.Value): ScriptContextBuilder = {
        val baseline = scJson("baseline")
        val patches = scJson("patches").arr.toSeq

        val base = baseline match {
            case ujson.Str("spending") => ScriptContextBuilder.spending()
            case ujson.Str(s) if s.startsWith("@") =>
                val ref = dataStructures(s.drop(1))
                buildScriptContextBuilder(ref("script_context"))
            case _ => throw new RuntimeException(s"Unknown baseline: $baseline")
        }

        patches.foldLeft(base)(applyPatch)
    }

    /** Apply a single patch operation to the builder */
    private def applyPatch(ctx: ScriptContextBuilder, patch: ujson.Value): ScriptContextBuilder = {
        patch("op").str match {
            case "set_redeemer" =>
                val redeemerStr = patch("redeemer").str
                ctx.copy(redeemer = CapeDataParser.parse(redeemerStr))

            case "add_signature" =>
                val pkh = resolvePubKeyHash(patch("pubkey_hash"))
                ctx.copy(signatories = ctx.signatories :+ PubKeyHash(pkh))

            case "remove_signature" =>
                val pkh = resolvePubKeyHash(patch("pubkey_hash"))
                ctx.copy(signatories = ctx.signatories.filterNot(_.hash == pkh))

            case "set_valid_range" =>
                val fromTime = BigInt(patch("from_time").num.toLong)
                // CAPE uses inclusive lower bound (Cardano PV9+)
                ctx.copy(validRange = Interval.after(fromTime))

            case "add_input_utxo" =>
                val utxoRefStr = patch("utxo_ref").str
                val lovelace = BigInt(patch("lovelace").num.toLong)
                val isOwnInput = patch.obj.get("is_own_input").exists(_.bool)
                val datumOpt = patch.obj.get("datum").map(resolveDataValue)

                val txOutRef = parseTxOutRef(utxoRefStr)
                val address =
                    if isOwnInput then
                        Address(Credential.ScriptCredential(scriptHash), SOption.None)
                    else
                        Address(
                          Credential.PubKeyCredential(
                            PubKeyHash(
                              hex"0000000000000000000000000000000000000000000000000000000000000000"
                            )
                          ),
                          SOption.None
                        )

                val outputDatum = datumOpt match {
                    case scala.Some(d) => OutputDatum.OutputDatum(d)
                    case scala.None    => OutputDatum.NoOutputDatum
                }

                val txInInfo = TxInInfo(
                  outRef = txOutRef,
                  resolved = TxOut(
                    address = address,
                    value = Value.lovelace(lovelace),
                    datum = outputDatum
                  )
                )

                val updatedCtx = ctx.copy(inputs = ctx.inputs :+ txInInfo)
                if isOwnInput then updatedCtx.copy(txOutRef = txOutRef)
                else updatedCtx

            case "add_output_utxo" =>
                val addrJson = patch("address")
                val lovelace = BigInt(patch("lovelace").num.toLong)
                val datumOpt = patch.obj.get("datum").map(resolveDataValue)

                val address = parseAddress(addrJson)
                val outputDatum = datumOpt match {
                    case scala.Some(d) => OutputDatum.OutputDatum(d)
                    case scala.None    => OutputDatum.NoOutputDatum
                }

                val txOut = TxOut(
                  address = address,
                  value = Value.lovelace(lovelace),
                  datum = outputDatum
                )

                ctx.copy(outputs = ctx.outputs :+ txOut)

            case "set_script_datum" =>
                val datum = resolveDataValue(patch("datum"))
                ctx.copy(scriptDatum = scala.Some(datum))

            case other =>
                throw new RuntimeException(s"Unknown patch operation: $other")
        }
    }

    /** Parse a TxOutRef from "txhash:index" format */
    private def parseTxOutRef(s: String): TxOutRef = {
        val parts = s.split(":")
        TxOutRef(TxId(ByteString.fromHex(parts(0))), BigInt(parts(1).toInt))
    }

    /** Parse an address from CAPE JSON format */
    private def parseAddress(addrJson: ujson.Value): Address = {
        addrJson("type").str match {
            case "script" =>
                val hash = ByteString.fromHex(addrJson("script_hash").str)
                Address(Credential.ScriptCredential(hash), SOption.None)
            case "pubkey" =>
                val pkh = resolvePubKeyHash(addrJson("pubkey_hash"))
                Address(Credential.PubKeyCredential(PubKeyHash(pkh)), SOption.None)
        }
    }
}
