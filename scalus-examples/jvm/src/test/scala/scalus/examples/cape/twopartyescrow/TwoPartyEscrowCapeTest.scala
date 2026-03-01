package scalus.examples.cape.twopartyescrow

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.testing.kit.ScalusTest
import scalus.uplc.*
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.*

/** CAPE test harness for the Two-Party Escrow benchmark.
  *
  * Parses cape-tests.json from the UPLC-CAPE repository and runs all test cases against the
  * compiled TwoPartyEscrowValidator. Uses the Scalus CEK machine (same as Cardano nodes) so ExUnits
  * results match the CAPE measurement tool exactly.
  */
class TwoPartyEscrowCapeTest extends AnyFunSuite with ScalusTest {

    // Use POption to avoid conflicts with scala.Option
    private type POption[A] = scalus.cardano.onchain.plutus.prelude.Option[A]
    private val PSome = scalus.cardano.onchain.plutus.prelude.Option.Some
    private val PNone = scalus.cardano.onchain.plutus.prelude.Option.None

    private val compiled = TwoPartyEscrowContract
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
        ByteString.fromHex("1111111111111111111111111111111111111111111111111111111111")

    // Default spending TxOutRef
    private val defaultTxOutRef = TxOutRef(
      TxId(
        ByteString.fromHex(
          "0000000000000000000000000000000000000000000000000000000000000000"
        )
      ),
      0
    )

    // Default TxId
    private val defaultTxId = TxId(
      ByteString.fromHex(
        "0000000000000000000000000000000000000000000000000000000000000000"
      )
    )

    test(s"Script size: ${compiled.script.script.size} bytes") {
        val size = compiled.script.script.size
        println(s"Two-Party Escrow script size: $size bytes (Plinth baseline: 3233 bytes)")
        assert(size > 0)
    }

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
                    parseBuiltinData(input("value").str)
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
                            println(
                              s"  $testName: CPU=${budget.steps}, Mem=${budget.memory}"
                            )
                        case Result.Failure(ex, budget, _, logs) =>
                            fail(
                              s"$testName: Expected success but got error: $ex\nLogs: ${logs.mkString(", ")}"
                            )
                    }
            }
        }
    }

    // --- Helper methods ---

    /** Parse CAPE builtin_data notation to Scalus Data */
    private def parseBuiltinData(value: String): Data = {
        val trimmed = value.trim
        if trimmed.startsWith("#") then
            // ByteString: #deadbeef
            Builtins.bData(ByteString.fromHex(trimmed.drop(1)))
        else if trimmed.startsWith("[") then
            // List: [1 2 3]
            val inner = trimmed.drop(1).dropRight(1).trim
            val items =
                if inner.isEmpty then scala.Nil
                else inner.split("\\s+").map(s => parseBuiltinData(s.trim)).toList
            Builtins.listData(items.foldRight(Builtins.mkNilData())(Builtins.mkCons(_, _)))
        else if trimmed.startsWith("{") then
            // Map: {1:42}
            val inner = trimmed.drop(1).dropRight(1).trim
            val pairs = inner.split(",").map { pair =>
                val parts = pair.split(":")
                (parseBuiltinData(parts(0).trim), parseBuiltinData(parts(1).trim))
            }
            val pairsList = pairs.foldRight(Builtins.mkNilPairData()) { (pair, acc) =>
                Builtins.mkCons(Builtins.mkPairData(pair._1, pair._2), acc)
            }
            Builtins.mapData(pairsList)
        else if trimmed.contains("(") then
            // Constructor: 0(0() 1000) or 0()
            val parenIdx = trimmed.indexOf('(')
            val tag = BigInt(trimmed.substring(0, parenIdx).trim)
            val inner = trimmed.substring(parenIdx + 1, trimmed.length - 1).trim
            if inner.isEmpty then Builtins.constrData(tag, Builtins.mkNilData())
            else
                val fields = parseConstructorFields(inner)
                val fieldsList = fields.foldRight(Builtins.mkNilData())(Builtins.mkCons(_, _))
                Builtins.constrData(tag, fieldsList)
        else
            // Integer
            Builtins.iData(BigInt(trimmed))
    }

    /** Parse constructor fields like "0() 1000" into a list of Data */
    private def parseConstructorFields(s: String): scala.List[Data] = {
        val result = scala.collection.mutable.ListBuffer.empty[Data]
        var i = 0
        while i < s.length do
            if s(i).isWhitespace then i += 1
            else if s(i).isDigit || s(i) == '-' then
                // Could be an integer or start of a constructor
                val start = i
                if s(i) == '-' then i += 1
                while i < s.length && s(i).isDigit do i += 1
                if i < s.length && s(i) == '(' then
                    // Constructor: find matching paren
                    var depth = 1
                    i += 1
                    while i < s.length && depth > 0 do
                        if s(i) == '(' then depth += 1
                        else if s(i) == ')' then depth -= 1
                        i += 1
                    result += parseBuiltinData(s.substring(start, i))
                else
                    // Plain integer
                    result += parseBuiltinData(s.substring(start, i))
            else if s(i) == '#' then
                val start = i
                i += 1
                while i < s.length && !s(i).isWhitespace do i += 1
                result += parseBuiltinData(s.substring(start, i))
            else i += 1
        result.toList
    }

    /** Resolve a data value that may be a @reference or inline */
    private def resolveDataValue(v: ujson.Value): Data = {
        v match {
            case ujson.Str(s) if s.startsWith("@") =>
                val resolved = dataStructures(s.drop(1))
                resolved("type").str match {
                    case "builtin_data" => parseBuiltinData(resolved("value").str)
                    case other =>
                        throw new RuntimeException(s"Cannot resolve data ref of type $other")
                }
            case ujson.Str(s) => parseBuiltinData(s)
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
            val sigs: PList[PubKeyHash] =
                signatories.foldRight(PList.empty[PubKeyHash])((a, b) => PList.Cons(a, b))
            // Ensure spending txOutRef has a matching input (for deposit, no is_own_input is set)
            val allInputs =
                if inputs.exists(_.outRef == txOutRef) then inputs
                else
                    TxInInfo(
                      outRef = txOutRef,
                      resolved = TxOut(
                        address = Address(Credential.ScriptCredential(scriptHash), PNone),
                        value = Value.lovelace(BigInt(0)),
                        datum = OutputDatum.NoOutputDatum
                      )
                    ) :: inputs
            val ins: PList[TxInInfo] =
                allInputs.foldRight(PList.empty[TxInInfo])((a, b) => PList.Cons(a, b))
            val outs: PList[TxOut] =
                outputs.foldRight(PList.empty[TxOut])((a, b) => PList.Cons(a, b))

            val txInfo = TxInfo(
              inputs = ins,
              outputs = outs,
              validRange = validRange,
              signatories = sigs,
              id = defaultTxId
            )

            val datumOpt: POption[Data] = scriptDatum match {
                case scala.Some(d) => PSome(d)
                case scala.None    => PNone
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
                ctx.copy(redeemer = parseBuiltinData(redeemerStr))

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
                    if isOwnInput then Address(Credential.ScriptCredential(scriptHash), PNone)
                    else
                        Address(
                          Credential.PubKeyCredential(
                            PubKeyHash(
                              ByteString.fromHex(
                                "0000000000000000000000000000000000000000000000000000000000000000"
                              )
                            )
                          ),
                          PNone
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
                Address(Credential.ScriptCredential(hash), PNone)
            case "pubkey" =>
                val pkh = resolvePubKeyHash(addrJson("pubkey_hash"))
                Address(Credential.PubKeyCredential(PubKeyHash(pkh)), PNone)
        }
    }
}
