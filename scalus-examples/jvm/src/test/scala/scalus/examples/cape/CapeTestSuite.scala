package scalus.examples.cape

import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v1.{IntervalBound, IntervalBoundType}
import scalus.cardano.onchain.plutus.prelude.{List as SList, Option as SOption}
import scalus.uplc.{Term, UplcParser}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** One resolved input of a CAPE test case. */
enum CapeInput {
    case Uplc(term: Term) // "(con integer 12)"
    case Dat(data: Data) // builtin_data
    case Ctx(data: Data) // built ScriptContext, as Data
}

case class CapeCase(
    name: String,
    description: String,
    inputs: Seq[CapeInput],
    expectError: Boolean,
    expectedTerm: scala.Option[Term], // parsed "content" when type=value
    isMeasurement: Boolean
)

/** Loads a UPLC-CAPE v3.0.0 `cape-tests.json` fixture and builds `ScriptContext` values that
  * reproduce `lib/Cape/ScriptContextBuilder.hs` / `lib/Cape/Tests.hs` from the upstream UPLC-CAPE
  * repository exactly.
  */
class CapeTestSuite(json: ujson.Value) {
    private val ds: Map[String, ujson.Value] =
        json.obj.get("data_structures").map(_.obj.toMap).getOrElse(Map.empty)
    private val termParser = UplcParser()

    def dataStructure(name: String): Data = parseDataValue(ds(name)("value"))

    /** builtin_data value: UPLC-text string ("Constr 0 [...]", "B #.." , "I 1") or detailed-schema
      * JSON object (cardano-cli style: {"constructor":..,"fields":[..]}, {"int":..}, {"bytes":..}).
      */
    def parseDataValue(v: ujson.Value): Data = v match
        case ujson.Str(s) =>
            UplcParser.dataTerm.parseAll(s.trim) match
                case Right(d)  => d
                case Left(err) => throw RuntimeException(s"Bad Data '$s': $err")
        case obj: ujson.Obj => Data.fromJson(ujson.write(obj))
        case other          => throw RuntimeException(s"Unexpected builtin_data: $other")

    /** Resolves a full `Data` value that may be a `@ref` into `data_structures`, or an inline
      * builtin_data value (UPLC text or detailed-schema JSON object).
      */
    private def resolveData(v: ujson.Value): Data = v match
        case ujson.Str(s) if s.startsWith("@") =>
            val r = ds(s.drop(1))
            require(r("type").str == "builtin_data", s"Expected builtin_data ref for '$s'")
            parseDataValue(r("value"))
        case other => parseDataValue(other)

    /** Resolves a raw bytestring value: a `@ref` to a builtin_data bytestring, or a plain `#hex`
      * literal (NOT UPLC-data-text notation — pubkey hashes / currency symbols / token names are
      * written as bare hex in the fixtures, without the "B " tag).
      */
    private def resolveBytes(v: ujson.Value): ByteString = v match
        case ujson.Str(s) if s.startsWith("@") =>
            val r = ds(s.drop(1))
            require(r("type").str == "builtin_data", s"Expected builtin_data ref for '$s'")
            parseDataValue(r("value")) match
                case Data.B(bs) => bs
                case d          => throw RuntimeException(s"Expected bytes ref '$s', got $d")
        case ujson.Str(s) if s.startsWith("#") => ByteString.fromHex(s.drop(1))
        case other => throw RuntimeException(s"Expected bytes value, got $other")

    /** Resolves a hash value (e.g. `script_hash`): a `@ref` (delegates to `resolveBytes`), a
      * `#`-prefixed hex literal, or bare hex — all three forms appear across the fixtures.
      */
    private def resolveHash(v: ujson.Value): ByteString = v match
        case ujson.Str(s) if s.startsWith("@") => resolveBytes(v)
        case ujson.Str(s)                      => ByteString.fromHex(s.stripPrefix("#"))
        case other => throw RuntimeException(s"Expected hash value, got $other")

    private def parseTerm(s: String): Term = termParser.term.parseAll(s.trim) match
        case Right(t)  => t
        case Left(err) => throw RuntimeException(s"Bad UPLC term '$s': $err")

    // ---- ScriptContext builder: mirrors upstream ScriptContextBuilder.hs exactly ----
    // (defined before `cases` below, which eagerly builds ScriptContext values that depend on
    // `zeroTxId` via `Builder`'s default `ownRef`)

    private val scriptHash =
        ByteString.fromHex("1111111111111111111111111111111111111111111111111111111111")
    private val zeroTxId = TxId(ByteString.fromHex("0" * 64))

    val cases: Seq[CapeCase] = {
        def mk(v: ujson.Value, meas: Boolean): CapeCase = CapeCase(
          name = v("name").str,
          description = v("description").str,
          inputs = v("inputs").arr.toSeq.map(parseInput),
          expectError = v("expected")("type").str == "error",
          expectedTerm = v("expected").obj.get("content").map(c => parseTerm(c.str)),
          isMeasurement = meas
        )
        json("measurements").arr.toSeq.map(mk(_, true))
            ++ json.obj.get("checks").map(_.arr.toSeq).getOrElse(scala.Seq.empty).map(mk(_, false))
    }

    private def parseInput(in: ujson.Value): CapeInput = in("type").str match
        case "uplc"           => CapeInput.Uplc(parseTerm(in("value").str))
        case "builtin_data"   => CapeInput.Dat(resolveData(in("value")))
        case "script_context" => CapeInput.Ctx(buildContext(in("script_context")).toData)
        case other            => throw RuntimeException(s"Unknown input type: $other")

    private case class Builder(
        redeemer: Data = ().toData,
        signatories: scala.List[PubKeyHash] = scala.Nil, // stored in FINAL order (already reversed)
        validRange: Interval = Interval.always,
        inputs: scala.List[TxInInfo] = scala.Nil,
        outputs: scala.List[TxOut] = scala.Nil,
        scriptDatum: scala.Option[Data] = scala.Some(().toData),
        ownRef: TxOutRef = TxOutRef(zeroTxId, 0)
    )

    private def buildContext(scJson: ujson.Value): ScriptContext = {
        val b = builderOf(scJson)
        ScriptContext(
          txInfo = TxInfo(
            inputs = SList.from(b.inputs),
            outputs = SList.from(b.outputs),
            validRange = b.validRange,
            signatories = SList.from(b.signatories),
            id = zeroTxId
          ),
          redeemer = b.redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            b.ownRef,
            b.scriptDatum.fold(SOption.None)(SOption.Some(_))
          )
        )
    }

    private def builderOf(scJson: ujson.Value): Builder = {
        val base = scJson("baseline") match
            case ujson.Str("spending") => Builder()
            case ujson.Str(s) if s.startsWith("@") =>
                builderOf(ds(s.drop(1))("script_context"))
            case other => throw RuntimeException(s"Unknown baseline: $other")
        scJson("patches").arr.foldLeft(base)(applyPatch)
    }

    private def applyPatch(b: Builder, p: ujson.Value): Builder = p("op").str match
        case "set_redeemer" => b.copy(redeemer = resolveData(p("redeemer")))
        case "add_signature" => // PREPEND (upstream cons)
            b.copy(signatories = PubKeyHash(resolveBytes(p("pubkey_hash"))) :: b.signatories)
        case "remove_signature" =>
            val pkh = resolveBytes(p("pubkey_hash"))
            b.copy(signatories = b.signatories.filterNot(_.hash == pkh))
        case "set_valid_range" =>
            def bound(key: String, inf: IntervalBoundType): IntervalBound =
                p.obj
                    .get(key)
                    .map(t => IntervalBound(IntervalBoundType.Finite(BigInt(t.num.toLong)), true))
                    .getOrElse(IntervalBound(inf, true))
            b.copy(validRange =
                Interval(
                  bound("from_time", IntervalBoundType.NegInf),
                  bound("to_time", IntervalBoundType.PosInf)
                )
            )
        case "add_input_utxo" =>
            val parts = p("utxo_ref").str.split(':')
            val ref = TxOutRef(TxId(ByteString.fromHex(parts(0))), BigInt(parts(1).toInt))
            val own = p("is_own_input").bool
            val addr =
                if own then Address(Credential.ScriptCredential(scriptHash), SOption.None)
                else
                    Address(Credential.PubKeyCredential(PubKeyHash(ByteString.empty)), SOption.None)
            val txIn = TxInInfo(ref, TxOut(addr, parseValue(p("value")), datumOf(p), SOption.None))
            val b2 = b.copy(inputs = txIn :: b.inputs) // PREPEND
            if own then b2.copy(ownRef = ref) else b2
        case "add_output_utxo" =>
            val addr = p("address")("type").str match
                case "script" =>
                    Address(
                      Credential.ScriptCredential(resolveHash(p("address")("script_hash"))),
                      SOption.None
                    )
                case "pubkey" =>
                    Address(
                      Credential.PubKeyCredential(
                        PubKeyHash(resolveBytes(p("address")("pubkey_hash")))
                      ),
                      SOption.None
                    )
                case other => throw RuntimeException(s"Unknown address type: $other")
            b.copy(outputs =
                TxOut(addr, parseValue(p("value")), datumOf(p), SOption.None) :: b.outputs
            ) // PREPEND
        case "remove_output_utxo" =>
            val i = p("index").num.toInt
            b.copy(outputs = b.outputs.patch(i, scala.Nil, 1))
        case "set_script_datum" => b.copy(scriptDatum = scala.Some(resolveData(p("datum"))))
        case other              => throw RuntimeException(s"Unknown patch op: $other")

    private def datumOf(p: ujson.Value): OutputDatum =
        p.obj
            .get("datum")
            .map(d => OutputDatum.OutputDatum(resolveData(d)))
            .getOrElse(OutputDatum.NoOutputDatum)

    private def parseValue(v: ujson.Value): Value = {
        val base = Value.lovelace(BigInt(v("lovelace").num.toLong))
        v.obj.get("assets").map(_.arr.toSeq).getOrElse(scala.Seq.empty).foldLeft(base) { (acc, a) =>
            acc + Value(
              resolveBytes(a("currency_symbol")),
              resolveBytes(a("token_name")),
              BigInt(a("quantity").num.toLong)
            )
        }
    }
}

object CapeTestSuite {
    def fromString(s: String): CapeTestSuite = new CapeTestSuite(ujson.read(s))

    def load(resourcePath: String): CapeTestSuite = {
        val stream = getClass.getResourceAsStream(resourcePath)
        assert(stream != null, s"$resourcePath not found in test resources")
        new CapeTestSuite(ujson.read(stream))
    }
}

/** Applies a program to a case's inputs and asserts the expected outcome. Returns the execution
  * budget on success.
  */
object CapeHarness {
    import scalus.uplc.Program
    import scalus.uplc.eval.{PlutusVM, Result}
    import scalus.cardano.ledger.ExUnits

    def run(program: Program, c: CapeCase)(using PlutusVM): scala.Option[ExUnits] = {
        val applied = c.inputs.foldLeft(program) { (p, in) =>
            in match
                case CapeInput.Uplc(t) => p $ t
                case CapeInput.Dat(d)  => p $ d
                case CapeInput.Ctx(d)  => p $ d
        }
        // Evaluate at the Term level (not Program.evaluateDebug), which enforces CIP-117's
        // Unit-only return value for Plutus V3 scripts. Not all CAPE examples are spending
        // validators (e.g. factorial/fibonacci return their computed value); correctness is
        // already fully covered by the `expectedTerm` equality check below, which every "value"
        // fixture case (including validator successes, via an explicit `(con unit ())` content)
        // provides.
        //
        // Dropping the Program-level evaluation also removes the VM's CIP-117 non-Unit-result
        // gate for `expectError` cases below. This is inert for Scalus-compiled validators: they
        // are statically typed to return Unit, so a CEK success with a non-Unit result is
        // unreachable, and even hypothetically, such a non-Unit success would still fail the
        // harness loudly via "expected error but succeeded" rather than silently pass. Validator
        // fixtures (htlc/two_party_escrow/linear_vesting) assert success via
        // `expectedTerm == (con unit ())`, which is at least as strong as the dropped gate.
        val result = applied.term.evaluateDebug
        if c.expectError then
            assert(result.isFailure, s"${c.name}: expected error but succeeded")
            scala.None
        else
            result match
                case Result.Success(term, budget, _, _) =>
                    c.expectedTerm.foreach(exp =>
                        assert(term == exp, s"${c.name}: expected $exp, got $term")
                    )
                    scala.Some(budget)
                case Result.Failure(err, _, _, logs) =>
                    org.scalatest.Assertions.fail(
                      s"${c.name}: expected success, got $err; logs: ${logs.mkString(", ")}"
                    )
    }
}
