package scalus.uplc

import scalus.cardano.ledger.ExUnits

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import scala.sys.process.*

/** Represents the result of evaluating a UPLC program using `uplc` CLI */
enum UplcEvalResult:
    /** Represents a successful evaluation of a UPLC program Contains the evaluated term and the
      * execution budget used for evaluation
      */
    case Success(term: Term, budget: ExUnits)

    /** Represents a failure in evaluating a UPLC program
      *
      * @param errorCode
      *   the error code returned by the `uplc` CLI
      * @param error
      *   the error message returned by the `uplc` CLI
      */
    case UplcFailure(errorCode: Int, error: String)

    /** Represents a failure in parsing the evaluated term Normally this should not happen and
      * indicates a bug in the UPLC parser or changes in the output format of `uplc`
      */
    case TermParsingError(error: String)

/** Cardano `uplc` CLI interface */
object UplcCli:
    private val budget = raw"""(?ims)\s*CPU budget:\s+(\d+)\R\s*Memory budget:\s+(\d+).*""".r

    /** Evaluates a UPLC program using the Cardano `uplc` CLI using the builtin semantics variant
      * 'B'.
      *
      * @param program
      *   the UPLC program
      */
    def evalFlat(program: Program): UplcEvalResult =
        evalFlat(program, BuiltinSemanticsVariant.C)

    /** Evaluates a UPLC program using the Cardano `uplc` CLI
      *
      * @param program
      *   the UPLC program
      */
    def evalFlat(program: Program, semanticsVariant: BuiltinSemanticsVariant): UplcEvalResult =
        import cats.implicits.toShow
        val flat = program.flatEncoded

        // val cmdPrint =
        //    s"uplc print"
        // val printOut = cmdPrint.#<(new ByteArrayInputStream(flat)).!!.trim
        // println(s"UPLC program:\n$printOut")

        val cmd =
            s"uplc evaluate --input-format flat --counting --print-mode Classic --trace-mode LogsWithBudgets --builtin-semantics-variant $semanticsVariant"

        var out = ""
        var err = ""
        val retCode = cmd
            .#<(new ByteArrayInputStream(flat))
            .!(ProcessLogger(o => out += o + "\n", e => err += e + "\n"))
        if retCode == 0 then
            UplcParser().term.parse(out) match
                case Right(budget(cpu, mem), term) =>
                    UplcEvalResult.Success(term, ExUnits(mem.toLong, cpu.toLong))
                case Right(left, term) =>
                    UplcEvalResult.Success(term, ExUnits(-1, -1))
                case Left(err) => UplcEvalResult.TermParsingError(err.show)
        else UplcEvalResult.UplcFailure(retCode, out)

    /** Converts a UPLC program to `flat` encoding using the Cardano `uplc` CLI
      *
      * @param program
      *   the textual representation of the UPLC program
      * @return
      *   byte array of the flat encoded program
      */
    def uplcToFlat(program: String): Array[Byte] =
        val cmd = "uplc convert --of flat"
        val outStream = new ByteArrayOutputStream()
        cmd.#<(new ByteArrayInputStream(program.getBytes("UTF-8"))).#>(outStream).!
        outStream.toByteArray
