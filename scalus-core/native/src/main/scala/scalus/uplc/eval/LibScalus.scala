package scalus.uplc
package eval

import scalus.uplc.builtin.Data
import scalus.cardano.ledger.{Language, MajorProtocolVersion, ProtocolParams}
import scalus.uplc

import java.nio.charset.Charset
import scala.scalanative.runtime.ffi
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** Scala Native bindings for the UPLC evaluation library.
  */
private object LibScalus:
    /** We must keep track of all the objects that are passed to the native code. This is necessary
      * because the native code may store references to these objects and the garbage collector must
      * not collect them while they are still in use.
      */
    private object GCRoots {
        private val references = new java.util.IdentityHashMap[Object, Unit]

        def addRoot(o: Object): Unit = references.put(o, ())

        @exported(name = "scalus_free")
        def removeRoot(o: Object): Unit = references.remove(o)

        def hasRoot(o: Object): Boolean = references.containsKey(o)

        override def toString: String = references.toString
    }

    extension [A <: AnyRef](obj: A)
        /** Adds a reference to the object to the GC roots.
          * @return
          *   The object itself
          */
        def gc: A = { GCRoots.addRoot(obj); obj }

    extension (str: String)
        def toCString(buf: CString, len: CSize): CSize =
            toCString(Charset.defaultCharset(), buf, len)

        def toCString(charset: Charset, cstr: CString, size: CSize): CSize = {
            if cstr == null || size == 0 then return 0.toCSize
            if str == null then
                cstr(0) = 0.toByte
                0.toCSize
            else
                val bytes = str.getBytes(charset)
                if bytes.nonEmpty then
                    val len = Math.min(bytes.length + 1, size.toInt) - 1
                    ffi.memcpy(cstr, bytes.atUnsafe(0), len.toCSize)
                    cstr(len + 1) = 0.toByte
                    size
                else
                    cstr(0) = 0.toByte
                    0.toCSize
        }

    extension (bytes: CString)
        def toArray(size: CSize): Array[Byte] = {
            val array = new Array[Byte](size.toInt)
            ffi.memcpy(array.atUnsafe(0), bytes, size)
            array
        }

    /* struct ex_budget {
     *     int64_t cpu;
     *     int64_t memory;
     * };
     * */
    type ExBudget = CStruct2[CLongLong, CLongLong]

    @exported(name = "scalus_get_machine_params_from_cardano_cli_protocol_params_json")
    def fromCardanoCliProtocolParamsJson(json: CString, plutusVersion: CInt): MachineParams = {
        val jsonStr = fromCString(json)
        try
            val pll = Language.fromOrdinal(plutusVersion - 1)
            val pparams = ProtocolParams.fromCardanoCliJson(jsonStr)
            MachineParams.fromProtocolParams(pparams, pll).gc
        catch case _: Exception => null
    }

    @exported(name = "scalus_get_default_machine_params")
    def defaultMachineParams(plutusVersion: CInt, protocolVersion: CInt): MachineParams = {
        try
            val pll = Language.fromOrdinal(plutusVersion - 1)
            MachineParams.defaultParamsFor(pll, MajorProtocolVersion(protocolVersion)).gc
        catch
            case e: Exception =>
                null
    }

    @exported(name = "scalus_data_from_cbor")
    def dataFromCbor(cbor: CString, size: CSize): Data = {
        try
            val bytes = cbor.toArray(size)
            Data.fromCbor(bytes).gc
        catch
            case e: Exception =>
                null
    }

    @exported(name = "scalus_data_from_json")
    def dataFromJson(json: CString): Data = {
        try Data.fromJson(fromCString(json)).gc
        catch
            case e: Exception =>
                null
    }

    @exported(name = "scalus_script_apply_data_arg")
    def applyDataArgs(scriptHex: CString, result: CString, len: CSize, arg: Data): CInt = {
        try
            val program = DeBruijnedProgram.fromDoubleCborHex(fromCString(scriptHex))
            val applied = program $ Term.Const(Constant.Data(arg))
            applied.doubleCborHex.toCString(result, len)
            0
        catch
            case e: Exception =>
                e.getMessage.toCString(result, len)
                1
    }

    @exported(name = "scalus_evaluate_script")
    def evaluateScript(
        scriptHex: CString,
        plutusVersion: CInt,
        machineParams: MachineParams,
        result: Ptr[ExBudget],
        logsBuffer: CString,
        logsLen: CSize,
        errorBuffer: CString,
        errorLen: CSize
    ): CInt = {
        try
            assert(machineParams != null, "Machine parameters must not be null")
            // Parse script from hex
            val program = DeBruijnedProgram.fromDoubleCborHex(fromCString(scriptHex))

            // Create appropriate VM based on version
            val vm = plutusVersion match
                case 1 => PlutusVM.makePlutusV1VM(machineParams)
                case 2 => PlutusVM.makePlutusV2VM(machineParams)
                case 3 => PlutusVM.makePlutusV3VM(machineParams)
                case _ =>
                    throw new IllegalArgumentException(
                      s"Unsupported Plutus version: $plutusVersion"
                    )

            // Evaluate script
            vm.evaluateScriptDebug(program) match
                case Result.Success(term, budget, costs, logs) =>
                    result._1 = budget.steps
                    result._2 = budget.memory
                    logs.mkString("\n").toCString(logsBuffer, logsLen)
                    0
                case Result.Failure(exception, budget, costs, logs) =>
                    result._1 = budget.steps
                    result._2 = budget.memory
                    logs.mkString("\n").toCString(logsBuffer, logsLen)
                    exception.getMessage.toCString(errorBuffer, errorLen)
                    1
        catch
            case exception: Exception =>
                exception.getMessage.toCString(errorBuffer, errorLen)
                2
    }
