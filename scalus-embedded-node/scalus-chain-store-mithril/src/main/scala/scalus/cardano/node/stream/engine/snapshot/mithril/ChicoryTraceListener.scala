package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.runtime.{ExecutionListener, MStack}
import com.dylibso.chicory.wasm.types.{Instruction, OpCode}

/** Chicory `ExecutionListener` that watches for table-related opcodes and logs the table index +
  * the operand that would trap, plus a short ring-buffer of the most recent opcodes leading up to
  * the observation. Use ONLY for debugging — every WASM instruction goes through this hook, so
  * enabling it in hot loops tanks performance.
  *
  * The hook fires BEFORE the instruction executes; for `TABLE_GET` that means the requested index
  * is still on top of the operand stack. If the index is >= the target table's size the subsequent
  * `TABLE_GET` would trap; we log the pair (tableIdx, requestedIdx) so the caller can reason about
  * which table is the victim and where the index came from.
  *
  * The ring buffer keeps the `RingCapacity` most recent instructions with their PC (WASM byte
  * offset) so you can dump it on trap for post-mortem.
  */
final class ChicoryTraceListener(
    tableSizeProbe: Int => Option[Int],
    memoryProbe: Option[(Int, Int) => Array[Byte]] = None,
    /** Set of WASM function indices to log on CALL — lets us observe otherwise-invisible
      * Rust-internal calls to exports like `__externref_table_alloc`. Pair with [[onAllocResult]]
      * to capture the returned slot index once the call returns.
      */
    callTraceFnIndices: Set[Int] = Set.empty,
    /** Optional callback fired the instruction AFTER a call to a function in [[callTraceFnIndices]]
      * whose top-of-stack is the call's return value. Useful for intercepting
      * `__externref_table_alloc` returns to zero-init the slot host-side.
      */
    onAllocResult: Option[(Int, Int) => Unit] = None,
    ringCapacity: Int = 32
) extends ExecutionListener {

    import ChicoryTraceListener.Entry

    private val ring = Array.ofDim[Entry](ringCapacity)
    private var ringHead = 0
    private var ringSize = 0

    def recent: Seq[Entry] =
        (0 until ringSize).map { offset =>
            val idx = (ringHead - 1 - offset + ringCapacity) % ringCapacity
            ring(idx)
        }

    // One-instruction deferred callback: when a CALL to a traced function fires, we record
    // the pre-call stack depth; on the FOLLOWING instruction the return value sits on top
    // of the stack and we can hand it to onAllocResult before moving on.
    private var pendingAllocCapture: Int = -1 // stackDepth the call was made at, or -1

    def onExecution(instr: Instruction, stack: MStack): Unit = {
        val op = instr.opcode()
        val entry = Entry(instr.address(), op, stack.size())

        // Fulfil any pending alloc-capture from the previous CALL before we overwrite ring.
        if pendingAllocCapture >= 0 && stack.size() == pendingAllocCapture + 1 then {
            val slot = stack.peek().toInt
            onAllocResult.foreach(_(instr.address(), slot))
            pendingAllocCapture = -1
        } else if pendingAllocCapture >= 0 then {
            // Unexpected stack depth — the call didn't return one value; bail.
            pendingAllocCapture = -1
        }

        ring(ringHead) = entry
        ringHead = (ringHead + 1) % ringCapacity
        if ringSize < ringCapacity then ringSize += 1

        op match
            case OpCode.TABLE_GET =>
                // Stack: ...index (top)
                val tableIdx = instr.operand(0).toInt
                val requestedIdx = if stack.size() > 0 then stack.peek() else Long.MinValue
                checkBounds(op, tableIdx, requestedIdx, instr.address())
            case OpCode.TABLE_SET =>
                // Stack: ...index, value (value on top)
                val tableIdx = instr.operand(0).toInt
                val requestedIdx =
                    if stack.size() > 1 then stack.array()(stack.size() - 2)
                    else Long.MinValue
                val value = if stack.size() > 0 then stack.peek() else Long.MinValue
                val size = tableSizeProbe(tableIdx).getOrElse(-1)
                if size >= 0 && (requestedIdx < 0 || requestedIdx >= size) then
                    ChicoryTraceListener.logger.error(
                      s"[trap-imminent] TABLE_SET tableIdx=$tableIdx requestedIdx=$requestedIdx value=$value size=$size pc=0x${instr
                              .address()
                              .toHexString}"
                    )
                    dumpRing()
            case OpCode.CALL_INDIRECT =>
                val tableIdx = instr.operand(1).toInt
                val requestedIdx = if stack.size() > 0 then stack.peek() else Long.MinValue
                checkBounds(op, tableIdx, requestedIdx, instr.address())
            case OpCode.CALL =>
                val targetFn = instr.operand(0).toInt
                if callTraceFnIndices.contains(targetFn) then {
                    ChicoryTraceListener.logger.info(
                      s"[call-trace] CALL fn=$targetFn at pc=0x${instr.address().toHexString} preStack=${stack.size()}"
                    )
                    pendingAllocCapture = stack.size()
                }
            case _ => ()
    }

    private def checkBounds(op: OpCode, tableIdx: Int, requestedIdx: Long, pc: Int): Unit = {
        val size = tableSizeProbe(tableIdx).getOrElse(-1)
        if size >= 0 && (requestedIdx < 0 || requestedIdx >= size) then
            ChicoryTraceListener.logger.error(
              s"[trap-imminent] $op tableIdx=$tableIdx requestedIdx=$requestedIdx size=$size pc=0x${pc.toHexString}"
            )
            dumpRing()
            // If the requested index is in WASM-memory-pointer range and we have a memory
            // probe, dump 32 bytes around it — the index may be a heap address Rust is
            // mistaking for a slot handle.
            memoryProbe.foreach { probe =>
                if requestedIdx > 0 && requestedIdx < (1 << 30) then {
                    val addr = math.max(0L, requestedIdx - 16).toInt
                    val bytes = probe(addr, 48)
                    val hex = bytes.map(b => f"${b & 0xff}%02x").mkString(" ")
                    ChicoryTraceListener.logger.error(
                      s"[trap-imminent] memory[0x${addr.toHexString}..+48]: $hex"
                    )
                }
            }
    }

    private def dumpRing(): Unit = {
        ChicoryTraceListener.logger.error(s"[trap-imminent] recent ops (newest first):")
        recent.take(16).foreach { e =>
            ChicoryTraceListener.logger.error(
              f"  pc=0x${e.address}%08x op=${e.opcode}%-30s stackSize=${e.stackSize}"
            )
        }
    }
}

object ChicoryTraceListener {

    final case class Entry(address: Int, opcode: OpCode, stackSize: Int)

    private val logger: scribe.Logger =
        scribe.Logger("scalus.cardano.node.stream.engine.snapshot.mithril.ChicoryTraceListener")
}
