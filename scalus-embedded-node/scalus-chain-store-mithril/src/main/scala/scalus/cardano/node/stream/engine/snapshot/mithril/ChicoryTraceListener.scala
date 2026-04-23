package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.runtime.{ExecutionListener, MStack}
import com.dylibso.chicory.wasm.types.{Instruction, OpCode}

/** Chicory `ExecutionListener` that watches for table-related opcodes and logs the table
  * index + the operand that would trap, plus a short ring-buffer of the most recent opcodes
  * leading up to the observation. Use ONLY for debugging — every WASM instruction goes
  * through this hook, so enabling it in hot loops tanks performance.
  *
  * The hook fires BEFORE the instruction executes; for `TABLE_GET` that means the requested
  * index is still on top of the operand stack. If the index is >= the target table's size
  * the subsequent `TABLE_GET` would trap; we log the pair (tableIdx, requestedIdx) so the
  * caller can reason about which table is the victim and where the index came from.
  *
  * The ring buffer keeps the `RingCapacity` most recent instructions with their PC (WASM
  * byte offset) so you can dump it on trap for post-mortem.
  */
final class ChicoryTraceListener(
    tableSizeProbe: Int => Option[Int],
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

    def onExecution(instr: Instruction, stack: MStack): Unit = {
        val op = instr.opcode()
        val entry = Entry(instr.address(), op, stack.size())
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
            case _ => ()
    }

    private def checkBounds(op: OpCode, tableIdx: Int, requestedIdx: Long, pc: Int): Unit = {
        val size = tableSizeProbe(tableIdx).getOrElse(-1)
        if size >= 0 && (requestedIdx < 0 || requestedIdx >= size) then
            ChicoryTraceListener.logger.error(
              s"[trap-imminent] $op tableIdx=$tableIdx requestedIdx=$requestedIdx size=$size pc=0x${pc.toHexString}"
            )
            dumpRing()
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
