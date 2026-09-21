package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.Term.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{Log, NoBudgetSpender, PlutusVM}
import scalus.uplc.jit.hybrid.HybridJIT
import scalus.uplc.jit.nativestack.JIT
import scalus.uplc.{Constant, DeBruijn, DefaultFun, NamedDeBruijn, Term}

import scala.util.Try

/** Case on Data in the JIT has to agree with the CEK machine: only Data.Constr is scrutinized, the
  * branch is selected by the constructor tag and receives the list of fields.
  */
class JITCaseOnDataTest extends AnyFunSuiteLike {
    private val vm = PlutusVM.makePlutusV3VM(MajorProtocolVersion.dijkstraPV)

    private val jits = List(JIT, mincont.JIT, HybridJIT)

    private def runJit(jit: JitRunner, term: Term): Try[Any] =
        Try(jit.jitUplc(term)(Log(), NoBudgetSpender, vm.machineParams))

    private def constr(tag: Int, fields: Data*): Term =
        Const(Constant.Data(Data.Constr(BigInt(tag), PList.from(fields.toList))))

    // λfields. unIData (headList fields)
    private val headAsInteger: Term = LamAbs(
      "fields",
      Apply(
        Builtin(DefaultFun.UnIData),
        Apply(Force(Builtin(DefaultFun.HeadList)), Var(NamedDeBruijn("fields", 0)))
      )
    )

    private def constant(i: Int): Term = LamAbs("fields", Const(Constant.Integer(i)))

    for jit <- jits do {
        val name = jit.getClass.getName

        test(s"Case on Data.Constr selects the branch by the tag and passes the fields: $name") {
            val term = Case(constr(1, Data.I(42), Data.I(7)), List(constant(-1), headAsInteger))
            assert(
              vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term)) == Const(Constant.Integer(42))
            )
            assert(runJit(jit, term).get == BigInt(42))
        }

        test(s"Case on Data.Constr with a tag without a branch fails: $name") {
            val term = Case(constr(2), List(constant(0), constant(1)))
            assert(runJit(jit, term).isFailure)
        }

        test(s"Case on a non-Constr Data fails: $name") {
            for d <- List(Data.I(1), Data.B(ByteString.fromHex("FF")), Data.List(PList.Nil)) do
                val term = Case(Const(Constant.Data(d)), List(constant(0)))
                assert(runJit(jit, term).isFailure, s"for $d")
        }
    }
}
