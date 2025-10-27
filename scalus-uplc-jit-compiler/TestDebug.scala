package scalus.uplc.eval

import scalus.uplc.{Constant, DefaultFun, Term, NamedDeBruijn}
import scalus.uplc.eval.defunc.{JITCompiler, JIT}

object TestDebugLambda extends App {
  println("\n=== Testing: (\\x -> (AddInteger x) 1) 5 ===\n")
  
  val term = Term.Apply(
    Term.LamAbs(
      "x",
      Term.Apply(
        Term.Apply(
          Term.Builtin(DefaultFun.AddInteger),
          Term.Var(NamedDeBruijn("x", 0))
        ),
        Term.Const(Constant.Integer(1))
      )
    ),
    Term.Const(Constant.Integer(5))
  )

  val prog = JITCompiler.compile(term)
  println(s"=== Compiled ${prog.instructions.length} instructions ===")
  prog.instructions.zipWithIndex.foreach { case (instr, i) =>
    val desc = instr.opcode match {
      case 0 => "RETURN"
      case 1 => s"APPLY(fun=${instr.data.asInstanceOf[(Int,Int)]._1}, arg=${instr.data.asInstanceOf[(Int,Int)]._2})"
      case 2 => "FORCE"
      case 3 => "EXEC_SNIPPET"
      case 4 => s"LAMBDA(body=${instr.data})"
      case _ => s"UNKNOWN(${instr.opcode})"
    }
    println(f"[$i%2d] $desc")
  }

  val myLogger = new Logger {
    def log(msg: String): Unit = println(s"  [LOG] $msg")
    def getLogs: Array[String] = Array.empty
  }

  println("\n=== Executing ===")
  val res = JIT.eval(prog, NoBudgetSpender, myLogger, MachineParams.defaultPlutusV2PostConwayParams)
  println(s"\n=== Result: $res (${res.getClass.getName}) ===")
}
