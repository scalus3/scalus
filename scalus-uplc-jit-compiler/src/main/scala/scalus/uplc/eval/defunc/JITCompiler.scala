package scalus.uplc.eval.defunc

import scalus.builtin.{BuiltinPair, Builtins, ByteString, Data}
import scalus.uplc.{Constant, DefaultFun, DefaultUni, NamedDeBruijn, Term}
import scalus.uplc.eval.{BudgetSpender, ExBudgetCategory, Logger, MachineParams, MemoryUsageJit, RuntimeHelper, StepKind}
import scalus.uplc.eval.defunc.{Closure, CompiledProgram, Instruction, Snippet}
import scalus.uplc.eval.defunc.JIT.{*, given}
import scala.annotation.switch
import scala.quoted.*
import scala.collection.mutable

/** Compiler for hybrid JIT: converts UPLC terms to defunctionalized instructions + JIT snippets. */
object JITCompiler {

    private given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    private given ByteStringToExpr: ToExpr[ByteString] with {
        def apply(x: ByteString)(using Quotes): Expr[ByteString] =
            '{ ByteString.fromArray(${ Expr(x.bytes) }) }
    }

    private given DataToExpr: ToExpr[Data] with {
        def apply(x: Data)(using Quotes): Expr[Data] = x match {
            case Data.Constr(tag, args) =>
                '{ Data.Constr(${ Expr(tag) }, ${ Expr.ofList(args.map(apply)) }) }
            case Data.List(value) =>
                '{ Data.List(${ Expr.ofList(value.map(apply)) }) }
            case Data.Map(values) =>
                val argsExpr = Expr.ofList(values.map { case (k, v) =>
                    Expr.ofTuple(apply(k), apply(v))
                })
                '{ Data.Map($argsExpr) }
            case Data.I(value) => '{ Data.I(${ Expr(value) }) }
            case Data.B(value) => '{ Data.B(${ Expr(value) }) }
        }
    }

    /** Compilation context - builds instructions and tracks state. */
    private class CompileContext {
        private val instructions = mutable.ArrayBuffer[Instruction]()
        private var nextInstrIdx = 0

        /** Add an instruction and return its index. */
        def emit(instr: Instruction): Int = {
            val idx = nextInstrIdx
            instructions += instr
            nextInstrIdx += 1
            idx
        }

        /** Update an instruction's data field (for back-patching). */
        def updateInstruction(idx: Int, bodyIdx: Int): Unit = {
            instructions(idx) = instructions(idx).copy(data = bodyIdx)
        }

        /** Get all emitted instructions. */
        def getInstructions: Array[Instruction] = instructions.toArray
    }

    /** Compile a UPLC term to a CompiledProgram. */
    def compile(term: Term): CompiledProgram = {
        // Build instructions at runtime
        val ctx = new CompileContext()

        // Compile the term
        val entryIdx = compileTerm(term, Nil)(using ctx)

        // Emit final RETURN
        ctx.emit(Instruction(opcode = JIT.OP_RETURN))

        val instructions = ctx.getInstructions

        CompiledProgram(
          instructions = instructions,
          entryPoint = entryIdx
        )
    }

    /** Compile a term to instructions, returning the entry instruction index.
      *
      * @param term
      *   The term to compile
      * @param env
      *   Environment mapping variable names to their stack locations
      * @return
      *   Index of the first instruction for this term
      */
    private def compileTerm(
        term: Term,
        env: List[(String, Int)]
    )(using CompileContext): Int = {
        term match {
            case Term.Const(const) =>
                // Constants: Create JIT snippet that returns the constant
                val snippet = compileConstant(const)
                val idx = summon[CompileContext].emit(
                  Instruction(
                    opcode = JIT.OP_EXEC_SNIPPET,
                    snippet = snippet
                  )
                )
                // Emit RETURN after snippet so control returns to caller
                summon[CompileContext].emit(Instruction(opcode = JIT.OP_RETURN))
                idx

            case Term.Var(name) =>
                // Variables: Look up from environment (data stack)
                // The environment is maintained on the dataStack
                // DeBruijn index tells us how far back to look
                val index = name.index
                val snippet = staging.run { (quotes: Quotes) ?=>
                    val idx = Expr(index)
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                // Look up variable from data stack using DeBruijn index
                                // index 0 = top of stack, 1 = second from top, etc.
                                dataStack.peek($idx)
                            }
                        }
                    }
                }
                summon[CompileContext].emit(
                  Instruction(opcode = JIT.OP_EXEC_SNIPPET, snippet = snippet)
                )

            case Term.LamAbs(name, body) =>
                // Lambda: Create a closure using OP_LAMBDA instruction
                // The body will be compiled and stored after this instruction
                
                // Emit OP_LAMBDA with placeholder for body index
                val lambdaIdx = summon[CompileContext].emit(
                  Instruction(opcode = JIT.OP_LAMBDA, data = null) // Will be filled with body index
                )
                
                // Emit RETURN after lambda so we don't fall through to the body
                summon[CompileContext].emit(Instruction(opcode = JIT.OP_RETURN))
                
                // Now compile the body - compileTerm returns the index of the body's first instruction
                val bodyIdx = compileTerm(body, (name -> 0) :: env.map { case (n, i) => (n, i + 1) })
                
                // Body already emits its own RETURN, no need for another
                
                // Update the lambda instruction with the body index
                summon[CompileContext].updateInstruction(lambdaIdx, bodyIdx)
                
                lambdaIdx

            case Term.Apply(fun, arg) =>
                // Apply: Use defunctionalized control flow
                val funIdx = compileTerm(fun, env)
                val argIdx = compileTerm(arg, env)

                val applyIdx = summon[CompileContext].emit(
                  Instruction(
                    opcode = JIT.OP_APPLY,
                    data = (funIdx, argIdx)
                  )
                )
                // Emit RETURN after Apply
                summon[CompileContext].emit(Instruction(opcode = JIT.OP_RETURN))
                applyIdx

            case Term.Force(body) =>
                // Force: Use defunctionalized control flow
                val bodyIdx = compileTerm(body, env)

                summon[CompileContext].emit(
                  Instruction(
                    opcode = JIT.OP_FORCE,
                    data = bodyIdx
                  )
                )

            case Term.Delay(body) =>
                // Delay: Create snippet that returns a thunk
                val bodyIdx = compileTerm(body, env)
                val snippet = staging.run { (quotes: Quotes) ?=>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = { () =>
                                {
                                    // TODO: Execute body
                                    throw new UnsupportedOperationException(
                                      "Delay execution not yet implemented"
                                    )
                                }
                            }
                        }
                    }
                }
                summon[CompileContext].emit(
                  Instruction(opcode = JIT.OP_EXEC_SNIPPET, snippet = snippet)
                )

            case Term.Builtin(bi) =>
                // Builtins: Create JIT snippets for direct execution
                val snippet = compileBuiltin(bi)
                val idx = summon[CompileContext].emit(
                  Instruction(opcode = JIT.OP_EXEC_SNIPPET, snippet = snippet)
                )
                // Emit RETURN after snippet
                summon[CompileContext].emit(Instruction(opcode = JIT.OP_RETURN))
                idx

            case Term.Error =>
                val snippet = staging.run { (quotes: Quotes) ?=>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                throw new RuntimeException("UPLC Error term evaluated")
                            }
                        }
                    }
                }
                summon[CompileContext].emit(
                  Instruction(opcode = JIT.OP_EXEC_SNIPPET, snippet = snippet)
                )

            case Term.Constr(tag, args) =>
                // Constructor: Create snippet
                val argIdxs = args.map(a => compileTerm(a, env))
                val snippet = staging.run { (quotes: Quotes) ?=>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                // TODO: Evaluate args and create tuple
                                throw new UnsupportedOperationException(
                                  "Constr not yet implemented"
                                )
                            }
                        }
                    }
                }
                summon[CompileContext].emit(
                  Instruction(opcode = JIT.OP_EXEC_SNIPPET, snippet = snippet)
                )

            case Term.Case(arg, cases) =>
                // Case: Create snippet
                val argIdx = compileTerm(arg, env)
                val caseIdxs = cases.map(c => compileTerm(c, env))
                val snippet = staging.run { (quotes: Quotes) ?=>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                // TODO: Evaluate scrutinee and select case
                                throw new UnsupportedOperationException("Case not yet implemented")
                            }
                        }
                    }
                }
                summon[CompileContext].emit(
                  Instruction(opcode = JIT.OP_EXEC_SNIPPET, snippet = snippet)
                )
        }
    }

    /** Compile a constant to a JIT snippet. */
    private def compileConstant(const: Constant): Snippet = {
        staging.run { (quotes: Quotes) ?=>
            const match {
                case Constant.Integer(value) =>
                    val v = Expr(value)
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                budget.spendBudget(
                                  ExBudgetCategory.Step(StepKind.Const),
                                  params.machineCosts.constCost,
                                  Nil
                                )
                                $v
                            }
                        }
                    }

                case Constant.ByteString(value) =>
                    val v = Expr(value)
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                budget.spendBudget(
                                  ExBudgetCategory.Step(StepKind.Const),
                                  params.machineCosts.constCost,
                                  Nil
                                )
                                $v
                            }
                        }
                    }

                case Constant.String(value) =>
                    val v = Expr(value)
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                budget.spendBudget(
                                  ExBudgetCategory.Step(StepKind.Const),
                                  params.machineCosts.constCost,
                                  Nil
                                )
                                $v
                            }
                        }
                    }

                case Constant.Bool(value) =>
                    val v = Expr(value)
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                budget.spendBudget(
                                  ExBudgetCategory.Step(StepKind.Const),
                                  params.machineCosts.constCost,
                                  Nil
                                )
                                $v
                            }
                        }
                    }

                case Constant.Unit =>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                budget.spendBudget(
                                  ExBudgetCategory.Step(StepKind.Const),
                                  params.machineCosts.constCost,
                                  Nil
                                )
                                ()
                            }
                        }
                    }

                case Constant.Data(value) =>
                    val v = Expr(value)
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                budget.spendBudget(
                                  ExBudgetCategory.Step(StepKind.Const),
                                  params.machineCosts.constCost,
                                  Nil
                                )
                                $v
                            }
                        }
                    }

                case _ =>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                throw new UnsupportedOperationException(
                                  s"Constant type not yet implemented: ${${ Expr(const.toString) }}"
                                )
                            }
                        }
                    }
            }
        }
    }

    /** Compile a builtin to a JIT snippet (direct bytecode execution). */
    private def compileBuiltin(bi: DefaultFun): Snippet = {
        staging.run { (quotes: Quotes) ?=>
            bi match {
                case DefaultFun.AddInteger =>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                // Return curried function
                                (x: Any) => (y: Any) =>
                                    {
                                        val xv = x.asInstanceOf[BigInt]
                                        val yv = y.asInstanceOf[BigInt]
                                        budget.spendBudget(
                                          ExBudgetCategory.Step(StepKind.Builtin),
                                          params.builtinCostModel.addInteger
                                              .calculateCostFromMemory(
                                                Seq(
                                                  MemoryUsageJit.memoryUsage(xv),
                                                  MemoryUsageJit.memoryUsage(yv)
                                                )
                                              ),
                                          Nil
                                        )
                                        xv + yv
                                    }
                            }
                        }
                    }

                case DefaultFun.SubtractInteger =>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = { (x: Any) => (y: Any) =>
                                {
                                    val xv = x.asInstanceOf[BigInt]
                                    val yv = y.asInstanceOf[BigInt]
                                    budget.spendBudget(
                                      ExBudgetCategory.Step(StepKind.Builtin),
                                      params.builtinCostModel.subtractInteger
                                          .calculateCostFromMemory(
                                            Seq(
                                              MemoryUsageJit.memoryUsage(xv),
                                              MemoryUsageJit.memoryUsage(yv)
                                            )
                                          ),
                                      Nil
                                    )
                                    xv - yv
                                }
                            }
                        }
                    }

                case DefaultFun.MultiplyInteger =>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = { (x: Any) => (y: Any) =>
                                {
                                    val xv = x.asInstanceOf[BigInt]
                                    val yv = y.asInstanceOf[BigInt]
                                    budget.spendBudget(
                                      ExBudgetCategory.Step(StepKind.Builtin),
                                      params.builtinCostModel.multiplyInteger
                                          .calculateCostFromMemory(
                                            Seq(
                                              MemoryUsageJit.memoryUsage(xv),
                                              MemoryUsageJit.memoryUsage(yv)
                                            )
                                          ),
                                      Nil
                                    )
                                    xv * yv
                                }
                            }
                        }
                    }

                case DefaultFun.EqualsData =>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = { (x: Any) => (y: Any) =>
                                {
                                    val xv = x.asInstanceOf[Data]
                                    val yv = y.asInstanceOf[Data]
                                    budget.spendBudget(
                                      ExBudgetCategory.Step(StepKind.Builtin),
                                      params.builtinCostModel.equalsData.calculateCostFromMemory(
                                        Seq(
                                          MemoryUsageJit.memoryUsage(xv),
                                          MemoryUsageJit.memoryUsage(yv)
                                        )
                                      ),
                                      Nil
                                    )
                                    Builtins.equalsData(xv, yv)
                                }
                            }
                        }
                    }

                case DefaultFun.IfThenElse =>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = { () => (c: Any) => (t: Any) => (f: Any) =>
                                {
                                    val cv = c.asInstanceOf[Boolean]
                                    budget.spendBudget(
                                      ExBudgetCategory.Step(StepKind.Builtin),
                                      params.builtinCostModel.ifThenElse.constantCost,
                                      Nil
                                    )
                                    if cv then t else f
                                }
                            }
                        }
                    }

                case _ =>
                    '{
                        new Snippet {
                            def execute(
                                acc: Any,
                                dataStack: DataStack,
                                budget: BudgetSpender,
                                logger: Logger,
                                params: MachineParams
                            ): Any = {
                                throw new UnsupportedOperationException(
                                  s"Builtin not yet implemented: ${${ Expr(bi.toString) }}"
                                )
                            }
                        }
                    }
            }
        }
    }
}
