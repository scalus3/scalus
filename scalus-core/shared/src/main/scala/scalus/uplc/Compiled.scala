package scalus.uplc

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Credential, Language, PlutusScript, Script}
import scalus.compiler
import scalus.compiler.sir.lowering.SirToUplcV3Lowering
import scalus.compiler.sir.lowering.simple.{ScottEncodingLowering, SumOfProductsLowering}
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRType, TargetLoweringBackend}
import scalus.compiler.{compileInlineWithOptions, Options}
import scalus.uplc.Constant.asConstant
import scalus.uplc.transform.*

import scala.annotation.threadUnsafe

/** A compiled Plutus script.
  *
  * This trait represents a script that has been compiled from Scala source code through the Scalus
  * compiler pipeline (Scala -> SIR -> UPLC -> Plutus Script). It contains original Scala code, its
  * type, [[scalus.compiler.sir.SIR]], compiler options (backend etc), and generated UPLC program
  * and serialized Plutus script.
  *
  * @tparam A
  *   the Scala type of the compiled code, typically a function type representing the validator
  *   signature
  */
trait Compiled[A] {

    /** The original Scala code that was compiled. Evaluating this executes the Scala version. */
    def code: A

    /** The Plutus language version (V1, V2, V3, etc.). */
    def language: Language

    /** The Scalus Intermediate Representation of the compiled code. */
    def sir: SIR

    /** The compiler options used during compilation. */
    def options: Options

    /** The compiled UPLC program ready for on-chain execution. */
    def program: Program

    /** The Plutus script in its serialized form for deployment. */
    def script: PlutusScript

    /** Derives a Cardano address for this script on the specified network.
      *
      * @param network
      *   the Cardano network (Mainnet or Testnet)
      * @return
      *   the script address
      */
    def address(network: Network): Address = Address(
      network,
      Credential.ScriptHash(script.scriptHash)
    )
}

/** Base implementation for compiled Plutus scripts of all versions.
  *
  * This sealed abstract class provides the common implementation for compiling Scala code to Plutus
  * scripts across all Plutus language versions (V1, V2, V3). Subclasses implement version-specific
  * details such as program version numbers, script hash prefixes, and optimizer selection.
  *
  * @tparam A
  *   the Scala type of the compiled code
  * @param lazyCode
  *   a thunk that evaluates to the original Scala code
  * @param sir
  *   the Scalus Intermediate Representation
  * @param options
  *   compiler options controlling lowering and optimization
  * @param optimizer
  *   the UPLC optimizer to apply (version-specific)
  */
sealed abstract class CompiledPlutus[A](
    val lazyCode: () => A,
    val sir: SIR,
    val options: Options,
    val optimizer: Optimizer
) extends Compiled[A] {

    /** Evaluates and returns the original Scala code. */
    def code: A = lazyCode()

    /** The Plutus language version. */
    def language: Language

    /** The compiled UPLC program. Lazily computed on first access. */
    @threadUnsafe lazy val program: Program = makeProgram(toUplc)

    /** The Plutus script in its serialized form. Lazily computed on first access. */
    @threadUnsafe lazy val script: PlutusScript = makeScript(program)

    /** Creates the program with the appropriate version. */
    protected def makeProgram(term: Term): Program

    /** Creates the script with the appropriate version. */
    protected def makeScript(program: Program): PlutusScript

    /** Returns a copy of this compiled script with error traces enabled.
      *
      * Error traces provide detailed error messages during script evaluation, useful for debugging.
      * This increases script size and execution cost.
      *
      * @return
      *   a new [[CompiledPlutus]] with `generateErrorTraces = true`
      */
    def withErrorTraces: CompiledPlutus[A]

    /** Lowers the SIR to UPLC using the configured backend and applies optimization if enabled. */
    protected def toUplc: Term = {
        val backend = options.targetLoweringBackend
        val uplc = backend match
            case TargetLoweringBackend.ScottEncodingLowering =>
                ScottEncodingLowering(sir, options.generateErrorTraces).lower()
            case TargetLoweringBackend.SumOfProductsLowering =>
                SumOfProductsLowering(sir, options.generateErrorTraces).lower()
            case TargetLoweringBackend.SirToUplcV3Lowering =>
                SirToUplcV3Lowering(
                  sir,
                  generateErrorTraces = options.generateErrorTraces,
                  debug = options.debug,
                  targetLanguage = language
                ).lower()
        if options.optimizeUplc then optimizer(uplc) else uplc
    }
}

/** A compiled Plutus V1 script.
  *
  * Plutus V1 was introduced in the Alonzo hard fork. It provides basic smart contract functionality
  * with the original set of builtin functions.
  *
  * @example
  *   {{{
  *   given scalus.compiler.Options = scalus.compiler.Options.release
  *   val validator = PlutusV1.compile((datum: Data, redeemer: Data, ctx: Data) => ...)
  *   val scriptHash = validator.script.scriptHash
  *   }}}
  *
  * @tparam A
  *   the Scala type of the compiled code
  * @param lazyCode
  *   a thunk that evaluates to the original Scala code
  * @param sir
  *   the Scalus Intermediate Representation
  * @param options
  *   compiler options controlling lowering and optimization
  * @param optimizer
  *   the UPLC optimizer to apply (if optimization is enabled)
  */
final case class PlutusV1[A] private[uplc] (
    override val lazyCode: () => A,
    override val sir: SIR,
    override val options: Options,
    override val optimizer: Optimizer
) extends CompiledPlutus[A](lazyCode, sir, options, optimizer) {

    /** Returns [[scalus.cardano.ledger.Language.PlutusV1]]. */
    def language: Language = Language.PlutusV1

    /** Creates a Plutus V1 program with version (1, 0, 0). */
    protected def makeProgram(term: Term): Program = Program.plutusV1(term)

    /** Creates a Plutus V1 script. */
    protected def makeScript(program: Program): Script.PlutusV1 =
        Script.PlutusV1(program.cborByteString)

    /** The serialized Plutus V1 script. Lazily computed on first access. */
    override lazy val script: Script.PlutusV1 = makeScript(program)

    /** Returns a copy of this compiled script with error traces enabled.
      *
      * Error traces provide detailed error messages during script evaluation, useful for debugging.
      * This increases script size and execution cost.
      *
      * @return
      *   a new [[PlutusV1]] with `generateErrorTraces = true`
      */
    def withErrorTraces: PlutusV1[A] = copy(options = options.copy(generateErrorTraces = true))
}

/** Factory methods for creating compiled Plutus V1 scripts. */
object PlutusV1 {

    /** Compiles Scala code to a Plutus V1 script.
      *
      * The code is compiled at compile-time using the Scalus compiler plugin.
      *
      * @example
      *   {{{
      *   given scalus.compiler.Options = scalus.compiler.Options.release
      *   val validator = PlutusV1.compile { (datum: Data, redeemer: Data, ctx: Data) =>
      *     val d = datum.to[MyDatum]
      *     require(d.value > 0)
      *   }
      *   }}}
      *
      * @tparam A
      *   the type of the code being compiled
      * @param code
      *   the Scala code to compile (must be within the Scalus-supported subset)
      * @param opts
      *   compiler options controlling compilation, lowering, and optimization
      * @return
      *   a [[PlutusV1]] containing the compiled script
      */
    inline def compile[A](inline code: A)(using opts: Options): PlutusV1[A] = {
        val sir = compileInlineWithOptions(opts, code)
        PlutusV1(() => code, sir, opts, new V1V2Optimizer())
    }

    /** Extension methods for applying arguments to compiled Plutus V1 functions.
      *
      * Enables partial application of compiled validators at the UPLC level.
      *
      * @example
      *   {{{
      *   val parameterized = PlutusV1.compile((config: Config) => (datum: Data) => ...)
      *   val applied = parameterized(myConfig) // PlutusV1[Data => Unit]
      *   }}}
      */
    extension [A: Constant.LiftValue, B](self: PlutusV1[A => B]) {

        /** Applies an argument to a compiled function, producing a new compiled script.
          *
          * @param arg
          *   the argument to apply, must have a [[Constant.LiftValue]] instance
          * @return
          *   a new [[PlutusV1]] with the argument applied
          */
        def apply(arg: A): PlutusV1[B] = {
            val const = arg.asConstant
            PlutusV1[B](
              () => self.code(arg),
              self.sir $ SIR.Const(const, SIRType.fromDefaultUni(const.tpe), AnnotationsDecl.empty),
              self.options,
              self.optimizer
            )
        }
    }
}

/** A compiled Plutus V2 script.
  *
  * Plutus V2 was introduced in the Vasil hard fork. It adds reference inputs, inline datums,
  * reference scripts, and the `SerialiseData` builtin.
  *
  * @example
  *   {{{
  *   given scalus.compiler.Options = scalus.compiler.Options.release
  *   val validator = PlutusV2.compile((datum: Data, redeemer: Data, ctx: Data) => ...)
  *   val scriptHash = validator.script.scriptHash
  *   }}}
  *
  * @tparam A
  *   the Scala type of the compiled code
  * @param lazyCode
  *   a thunk that evaluates to the original Scala code
  * @param sir
  *   the Scalus Intermediate Representation
  * @param options
  *   compiler options controlling lowering and optimization
  * @param optimizer
  *   the UPLC optimizer to apply (if optimization is enabled)
  */
final case class PlutusV2[A] private[uplc] (
    override val lazyCode: () => A,
    override val sir: SIR,
    override val options: Options,
    override val optimizer: Optimizer
) extends CompiledPlutus[A](lazyCode, sir, options, optimizer) {

    /** Returns [[scalus.cardano.ledger.Language.PlutusV2]]. */
    def language: Language = Language.PlutusV2

    /** Creates a Plutus V2 program with version (1, 0, 0). */
    protected def makeProgram(term: Term): Program = Program.plutusV2(term)

    /** Creates a Plutus V2 script. */
    protected def makeScript(program: Program): Script.PlutusV2 =
        Script.PlutusV2(program.cborByteString)

    /** The serialized Plutus V2 script. Lazily computed on first access. */
    override lazy val script: Script.PlutusV2 = makeScript(program)

    /** Returns a copy of this compiled script with error traces enabled.
      *
      * Error traces provide detailed error messages during script evaluation, useful for debugging.
      * This increases script size and execution cost.
      *
      * @return
      *   a new [[PlutusV2]] with `generateErrorTraces = true`
      */
    def withErrorTraces: PlutusV2[A] = copy(options = options.copy(generateErrorTraces = true))
}

/** Factory methods for creating compiled Plutus V2 scripts. */
object PlutusV2 {

    /** Compiles Scala code to a Plutus V2 script.
      *
      * The code is compiled at compile-time using the Scalus compiler plugin.
      *
      * @example
      *   {{{
      *   given scalus.compiler.Options = scalus.compiler.Options.release
      *   val validator = PlutusV2.compile { (datum: Data, redeemer: Data, ctx: Data) =>
      *     val d = datum.to[MyDatum]
      *     require(d.value > 0)
      *   }
      *   }}}
      *
      * @tparam A
      *   the type of the code being compiled
      * @param code
      *   the Scala code to compile (must be within the Scalus-supported subset)
      * @param opts
      *   compiler options controlling compilation, lowering, and optimization
      * @return
      *   a [[PlutusV2]] containing the compiled script
      */
    inline def compile[A](inline code: A)(using opts: Options): PlutusV2[A] = {
        val sir = compileInlineWithOptions(opts, code)
        PlutusV2(() => code, sir, opts, new V1V2Optimizer())
    }

    /** Extension methods for applying arguments to compiled Plutus V2 functions.
      *
      * Enables partial application of compiled validators at the UPLC level.
      *
      * @example
      *   {{{
      *   val parameterized = PlutusV2.compile((config: Config) => (datum: Data) => ...)
      *   val applied = parameterized(myConfig) // PlutusV2[Data => Unit]
      *   }}}
      */
    extension [A: Constant.LiftValue, B](self: PlutusV2[A => B]) {

        /** Applies an argument to a compiled function, producing a new compiled script.
          *
          * @param arg
          *   the argument to apply, must have a [[Constant.LiftValue]] instance
          * @return
          *   a new [[PlutusV2]] with the argument applied
          */
        def apply(arg: A): PlutusV2[B] = {
            val const = arg.asConstant
            PlutusV2[B](
              () => self.code(arg),
              self.sir $ SIR.Const(const, SIRType.fromDefaultUni(const.tpe), AnnotationsDecl.empty),
              self.options,
              self.optimizer
            )
        }
    }
}

/** A compiled Plutus V3 script.
  *
  * This is the primary type for working with compiled Cardano smart contracts in Scalus. It holds
  * the intermediate representation (SIR), compilation options, and lazily computes the final UPLC
  * program and script.
  *
  * Plutus V3 was introduced in the Conway hard fork. It adds BLS12-381 elliptic curve operations,
  * bitwise primitives, and more efficient case/constr optimization.
  *
  * @example
  *   {{{
  *   given scalus.compiler.Options = scalus.compiler.Options.release
  *   val validator = PlutusV3.compile((datum: Data) => ...)
  *   val scriptHash = validator.script.scriptHash
  *   val address = validator.address(Network.Testnet)
  *   }}}
  *
  * @tparam A
  *   the Scala type of the compiled code
  * @param lazyCode
  *   a thunk that evaluates to the original Scala code
  * @param sir
  *   the Scalus Intermediate Representation
  * @param options
  *   compiler options controlling lowering and optimization
  * @param optimizer
  *   the UPLC optimizer to apply (if optimization is enabled)
  */
final case class PlutusV3[A] private[uplc] (
    override val lazyCode: () => A,
    override val sir: SIR,
    override val options: Options,
    override val optimizer: Optimizer
) extends CompiledPlutus[A](lazyCode, sir, options, optimizer) {

    /** Returns [[scalus.cardano.ledger.Language.PlutusV3]]. */
    def language: Language = Language.PlutusV3

    /** Creates a Plutus V3 program with version (1, 1, 0). */
    protected def makeProgram(term: Term): Program = Program.plutusV3(term)

    /** Creates a Plutus V3 script. */
    protected def makeScript(program: Program): Script.PlutusV3 =
        Script.PlutusV3(program.cborByteString)

    /** The serialized Plutus V3 script. Lazily computed on first access. */
    override lazy val script: Script.PlutusV3 = makeScript(program)

    /** Returns a copy of this compiled script with error traces enabled.
      *
      * Error traces provide detailed error messages during script evaluation, useful for debugging.
      * This increases script size and execution cost.
      *
      * @return
      *   a new [[PlutusV3]] with `generateErrorTraces = true`
      */
    def withErrorTraces: PlutusV3[A] = copy(options = options.copy(generateErrorTraces = true))
}

/** Factory methods for creating compiled Plutus V3 scripts. */
object PlutusV3 {

    /** Compiles Scala code to a Plutus V3 script.
      *
      * This is the main entry point for compiling Cardano smart contracts. The code is compiled at
      * compile-time using the Scalus compiler plugin.
      *
      * @example
      *   {{{
      *   given scalus.compiler.Options = scalus.compiler.Options.release
      *   val validator = PlutusV3.compile { (datum: Data) =>
      *     val d = datum.to[MyDatum]
      *     require(d.value > 0)
      *   }
      *   }}}
      *
      * @tparam A
      *   the type of the code being compiled
      * @param code
      *   the Scala code to compile (must be within the Scalus-supported subset)
      * @param opts
      *   compiler options controlling compilation, lowering, and optimization
      * @return
      *   a [[PlutusV3]] containing the compiled script
      */
    inline def compile[A](inline code: A)(using opts: Options): PlutusV3[A] = {
        val sir = compileInlineWithOptions(opts, code)
        PlutusV3(() => code, sir, opts, new V3Optimizer())
    }

    /** Extension methods for applying arguments to compiled Plutus V3 functions.
      *
      * Enables partial application of compiled validators at the UPLC level. When you apply a Scala
      * value to a compiled function, the value is lifted to a UPLC constant and applied to the SIR
      * representation.
      *
      * @example
      *   {{{
      *   val parameterizedValidator = PlutusV3.compile((config: Config) => (datum: Data) => ...)
      *   val appliedValidator = parameterizedValidator(myConfig) // PlutusV3[Data => Unit]
      *   }}}
      */
    // TODO: The constraint `A: Constant.LiftValue` is too restrictive. It only works for UPLC
    // primitive types (BigInt, ByteString, Data, etc.) but not for Scalus case classes like
    // TxOutRef that have ToData instances. We should introduce a typeclass (e.g., `Liftable[A]`)
    // that supports both:
    //   1. Types with LiftValue (direct UPLC primitives)
    //   2. Types with ToData (converted to Data, then lifted)
    // This would allow `ParameterizedValidator[TxOutRef]` to work directly without requiring
    // users to switch to DataParameterizedValidator and manually convert to Data.
    extension [A: Constant.LiftValue, B](self: PlutusV3[A => B]) {

        /** Applies an argument to a compiled function, producing a new compiled script.
          *
          * The argument is converted to a UPLC constant and applied to the SIR representation,
          * resulting in a specialized script with the parameter baked in.
          *
          * @param arg
          *   the argument to apply, must have a [[Constant.LiftValue]] instance
          * @return
          *   a new [[PlutusV3]] with the argument applied
          */
        def apply(arg: A): PlutusV3[B] = {
            val const = arg.asConstant
            PlutusV3[B](
              () => self.code(arg),
              self.sir $ SIR.Const(const, SIRType.fromDefaultUni(const.tpe), AnnotationsDecl.empty),
              self.options,
              self.optimizer
            )
        }
    }
}
