package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.*
import scalus.uplc.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*

/** Tests for the IntrinsicResolver — verifies that intrinsic substitution produces optimized UPLC
  * (e.g., `nullList` instead of match-based `chooseList`).
  */
class IntrinsicResolverTest extends AnyFunSuite {

    private val ae = AnnotationsDecl.empty

    // --- Helper types ---
    private val typeA = SIRType.TypeVar("A", Some(1), isBuiltin = false)
    private val listTypeA = SIRType.List(typeA)
    private val builtinListData = SIRType.BuiltinList(SIRType.Data.tp)

    // --- SIR helpers ---

    private val helperModule = "scalus.compiler.intrinsics.IntrinsicHelpers$"
    private val typeProxyReprName = s"$helperModule.typeProxyRepr"
    private val typeProxyName = s"$helperModule.typeProxy"

    private def mkTypeProxyRepr(
        arg: AnnotatedSIR,
        targetType: SIRType,
        reprName: String
    ): AnnotatedSIR =
        SIR.Apply(
          SIR.ExternalVar(
            helperModule,
            typeProxyReprName,
            SIRType.Fun(SIRType.FreeUnificator, targetType),
            ae
          ),
          arg,
          targetType,
          ae + ("repr" -> SIR.Const(Constant.String(reprName), SIRType.String, ae))
        )

    private def mkTypeProxy(arg: AnnotatedSIR, targetType: SIRType): AnnotatedSIR =
        SIR.Apply(
          SIR.ExternalVar(
            helperModule,
            typeProxyName,
            SIRType.Fun(SIRType.FreeUnificator, targetType),
            ae
          ),
          arg,
          targetType,
          ae
        )

    // --- Build provider module SIR (matches new ListIntrinsics structure) ---

    private val listReprName = "SumBuiltinList(DataData)"
    private val builtinListA = SIRType.BuiltinList(typeA)

    /** isEmpty[A](self: List[A]): Boolean = nullList(typeProxyRepr[BuiltinList[A]](self, sentinel))
      */
    private def buildIsEmptyBinding: Binding = {
        val selfVar = SIR.Var("self", listTypeA, ae)
        val proxy = mkTypeProxyRepr(selfVar, builtinListA, listReprName)
        val body = SIR.Apply(SIRBuiltins.nullList, proxy, SIRType.Boolean, ae)
        val lambda = SIR.LamAbs(selfVar, body, List.empty, ae)
        Binding(
          "scalus.compiler.intrinsics.BuiltinListOperations$.isEmpty",
          SIRType.Fun(listTypeA, SIRType.Boolean),
          lambda
        )
    }

    /** head[A](self: List[A]): A = headList(typeProxyRepr[BuiltinList[A]](self, sentinel)) */
    private def buildHeadBinding: Binding = {
        val selfVar = SIR.Var("self", listTypeA, ae)
        val proxy = mkTypeProxyRepr(selfVar, builtinListA, listReprName)
        val body = SIR.Apply(SIRBuiltins.headList, proxy, typeA, ae)
        val lambda = SIR.LamAbs(selfVar, body, List.empty, ae)
        Binding(
          "scalus.compiler.intrinsics.BuiltinListOperations$.head",
          SIRType.Fun(listTypeA, typeA),
          lambda
        )
    }

    /** tail[A](self: List[A]): List[A] =
      * typeProxy[List[A]](tailList(typeProxyRepr[BuiltinList[A]](self, sentinel)))
      */
    private def buildTailBinding: Binding = {
        val selfVar = SIR.Var("self", listTypeA, ae)
        val innerProxy = mkTypeProxyRepr(selfVar, builtinListA, listReprName)
        val tailCall = SIR.Apply(SIRBuiltins.tailList, innerProxy, builtinListA, ae)
        val outerProxy = mkTypeProxy(tailCall, listTypeA)
        val lambda = SIR.LamAbs(selfVar, outerProxy, List.empty, ae)
        Binding(
          "scalus.compiler.intrinsics.BuiltinListOperations$.tail",
          SIRType.Fun(listTypeA, listTypeA),
          lambda
        )
    }

    /** drop[A](self: List[A], n: BigInt): List[A] = typeProxy[List[A]](dropList(n,
      * typeProxyRepr[BuiltinList[A]](self, sentinel)))
      */
    private def buildDropBinding: Binding = {
        val selfVar = SIR.Var("self", listTypeA, ae)
        val nVar = SIR.Var("n", SIRType.Integer, ae)
        val innerProxy = mkTypeProxyRepr(selfVar, builtinListA, listReprName)
        val dropCall = SIR.Apply(
          SIR.Apply(SIRBuiltins.dropList, nVar, SIRType.Fun(builtinListA, builtinListA), ae),
          innerProxy,
          builtinListA,
          ae
        )
        val outerProxy = mkTypeProxy(dropCall, listTypeA)
        val innerLambda = SIR.LamAbs(nVar, outerProxy, List.empty, ae)
        val lambda = SIR.LamAbs(selfVar, innerLambda, List.empty, ae)
        Binding(
          "scalus.compiler.intrinsics.BuiltinListOperationsV11$.drop",
          SIRType.Fun(listTypeA, SIRType.Fun(SIRType.Integer, listTypeA)),
          lambda
        )
    }

    private val builtinListOpsModule = Module(
      version = SIRVersion,
      name = "scalus.compiler.intrinsics.BuiltinListOperations$",
      linked = false,
      requireBackend = None,
      defs = List(buildIsEmptyBinding, buildHeadBinding, buildTailBinding)
    )

    private val builtinListOpsV11Module = Module(
      version = SIRVersion,
      name = "scalus.compiler.intrinsics.BuiltinListOperationsV11$",
      linked = false,
      requireBackend = None,
      defs = List(buildDropBinding)
    )

    private val intrinsicModules: Map[String, Module] = Map(
      builtinListOpsModule.name -> builtinListOpsModule,
      builtinListOpsV11Module.name -> builtinListOpsV11Module
    )

    // --- Helper to lower with intrinsic modules ---

    private def lower(
        sir: SIR,
        modules: Map[String, Module] = intrinsicModules,
        supportModules: Map[String, Module] = Map.empty,
        targetProtocolVersion: MajorProtocolVersion = MajorProtocolVersion.changPV,
        debug: Boolean = false
    ): Term =
        SirToUplcV3Lowering(
          sir,
          generateErrorTraces = false,
          debug = debug,
          intrinsicModules = modules,
          supportModules = supportModules,
          targetProtocolVersion = targetProtocolVersion
        ).lower()

    private def lowerWithoutModules(sir: SIR): Term =
        SirToUplcV3Lowering(
          sir,
          generateErrorTraces = false
        ).lower()

    extension (term: Term)
        infix def alphaEq(other: Term): Boolean =
            DeBruijn.deBruijnTerm(term) α_== DeBruijn.deBruijnTerm(other)

    // --- Build usage SIR (simulating linked output) ---

    /** Build: let isEmpty = <match-based-impl> in isEmpty(listVar)
      *
      * After linking, list.isEmpty compiles to: Let([Binding("List$.isEmpty", <match impl>)],
      * Apply(Var("List$.isEmpty"), listVar))
      */
    private def buildIsEmptyUsage(listSir: AnnotatedSIR): SIR = {
        val isEmptyName = "scalus.cardano.onchain.plutus.prelude.List$.isEmpty"
        val isEmptyType = SIRType.Fun(listTypeA, SIRType.Boolean)
        // The match-based fallback (simplified — real one would pattern match)
        val matchImpl = SIR.LamAbs(
          SIR.Var("_self", listTypeA, ae),
          SIR.Const(Constant.Bool(true), SIRType.Boolean, ae), // placeholder
          List.empty,
          ae
        )
        SIR.Let(
          List(Binding(isEmptyName, isEmptyType, matchImpl)),
          SIR.Apply(
            SIR.Var(isEmptyName, isEmptyType, ae),
            listSir,
            SIRType.Boolean,
            ae
          ),
          SIR.LetFlags.None,
          ae
        )
    }

    // --- Tests ---

    test("isEmpty intrinsic: SumDataList representation produces nullList") {
        // Build a list constant (will have SumDataList representation after lowering)
        val listConst = SIR.Const(
          Constant.List(DefaultUni.Data, List.empty),
          SIRType.List(SIRType.Data.tp),
          ae
        )
        val sir = buildIsEmptyUsage(listConst)
        val uplc = lower(sir)

        // The UPLC should contain NullList applied to the list constant.
        // Without intrinsics, it would use the match-based implementation.
        assert(
          containsBuiltin(uplc, NullList),
          s"Expected nullList in UPLC but got: ${uplc.pretty.render(100)}"
        )
    }

    test("isEmpty fallback: without intrinsic modules, uses original implementation") {
        val listConst = SIR.Const(
          Constant.List(DefaultUni.Data, List.empty),
          SIRType.List(SIRType.Data.tp),
          ae
        )
        val sir = buildIsEmptyUsage(listConst)
        val uplc = lowerWithoutModules(sir)

        // Without modules, intrinsics don't fire. The result uses the placeholder implementation.
        // It should NOT contain nullList builtin from intrinsic resolution.
        // (The match-based impl is a placeholder returning true, so no NullList here)
        assert(
          !containsBuiltin(uplc, NullList),
          s"Expected no nullList (no modules) but got: ${uplc.pretty.render(100)}"
        )
    }

    test("head intrinsic: SumDataList representation produces headList") {
        val headName = "scalus.cardano.onchain.plutus.prelude.List$.head"
        val headType = SIRType.Fun(listTypeA, typeA)
        val matchImpl = SIR.LamAbs(
          SIR.Var("_self", listTypeA, ae),
          SIR.Error("head of empty list", ae),
          List.empty,
          ae
        )
        val listConst = SIR.Const(
          Constant.List(DefaultUni.Data, List.empty),
          SIRType.List(SIRType.Data.tp),
          ae
        )
        val sir = SIR.Let(
          List(Binding(headName, headType, matchImpl)),
          SIR.Apply(SIR.Var(headName, headType, ae), listConst, typeA, ae),
          SIR.LetFlags.None,
          ae
        )
        val uplc = lower(sir)
        assert(
          containsBuiltin(uplc, HeadList),
          s"Expected headList in UPLC but got: ${uplc.pretty.render(100)}"
        )
    }

    test("tail intrinsic: SumDataList representation produces tailList") {
        val tailName = "scalus.cardano.onchain.plutus.prelude.List$.tail"
        val tailType = SIRType.Fun(listTypeA, listTypeA)
        val matchImpl = SIR.LamAbs(
          SIR.Var("_self", listTypeA, ae),
          SIR.Error("tail of empty list", ae),
          List.empty,
          ae
        )
        val listConst = SIR.Const(
          Constant.List(DefaultUni.Data, List.empty),
          SIRType.List(SIRType.Data.tp),
          ae
        )
        val sir = SIR.Let(
          List(Binding(tailName, tailType, matchImpl)),
          SIR.Apply(SIR.Var(tailName, tailType, ae), listConst, listTypeA, ae),
          SIR.LetFlags.None,
          ae
        )
        val uplc = lower(sir)
        assert(
          containsBuiltin(uplc, TailList),
          s"Expected tailList in UPLC but got: ${uplc.pretty.render(100)}"
        )
    }

    test("drop intrinsic: not available at changPV (version 9)") {
        val dropName = "scalus.cardano.onchain.plutus.prelude.List$.drop"
        val dropType = SIRType.Fun(listTypeA, SIRType.Fun(SIRType.Integer, listTypeA))
        // Fallback returns the list unchanged (valid SumDataList representation)
        val matchImpl = SIR.LamAbs(
          SIR.Var("_self", listTypeA, ae),
          SIR.LamAbs(
            SIR.Var("_n", SIRType.Integer, ae),
            SIR.Var("_self", listTypeA, ae),
            List.empty,
            ae
          ),
          List.empty,
          ae
        )
        val listConst = SIR.Const(
          Constant.List(DefaultUni.Data, List.empty),
          SIRType.List(SIRType.Data.tp),
          ae
        )
        val nConst = SIR.Const(Constant.Integer(2), SIRType.Integer, ae)
        val sir = SIR.Let(
          List(Binding(dropName, dropType, matchImpl)),
          SIR.Apply(
            SIR.Apply(
              SIR.Var(dropName, dropType, ae),
              listConst,
              SIRType.Fun(SIRType.Integer, listTypeA),
              ae
            ),
            nConst,
            listTypeA,
            ae
          ),
          SIR.LetFlags.None,
          ae
        )
        // At changPV (9), drop intrinsic (minVersion=11) should NOT be available
        val uplc = lower(sir, targetProtocolVersion = MajorProtocolVersion.changPV)
        assert(
          !containsBuiltin(uplc, DropList),
          s"Expected no dropList at changPV but got: ${uplc.pretty.render(100)}"
        )
    }

    test("drop intrinsic: available at vanRossemPV (version 11)") {
        val dropName = "scalus.cardano.onchain.plutus.prelude.List$.drop"
        val dropType = SIRType.Fun(listTypeA, SIRType.Fun(SIRType.Integer, listTypeA))
        val matchImpl = SIR.LamAbs(
          SIR.Var("_self", listTypeA, ae),
          SIR.LamAbs(
            SIR.Var("_n", SIRType.Integer, ae),
            SIR.Error("drop fallback", ae),
            List.empty,
            ae
          ),
          List.empty,
          ae
        )
        val listConst = SIR.Const(
          Constant.List(DefaultUni.Data, List.empty),
          SIRType.List(SIRType.Data.tp),
          ae
        )
        val nConst = SIR.Const(Constant.Integer(2), SIRType.Integer, ae)
        val sir = SIR.Let(
          List(Binding(dropName, dropType, matchImpl)),
          SIR.Apply(
            SIR.Apply(
              SIR.Var(dropName, dropType, ae),
              listConst,
              SIRType.Fun(SIRType.Integer, listTypeA),
              ae
            ),
            nConst,
            listTypeA,
            ae
          ),
          SIR.LetFlags.None,
          ae
        )
        // At vanRossemPV (11), drop intrinsic should be available
        val uplc = lower(sir, targetProtocolVersion = MajorProtocolVersion.vanRossemPV)
        assert(
          containsBuiltin(uplc, DropList),
          s"Expected dropList at vanRossemPV but got: ${uplc.pretty.render(100)}"
        )
    }

    // --- Helpers ---

    /** Check if a UPLC term tree contains a reference to the given builtin. */
    private def containsBuiltin(term: Term, fun: DefaultFun): Boolean = term match
        case Term.Builtin(f, _) if f == fun => true
        case Term.Apply(f, arg, _)          => containsBuiltin(f, fun) || containsBuiltin(arg, fun)
        case Term.LamAbs(_, body, _)        => containsBuiltin(body, fun)
        case Term.Force(inner, _)           => containsBuiltin(inner, fun)
        case Term.Delay(inner, _)           => containsBuiltin(inner, fun)
        case _                              => false
}
