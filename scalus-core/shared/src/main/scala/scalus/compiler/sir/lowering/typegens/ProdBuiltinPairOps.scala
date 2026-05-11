package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*
import scalus.uplc.Term

/** Emitter for `ProductCaseClassRepresentation.ProdBuiltinPair(_, _)` scrutinees.
  *
  * Owns the `Match` shape for a value already in `BuiltinPair` form: extracts the two components
  * via `Case` on Pair (V4+) or `fstPair`/`sndPair` (V1-V3), binds them in scope, and lowers the
  * case body.
  *
  * Constr emission for `ProdBuiltinPair` flows through `ProductCaseEmitter.genConstrLowered` →
  * `ProdDataListOps.genConstr` → conversions in `ProdDispatch`; only the `Match` path is
  * per-emitter logic.
  */
object ProdBuiltinPairOps {

    /** Match emission for a `ProdBuiltinPair` scrutinee. */
    def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val (typeParams, constrDecl, typeArgs) = SIRType
            .collectProd(loweredScrutinee.sirType)
            .getOrElse(
              throw LoweringException(
                s"Expected product type, got ${loweredScrutinee.sirType.show}",
                matchData.anns.pos
              )
            )
        val myCase = ProductCaseEmitter.selectMatchCase(
          matchData,
          loweredScrutinee,
          constrDecl
        )
        val (matchVal, addMatchValToScope) = lvAsIdentifiable(
          loweredScrutinee,
          "match_pair_data",
          loweredScrutinee.sirType,
          loweredScrutinee.representation,
          matchData.anns.pos
        )
        val (frsName, sndName) = myCase.pattern match {
            case SIR.Pattern.Constr(constr, bindings, typeParamsBindinsg) =>
                (bindings.head, bindings.tail.head)
            case SIR.Pattern.Const(_) =>
                throw LoweringException(
                  s"Constant pattern not supported for product case class ${constrDecl.name}",
                  matchData.anns.pos
                )
            case SIR.Pattern.Wildcard =>
                ("_unused1", "_unused2")
        }
        val argsMapping = constrDecl.typeParams.zip(typeArgs).toMap
        val frsTp = SIRType.substitute(constrDecl.params.head.tp, argsMapping, Map.empty)
        val sndTp = SIRType.substitute(constrDecl.params.tail.head.tp, argsMapping, Map.empty)
        val frsRepr = SirTypeUplcGenerator.defaultDataRepresentation(frsTp)
        val sndRepr = SirTypeUplcGenerator.defaultDataRepresentation(sndTp)

        if lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV then {
            // For PlutusV4: use Case on Pair - frs and snd are lambda parameters
            val frsVarId = lctx.uniqueVarName(frsName)
            val frsVar = new VariableLoweredValue(
              id = frsVarId,
              name = frsName,
              sir = SIR.Var(frsVarId, frsTp, AnnotationsDecl.empty),
              representation = frsRepr,
              optRhs = None // lambda parameter, not derived from builtin
            )
            val sndVarId = lctx.uniqueVarName(sndName)
            val sndVar = new VariableLoweredValue(
              id = sndVarId,
              name = sndName,
              sir = SIR.Var(sndVarId, sndTp, AnnotationsDecl.empty),
              representation = sndRepr,
              optRhs = None // lambda parameter, not derived from builtin
            )
            // Add pair element vars to scope so the body can reference them, then restore the
            // previous scope so these match-local bindings don't leak to siblings. The returned
            // LoweredValue still references frsVar/sndVar via lvCasePair, so they're emitted
            // correctly by termWithNeededVars.
            val prevScope = lctx.scope
            lctx.scope = lctx.scope.addAll(scala.collection.immutable.List(frsVar, sndVar))
            val lwBody = lctx.lower(myCase.body, optTargetType)
            lctx.scope = prevScope
            lvCasePair(matchVal, frsVar, sndVar, lwBody, matchData.anns.pos)
        } else {
            // For V1-V3: use fstPair/sndPair builtins. lvNewLazyNamedVar mutates lctx.scope —
            // save/restore around body lowering so the frs/snd bindings don't leak; they remain
            // referenced from the returned MatchPairDataLoweredValue.
            val prevScope = lctx.scope
            val frs = lvNewLazyNamedVar(
              frsName,
              frsTp,
              frsRepr,
              lvBuiltinApply(SIRBuiltins.fstPair, matchVal, frsTp, frsRepr, myCase.anns.pos),
              myCase.anns.pos
            )
            val snd = lvNewLazyNamedVar(
              sndName,
              sndTp,
              sndRepr,
              lvBuiltinApply(SIRBuiltins.sndPair, matchVal, sndTp, sndRepr, myCase.anns.pos),
              myCase.anns.pos
            )
            val lwBody = lctx.lower(myCase.body, optTargetType)
            lctx.scope = prevScope
            MatchPairDataLoweredValue(
              frs,
              snd,
              matchVal,
              addMatchValToScope,
              lwBody,
              matchData.anns.pos
            )
        }
    }

    class MatchPairDataLoweredValue(
        frs: IdentifiableLoweredValue,
        snd: IdentifiableLoweredValue,
        scrutinee: IdentifiableLoweredValue,
        addScrutineeToScope: Boolean,
        body: LoweredValue,
        inPos: SIRPosition
    ) extends ComplexLoweredValue(
          Set(frs, snd) ++ (if addScrutineeToScope then Set(scrutinee) else Set.empty),
          scrutinee,
          body
        ) {

        override def sirType: SIRType = body.sirType

        override def representation: LoweredValueRepresentation = body.representation

        override def pos: SIRPosition = inPos

        override def termInternal(gctx: TermGenerationContext): Term = {
            body.termWithNeededVars(gctx)
        }

        override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
            val left = Doc.text("MatchPairData(")
            val right = Doc.text(")")
            body.docRef(ctx).bracketBy(left, right)
        }

    }

}
