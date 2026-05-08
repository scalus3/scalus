package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.ProductCaseClassRepresentation.*
import scalus.compiler.sir.*
import scalus.uplc.Term

import scala.util.control.NonFatal

/** Emitter for `ProductCaseClassRepresentation.ProdDataList` and the wider Data-shape product
  * family (`ProdDataConstr`, `PackedDataList`, `PairIntDataList`) — they all share the
  * `unConstrData`/`headList`/`tailList` extraction pattern and converge on `ProdDataList` for
  * `Match`.
  *
  * Owns:
  *   - `genConstr` (Data-list-shaped Constr emission).
  *   - `genSelect` (field projection through `ProdDataList`).
  *   - `genMatch` (single-constructor extraction; multi-case input is shrunk via `selectMatchCase`,
  *     then handed to `DataConstrEmitter.genMatchDataConstrCase` for the actual binding + body
  *     lowering).
  *
  * Constructor-handling helpers (`retrieveConstrDecl`, `retrieveConstrIndex`, `selectMatchCase`)
  * still live on `ProductCaseSirTypeGenerator` during the Phase 4c migration — they're shared with
  * `ProdBuiltinPairEmitter` and several Sum-side typegens.
  */
object ProdDataListEmitter {

    /** Data-list-shaped Constr emission: convert each arg to its data form, fold into a `mkCons`
      * chain, wrap as a `ProdDataList`-tagged value carrying `constr.tp` as the SIR type.
      */
    def genConstr(
        constr: SIR.Constr,
        loweredArgs: Seq[LoweredValue],
        argTypeGens: Seq[SirTypeUplcGenerator],
    )(using lctx: LoweringContext): LoweredValue = {
        val dataRepresentations = loweredArgs.zip(argTypeGens).map { case (arg, typeGen) =>
            try
                arg.toRepresentation(
                  typeGen.defaultDataRepresentation(arg.sirType),
                  constr.anns.pos
                )
            catch
                case NonFatal(e) =>
                    println(
                      s"error while converting to data representation: arg=${arg}, argTypeGen=${typeGen} "
                    )
                    println(
                      s"arg.sirType = ${arg.sirType.show}, representation = ${arg.representation}, constr.anns.pos = ${constr.anns.pos}"
                    )
                    println(
                      s"defaultDataRepresentation(${arg.sirType.show}) = ${typeGen.defaultDataRepresentation(arg.sirType)}"
                    )
                    println(s"typeGen = ${typeGen}")
                    println(
                      s"defaultTypeGen(${arg.sirType.show}) = ${SirTypeUplcGenerator(arg.sirType)}"
                    )
                    println(
                      s"arg created from: ${constr.anns.pos.file}:${constr.anns.pos.startLine + 1}"
                    )
                    throw e
        }
        // TODO: consider using ProdUplcConstr for more efficient encoding
        val s0 = lvDataDataListNil(constr.anns.pos)
        val dataList = dataRepresentations.foldRight(s0) { (arg, acc) =>
            lvBuiltinApply2(
              SIRBuiltins.mkCons,
              arg,
              acc,
              SIRType.List(SIRType.Data.tp),
              SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData),
              constr.anns.pos
            )
        }

        val retval = new ProxyLoweredValue(dataList) {
            override def sirType: SIRType = constr.tp

            override def representation: LoweredValueRepresentation = ProdDataList

            override def termInternal(gctx: TermGenerationContext): Term = {
                dataList.termWithNeededVars(gctx)
            }

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val left = Doc.text("Constr(") + Doc.text(constr.tp.show) + Doc.comma
                val right = Doc.text(")")
                dataList.docRef(ctx).bracketBy(left, right)
            }

            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)

        }
        retval
    }

    /** Field projection on a Data-shape product: convert the scrutinee to `ProdDataList`, then walk
      * to the chosen field via `dropList` (V4+, fieldIdx ≥ 2) or chained `tailList` (V1-V3 /
      * fieldIdx < 2), and read with `headList`.
      */
    def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val dataListScrutinee = loweredScrutinee.toRepresentation(ProdDataList, sel.anns.pos)
        val (list0, addList0ToScope) =
            if dataListScrutinee.isInstanceOf[IdentifiableLoweredValue] then
                (dataListScrutinee.asInstanceOf[IdentifiableLoweredValue], false)
            else
                (
                  lvNewLazyIdVar(
                    lctx.uniqueVarName("list_sel"),
                    dataListScrutinee.sirType,
                    SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData),
                    dataListScrutinee,
                    sel.anns.pos
                  ),
                  true
                )
        val list0id = list0.id
        val constrDecl = ProductCaseSirTypeGenerator.retrieveConstrDecl(
          loweredScrutinee.sirType,
          sel.anns.pos
        )
        val fieldIndex = constrDecl.params.indexWhere(_.name == sel.field)
        if fieldIndex < 0 then
            throw LoweringException(
              s"Unknown field ${sel.field} for ${constrDecl.name}",
              sel.anns.pos
            )
        val scopeVars0: Set[IdentifiableLoweredValue] =
            if addList0ToScope then Set(list0) else Set.empty
        val (selHeadList, scopeVars) =
            if lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV && fieldIndex >= 2
            then
                // dropList(fieldIndex, list) is more efficient for n >= 2
                val droppedId = list0id + s"_drop_$fieldIndex"
                val droppedList = lctx.scope.getById(droppedId) match
                    case Some(v) => v
                    case None =>
                        lvNewLazyIdVar(
                          droppedId,
                          SIRType.List(SIRType.Data.tp),
                          SumCaseClassRepresentation.SumBuiltinList(
                            SumCaseClassRepresentation.DataData
                          ),
                          lvBuiltinApply2(
                            SIRBuiltins.dropList,
                            lvIntConstant(fieldIndex, sel.anns.pos),
                            list0,
                            SIRType.List(SIRType.Data.tp),
                            SumCaseClassRepresentation.SumBuiltinList(
                              SumCaseClassRepresentation.DataData
                            ),
                            sel.anns.pos
                          ),
                          sel.anns.pos
                        )
                (droppedList, scopeVars0 + droppedList)
            else
                // Original tail-chaining for V1-V3 or small indices (0, 1)
                (0 until fieldIndex).foldLeft((list0, scopeVars0)) { (acc, idx) =>
                    val tailId = list0id + s"_tail_${idx + 1}"
                    val tailLazyVar = lctx.scope.getById(tailId) match
                        case Some(v) => v
                        case None =>
                            lvNewLazyIdVar(
                              tailId,
                              SIRType.List(SIRType.Data.tp),
                              SumCaseClassRepresentation.SumBuiltinList(
                                SumCaseClassRepresentation.DataData
                              ),
                              lvBuiltinApply(
                                SIRBuiltins.tailList,
                                acc._1,
                                SIRType.List(SIRType.Data.tp),
                                SumCaseClassRepresentation.SumBuiltinList(
                                  SumCaseClassRepresentation.DataData
                                ),
                                sel.anns.pos
                              ),
                              sel.anns.pos
                            )
                    (tailLazyVar, acc._2 + tailLazyVar)
                }
        val body = lvBuiltinApply(
          SIRBuiltins.headList,
          selHeadList,
          sel.tp,
          SirTypeUplcGenerator.defaultDataRepresentation(sel.tp),
          sel.anns.pos
        )
        ScopeBracketsLoweredValue(scopeVars, body)
    }

    /** Match emission on a Data-shape product scrutinee. For >1-case input the applicable case is
      * selected via `ProductCaseSirTypeGenerator.selectMatchCase` and the rest are statically
      * dropped (info-logged). Single-case input flows directly to
      * `DataConstrEmitter.genMatchDataConstrCase`, which owns the actual field-extraction +
      * body-lowering inside the Data list scope.
      */
    def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val constrDecl = ProductCaseSirTypeGenerator.retrieveConstrDecl(
          loweredScrutinee.sirType,
          matchData.anns.pos
        )
        matchData.cases match {
            case oneCase :: Nil =>
                val matchCase = oneCase.pattern match
                    case cs @ SIR.Pattern.Constr(constrDecl1, args, _) =>
                        if constrDecl1.name != constrDecl.name then
                            throw LoweringException(
                              s"Expected constructor ${constrDecl.name}, got ${constrDecl1.name}",
                              matchData.anns.pos
                            )
                        oneCase
                    case SIR.Pattern.Const(_) =>
                        throw LoweringException(
                          s"Constant pattern not supported for product case class ${constrDecl.name}",
                          matchData.anns.pos
                        )
                    case SIR.Pattern.Wildcard =>
                        val argsNames = constrDecl.params.map(_.name)
                        val argsTypes = constrDecl.params.map(_.tp)
                        // TODO: add typeArgs to env ?
                        oneCase.copy(pattern = SIR.Pattern.Constr(constrDecl, argsNames, argsTypes))
                val (dataList, addToScope) = lvAsIdentifiable(
                  loweredScrutinee.toRepresentation(
                    ProductCaseClassRepresentation.ProdDataList,
                    loweredScrutinee.pos
                  ),
                  "_match_data_list",
                  SIRType.List(SIRType.Data.tp),
                  SumCaseClassRepresentation.SumBuiltinList(
                    SumCaseClassRepresentation.DataData
                  ),
                  matchData.anns.pos
                )
                DataConstrEmitter.genMatchDataConstrCase(
                  matchCase,
                  dataList,
                  optTargetType,
                  addToScope
                )
            case _ =>
                val myCase = ProductCaseSirTypeGenerator.selectMatchCase(
                  matchData,
                  loweredScrutinee,
                  constrDecl
                )
                lctx.info(
                  s"Product case class match should have only one case, but ${matchData.cases.length} found. Non-matched cases will be statically optimized out",
                  matchData.anns.pos
                )
                genMatch(
                  matchData.copy(cases = List(myCase)),
                  loweredScrutinee,
                  optTargetType
                )
        }
    }

}
