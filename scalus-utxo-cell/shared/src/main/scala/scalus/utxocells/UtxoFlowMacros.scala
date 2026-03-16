package scalus.utxocells

import scala.quoted.*
import cps.*
import cps.macros.Async
import scalus.uplc.builtin.{BuiltinList, BuiltinPair, Builtins, Data, FromData, ToData}

object UtxoFlowMacros {

    def defineImpl(
        body: Expr[CpsMonadContext[UtxoFlow] ?=> CellContext => Unit]
    )(using Quotes): Expr[UtxoFlowDispatch] = {
        import quotes.reflect.*

        // ================================================================
        // Step 1: Unwrap the two layers of lambdas
        // ================================================================

        val bodyTerm = body.asTerm

        val (cpsCtxParams, afterCtxFn, _) = Async.extractContextLambda(bodyTerm)
        if cpsCtxParams.size != 1 then
            report.errorAndAbort(
              s"Expected exactly one CpsMonadContext parameter, got ${cpsCtxParams.size}"
            )
        val cpsCtxParam = cpsCtxParams.head

        def extractLambda(t: Term): (ValDef, Term) = t match
            case Lambda(List(cellParam), lambdaBody) =>
                (cellParam, lambdaBody)
            case Block(Nil, inner) =>
                extractLambda(inner)
            case Block(List(defDef: DefDef), _: Closure) =>
                val params = defDef.paramss.flatMap(_.params).collect { case v: ValDef => v }
                if params.size != 1 then
                    report.errorAndAbort(
                      s"Expected single CellContext parameter, got ${params.size}"
                    )
                (
                  params.head,
                  defDef.rhs.getOrElse(
                    report.errorAndAbort("CellContext lambda has no body")
                  )
                )
            case Inlined(_, _, inner) =>
                extractLambda(inner)
            case other =>
                report.errorAndAbort(s"Expected CellContext lambda, got: ${other.show}")

        val (cellCtxParam, userBody) = extractLambda(afterCtxFn)

        // ================================================================
        // Step 2: Call Async.transformMonad
        // ================================================================

        val monadExpr = Expr
            .summon[CpsMonad[UtxoFlow]]
            .getOrElse(
              report.errorAndAbort("Cannot summon CpsMonad[UtxoFlow]")
            )

        val userBodyExpr = userBody.changeOwner(Symbol.spliceOwner).asExprOf[Unit]
        val cpsCtxRef = Ref(cpsCtxParam.symbol).asExprOf[CpsMonadContext[UtxoFlow]]

        val monadicExpr: Expr[UtxoFlow[Unit]] =
            Async.transformMonad[UtxoFlow, Unit, CpsMonadContext[UtxoFlow]](
              userBodyExpr,
              monadExpr,
              cpsCtxRef
            )

        val monadicTerm = monadicExpr.asTerm

        report.info(s"UtxoFlow monadic tree:\n${monadicTerm.show}")

        // ================================================================
        // Local helpers
        // ================================================================

        def isSuspend(term: Term): Boolean = term match
            case Inlined(_, _, inner) => isSuspend(inner)
            case Apply(TypeApply(sel @ Select(_, "suspend"), _), _) =>
                sel.qualifier.tpe <:< TypeRepr.of[UtxoFlow.type]
            case Apply(sel @ Select(_, "suspend"), _) =>
                sel.qualifier.tpe <:< TypeRepr.of[UtxoFlow.type]
            case _ => false

        def extractSuspendTypeRepr(t: Term): TypeRepr = t match
            case Inlined(_, _, inner)              => extractSuspendTypeRepr(inner)
            case Apply(TypeApply(_, List(tpt)), _) => tpt.tpe
            case _                                 => TypeRepr.of[Any]

        def isMonadicOp(term: Term): Boolean = term match
            case Apply(Apply(TypeApply(Select(_, name), _), _), _)
                if name == "flatMap" || name == "map" =>
                true
            case Apply(Apply(Select(_, name), _), _) if name == "flatMap" || name == "map" =>
                true
            case Apply(TypeApply(Select(_, "pure"), _), _) => true
            case Apply(Select(_, "pure"), _)               => true
            case _                                         => false

        /** Multi-symbol substitution via TreeMap. */
        def substituteSymbols(
            tree: Term,
            mapping: Map[Symbol, Symbol],
            own: Symbol
        ): Term = {
            if mapping.isEmpty then return tree
            val mapper = new TreeMap {
                override def transformTerm(t: Term)(o: Symbol): Term = t match
                    case Ident(_) if mapping.contains(t.symbol) => Ref(mapping(t.symbol))
                    case _                                      => super.transformTerm(t)(o)
            }
            mapper.transformTerm(tree)(own)
        }

        /** Strip the monadic tail (flatMap/map/pure) from a term, keeping only effect code. */
        def stripMonadicTail(term: Term): Term = term match
            case Inlined(call, bindings, b) =>
                Inlined(call, bindings, stripMonadicTail(b))
            case Block(stats, expr) if isMonadicOp(expr) =>
                if stats.isEmpty then '{ () }.asTerm
                else
                    stats.last match
                        case lastTerm: Term => Block(stats.init, lastTerm)
                        case _              => Block(stats, '{ () }.asTerm)
            case Block(stats, expr) =>
                Block(stats, stripMonadicTail(expr))
            case _ if isMonadicOp(term) => '{ () }.asTerm
            case other                  => other

        /** Flatten nested Block and Inlined chains into (statements, finalExpr). Inlined wrappers
          * from dotty-cps-async are stripped to avoid scope issues: the Splicer's
          * checkEscapedVariables only marks DefTree (ValDef/DefDef) at the Block stats level, so
          * ValDefs wrapped in Inlined are invisible.
          */
        def flattenBlock(term: Term): (List[Statement], Term) = term match
            case Block(stats, expr) =>
                val flatStats = stats.flatMap(flattenStatement)
                val (innerStats, innerExpr) = flattenBlock(expr)
                (flatStats ++ innerStats, innerExpr)
            case Inlined(_, bindings, inner) =>
                val flatBindings = bindings.flatMap(flattenStatement)
                val (innerStats, innerExpr) = flattenBlock(inner)
                (flatBindings ++ innerStats, innerExpr)
            case _ => (Nil, term)

        /** Flatten a single statement, stripping Inlined and Block wrappers. */
        def flattenStatement(stmt: Statement): List[Statement] = stmt match
            case Inlined(_, bindings, inner) =>
                val flatBindings = bindings.flatMap(flattenStatement)
                inner match
                    case t: Term =>
                        val (innerStats, innerExpr) = flattenBlock(t)
                        flatBindings ++ innerStats ++ (
                          innerExpr match
                              case Literal(UnitConstant()) => Nil
                              case _                       => List(innerExpr)
                        )
                    case _ => flatBindings :+ inner
            case Block(stats, expr) =>
                val flatStats = stats.flatMap(flattenStatement)
                val (innerStats, innerExpr) = flattenBlock(expr)
                flatStats ++ innerStats ++ (
                  innerExpr match
                      case Literal(UnitConstant()) => Nil
                      case _                       => List(innerExpr)
                )
            case _ => List(stmt)

        // ================================================================
        // Free variable analysis
        // ================================================================

        /** Collect all symbols DEFINED within a tree (ValDef symbols). */
        def collectDefinedSymbols(tree: Tree): Set[Symbol] = {
            val acc = new TreeAccumulator[Set[Symbol]] {
                override def foldTree(syms: Set[Symbol], tree: Tree)(owner: Symbol): Set[Symbol] =
                    tree match
                        case vd: ValDef => foldOverTree(syms + vd.symbol, tree)(owner)
                        case dd: DefDef => foldOverTree(syms + dd.symbol, tree)(owner)
                        case _          => foldOverTree(syms, tree)(owner)
            }
            acc.foldTree(Set.empty, tree)(Symbol.spliceOwner)
        }

        /** Check if a symbol is a local parameter/val (not top-level, not a package member). */
        def isLocalSymbol(sym: Symbol): Boolean = {
            val owner = sym.owner
            owner.exists && !owner.isNoSymbol && !owner.flags.is(Flags.Package)
        }

        /** Collect free variables in a term — Ident references not in `bound` and not defined
          * within the tree itself.
          */
        def freeVars(term: Term, bound: Set[Symbol]): Set[Symbol] = {
            val defined = collectDefinedSymbols(term)
            val allBound = bound ++ defined
            val acc = new TreeAccumulator[Set[Symbol]] {
                override def foldTree(syms: Set[Symbol], tree: Tree)(owner: Symbol): Set[Symbol] =
                    tree match
                        case id @ Ident(_) =>
                            val sym = id.symbol
                            if !allBound.contains(sym)
                                && sym.exists
                                && !sym.isNoSymbol
                                && !sym.flags.is(Flags.Module)
                                && !sym.flags.is(Flags.Package)
                                && !sym.isClassDef
                                && !sym.isTypeDef
                                && (sym.isValDef || sym.isDefDef || sym.isTerm)
                                && isLocalSymbol(sym)
                            then syms + sym
                            else syms
                        case _ => foldOverTree(syms, tree)(owner)
            }
            acc.foldTree(Set.empty, term)(Symbol.spliceOwner)
        }

        /** Create fresh symbols for all ValDefs in a tree and substitute everywhere. Returns the
          * transformed tree and the old->new symbol mapping. This prevents ScopeException when
          * symbols from the monadic tree are referenced in the generated dispatch lambda.
          */
        def freshenLocals(tree: Term, owner: Symbol): (Term, Map[Symbol, Symbol]) = {
            // First pass: collect all ValDef symbols
            val valSymbols = scala.collection.mutable.ListBuffer.empty[Symbol]
            val collector = new TreeAccumulator[Unit] {
                override def foldTree(acc: Unit, tree: Tree)(o: Symbol): Unit = tree match
                    case vd: ValDef =>
                        valSymbols += vd.symbol
                        foldOverTree(acc, tree)(o)
                    case _ => foldOverTree(acc, tree)(o)
            }
            collector.foldTree((), tree)(owner)

            if valSymbols.isEmpty then return (tree, Map.empty)

            // Create fresh symbols
            val mapping: Map[Symbol, Symbol] = valSymbols.map { sym =>
                sym -> Symbol.newVal(
                  owner,
                  sym.name,
                  sym.termRef.widenTermRefByName,
                  Flags.EmptyFlags,
                  Symbol.noSymbol
                )
            }.toMap

            // Second pass: rebuild tree with fresh symbols
            val mapper = new TreeMap {
                override def transformStatement(tree: Statement)(o: Symbol): Statement = tree match
                    case vd: ValDef if mapping.contains(vd.symbol) =>
                        val freshSym = mapping(vd.symbol)
                        val newRhs = vd.rhs.map(rhs => transformTerm(rhs)(freshSym))
                        ValDef(freshSym, newRhs.map(_.changeOwner(freshSym)))
                    case _ => super.transformStatement(tree)(o)

                override def transformTerm(t: Term)(o: Symbol): Term = t match
                    case Ident(_) if mapping.contains(t.symbol) => Ref(mapping(t.symbol))
                    case _                                      => super.transformTerm(t)(o)
            }

            (mapper.transformTerm(tree)(owner), mapping)
        }

        // ================================================================
        // Step 3: Extract chunk info from monadic tree
        // ================================================================

        case class ChunkField(name: String, symbol: Symbol, tpe: TypeRepr)

        case class ChunkInfo(
            index: Int,
            datumFields: List[ChunkField],
            redeemerParam: ValDef,
            redeemerType: TypeRepr,
            effectBody: Term,
            isTerminal: Boolean
        )

        case class ChunkBody(param: ValDef, body: Term, suspendType: TypeRepr)

        def extractChunkBodies(term: Term): List[ChunkBody] = term match
            case Inlined(_, _, b) => extractChunkBodies(b)
            case Block(_, expr)   => extractChunkBodies(expr)

            case Apply(
                  Apply(TypeApply(Select(_, "flatMap"), _), List(fa)),
                  List(Lambda(List(p), b))
                ) if isSuspend(fa) =>
                ChunkBody(p, b, extractSuspendTypeRepr(fa)) :: extractChunkBodies(b)
            case Apply(Apply(Select(_, "flatMap"), List(fa)), List(Lambda(List(p), b)))
                if isSuspend(fa) =>
                ChunkBody(p, b, extractSuspendTypeRepr(fa)) :: extractChunkBodies(b)

            case Apply(Apply(TypeApply(Select(_, "map"), _), List(fa)), List(Lambda(List(p), b)))
                if isSuspend(fa) =>
                List(ChunkBody(p, b, extractSuspendTypeRepr(fa)))
            case Apply(Apply(Select(_, "map"), List(fa)), List(Lambda(List(p), b)))
                if isSuspend(fa) =>
                List(ChunkBody(p, b, extractSuspendTypeRepr(fa)))

            case _ => Nil

        val chunkBodies = extractChunkBodies(monadicTerm)

        if chunkBodies.isEmpty then
            report.errorAndAbort("No chunk bodies found — the flow has no await calls")

        // Symbols that should never be captured
        val excludedSymbols = Set(cpsCtxParam.symbol, cellCtxParam.symbol)

        val chunkInfos: List[ChunkInfo] = {
            var boundSoFar = excludedSymbols

            chunkBodies.zipWithIndex.map { case (chunk, idx) =>
                val isTerminal = idx == chunkBodies.size - 1
                val effectBody = stripMonadicTail(chunk.body)

                val chunkBound = boundSoFar + chunk.param.symbol
                val fv = freeVars(effectBody, chunkBound)

                val datumFields = fv.toList
                    .sortBy(_.name)
                    .map { sym =>
                        ChunkField(sym.name, sym, sym.termRef.widenTermRefByName)
                    }

                val info = ChunkInfo(
                  index = idx,
                  datumFields = datumFields,
                  redeemerParam = chunk.param,
                  redeemerType = chunk.suspendType,
                  effectBody = effectBody,
                  isTerminal = isTerminal
                )

                boundSoFar = chunkBound
                info
            }
        }

        // Report chunk info
        report.info(s"UtxoFlow chunks: ${chunkInfos.size}")
        chunkInfos.foreach { chunk =>
            val fields = chunk.datumFields.map(f => s"${f.name}: ${f.tpe.show}").mkString(", ")
            report.info(
              s"  Chunk ${chunk.index}: await=${chunk.redeemerType.show}, " +
                  s"binding=${chunk.redeemerParam.name}, " +
                  s"datumFields=[$fields], terminal=${chunk.isTerminal}"
            )
        }

        // ================================================================
        // Step 4: Generate off-chain dispatch
        // ================================================================

        /** Build a Term that converts Data to the given type via FromData. */
        def buildFromDataCall(dataRef: Term, tp: TypeRepr): Term = {
            tp.asType match
                case '[t] =>
                    Expr.summon[FromData[t]] match
                        case Some(fd) =>
                            Select.unique(fd.asTerm, "apply").appliedTo(dataRef)
                        case None =>
                            report.errorAndAbort(s"Cannot summon FromData[${tp.show}]")
        }

        /** Build a Term that converts a value to Data via ToData. */
        def buildToDataCall(valueRef: Term, tp: TypeRepr): Term = {
            tp.asType match
                case '[t] =>
                    Expr.summon[ToData[t]] match
                        case Some(td) =>
                            Select.unique(td.asTerm, "apply").appliedTo(valueRef)
                        case None =>
                            report.errorAndAbort(s"Cannot summon ToData[${tp.show}]")
        }

        /** Build the body for a chunk, given refs to datum fields list, redeemer, and ctx. */
        def buildChunkBody(
            chunk: ChunkInfo,
            fieldsRef: Term,
            redeemerRef: Term,
            ctxRef: Term,
            owner: Symbol
        ): Term = {
            val stmts = scala.collection.mutable.ListBuffer.empty[Statement]
            var symMapping = Map.empty[Symbol, Symbol]

            // 1. Extract datum fields from BuiltinList[Data]
            val builtinsMod = Ref(TypeRepr.of[Builtins.type].termSymbol)
            var currentListRef: Term = fieldsRef
            chunk.datumFields.foreach { field =>
                val dataSym = Symbol.newVal(
                  owner,
                  s"${field.name}_data",
                  TypeRepr.of[Data],
                  Flags.EmptyFlags,
                  Symbol.noSymbol
                )
                val headCall = Select.overloaded(
                  builtinsMod,
                  "headList",
                  List(TypeRepr.of[Data]),
                  List(currentListRef)
                )
                stmts += ValDef(dataSym, Some(headCall.changeOwner(dataSym)))

                val decodedSym = Symbol.newVal(
                  owner,
                  field.name,
                  field.tpe,
                  Flags.EmptyFlags,
                  Symbol.noSymbol
                )
                val decodeCall = buildFromDataCall(Ref(dataSym), field.tpe)
                stmts += ValDef(decodedSym, Some(decodeCall.changeOwner(decodedSym)))

                symMapping = symMapping + (field.symbol -> decodedSym)

                val restSym = Symbol.newVal(
                  owner,
                  s"${field.name}_rest",
                  TypeRepr.of[BuiltinList[Data]],
                  Flags.EmptyFlags,
                  Symbol.noSymbol
                )
                val tailCall = Select.overloaded(
                  builtinsMod,
                  "tailList",
                  List(TypeRepr.of[Data]),
                  List(currentListRef)
                )
                stmts += ValDef(restSym, Some(tailCall.changeOwner(restSym)))
                currentListRef = Ref(restSym)
            }

            // 2. Decode redeemer
            val redeemerBindingSym = Symbol.newVal(
              owner,
              chunk.redeemerParam.name,
              chunk.redeemerParam.tpt.tpe,
              Flags.EmptyFlags,
              Symbol.noSymbol
            )
            val fromDataCall = buildFromDataCall(redeemerRef, chunk.redeemerType)
            stmts += ValDef(redeemerBindingSym, Some(fromDataCall.changeOwner(redeemerBindingSym)))
            symMapping = symMapping + (chunk.redeemerParam.symbol -> redeemerBindingSym)

            // 3. Apply substitutions to effect body (redeemer + datum fields + ctx)
            val allSubs = symMapping + (cellCtxParam.symbol -> ctxRef.symbol)
            val substituted = substituteSymbols(chunk.effectBody, allSubs, owner)

            // 4. Freshen all local ValDefs to avoid ScopeException
            val (freshBody, freshMapping) = freshenLocals(substituted, owner)
            symMapping = symMapping ++ freshMapping

            // 5. Flatten the effect body and add to stmts.
            //    flattenBlock strips Inlined wrappers from dotty-cps-async so that
            //    ValDefs appear as direct Block stats (required by Splicer scope check).
            val (bodyStmts, bodyFinalExpr) = flattenBlock(freshBody)
            stmts ++= bodyStmts
            val isUnitLiteral = bodyFinalExpr match
                case Literal(UnitConstant()) => true
                case _                       => false
            if !isUnitLiteral then stmts += bodyFinalExpr

            // 6. Build return value using Term-level reflect API (not quasi-quotes)
            if chunk.isTerminal then
                val noneExpr = '{ scala.None: Option[Data] }.asTerm
                Block(stmts.toList, noneExpr)
            else
                // Non-terminal: return Some(Data.Constr(nextTag, List(field1.toData, ...)))
                val nextChunk = chunkInfos(chunk.index + 1)
                val nextTagValue = chunk.index + 1

                val nextFieldExprs = nextChunk.datumFields.map { field =>
                    val sym = symMapping.getOrElse(field.symbol, field.symbol)
                    buildToDataCall(Ref(sym), field.tpe)
                }

                // Build PList[Data]
                val nilTerm: Term = {
                    val nilSym = TypeRepr
                        .of[scalus.cardano.onchain.plutus.prelude.List.type]
                        .termSymbol
                        .methodMember("empty")
                        .head
                    TypeApply(
                      Select(
                        Ref(
                          TypeRepr.of[scalus.cardano.onchain.plutus.prelude.List.type].termSymbol
                        ),
                        nilSym
                      ),
                      List(TypeTree.of[Data])
                    )
                }

                val fieldsList = nextFieldExprs.foldRight(nilTerm) { (elem, acc) =>
                    val consTpe = TypeRepr.of[scalus.cardano.onchain.plutus.prelude.List.Cons[Data]]
                    val consSym = consTpe.typeSymbol.companionModule
                    Select.overloaded(
                      Ref(consSym),
                      "apply",
                      List(TypeRepr.of[Data]),
                      List(elem, acc)
                    )
                }

                // Data.Constr(BigInt(nextTag), fieldsList)
                val bigIntTerm = Select.overloaded(
                  Ref(TypeRepr.of[BigInt.type].termSymbol),
                  "apply",
                  Nil,
                  List(Literal(IntConstant(nextTagValue)))
                )

                val dataConstr = Select.overloaded(
                  Ref(TypeRepr.of[Data.Constr.type].termSymbol),
                  "apply",
                  Nil,
                  List(bigIntTerm, fieldsList)
                )

                // Some[Data](dataConstr) : Option[Data]
                val someResult = Typed(
                  Select.overloaded(
                    Ref(TypeRepr.of[Some.type].termSymbol),
                    "apply",
                    List(TypeRepr.of[Data]),
                    List(dataConstr)
                  ),
                  TypeTree.of[Option[Data]]
                )

                Block(stmts.toList, someResult)
        }

        /** Build if-else chain dispatching on tag. */
        def buildDispatchChain(
            chunks: List[ChunkInfo],
            idx: Int,
            tagRef: Term,
            fieldsRef: Term,
            redeemerRef: Term,
            ctxRef: Term,
            owner: Symbol
        ): Term = {
            if idx >= chunks.size then
                '{ throw new Exception("UtxoFlow: unknown chunk tag") }.asTerm
            else
                val chunk = chunks(idx)
                val chunkBody = buildChunkBody(chunk, fieldsRef, redeemerRef, ctxRef, owner)
                val elseBody =
                    buildDispatchChain(
                      chunks,
                      idx + 1,
                      tagRef,
                      fieldsRef,
                      redeemerRef,
                      ctxRef,
                      owner
                    )

                val condition = '{
                    ${ tagRef.asExprOf[BigInt] } == BigInt(${ Expr(idx) })
                }.asTerm
                If(condition, chunkBody, elseBody)
        }

        // Build the dispatch lambda: (datum, redeemer, ctx) => { ... }
        val dispatchFnType = MethodType(List("datum", "redeemer", "ctx"))(
          _ => List(TypeRepr.of[Data], TypeRepr.of[Data], TypeRepr.of[CellContext]),
          _ => TypeRepr.of[Option[Data]]
        )

        val dispatchLambda = Lambda(
          Symbol.spliceOwner,
          dispatchFnType,
          (owner, params) => {
              val datumRef = params(0).asInstanceOf[Term]
              val redeemerRef = params(1).asInstanceOf[Term]
              val ctxRef = params(2).asInstanceOf[Term]

              // val pair = Builtins.unConstrData(datum)
              val pairSym = Symbol.newVal(
                owner,
                "pair",
                TypeRepr.of[BuiltinPair[BigInt, BuiltinList[Data]]],
                Flags.EmptyFlags,
                Symbol.noSymbol
              )
              val pairRhs = '{ Builtins.unConstrData(${ datumRef.asExprOf[Data] }) }.asTerm
              val pairValDef = ValDef(pairSym, Some(pairRhs.changeOwner(pairSym)))

              // val tag = pair.fst
              val tagSym = Symbol.newVal(
                owner,
                "tag",
                TypeRepr.of[BigInt],
                Flags.EmptyFlags,
                Symbol.noSymbol
              )
              val tagRhs = Select.unique(Ref(pairSym), "fst")
              val tagValDef = ValDef(tagSym, Some(tagRhs.changeOwner(tagSym)))

              // val fields = pair.snd
              val fieldsSym = Symbol.newVal(
                owner,
                "fields",
                TypeRepr.of[BuiltinList[Data]],
                Flags.EmptyFlags,
                Symbol.noSymbol
              )
              val fieldsRhs = Select.unique(Ref(pairSym), "snd")
              val fieldsValDef = ValDef(fieldsSym, Some(fieldsRhs.changeOwner(fieldsSym)))

              val dispatchBody = buildDispatchChain(
                chunkInfos,
                0,
                Ref(tagSym),
                Ref(fieldsSym),
                redeemerRef,
                ctxRef,
                owner
              )

              Block(List(pairValDef, tagValDef, fieldsValDef), dispatchBody)
          }
        )

        val dispatchExpr = dispatchLambda.asExprOf[(Data, Data, CellContext) => Option[Data]]
        val chunkCount = Expr(chunkInfos.size)
        '{ new UtxoFlowDispatch($dispatchExpr, $chunkCount) }
    }
}
