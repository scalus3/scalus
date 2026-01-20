package scalus.cardano.node

import scala.quoted.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

/** Macro utilities for building UtxoQuery from lambda expressions at compile time.
  *
  * This provides a HOAS (Higher-Order Abstract Syntax) embedding that translates lambda expressions
  * to the UtxoQuery ADT.
  *
  * Example:
  * {{{
  * import scalus.cardano.node.UtxoQueryMacros.buildQuery
  *
  * val query = buildQuery { u =>
  *   u.output.address == myAddress && u.output.value.hasAsset(policyId, assetName)
  * }
  * // Compiles to: UtxoQuery(FromAddress(myAddress)) && HasAsset(policyId, assetName)
  * }}}
  */
object UtxoQueryMacros {

    /** Build UtxoQuery from lambda at compile time.
      *
      * Supported expressions:
      *   - `u.output.address == addr` - query by address
      *   - `u.input.transactionId == txId` - query by transaction
      *   - `u.output.value.hasAsset(policyId, assetName)` - query/filter by asset
      *   - `u.output.value.coin >= amount` - filter by minimum lovelace
      *   - `u.output.hasDatumHash(hash)` - filter by datum hash
      *   - `u.output.datumOption.exists(_.dataHash == hash)` - filter by datum hash (alternative)
      *   - `&&` - AND combination
      *   - `||` - OR combination
      *
      * @param f
      *   Lambda expression from Utxo to Boolean
      * @return
      *   UtxoQuery representing the lambda
      */
    inline def buildQuery(inline f: Utxo => Boolean): UtxoQuery =
        ${ buildQueryImpl('f) }

    private def buildQueryImpl(f: Expr[Utxo => Boolean])(using Quotes): Expr[UtxoQuery] = {
        import quotes.reflect.*

        // Represents an intermediate query component during translation
        enum QueryComponent {
            case Source(expr: Expr[UtxoSource])
            case Filter(expr: Expr[UtxoFilter])
            case Query(expr: Expr[UtxoQuery])
        }

        // Check if a term references the lambda parameter
        def referencesParam(term: Term, paramSym: Symbol): Boolean = {
            var found = false
            val traverser = new TreeTraverser {
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case Ident(name) if tree.symbol == paramSym => found = true
                    case _                                      => super.traverseTree(tree)(owner)
            }
            traverser.traverseTree(term)(Symbol.spliceOwner)
            found
        }

        // Translate a boolean expression to QueryComponent
        def translate(term: Term, paramSym: Symbol): QueryComponent = term match {
            // Unwrap Block with no statements
            case Block(Nil, inner) =>
                translate(inner, paramSym)
            // u.output.address == addr
            case Apply(
                  Select(
                    Select(Select(Ident(_), "output"), "address"),
                    "=="
                  ),
                  List(addrTerm)
                ) if !referencesParam(addrTerm, paramSym) =>
                val addrExpr = addrTerm.asExprOf[Address]
                QueryComponent.Source('{ UtxoSource.FromAddress($addrExpr) })

            // addr == u.output.address (reversed)
            case Apply(
                  Select(addrTerm, "=="),
                  List(Select(Select(Ident(_), "output"), "address"))
                ) if !referencesParam(addrTerm, paramSym) =>
                val addrExpr = addrTerm.asExprOf[Address]
                QueryComponent.Source('{ UtxoSource.FromAddress($addrExpr) })

            // u.input.transactionId == txId
            case Apply(
                  Select(
                    Select(Select(Ident(_), "input"), "transactionId"),
                    "=="
                  ),
                  List(txIdTerm)
                ) if !referencesParam(txIdTerm, paramSym) =>
                val txIdExpr = txIdTerm.asExprOf[TransactionHash]
                QueryComponent.Source('{ UtxoSource.FromTransaction($txIdExpr) })

            // txId == u.input.transactionId (reversed)
            case Apply(
                  Select(txIdTerm, "=="),
                  List(Select(Select(Ident(_), "input"), "transactionId"))
                ) if !referencesParam(txIdTerm, paramSym) =>
                val txIdExpr = txIdTerm.asExprOf[TransactionHash]
                QueryComponent.Source('{ UtxoSource.FromTransaction($txIdExpr) })

            // u.output.value.hasAsset(policyId, assetName)
            // Extension method compiles as: Value.hasAsset(u.output.value)(policyId, assetName)
            case Apply(
                  Apply(
                    Select(Ident("Value"), "hasAsset"),
                    List(Select(Select(Ident(_), "output"), "value"))
                  ),
                  List(policyIdTerm, assetNameTerm)
                )
                if !referencesParam(policyIdTerm, paramSym) && !referencesParam(
                  assetNameTerm,
                  paramSym
                ) =>
                val policyIdExpr = policyIdTerm.asExprOf[PolicyId]
                val assetNameExpr = assetNameTerm.asExprOf[AssetName]
                // hasAsset can be either a source or a filter depending on context
                // We'll mark it as a source, and promote to filter when combined with another source
                QueryComponent.Source('{ UtxoSource.FromAsset($policyIdExpr, $assetNameExpr) })

            // u.output.value.coin >= amount (or >)
            case Apply(
                  Select(
                    Select(Select(Select(Ident(_), "output"), "value"), "coin"),
                    op
                  ),
                  List(amountTerm)
                ) if (op == ">=" || op == ">") && !referencesParam(amountTerm, paramSym) =>
                val amountExpr = amountTerm.asExprOf[Coin]
                QueryComponent.Filter('{ UtxoFilter.MinLovelace($amountExpr) })

            // amount <= u.output.value.coin (reversed)
            case Apply(
                  Select(amountTerm, op),
                  List(Select(Select(Select(Ident(_), "output"), "value"), "coin"))
                ) if (op == "<=" || op == "<") && !referencesParam(amountTerm, paramSym) =>
                val amountExpr = amountTerm.asExprOf[Coin]
                QueryComponent.Filter('{ UtxoFilter.MinLovelace($amountExpr) })

            // u.output.hasDatumHash(hash)
            // Extension method compiles as: TransactionOutput.hasDatumHash(u.output)(hash)
            case Apply(
                  Apply(
                    Select(Ident("TransactionOutput"), "hasDatumHash"),
                    List(Select(Ident(_), "output"))
                  ),
                  List(hashTerm)
                ) if !referencesParam(hashTerm, paramSym) =>
                val hashExpr = hashTerm.asExprOf[DataHash]
                QueryComponent.Filter('{ UtxoFilter.HasDatumHash($hashExpr) })

            // u.output.datumOption.exists(_.dataHash == hash)
            case Apply(
                  Select(
                    Select(Select(Ident(_), "output"), "datumOption"),
                    "exists"
                  ),
                  List(
                    Block(
                      List(
                        DefDef(
                          _,
                          _,
                          _,
                          Some(Apply(Select(Select(Ident(_), "dataHash"), "=="), List(hashTerm)))
                        )
                      ),
                      _
                    )
                  )
                ) if !referencesParam(hashTerm, paramSym) =>
                val hashExpr = hashTerm.asExprOf[DataHash]
                QueryComponent.Filter('{ UtxoFilter.HasDatumHash($hashExpr) })

            // hash == _.dataHash variant
            case Apply(
                  Select(
                    Select(Select(Ident(_), "output"), "datumOption"),
                    "exists"
                  ),
                  List(
                    Block(
                      List(
                        DefDef(
                          _,
                          _,
                          _,
                          Some(Apply(Select(hashTerm, "=="), List(Select(Ident(_), "dataHash"))))
                        )
                      ),
                      _
                    )
                  )
                ) if !referencesParam(hashTerm, paramSym) =>
                val hashExpr = hashTerm.asExprOf[DataHash]
                QueryComponent.Filter('{ UtxoFilter.HasDatumHash($hashExpr) })

            // expr1 && expr2
            case Apply(Select(left, "&&"), List(right)) =>
                val leftComp = translate(left, paramSym)
                val rightComp = translate(right, paramSym)
                combineAnd(leftComp, rightComp)

            // expr1 || expr2
            case Apply(Select(left, "||"), List(right)) =>
                val leftComp = translate(left, paramSym)
                val rightComp = translate(right, paramSym)
                combineOr(leftComp, rightComp)

            case other =>
                report.errorAndAbort(
                  s"Unsupported expression in UTxO query: ${other.show}\n" +
                      s"Supported patterns:\n" +
                      s"  - u.output.address == addr\n" +
                      s"  - u.input.transactionId == txId\n" +
                      s"  - u.output.value.hasAsset(policyId, assetName)\n" +
                      s"  - u.output.value.coin >= amount\n" +
                      s"  - u.output.hasDatumHash(hash)\n" +
                      s"  - u.output.datumOption.exists(_.dataHash == hash)\n" +
                      s"  - expr1 && expr2\n" +
                      s"  - expr1 || expr2",
                  term.pos
                )
        }

        // Combine two components with AND
        def combineAnd(left: QueryComponent, right: QueryComponent): QueryComponent =
            (left, right) match {
                // Source && Source -> Source.And or Query with filter
                case (QueryComponent.Source(s1), QueryComponent.Source(s2)) =>
                    // If one is FromAsset, convert it to a filter
                    (s1, s2) match {
                        case (
                              '{ UtxoSource.FromAsset($p1, $n1) },
                              '{ UtxoSource.FromAsset($p2, $n2) }
                            ) =>
                            // Both are asset queries - keep first as source, second as filter
                            QueryComponent.Query('{
                                UtxoQuery(UtxoSource.FromAsset($p1, $n1)) && UtxoFilter.HasAsset(
                                  $p2,
                                  $n2
                                )
                            })
                        case ('{ UtxoSource.FromAsset($p, $n) }, other) =>
                            // Asset + other source -> other source with asset filter
                            QueryComponent.Query('{
                                UtxoQuery($other) && UtxoFilter.HasAsset($p, $n)
                            })
                        case (other, '{ UtxoSource.FromAsset($p, $n) }) =>
                            // Other source + asset -> source with asset filter
                            QueryComponent.Query('{
                                UtxoQuery($other) && UtxoFilter.HasAsset($p, $n)
                            })
                        case _ =>
                            // Two non-asset sources -> use Source.And
                            QueryComponent.Source('{ $s1 && $s2 })
                    }

                // Source && Filter -> Query
                case (QueryComponent.Source(s), QueryComponent.Filter(f)) =>
                    QueryComponent.Query('{ UtxoQuery($s) && $f })

                // Filter && Source -> Query
                case (QueryComponent.Filter(f), QueryComponent.Source(s)) =>
                    QueryComponent.Query('{ UtxoQuery($s) && $f })

                // Filter && Filter -> Filter.And
                case (QueryComponent.Filter(f1), QueryComponent.Filter(f2)) =>
                    QueryComponent.Filter('{ $f1 && $f2 })

                // Query && Filter -> Query with filter
                case (QueryComponent.Query(q), QueryComponent.Filter(f)) =>
                    QueryComponent.Query('{ $q && $f })

                // Filter && Query -> Query with filter
                case (QueryComponent.Filter(f), QueryComponent.Query(q)) =>
                    QueryComponent.Query('{ $q && $f })

                // Query && Source (asset) -> Query with filter
                case (
                      QueryComponent.Query(q),
                      QueryComponent.Source('{ UtxoSource.FromAsset($p, $n) })
                    ) =>
                    QueryComponent.Query('{ $q && UtxoFilter.HasAsset($p, $n) })

                // Source (asset) && Query -> Query with filter
                case (
                      QueryComponent.Source('{ UtxoSource.FromAsset($p, $n) }),
                      QueryComponent.Query(q)
                    ) =>
                    QueryComponent.Query('{ $q && UtxoFilter.HasAsset($p, $n) })

                // Query && Query -> need to merge (not typical use case)
                case (QueryComponent.Query(q1), QueryComponent.Query(q2)) =>
                    report.errorAndAbort(
                      "Cannot combine two complex queries with &&. Use simpler expressions.",
                      Position.ofMacroExpansion
                    )

                // Other combinations
                case (QueryComponent.Query(q), QueryComponent.Source(s)) =>
                    report.errorAndAbort(
                      "Cannot add a source to an existing query with &&. Sources must come first.",
                      Position.ofMacroExpansion
                    )

                case (QueryComponent.Source(s), QueryComponent.Query(q)) =>
                    report.errorAndAbort(
                      "Cannot combine source with complex query using &&.",
                      Position.ofMacroExpansion
                    )
            }

        // Combine two components with OR
        def combineOr(left: QueryComponent, right: QueryComponent): QueryComponent = {
            def toQuery(comp: QueryComponent): Expr[UtxoQuery] = comp match {
                case QueryComponent.Source(s) => '{ UtxoQuery($s) }
                case QueryComponent.Filter(f) =>
                    report.errorAndAbort(
                      "Cannot use OR with standalone filters. Filters must be combined with a source.",
                      Position.ofMacroExpansion
                    )
                case QueryComponent.Query(q) => q
            }
            val q1 = toQuery(left)
            val q2 = toQuery(right)
            QueryComponent.Query('{ $q1 || $q2 })
        }

        // Convert final component to UtxoQuery
        def toUtxoQuery(comp: QueryComponent): Expr[UtxoQuery] = comp match {
            case QueryComponent.Source(s) => '{ UtxoQuery($s) }
            case QueryComponent.Filter(f) =>
                report.errorAndAbort(
                  "Query must have a source. Standalone filters are not allowed.\n" +
                      "Add a source like: u.output.address == addr && <your filter>",
                  Position.ofMacroExpansion
                )
            case QueryComponent.Query(q) => q
        }

        // Extract lambda parameter and body from different AST shapes
        // Note: Lambda.unapply takes Block and returns Option[(List[ValDef], Term)]
        // Lambda case must come before Block case per Quotes.scala documentation
        def extractLambda(term: Term): (Symbol, Term) = term match {
            case Inlined(_, Nil, inner) =>
                extractLambda(inner)
            case Inlined(_, bindings, inner) =>
                report.errorAndAbort(
                  s"Unexpected bindings in Inlined node: ${bindings.map(_.show).mkString(", ")}\n" +
                      s"Full term: ${term.show}",
                  term.pos
                )
            case Typed(inner, _) =>
                extractLambda(inner)
            case Lambda(List(param), body) =>
                (param.symbol, body)
            case Lambda(params, body) =>
                report.errorAndAbort(
                  s"Expected lambda with single parameter, got ${params.size} parameters: ${params.map(_.show).mkString(", ")}",
                  term.pos
                )
            case Block(Nil, inner) =>
                extractLambda(inner)
            case block: Block =>
                report.errorAndAbort(
                  s"Block not matched by Lambda extractor.\n" +
                      s"Lambda.unapply result: ${Lambda.unapply(block)}\n" +
                      s"Block statements: ${block.statements.map(_.show).mkString("; ")}\n" +
                      s"Block expr: ${block.expr.show}",
                  term.pos
                )
            case _ =>
                report.errorAndAbort(
                  s"Expected a lambda expression (u => ...), got ${term.getClass.getSimpleName}: ${term.show}",
                  term.pos
                )
        }

        // Extract and process the lambda
        val (paramSym, body) = extractLambda(f.asTerm)
        val comp = translate(body, paramSym)
        toUtxoQuery(comp)
    }
}
