package scalus.cardano.onchain.plutus.prelude

import scala.quoted.*

private object EqMacros {

    def eqImpl[A: Type](using Quotes): Expr[Eq[A]] = {
        import quotes.reflect.*
        val tpe = TypeRepr.of[A].dealias.widen

        if tpe <:< TypeRepr.of[AnyRef] then
            val children = tpe.typeSymbol.children
            if children.isEmpty then
                if tpe.typeSymbol.flags.is(Flags.Trait) then
                    report.errorAndAbort(
                      s"Cannot derive Eq for trait ${tpe.typeSymbol.fullName} with no children"
                    )
                else if tpe.typeSymbol.flags.is(Flags.Case) || tpe.typeSymbol.flags.is(Flags.Enum)
                then deriveEqCaseClass[A]
                else
                    report.errorAndAbort(
                      s"Cannot derive Eq for ${tpe.typeSymbol.fullName} which is not a case class, enum, or sealed trait"
                    )
            else deriveEqSumType[A]
        else
            report.errorAndAbort(
              s"Cannot derive Eq for ${tpe.typeSymbol.fullName}"
            )
    }

    private def deriveEqCaseClass[A: Type](using Quotes): Expr[Eq[A]] = {
        import quotes.reflect.*

        val tpeA = TypeRepr.of[A]
        val classSym = TypeTree.of[A].symbol
        val caseFields = classSym.caseFields

        if caseFields.isEmpty then '{ (_: A, _: A) => true }
        else
            // Use direct field access instead of pattern matching with unapply
            // This generates: lhs.field1 === rhs.field1 && lhs.field2 === rhs.field2 && ...
            '{ (lhs: A, rhs: A) =>
                ${
                    val comparisons = caseFields.map { field =>
                        // Use memberType to get the field type as seen from type A
                        // This avoids path-dependent type issues (Datum.this.x vs lhs.x)
                        val fieldType = tpeA.memberType(field).widen.dealias
                        fieldType.asType match
                            case '[t] =>
                                Expr.summon[Eq[t]] match
                                    case None =>
                                        report.errorAndAbort(
                                          s"Could not find given Eq[${fieldType.show}] for field ${field.name} in ${TypeRepr.of[A].show}"
                                        )
                                    case Some(eq) =>
                                        val lhsField =
                                            Select.unique('lhs.asTerm, field.name).asExprOf[t]
                                        val rhsField =
                                            Select.unique('rhs.asTerm, field.name).asExprOf[t]
                                        '{ $eq($lhsField, $rhsField) }
                    }
                    comparisons.reduceLeft((a, b) => '{ $a && $b })
                }
            }
    }

    private def deriveEqSumType[A: Type](using Quotes): Expr[Eq[A]] = {
        import quotes.reflect.*

        val typeSymbol = TypeRepr.of[A].widen.dealias.typeSymbol
        val children = typeSymbol.children

        def createBindings(params: scala.List[(String, TypeRepr)], prefix: String) =
            params.map { (name, tpe) =>
                (Symbol.newBind(Symbol.noSymbol, s"${prefix}_$name", Flags.EmptyFlags, tpe), tpe)
            }

        def genComparisons(
            lhsBinds: scala.List[(Symbol, TypeRepr)],
            rhsBinds: scala.List[(Symbol, TypeRepr)],
            contextName: String
        ): Expr[Boolean] = {
            if lhsBinds.isEmpty then '{ true }
            else
                val comps = lhsBinds.zip(rhsBinds).map { case ((lSym, tpe), (rSym, _)) =>
                    tpe.widen.asType match
                        case '[t] =>
                            Expr.summon[Eq[t]] match
                                case None =>
                                    report.errorAndAbort(
                                      s"Could not find given Eq[${tpe.widen.show}] for field in $contextName"
                                    )
                                case Some(eq) =>
                                    val l = Ident(lSym.termRef).asExprOf[t]
                                    val r = Ident(rSym.termRef).asExprOf[t]
                                    '{ $eq($l, $r) }
                }
                comps.reduceLeft((a, b) => '{ $a && $b })
        }

        def genCaseForChild(rhs: Expr[A], childSym: Symbol): CaseDef = {
            val caseFields = childSym.caseFields
            val isSealed = childSym.flags.is(Flags.Sealed)
            val isCaseClass = childSym.flags.is(Flags.Case)

            if isSealed && !isCaseClass then
                // Child is a sealed trait - delegate to its Eq instance
                val childType = childSym.typeRef
                childType.asType match
                    case '[t] =>
                        Expr.summon[Eq[t]] match
                            case None =>
                                report.errorAndAbort(
                                  s"Could not find given Eq[${childType.show}] for sealed trait child of ${TypeRepr.of[A].show}. " +
                                      s"Define Eq for ${childType.show} separately."
                                )
                            case Some(eqInstance) =>
                                val lhsBind =
                                    Symbol.newBind(
                                      Symbol.noSymbol,
                                      "lhs_child",
                                      Flags.EmptyFlags,
                                      childType
                                    )
                                val rhsBind =
                                    Symbol.newBind(
                                      Symbol.noSymbol,
                                      "rhs_child",
                                      Flags.EmptyFlags,
                                      childType
                                    )

                                val lhsPattern =
                                    Bind(lhsBind, Typed(Wildcard(), TypeTree.ref(childSym)))
                                val rhsPattern =
                                    Bind(rhsBind, Typed(Wildcard(), TypeTree.ref(childSym)))

                                val eqCall = '{
                                    $eqInstance(
                                      ${ Ident(lhsBind.termRef).asExprOf[t] },
                                      ${ Ident(rhsBind.termRef).asExprOf[t] }
                                    )
                                }

                                val rhsMatch = Match(
                                  rhs.asTerm,
                                  scala.List(
                                    CaseDef(rhsPattern, None, eqCall.asTerm),
                                    CaseDef(Wildcard(), None, Literal(BooleanConstant(false)))
                                  )
                                )

                                CaseDef(lhsPattern, None, rhsMatch)
            else if caseFields.isEmpty && (childSym.flags.is(Flags.Module) ||
                    childSym.companionModule.methodMember("unapply").isEmpty)
            then
                // Singleton: case object or enum case without fields
                val childIdent = Ident(childSym.termRef)

                val rhsMatch = Match(
                  rhs.asTerm,
                  scala.List(
                    CaseDef(childIdent, None, Literal(BooleanConstant(true))),
                    CaseDef(Wildcard(), None, Literal(BooleanConstant(false)))
                  )
                )

                CaseDef(childIdent, None, rhsMatch)
            else
                // Case class with fields - use unapply pattern matching
                // (direct field access doesn't work due to path-dependent type issues
                // with intersection types in type test patterns)
                val companionModuleRef = childSym.companionModule
                val unapplyRef = companionModuleRef.methodMember("unapply").head.termRef
                val constr = childSym.primaryConstructor
                val params = constr.paramSymss.flatten.filter(_.isTerm)
                val paramsNameType = params.map(p => p.name -> p.typeRef)

                val lhsBindings = createBindings(paramsNameType, "lhs")
                val rhsBindings = createBindings(paramsNameType, "rhs")
                val lhsPatterns = lhsBindings.map((s, _) => Bind(s, Wildcard()))
                val rhsPatterns = rhsBindings.map((s, _) => Bind(s, Wildcard()))
                val comparisons =
                    genComparisons(lhsBindings, rhsBindings, childSym.fullName.toString)

                val childTypeRef = Inferred(TypeTree.ref(childSym).tpe)

                val rhsMatch = Match(
                  rhs.asTerm,
                  scala.List(
                    CaseDef(
                      Typed(
                        Unapply(Ident(unapplyRef), Nil, rhsPatterns).asInstanceOf[Term],
                        childTypeRef
                      ),
                      None,
                      comparisons.asTerm
                    ),
                    CaseDef(Wildcard(), None, Literal(BooleanConstant(false)))
                  )
                )

                CaseDef(
                  Typed(
                    Unapply(Ident(unapplyRef), Nil, lhsPatterns).asInstanceOf[Term],
                    childTypeRef
                  ),
                  None,
                  rhsMatch
                )
        }

        '{ (lhs: A, rhs: A) =>
            ${
                val cases = children.map(child => genCaseForChild('rhs, child))
                Match('lhs.asTerm, cases).asExprOf[Boolean]
            }
        }
    }
}
