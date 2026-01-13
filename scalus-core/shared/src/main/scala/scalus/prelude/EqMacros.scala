package scalus.prelude

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

        val classSym = TypeTree.of[A].symbol

        if classSym.caseFields.isEmpty then '{ (_: A, _: A) => true }
        else
            val companionModuleRef = classSym.companionModule
            val unapplyRef = companionModuleRef.methodMember("unapply").head.termRef
            val constr = classSym.primaryConstructor
            val params = constr.paramSymss.flatten.filter(_.isTerm)
            val paramsNameType = params.map(p => p.name -> p.typeRef)

            def createBindings(prefix: String) = paramsNameType.map { (name, tpe) =>
                (Symbol.newBind(Symbol.noSymbol, s"${prefix}_$name", Flags.EmptyFlags, tpe), tpe)
            }

            def genComparisons(
                lhsBinds: scala.List[(Symbol, TypeRepr)],
                rhsBinds: scala.List[(Symbol, TypeRepr)]
            ): Expr[Boolean] = {
                val comps = lhsBinds.zip(rhsBinds).map { case ((lSym, tpe), (rSym, _)) =>
                    tpe.widen.asType match
                        case '[t] =>
                            Expr.summon[Eq[t]] match
                                case None =>
                                    report.errorAndAbort(
                                      s"Could not find given Eq[${tpe.widen.show}] for field in ${TypeRepr.of[A].show}"
                                    )
                                case Some(eq) =>
                                    val l = Ident(lSym.termRef).asExprOf[t]
                                    val r = Ident(rSym.termRef).asExprOf[t]
                                    '{ $eq($l, $r) }
                }
                comps.reduceLeft((a, b) => '{ $a && $b })
            }

            '{ (lhs: A, rhs: A) =>
                ${
                    val lhsBindings = createBindings("lhs")
                    val rhsBindings = createBindings("rhs")
                    val lhsPatterns = lhsBindings.map((s, _) => Bind(s, Wildcard()))
                    val rhsPatterns = rhsBindings.map((s, _) => Bind(s, Wildcard()))
                    val comparisons = genComparisons(lhsBindings, rhsBindings)

                    val rhsMatch = Match(
                      'rhs.asTerm,
                      scala.List(
                        CaseDef(
                          Unapply(Ident(unapplyRef), Nil, rhsPatterns),
                          None,
                          comparisons.asTerm
                        )
                      )
                    )

                    Match(
                      'lhs.asTerm,
                      scala.List(
                        CaseDef(Unapply(Ident(unapplyRef), Nil, lhsPatterns), None, rhsMatch)
                      )
                    ).asExprOf[Boolean]
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
            else if caseFields.isEmpty then
                // Singleton (case object or parameterless case)
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
                // Case class with fields
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
