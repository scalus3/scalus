package scalus.uplc.builtin

import scala.quoted.*

private object ToDataMacros {

    import scalus.uplc.builtin.internal.UplcReprMacroUtils.{getUplcRepr, resolveOpaqueAlias}

    /** Summon ToData for a field type, falling back to translucentSuperType for opaque types. */
    private def summonToDataForField(using
        Quotes
    )(
        binding: quotes.reflect.Symbol,
        tpe: quotes.reflect.TypeRepr,
        contextTypeName: String
    ): Expr[Data] = {
        import quotes.reflect.*
        val resolvedTpe = tpe.widen.dealias
        resolvedTpe.asType match
            case '[t] =>
                Expr.summon[ToData[t]] match
                    case Some(toData) =>
                        val arg = Ident(binding.termRef).asExprOf[t]
                        '{ $toData($arg) }
                    case None =>
                        // Try resolving opaque type via translucentSuperType
                        val opaqueTpe = resolveOpaqueAlias(resolvedTpe)
                        opaqueTpe.asType match
                            case '[u] =>
                                Expr.summon[ToData[u]] match
                                    case Some(toData) =>
                                        // Cast the opaque-typed binding to the underlying type
                                        val arg = Ident(binding.termRef).asExpr
                                        '{ $toData($arg.asInstanceOf[u]) }
                                    case None =>
                                        report.errorAndAbort(
                                          s"Could not find given ToData[${resolvedTpe.show}] within $contextTypeName"
                                        )
    }

    def toDataImpl[A: Type](using Quotes): Expr[ToData[A]] = {
        '{ (a: A) =>
            ${ generateToDataApply[A]('a) }
        }
    }

    private def generateToDataApply[A: Type](a: Expr[A])(using Quotes): Expr[Data] = {
        import quotes.reflect.*
        // TODO:  SIRValue(SIRType[A]).generateToDataApply(a)
        val tpe = TypeRepr.of[A].dealias.widen
        if tpe <:< TypeRepr.of[AnyRef] then
            val typeArgs = tpe.typeArgs
            val children = tpe.typeSymbol.children
            if children.isEmpty then
                if tpe.typeSymbol.flags.is(Flags.Trait) then
                    report.errorAndAbort(
                      s"Cannot derive ToData for trait ${tpe.typeSymbol.fullName}"
                    )
                else if tpe.typeSymbol.flags.is(Flags.Case) || tpe.typeSymbol.flags.is(Flags.Enum)
                then
                    getUplcRepr[A] match
                        case Some("ProductCaseOneElement") =>
                            deriveToDataProductCaseOneElement[A](a)
                        case Some("ProductCase") | None =>
                            val constrIndex = findADTConstrIndex[A]
                            deriveToDataCaseClassApply[A](a, constrIndex)
                        case Some(other) =>
                            report.errorAndAbort(
                              s"@UplcRepr($other) is not supported by ToData.derived for ${tpe.typeSymbol.fullName}. Please provide your own ToData instance."
                            )
                else
                    report.errorAndAbort(
                      s"Cannot derive ToData for ${tpe.typeSymbol.fullName} which is not a case class or enum"
                    )
            else
                getUplcRepr[A] match
                    case Some("SumCase") | None =>
                        deriveToDataSumCaseClassApply[A](a)
                    case Some(other) =>
                        report.errorAndAbort(
                          s"@UplcRepr($other) is not supported by ToData.derived for enum ${tpe.typeSymbol.fullName}. Please provide your own ToData instance."
                        )
        else
            report.errorAndAbort(
              s"Cannot derive ToData for ${tpe.typeSymbol.fullName}"
            )
    }

    private def deriveToDataProductCaseOneElement[A: Type](a: Expr[A])(using
        Quotes
    ): Expr[Data] = {
        import quotes.reflect.*
        val classSym = TypeTree.of[A].symbol
        val constr = classSym.primaryConstructor
        val params = constr.paramSymss.flatten
        if params.size != 1 then
            report.errorAndAbort(
              s"@UplcRepr(ProductCaseOneElement) requires exactly one constructor parameter, but ${classSym.fullName} has ${params.size}"
            )
        val param = params.head
        val tpe = param.termRef.widen.dealias
        tpe.asType match
            case '[t] =>
                Expr.summon[ToData[t]] match
                    case None =>
                        report.errorAndAbort(
                          s"Could not find given ToData[${tpe.show}] for @UplcRepr(ProductCaseOneElement) derivation of ${TypeRepr.of[A].show}"
                        )
                    case Some(toDataInner) =>
                        val fieldAccess =
                            Select(a.asTerm, classSym.fieldMember(param.name)).asExprOf[t]
                        '{ $toDataInner($fieldAccess) }
    }

    private def deriveToDataCaseClassApply[A: Type](
        a: Expr[A],
        constrIdx: Int
    )(using quotes0: Quotes): Expr[Data] = {
        import quotes.reflect.*

        val classSym = TypeTree.of[A].symbol
        val companionModuleRef = classSym.companionModule
        val unapplyRef = companionModuleRef.methodMember("unapply").head.termRef
        val constr = classSym.primaryConstructor
        val params = constr.paramSymss.flatten
        val paramsNameType = params.map(p => p.name -> p.termRef.widen)

        /*
        Generate a pattern match to introduce all the params,
        to avoid a.field1, a.field2, etc.
        Something ike:
          a match
            case A(field1, field2, ...) =>
              constrData(
                BigInt($constrIdx),
                mkCons(field1.toData, mkCons(field2.toData, ...))
              )
         */
        def genMatch(prodTerm: Term, params: scala.List[(String, TypeRepr)])(using Quotes) = {
            val bindingsSymbols = params.map { (name, tpe) =>
                (Symbol.newBind(Symbol.noSymbol, name, Flags.EmptyFlags, tpe), tpe)
            }

            val bindings = bindingsSymbols.map { (symbol, _) =>
                Bind(symbol, Wildcard())
            }
            val rhs = genRhs(bindingsSymbols).asTerm
            val uncheckedProdTerm = annotateUnchecked(using quotes0)(prodTerm)
            Match(
              uncheckedProdTerm,
              scala.List(CaseDef(Unapply(Ident(unapplyRef), Nil, bindings), None, rhs))
            )
        }

        def genRhs(bindings: scala.List[(Symbol, TypeRepr)])(using Quotes) = '{
            Builtins.constrData(
              BigInt(${ Expr(constrIdx) }),
              ${
                  val args = bindings
                      .map { case (binding, tpe) =>
                          summonToDataForField(using quotes0)(binding, tpe, TypeRepr.of[A].show)
                      }
                  args.foldRight('{ scalus.uplc.builtin.Builtins.mkNilData() }) { (data, acc) =>
                      '{ scalus.uplc.builtin.Builtins.mkCons($data, $acc) }
                  }
              }
            )
        }

        genMatch(a.asTerm, paramsNameType).asExprOf[Data]

    }

    private def deriveToDataSumCaseClassApply[A: Type](
        value: Expr[A]
    )(using quotes0: Quotes): Expr[Data] = {
        import quotes.reflect.*
        val tpe = TypeRepr.of[A].dealias.widen
        val originTpe = tpe
        val children = tpe.typeSymbol.children
        val constrTpe = TypeRepr.of[A]
        val typeSymbol = TypeRepr.of[A].widen.dealias.typeSymbol

        def genRhs(constrIdx: Int, bindings: scala.List[(Symbol, TypeRepr)])(using Quotes) = '{
            Builtins.constrData(
              BigInt(${ Expr(constrIdx) }),
              ${
                  val args = bindings
                      .map { case (binding, atpe) =>
                          summonToDataForField(using quotes0)(binding, atpe, originTpe.widen.show)
                      }
                  args.foldRight('{ scalus.uplc.builtin.Builtins.mkNilData() }) { (data, acc) =>
                      '{ scalus.uplc.builtin.Builtins.mkCons($data, $acc) }
                  }
              }
            )
        }

        def genMatch(prodTerm: Term)(using Quotes) = {
            val cases = typeSymbol.children.zipWithIndex.map { (childTypeSymbol, tag) =>
                if childTypeSymbol.caseFields.isEmpty then
                    val rhs = genRhs(tag, Nil).asTerm
                    CaseDef(Ident(childTypeSymbol.termRef), None, rhs)
                else
                    val classSym = childTypeSymbol
                    val companionModuleRef = classSym.companionModule
                    val unapplyRef = companionModuleRef.methodMember("unapply").head.termRef
                    val constr = classSym.primaryConstructor
                    val constrNudeType = TypeIdent(classSym).tpe
                    val params = constr.paramSymss.flatten

                    val paramsNameType = params.map { p =>
                        p.name -> p.termRef.widen
                    }
                    val bindingsSymbols = paramsNameType.map { (name, tpe) =>
                        (Symbol.newBind(Symbol.noSymbol, name, Flags.EmptyFlags, tpe), tpe)
                    }

                    val bindings = bindingsSymbols.map { (symbol, _) =>
                        Bind(symbol, Wildcard())
                    }
                    val rhs = genRhs(tag, bindingsSymbols).asTerm

                    // bug in scalac:  TypeTree.ref not set position, which later trigger assertion.
                    //  Inferred set position to the position of macro-expansion.
                    val childTypeRef = Inferred(TypeTree.ref(childTypeSymbol).tpe)
                    CaseDef(
                      Typed(
                        Unapply(Ident(unapplyRef), Nil, bindings).asInstanceOf[Term],
                        childTypeRef
                      ),
                      None,
                      rhs
                    )
            }
            val uncheckedProdTerm = annotateUnchecked(using quotes0)(prodTerm)
            val m = Match(uncheckedProdTerm, cases)
            m
        }

        genMatch(value.asTerm).asExprOf[Data]
    }

    /** Find the index of the given type constructor in the ADT. 0 if this index is not a giving
      * type hierarchy.
      *
      * <pre> trait A; case class B(x: Int) extends A; case class C(x: Int) extends A </pre> Here
      * `A.childs` - > List(B, C) and `findADTConstrIndex[A]` will return 0. `findADTConstrIndex[B]`
      * will return 1
      */
    private def findADTConstrIndex[A: Type](using Quotes): Int = {
        import quotes.reflect.*
        val at = TypeRepr.of[A]
        val baseClasses = at.baseClasses
        val sealedBaseClasses = baseClasses.filter(_.flags.is(Flags.Sealed))
        if sealedBaseClasses.isEmpty then 0
        else
            val firstSealedBaseClass = sealedBaseClasses.head
            val otherSealedBaseClasses = sealedBaseClasses.tail
            if otherSealedBaseClasses.nonEmpty then
                report.errorAndAbort(
                  s"Cannot derive ToData for ${Type.show[A]} which is not a case class or enum"
                )
            else
                val myIndex = firstSealedBaseClass.children.indexWhere(_ == at.typeSymbol)
                if myIndex == -1 then
                    report.errorAndAbort(
                      s"Cannot derive ToData for ${Type.show[A]} (can't find index of constructor in ${firstSealedBaseClass})"
                    )
                else myIndex
    }

    private def annotateUnchecked(using
        q: Quotes
    )(prodTerm: q.reflect.Term): q.reflect.Term = {
        import q.reflect.*
        val uncheckedAnn: Term = New(TypeIdent(Symbol.requiredClass("scala.unchecked")))
        val annotatedType = Annotated(Inferred(prodTerm.tpe), uncheckedAnn)
        Typed(prodTerm, annotatedType)
    }

}
