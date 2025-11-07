package scalus.builtin

import scala.quoted.*

private object ToDataMacros {

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
                    val constrIndex = findADTConstrIndex[A]
                    deriveToDataCaseClassApply[A](a, constrIndex)
                else
                    report.errorAndAbort(
                      s"Cannot derive ToData for ${tpe.typeSymbol.fullName} which is not a case class or enum"
                    )
            else deriveToDataSumCaseClassApply[A](a)
        else
            report.errorAndAbort(
              s"Cannot derive ToData for ${tpe.typeSymbol.fullName}"
            )
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
        val paramsNameType = params.map(p => p.name -> p.typeRef)

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
                          tpe.widen.asType match
                              case '[t] =>
                                  Expr.summon[ToData[t]] match
                                      case None =>
                                          report.errorAndAbort(
                                            s"Could not find given ToData[${tpe.widen.show}] within ${TypeRepr.of[A].show} when generate product"
                                          )
                                      case Some(toData) =>
                                          val arg = Ident(binding.termRef).asExprOf[t]
                                          '{ $toData($arg) }
                      }
                  args.foldRight('{ scalus.builtin.Builtins.mkNilData() }) { (data, acc) =>
                      '{ scalus.builtin.Builtins.mkCons($data, $acc) }
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
                          atpe.asType match
                              case '[t] =>
                                  Expr.summon[ToData[t]] match
                                      case None =>
                                          println(
                                            s"atpe=${atpe.show}, atpe.widen=${atpe.widen.show}, atpe."
                                          )
                                          println(
                                            s"constrIdex = ${constrIdx}, binding = ${binding}, bindings = ${bindings}"
                                          )
                                          report.errorAndAbort(
                                            s"Could not find given ToData[${atpe.widen.show}] within ${originTpe.widen.show} when generating sum"
                                          )
                                      case Some(toData) =>
                                          val arg = Ident(binding.termRef).asExprOf[t]
                                          '{ $toData($arg) }.asExprOf[Data]
                      }
                  args.foldRight('{ scalus.builtin.Builtins.mkNilData() }) { (data, acc) =>
                      '{ scalus.builtin.Builtins.mkCons($data, $acc) }
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
                    // val paramTypes = constrNudeType.memberType(constr) match
                    //    case PolyType(_, tpe) => tpe
                    //    case tpe              => tpe
                    // }
                    val params = constr.paramSymss.flatten

                    // println(s"constr = ${constr}, typeParams=${typeParams}, params = ${params}")
                    val paramsNameType = params.map { p =>
                        // p.name -> p.info
                        p.name -> p.typeRef
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
