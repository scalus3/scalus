package scalus.uplc.builtin

import scala.quoted.*

private object FromDataMacros {

    def fromDataImpl[A: Type](using Quotes): Expr[FromData[A]] = {
        import quotes.reflect.*
        val ta = TypeRepr.of[A].dealias.widen
        if ta <:< TypeRepr.of[AnyRef] then
            val children = ta.typeSymbol.children
            val retval =
                if children.isEmpty then
                    if ta.typeSymbol.flags.is(Flags.Trait) then
                        report.errorAndAbort(
                          s"Cannot derive FromData for trait ${ta.typeSymbol.fullName}"
                        )
                    else if ta.typeSymbol.flags.is(Flags.Case) || ta.typeSymbol.flags.is(Flags.Enum)
                    then {
                        deriveFromDataCaseClassApply[A]
                    } else {
                        report.errorAndAbort(
                          s"Cannot derive FromData for ${ta.typeSymbol.fullName} which is not a case class or enum"
                        )
                    }
                else deriveFromDataSumCaseClassApply[A]
            // println(s"fromDataImpl for ${Type.show[A]}:\n${retval.show}")
            retval
        else
            report.errorAndAbort(
              s"Cannot derive FromData for ${ta.typeSymbol.fullName} which is not a case class or enum"
            )
    }

    private def deriveFromDataCaseClassApply[A: Type](using Quotes): Expr[FromData[A]] = {
        '{ (d: Data) =>
            d match
                case Data.Constr(_, args) =>
                    // generate f = (args) => new Constructor(args.head, args.tail.head, ...)
                    // then apply to args: f(args)
                    // and finally beta reduce it in compile time
                    ${ Expr.betaReduce('{ ${ deriveFromDataCaseClassConstructor[A] }(args) }) }
                case _ => throw new Exception("Expected Constr")
        }
    }

    private[scalus] def deriveFromDataCaseClassConstructor[T: Type](using
        Quotes
    ): Expr[scalus.uplc.builtin.BuiltinList[Data] => T] = {
        import quotes.reflect.*
        val classSym = TypeTree.of[T].symbol
        val constr = classSym.primaryConstructor
        val params = constr.paramSymss.flatten
        val fromDataOfArgs = params.map { param =>
            val tpe = param.termRef.widen.dealias
            tpe.asType match
                case '[t] =>
                    Expr.summon[FromData[t]] match
                        case None =>
                            report.errorAndAbort(
                              s"Could not find given FromData[${tpe.show}] in case class constructor generation for ${TypeRepr.of[T].show}]"
                            )
                        case Some(value) => value
        }

        def genConstructorCall(
            a: Expr[scalus.uplc.builtin.BuiltinList[scalus.uplc.builtin.Data]]
        )(using Quotes): Expr[T] = {
            import quotes.reflect.*

            def rec(
                lst: Expr[scalus.uplc.builtin.BuiltinList[Data]],
                fromDatas: List[Expr[Any]],
                ps: List[Symbol],
                args: List[Term]
            ): Expr[T] = {
                ps match
                    case Nil =>
                        New(TypeTree.of[T]).select(constr).appliedToArgs(args.reverse).asExprOf[T]
                    case p :: ptails =>
                        val appl = fromDatas.head
                        val tpe = p.termRef.widen.dealias
                        tpe.asType match
                            case '[t] =>
                                '{
                                    val h = $lst.head
                                    val t = $lst.tail
                                    ${
                                        rec(
                                          '{ t },
                                          fromDatas.tail,
                                          ptails,
                                          '{ ${ appl.asExprOf[FromData[t]] }(h) }.asTerm :: args
                                        )
                                    }
                                }
            }
            rec(a, fromDataOfArgs, params, Nil)
        }

        '{ (args: scalus.uplc.builtin.BuiltinList[scalus.uplc.builtin.Data]) =>
            ${ genConstructorCall('{ args }) }
        }

    }

    private def deriveFromDataSumCaseClassApply[A: Type](using Quotes): Expr[FromData[A]] = {
        import quotes.reflect.*
        val typeSymbol = TypeRepr.of[A].widen.dealias.typeSymbol
        if !typeSymbol.flags.is(Flags.Enum) then
            report.errorAndAbort(
              s"derived can only be used with enums or sealed hierarchy of type classes ${typeSymbol.fullName}"
            )

        val mappingRhs: scala.List[Expr[scalus.uplc.builtin.BuiltinList[Data] => A]] =
            typeSymbol.children.map { child =>
                child.typeRef.asType match
                    case '[t] =>
                        // println(s"child: ${child}, ${child.flags.show} ${child.caseFields}")
                        if child.caseFields.isEmpty then
                            '{ (_: scalus.uplc.builtin.BuiltinList[Data]) =>
                                ${ Ident(child.termRef).asExprOf[t] }
                            }.asExprOf[scalus.uplc.builtin.BuiltinList[Data] => A]
                        else {
                            deriveFromDataCaseClassConstructor[t]
                                .asExprOf[scalus.uplc.builtin.BuiltinList[Data] => A]
                        }
                    case _ =>
                        report.errorAndAbort(
                          s"Cannot derive FromData for ${child.typeRef.show} "
                        )

            }
        // .asInstanceOf[scala.List[(Expr[scalus.uplc.builtin.BuiltinList[Data] => A], Int)]]

        // stage programming is cool, but it's hard to comprehend what's going on
        val typeA: String = Type.show[A]
        '{ (d: Data) =>
            d match
                case Data.Constr(tag, args) =>
                    ${
                        import quotes.reflect.*
                        val matchCases = mappingRhs.zipWithIndex.map { case (code, t) =>
                            CaseDef(Literal(IntConstant(t)), None, '{ $code(args) }.asTerm)
                        }
                        val defaultCase =
                            CaseDef(
                              Wildcard(),
                              None,
                              '{ throw new Exception("Invalid tag") }.asTerm
                            )
                        Match('{ tag }.asTerm, matchCases :+ defaultCase).asExprOf[A]
                    }
                case _ => throw new Exception("Expected Constr")
        }

    }

}
