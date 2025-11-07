package scalus.builtin

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
            val args = scalus.builtin.Builtins.unConstrData(d).snd

            // generate f = (args) => new Constructor(args.head, args.tail.head, ...)
            // then apply to args: f(args)
            // and finally beta reduce it in compile time
            ${ Expr.betaReduce('{ ${ deriveFromDataCaseClassConstructor[A] }(args) }) }
        }
    }

    private[scalus] def deriveFromDataCaseClassConstructor[T: Type](using
        Quotes
    ): Expr[scalus.builtin.BuiltinList[Data] => T] = {
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

        def genGetter(init: Expr[scalus.builtin.BuiltinList[Data]], idx: Int)(using
            Quotes
        ): Expr[Data] =
            var expr = init
            var i = 0
            while i < idx do
                val exp = expr // save the current expr, otherwise it will loop forever
                expr = '{ $exp.tail }
                i += 1
            '{ $expr.head }

        def genConstructorCall(
            a: Expr[scalus.builtin.BuiltinList[scalus.builtin.Data]]
        )(using Quotes): Expr[T] = {
            val args = fromDataOfArgs.zipWithIndex.map { case (appl, idx) =>
                val arg = genGetter(a, idx)
                '{ $appl($arg) }.asTerm
            }
            // Couldn't find a way to do this using quotes, so just construct the tree manually
            New(TypeTree.of[T]).select(constr).appliedToArgs(args).asExprOf[T]
        }

        '{ (args: scalus.builtin.BuiltinList[scalus.builtin.Data]) =>
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

        val mappingRhs: scala.List[Expr[scalus.builtin.BuiltinList[Data] => A]] =
            typeSymbol.children.map { child =>
                child.typeRef.asType match
                    case '[t] =>
                        // println(s"child: ${child}, ${child.flags.show} ${child.caseFields}")
                        if child.caseFields.isEmpty then
                            '{ (_: scalus.builtin.BuiltinList[Data]) =>
                                ${ Ident(child.termRef).asExprOf[t] }
                            }.asExprOf[scalus.builtin.BuiltinList[Data] => A]
                        else {
                            deriveFromDataCaseClassConstructor[t]
                                .asExprOf[scalus.builtin.BuiltinList[Data] => A]
                        }
                    case _ =>
                        report.errorAndAbort(
                          s"Cannot derive FromData for ${child.typeRef.show} "
                        )

            }
        // .asInstanceOf[scala.List[(Expr[scalus.builtin.BuiltinList[Data] => A], Int)]]

        // stage programming is cool, but it's hard to comprehend what's going on
        val typeA: String = Type.show[A]
        '{ (d: Data) =>
            val pair = Builtins.unConstrData(d)
            val tag = pair.fst
            val args = pair.snd
            ${
                mappingRhs.zipWithIndex.foldRight('{
                    throw new Exception("Invalid tag")
                }.asExprOf[A]) { case ((code, t), acc) =>
                    '{
                        if Builtins.equalsInteger(tag, BigInt(${ Expr(t) })) then $code(args)
                        else $acc
                    }
                }
            }
        }

    }

}
