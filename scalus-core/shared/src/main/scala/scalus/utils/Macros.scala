package scalus.utils

import scalus.uplc.builtin.Builtins
import scalus.uplc.builtin.Data
import scalus.cardano.ledger.{ProtocolParams, ProtocolParamsToExpr}
import scalus.compiler.sir.SIR
import scalus.uplc.{BuiltinRuntime, CardanoBuiltins, DefaultFun, Expr as Exp, NamedDeBruijn, Term as Trm}
import scalus.uplc.ExprBuilder.*

import java.io.File
import java.nio.file.*
import scala.quoted.*
import scala.annotation.nowarn
import scala.collection.mutable.ListBuffer

object Macros {

    /** Converts a quoted lambda value of type Exp[A] => Exp[B] into a quoted UPLC lambda expression
      * of type Exp[A => B].
      *
      * This macro extracts the parameter name from the provided lambda and creates a Trm.LamAbs
      * term wrapping the body. It expects a simple lambda value (e.g. lam(x => ...)).
      *
      * @tparam A
      *   the input Exp type
      * @tparam B
      *   the output Exp type
      * @param f
      *   quoted function value of type Exp[A] => Exp[B]
      * @return
      *   a quoted Exp representing a UPLC lambda of A => B
      * @note
      *   aborts compilation if the supplied expression does not match the expected lambda shape
      */
    @nowarn
    def lamMacro[A: Type, B: Type](f: Expr[Exp[A] => Exp[B]])(using Quotes): Expr[Exp[A => B]] =
        import quotes.reflect.*
        val name = f.asTerm match
            // lam(x => body)
            case Inlined(
                  _,
                  _,
                  Block(List(DefDef(_, List(List(ValDef(name, _, _))), _, body)), _)
                ) =>
                Expr(name)
            // lam { x => body }
            case Inlined(
                  _,
                  _,
                  Block(List(), Block(List(DefDef(_, List(List(ValDef(name, _, _))), _, body)), _))
                ) =>
                Expr(name)
            case x => report.errorAndAbort(x.toString)
        '{
            Exp(Trm.LamAbs($name, $f(vr($name)).term))
        }

    /** Converts a lambda value of type [[scalus.uplc.Term]] => [[scalus.uplc.Term]] into a UPLC
      * [[scalus.uplc.Term.LamAbs]] term expression.
      *
      * This macro extracts the parameter name from the provided lambda and creates a
      * [[scalus.uplc.Term.LamAbs]] term wrapping the body. It expects a simple lambda value (e.g.
      * lam(x => x)).
      *
      * @param f
      *   quoted lambda expression
      */
    @nowarn
    def lamTermMacro(f: Expr[Trm => Trm])(using Quotes): Expr[Trm] =
        import quotes.reflect.*
        val name = f.asTerm match
            // lam(x => body)
            case Inlined(
                  _,
                  _,
                  Block(List(DefDef(_, List(List(ValDef(name, _, _))), _, body)), _)
                ) =>
                Expr(name)
            // lam { x => body }
            case Inlined(
                  _,
                  _,
                  Block(List(), Block(List(DefDef(_, List(List(ValDef(name, _, _))), _, body)), _))
                ) =>
                Expr(name)
            case x => report.errorAndAbort(x.toString)
        '{
            Trm.LamAbs($name, $f(Trm.Var(NamedDeBruijn($name))))
        }

    /** Create a quoted getter function of type Exp[Data] => Exp[Data] from a field selection lambda
      * (e.g. (_.txInfo.id)).
      *
      * The macro accepts an expression selecting nested case-class fields and generates an
      * expression that, at runtime, extracts the corresponding Data from an Exp[Data] value.
      *
      * Supported input shapes: chain of Select/Ident nodes corresponding to nested fields.
      *
      * @param e
      *   quoted function A => Any that selects a field path (e.g. _.a.b)
      * @tparam A
      *   the root type of the selection
      * @return
      *   quoted function Exp[Data] => Exp[Data] extracting the selected nested field
      * @note
      *   aborts compilation on unsupported shapes or missing fields
      */
    def fieldAsExprDataMacro[A: Type](e: Expr[A => Any])(using
        Quotes
    ): Expr[Exp[Data] => Exp[Data]] =
        import quotes.reflect.*
        e.asTerm match
            case Inlined(
                  _,
                  _,
                  Block(List(DefDef(_, _, _, Some(select @ Select(_, fieldName)))), _)
                ) =>
                def genGetter(
                    typeSymbol: Symbol,
                    fieldName: String
                ): Expr[Exp[Data] => Exp[Data]] =
                    val typeSymbolOfA = typeSymbol.typeRef.dealias.typeSymbol
                    val fields = typeSymbolOfA.caseFields.filter(_.isValDef)
                    val fieldOpt: Option[(Symbol, Int)] =
                        fields.zipWithIndex.find(_._1.name.stripTrailing() == fieldName)
//          report.info(s"$typeSymbolOfA => fieldOpt: $fieldOpt")
                    fieldOpt match
                        case Some((fieldSym: Symbol, idx)) =>
                            val idxExpr = Expr(idx)
                            '{
                                var expr: Exp[Data] => Exp[List[Data]] = d =>
                                    sndPair(unConstrData(d))
                                var i = 0
                                while i < $idxExpr do
                                    val exp =
                                        expr // save the current expr, otherwise it will loop forever
                                    expr = d => tailList(exp(d))
                                    i += 1
                                d => headList(expr(d))
                            }
                        case None =>
                            report.errorAndAbort(
                              s"fieldAsData: can't find a field `$fieldName` in $typeSymbolOfA. Available fields: ${fields
                                      .map(_.name)}"
                            )

                def composeGetters(tree: Tree): Expr[Exp[Data] => Exp[Data]] = tree match
                    case Select(select @ Select(_, _), fieldName) =>
                        val a = genGetter(select.tpe.typeSymbol, fieldName)
                        val b = composeGetters(select)
                        '{ $a compose $b }
                    case Select(ident @ Ident(_), fieldName) =>
                        genGetter(ident.tpe.typeSymbol, fieldName)
                    case _ =>
                        report.errorAndAbort(
                          s"field macro supports only this form: _.caseClassField1.field2, but got " + tree.show
                        )
                composeGetters(select)
            case x => report.errorAndAbort(s"fieldAsExprDataMacro: $x")

    /** Build a runtime Data => Data getter from a field-selection expression.
      *
      * This is a convenience wrapper that forwards to the term-level implementation. Accepts a
      * quoted lambda selecting nested fields of A and returns a plain function that reads nested
      * Data values.
      *
      * @param e
      *   quoted selection A => Any
      * @tparam A
      *   root type of selection
      * @return
      *   a Data => Data function performing the extraction
      */
    def fieldAsDataMacro[A: Type](e: Expr[A => Any])(using Quotes): Expr[Data => Data] =
        import quotes.reflect.*
        fieldAsDataMacroTerm(e.asTerm)

    /** Term-level implementation of the field-as-Data getter macro.
      *
      * This function performs the reflection on the quoted Term and builds an Expr[Data => Data]
      * that navigates the builtin Data representation according to the nested selects in `e`.
      *
      * Supported shapes are nested Select nodes; other shapes will abort compilation.
      *
      * @param e
      *   reflected term representing the selection
      * @return
      *   Expr[Data => Data] that extracts the nested field
      * @note
      *   aborts compilation on unsupported tree shapes or missing fields
      */
    def fieldAsDataMacroTerm(using q: Quotes)(e: q.reflect.Term): Expr[Data => Data] =
        import quotes.reflect.*
        e match
            case Inlined(_, _, block) => fieldAsDataMacroTerm(block)
            case Block(List(DefDef(_, _, _, Some(select @ Select(_, fieldName)))), _) =>
                def collectSymbolsAndFields(tree: Tree): List[(Symbol, String)] = tree match
                    case Select(select @ Select(_, _), fieldName) =>
                        collectSymbolsAndFields(
                          select
                        ) :+ (select.tpe.widen.dealias.typeSymbol, fieldName)
                    case Select(ident @ Ident(_), fieldName) =>
                        List((ident.tpe.widen.dealias.typeSymbol, fieldName))
                    case _ =>
                        report.errorAndAbort(
                          s"field macro supports only this form: _.caseClassField1.field2, but got " + tree.show
                        )

                /* I tried to optimize the code below, but it's not worth it.
                  1. Reuse val double = (d: builtin.BuiltinList[Data]) => d.tail.tail.tail.tail (2,3,4,5,6,7 tails)
                  Expression: _.txInfo.id
                  Simple
                  tail 1 budget: { mem: 0.005780, cpu: 1.890490 }
                  tail 2 budget: { mem: 0.007280, cpu: 2.235490 }
                  tail 3 budget: { mem: 0.006980, cpu: 2.166490 }
                  tail 4 budget: { mem: 0.006680, cpu: 2.097490 }
                  tail 5 budget: { mem: 0.006380, cpu: 2.028490 }
                  tail 6 budget: { mem: 0.006380, cpu: 2.028490 }
                  tail 7 budget: { mem: 0.006380, cpu: 2.028490 }
                  Optimized:
                  tail 1 budget: { mem: 0.005580, cpu: 1.844490 }
                  tail 2 budget: { mem: 0.007080, cpu: 2.189490 }
                  tail 4 budget: { mem: 0.006480, cpu: 2.051490 }
                  Optimized & eta-reduced:
                  tail 1 budget: { mem: 0.005580, cpu: 1.844490 }
                  2. Reuse val getargs = (d: Data) => Builtins.unConstrData(d).snd
                  Expression: _.txInfo.id
                  getargs 2 usages
                  Optimized: budget: { mem: 0.006480, cpu: 2.051490 }
                  Expression: _.txInfo.id.hash
                  getargs 0 usages
                  Optimized: budget: { mem: 0.006276, cpu: 2.144366 }
                  getargs 3 usages
                  Optimized: budget: { mem: 0.007476, cpu: 2.420366 }

                  Conclusion: it's not worth it to abstract and reuse. Inlining is better.
                 */

                '{ d =>
                    ${
                        def getField(idx: Int, d: Expr[Data]): Expr[Data] = {
                            var expr = '{ Builtins.unConstrData($d).snd }
                            var i = 0
                            while i < idx do
                                // save the current expr, otherwise it will loop forever
                                val exp = expr
                                expr = '{ $exp.tail }
                                i += 1
                            '{ $expr.head }
                        }

                        var ddd = '{ d }
                        for (typeSymbolOfA, fieldName) <- collectSymbolsAndFields(select) do {
                            val fields = typeSymbolOfA.caseFields.filter(_.isValDef)
                            val fieldOpt: Option[(Symbol, Int)] =
                                // OMG, don't ask me why, but Scala 3.3.3 adds a trailing space to the field name
                                // specifically in the case _1, _2, etc in Tuples.
                                // it's fixed in 3.4.2
                                // FIXME: remove stripTrailing when we upgrade to 3.4.2
                                fields.zipWithIndex.find(_._1.name.stripTrailing == fieldName)
                            // report.info(s"$typeSymbolOfA => ${fields.map(s => s"'${s.name}'")}")
                            fieldOpt match
                                case Some((_: Symbol, idx)) =>
                                    ddd = getField(idx, ddd)
                                case None =>
                                    report.errorAndAbort(
                                      s"""fieldAsData: can't find a field `$fieldName` in $typeSymbolOfA.
                                      |Available fields: ${fields.map(s =>
                                            s"'${s.name}'"
                                        )}""".stripMargin
                                    )
                        }
                        ddd
                    }
                }

            case x => report.errorAndAbort(x.toString)

    /** Derive an upickle ReadWriter[A] for a mutable class-like type whose fields are longs.
      *
      * The macro inspects declared fields of A and generates a readwriter that serializes to
      * ujson.Value and deserializes by constructing a new instance of A and assigning fields from
      * the JSON object.
      *
      * Note: generated code assumes fields are of type Long and that A has a no-arg constructor.
      *
      * @tparam A
      *   the target type for which a ReadWriter is derived
      * @return
      *   an Expr of ReadWriter[A]
      */
    import upickle.default.*
    def mkReadWriterImpl[A: Type](using Quotes): Expr[ReadWriter[A]] = {
        import scala.quoted.*
        import quotes.reflect.*
        val tpe = TypeTree.of[A]
        val fields = tpe.symbol.declaredFields
        val fieldNames = fields.map(_.name)
        val impl = '{
            upickle.default
                .readwriter[ujson.Value]
                .bimap[A](
                  m =>
                      ujson.Obj.from(${
                          Expr.ofList(
                            fields.map(name =>
                                '{
                                    (
                                      ${ Expr(name.name) },
                                      writeJs[Long](${ Select('{ m }.asTerm, name).asExprOf[Long] })
                                    )
                                }
                            )
                          )
                      }),
                  json =>
                      ${
                          val stats = ListBuffer[Statement]()
                          // val params = new A()
                          val value = ValDef(
                            Symbol.newVal(
                              Symbol.spliceOwner,
                              "params",
                              tpe.tpe.widen,
                              Flags.EmptyFlags,
                              Symbol.noSymbol
                            ),
                            Some(New(tpe).select(tpe.symbol.primaryConstructor).appliedToNone)
                          )
                          val ref = Ref(value.symbol)
                          stats += value
                          // params.field1 = read[Long](json.obj("field1"))
                          // ...
                          fields.foreach { field =>
                              stats += Assign(
                                ref.select(field),
                                '{ read[Long](json.obj(${ Expr(field.name.toString) })) }.asTerm
                              )
                          }
                          // { val params = new A(); params.field1 = read[Long](json.obj("field1")); ...; params }
                          Block(stats.toList, ref).asExprOf[A]
                      }
                )
        }
        // println(impl.asTerm.show(using Printer.TreeShortCode))
        impl
    }

    /** Generates a pair of functions to convert a class to a sequence of longs and vice versa. The
      * generated code is equivalent to the following:
      * {{{
      *   (
      *     (m: A) => Seq(m.field1, m.field2, ...),
      *     (seq: Seq[Long]) => {
      *       val params = new A()
      *       params.field1 = if idx < seq.size then seq(0) else 0L
      *       ...
      *       params
      *     }
      *   )
      * }}}
      *
      * @tparam A
      *   the type of the class
      * @return
      *   a pair of functions
      */
    def mkClassFieldsFromSeqIsoImpl[A: Type](using
        Quotes
    ): Expr[(A => Seq[Long], Seq[Long] => A)] = {
        import scala.quoted.*
        import quotes.reflect.*
        val tpe = TypeTree.of[A]
        val fields = tpe.symbol.declaredFields
        val impl = '{
            (
              (m: A) =>
                  ${ Expr.ofSeq(fields.map(name => '{ m }.asTerm.select(name).asExprOf[Long])) },
              (seq: Seq[Long]) =>
                  ${
                      val stmts = ListBuffer[Statement]()
                      // val params = new A()
                      val value = ValDef(
                        Symbol.newVal(
                          Symbol.spliceOwner,
                          "params",
                          tpe.tpe.widen,
                          Flags.EmptyFlags,
                          Symbol.noSymbol
                        ),
                        Some(New(tpe).select(tpe.symbol.primaryConstructor).appliedToNone)
                      )
                      val ref = Ref(value.symbol)
                      stmts += value
                      val size = '{ seq.size }
                      // params.field1 = if idx < seq.size then seq(0) else 0L
                      // ...
                      for (field, index) <- fields.zipWithIndex do
                          val idx = Expr(index)
                          stmts += Assign(
                            ref.select(field),
                            '{ if $idx < $size then seq($idx) else 300_000_000L }.asTerm
                          )

                      // { val params = new A(); params.field1 =...; params }
                      Block(stmts.toList, ref).asExprOf[A]
                  }
            )
        }
        // println(impl.asTerm.show(using Printer.TreeShortCode))
        impl
    }

    /** Read a textual resource from the project sources at compile time.
      *
      * This macro reads the resource file named `name` from the compile-time source root (defaults
      * to src/main/resources) and returns its contents as a quoted String.
      *
      * @param name
      *   quoted resource filename
      * @return
      *   quoted resource contents
      * @throws java.lang.IllegalArgumentException
      *   if the resource file is not found
      */
    def inlineResource(using Quotes)(name: Expr[String]): Expr[String] = {
        val string = readResource(name.value.get)
        Expr(string)
    }

    /** Read a Blockfrost JSON resource at compile time and generate ProtocolParams constructor
      * code.
      *
      * This macro reads the JSON file, parses it into ProtocolParams, and generates Scala code that
      * constructs the ProtocolParams directly. This avoids embedding the JSON string in the
      * compiled artifacts, significantly reducing bundle size for JS targets.
      *
      * @param name
      *   quoted resource filename (e.g., "blockfrost-params-epoch-544.json")
      * @return
      *   quoted ProtocolParams constructor
      */
    def inlineProtocolParams(using Quotes)(name: Expr[String]): Expr[ProtocolParams] = {
        import ProtocolParamsToExpr.given
        val json = readResource(name.value.get)
        val params = ProtocolParams.fromBlockfrostJson(json)
        Expr(params)
    }

    /** Read a resource file from disk using the compile-time source root.
      *
      * This inline helper computes the path to the resource and returns its contents.
      *
      * @param name
      *   the resource file name
      * @param resPath
      *   the resources subpath relative to the source root (default "resources")
      * @return
      *   file contents as string
      * @throws java.lang.IllegalArgumentException
      *   if the file is missing
      */
    inline def readResource(using Quotes)(name: String, resPath: String = "resources"): String = {
        val path = sourcesRoot().resolve(resPath).resolve(name)
        require(Files.exists(path), s"Resource $name is not found on path $path")
        Files.readString(path)
    }

    /** Compute the project's source root Path at compile time.
      *
      * The function locates SourceFile.current.path and searches backward for `srcRoot` to
      * determine the project root. Defaults to "/src/main/".
      *
      * @param srcRoot
      *   path fragment to locate the project source root (default: "/src/main/")
      * @return
      *   Path pointing to the located source root directory
      * @throws java.lang.IllegalArgumentException
      *   if the fragment is not found in current path
      */
    inline def sourcesRoot(using
        Quotes
    )(srcRoot: String = File.separator + "src" + File.separator + "main" + File.separator): Path = {
        val path = quotes.reflect.SourceFile.current.path
        val pos = path.lastIndexOf(srcRoot)
        require(pos > 0, s"Not found source root '$srcRoot' in path '$path'")
        Paths.get(path.substring(0, pos), srcRoot)
    }

    /** Return quoted boolean expression that traces when condition is false.
      *
      * This helper expands to an if-expression that returns true when the input is true; otherwise
      * it calls Builtins.trace with a diagnostic string and returns false.
      *
      * @param x
      *   quoted boolean expression
      * @return
      *   quoted boolean that is identical to x when true, and traces + returns false otherwise
      */
    def questionMark(using Quotes)(x: Expr[Boolean]): Expr[Boolean] = {
        import scala.quoted.*
        '{ if $x then true else Builtins.trace(${ Expr(x.show + " ? False") })(false) }
    }

    /** Generates `match` expression on [[scalus.uplc.DefaultFun]] ordinals that should be
      * efficiently compiled to table switch (and it is).
      *
      * {{{
      *   fun.ordinal() match {
      *     case 0 => bm.AddInteger
      *     // ...
      *   }
      * }}}
      */
    def mkGetBuiltinRuntime(
        bm: Expr[CardanoBuiltins]
    )(using Quotes): Expr[DefaultFun => BuiltinRuntime] = {
        import quotes.reflect.*
        val cases: List[CaseDef] = DefaultFun.values.toList.map { fun =>
            val funOrdinal = Expr(fun.ordinal())
            val funCase = CaseDef(funOrdinal.asTerm, None, Select.unique(bm.asTerm, fun.toString))
            funCase
        }
        '{ (fun: DefaultFun) =>
            ${ Match('{ fun.ordinal() }.asTerm, cases).asExprOf[BuiltinRuntime] }
        }
    }

    /** Compile the provided quoted AST into a SIR using the project's Compiler.
      *
      * @param code
      *   quoted code/term to be compiled
      * @return
      *   quoted SIR representation
      */
    def generateCompileCall(code: Expr[Any])(using Quotes): Expr[SIR] = '{
        scalus.compiler.compile($code)
    }

    /** Compile the provided quoted AST into a SIR using Compiler with options.
      *
      * @param options
      *   quoted scalus.compiler.Options
      * @param code
      *   quoted code/term to be compiled
      * @return
      *   quoted SIR representation
      */
    def generateCompileCall(options: Expr[scalus.compiler.Options], code: Expr[Any])(using
        Quotes
    ): Expr[SIR] = '{
        scalus.compiler.compileWithOptions($options, $code)
    }
}
