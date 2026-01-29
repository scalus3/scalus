package scalus.compiler.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import scalus.serialization.flat.FlatInstances.ModuleHashSetReprFlat
import scalus.compiler.sir.{Module as SIRModule, *}

/** Preprocess SIR - run before the Pickling and sbt.ExtreactApi phases and add toSIR-compiled
  * modules fields, which later set in Scalus phase and used during linking.
  *
  * from
  * {{{
  * @Compile
  * object Mybjs {
  *   ....
  * }
  * }}}
  * to
  *
  * {{{
  * @Compile
  * object Mybjs {
  *   ...
  *
  *   val sirModule: scalus.compiler.sir.Module = [NOT-SET]
  *   val sirDeps: List[SIRCompiled] = [NOT-SETT]
  * }
  * }}}
  * sir_ and dpes_ wi
  *
  * Also, add default implementations for non-overriden inline abstract methods in Validator
  * subclasses.
  */
class SIRPreprocessor(thisPhase: ScalusPreparePhase, debugLevel: Int)(using ctx: Context) {

    private val ignoreAnnotRef = requiredClassRef("scalus.compiler.Ignore")
    private val ignoreAnnot = ignoreAnnotRef.symbol.asClass
    private val sirModuleType = requiredClassRef("scalus.compiler.sir.Module")
    private val sirModuleWithDepsType = requiredClassRef("scalus.compiler.sir.SIRModuleWithDeps")
    private val listSirModuleWithDepsType = defn.ListClass.typeRef.appliedTo(sirModuleWithDepsType)

    def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
        // If the template has a compile annotation, we need to add a variable for SIR
        tree.rhs match
            case template: tpd.Template =>
                val sirSym = Symbols
                    .newSymbol(
                      tree.symbol,
                      Plugin.SIR_MODULE_VAL_NAME.toTermName,
                      Flags.Lazy | Flags.Permanent,
                      sirModuleType
                    )
                sirSym.addAnnotation(ignoreAnnot)
                sirSym.enteredAfter(thisPhase)
                val module = SIRModule(SIRVersion, "init", false, None, List.empty)
                val moduleToExprSym = Symbols.requiredModule("scalus.compiler.sir.ModuleToExpr")
                val moduleTree =
                    convertFlatToTree(
                      module,
                      ModuleHashSetReprFlat,
                      moduleToExprSym,
                      tree.span,
                      debugLevel > 0
                    )
                val sirModuleVal = tpd
                    .ValDef(sirSym, moduleTree)
                    .withSpan(tree.span)
                val sirDepsSym = Symbols
                    .newSymbol(
                      tree.symbol,
                      Plugin.SIR_DEPS_VAL_NAME.toTermName,
                      Flags.Lazy | Flags.Permanent,
                      listSirModuleWithDepsType
                    )
                sirDepsSym.addAnnotation(ignoreAnnot)
                sirDepsSym.enteredAfter(thisPhase)
                val sirDepsVal = tpd
                    .ValDef(
                      sirDepsSym,
                      tpd.ref(defn.NilModule).withSpan(tree.span)
                    )
                    .withSpan(tree.span)
                val newTemplate = cpy.Template(template)(
                  body = template.body ++ List(
                    sirModuleVal,
                    sirDepsVal
                  ) ++ defaultNonOverridenValidatorMethods(tree, template)
                )
                val retval = cpy.TypeDef(tree)(name = tree.name, rhs = newTemplate)
                retval
            case _ =>
                report.warning(
                  s"ScalusPrepare: Expected a template for type definition, but found: ${tree.show}",
                  tree.srcPos.startPos
                )
                tree
    }

    def defaultNonOverridenValidatorMethods(tree: Tree, template: Template)(using
        Context
    ): List[Tree] = {
        val validatorSymbols = List(
          requiredClassRef("scalus.cardano.onchain.plutus.v3.Validator").symbol,
          requiredClassRef("scalus.cardano.onchain.plutus.v3.ParameterizedValidator").symbol,
          requiredClassRef("scalus.cardano.onchain.plutus.v3.DataParameterizedValidator").symbol
        )
        validatorSymbols.find(s => tree.symbol.isSubClass(s)) match {
            case Some(validatorSym) =>
                if tree.symbol.is(Module) && tree.symbol.isSubClass(validatorSym) then {
                    val methods = validatorSym.info.decls
                        .filter { m =>
                            if m.is(Flags.Method) then {
                                // true
                                val retval = m.is(Flags.Deferred) && m.is(Flags.Inline)
                                retval
                            } else false

                        }
                        .map(_.asTerm)
                    val existingMethods = template.body.collect {
                        case dd: tpd.DefDef if dd.symbol.is(Method) =>
                            dd.symbol.asTerm
                    }
                    // Compare methods by (name, signature) instead of symbol identity
                    // because method symbols in base class differ from subclass symbols
                    val existingMethodSigs = existingMethods.map { m =>
                        (m.name.toString, m.info.signature)
                    }.toSet
                    val toImplement = methods.filter { m =>
                        !existingMethodSigs.contains((m.name.toString, m.info.signature)) &&
                        !m.is(Flags.Private)
                    }
                    toImplement.map { m =>
                        // Get the method type as seen from the subclass perspective
                        val methodTypeInSubclass =
                            m.info.asSeenFrom(tree.symbol.thisType, validatorSym)
                        val methodType = methodTypeInSubclass.widen.asInstanceOf[MethodType]
                        val body = generateThrow("abstract method in Validator")
                        val methodSymbol = Symbols
                            .newSymbol(
                              tree.symbol,
                              m.name,
                              Flags.Override | Flags.Method | Flags.Inline | Flags.Synthetic,
                              methodType
                            )
                            .enteredAfter(thisPhase)
                        // Add BodyAnnot annotation so the method can be inlined
                        methodSymbol.addAnnotation(
                          Annotations.ConcreteBodyAnnotation(body)
                        )
                        val newMethod = DefDef(methodSymbol, _ => body).withSpan(tree.span)
                        newMethod
                    }
                } else Nil
            case None => Nil
        }
    }

    private def generateThrow(message: String)(using Context): tpd.Tree = {
        val exceptionClass = defn.RuntimeExceptionClass
        // Find the constructor that takes a String parameter
        val ctor = exceptionClass.info.decls
            .filter(_.isConstructor)
            .find { c =>
                c.info match
                    case mt: MethodType =>
                        mt.paramInfos.length == 1 && (mt.paramInfos.head =:= defn.StringType)
                    case _ => false
            }
            .getOrElse {
                report.error(
                  s"Could not find RuntimeException(String) constructor"
                )
                exceptionClass.info.decls.filter(_.isConstructor).head
            }
        val newException = tpd.New(exceptionClass.typeRef)
        val selectCtor = tpd.Select(newException, ctor.termRef)
        val applyCtor = tpd.Apply(selectCtor, List(tpd.Literal(Constant(message))))
        tpd.Throw(applyCtor)
    }
}
