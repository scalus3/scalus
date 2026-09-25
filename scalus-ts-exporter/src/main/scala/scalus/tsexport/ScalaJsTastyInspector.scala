package scalus.tsexport

import dotty.tools.dotc.{CompilationUnit, Compiler, Driver, Run}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.fromtasty.{ReadTasty, TASTYCompiler, TASTYRun}
import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.dotc.util.ClasspathFromClassloader
import dotty.tools.unsupported
import scala.quoted.runtime.impl.QuotesImpl
import scala.tasty.inspector.{Inspector, Tasty}

import java.io.File.pathSeparator

/** `scala.tasty.inspector.TastyInspector` with the compiler in Scala.js mode.
  *
  * The facades are compiled with `-scalajs`, and their TASTy carries signatures computed under that
  * mode: `js.UndefOr[A]` is the Scala 2 pseudo-union `js.|[A, Unit]` in the scalajs-library
  * pickles, which `-scalajs` unpickles as the union `A | Unit` and erases to `Object`. The stock
  * inspector runs without the flag, keeps `js.|` as a class, and computes a different signature;
  * the first class with two constructors and a `js.UndefOr` parameter then fails to unpickle with
  * `undefined: this # -1`. This is the stock driver with `-scalajs` added to its arguments.
  */
object ScalaJsTastyInspector {

    /** Load and process TASTy files with the given inspector. Returns false on any error. */
    def inspectAllTastyFiles(tastyFiles: List[String], dependenciesClasspath: List[String])(
        inspector: Inspector
    ): Boolean = {
        if tastyFiles.isEmpty then true
        else
            val currentClasspath = ClasspathFromClassloader(getClass.getClassLoader)
            val fullClasspath =
                (dependenciesClasspath :+ currentClasspath).mkString(pathSeparator)
            val args = "-scalajs" :: "-from-tasty" :: "-Yretain-trees" :: "-classpath" ::
                fullClasspath :: tastyFiles
            val reporter = inspectorDriver(inspector).process(args.toArray)
            !reporter.hasErrors
    }

    private def inspectorDriver(inspector: Inspector): Driver = {
        class TastyInspectorPhase extends Phase {
            override def phaseName: String = "tastyInspector"

            override def runOn(units: List[CompilationUnit])(using
                ctx0: Context
            ): List[CompilationUnit] = {
                val ctx = QuotesCache.init(ctx0.fresh)
                runOnImpl(units)(using ctx)
            }

            private def runOnImpl(units: List[CompilationUnit])(using
                Context
            ): List[CompilationUnit] = {
                val quotesImpl = QuotesImpl()
                class TastyImpl(val path: String, val ast: quotesImpl.reflect.Tree)
                    extends Tasty[quotesImpl.type] {
                    val quotes = quotesImpl
                }
                val tastys = units.map(unit =>
                    new TastyImpl(
                      unit.source.path,
                      unit.tpdTree.asInstanceOf[quotesImpl.reflect.Tree]
                    )
                )
                inspector.inspect(using quotesImpl)(tastys)
                units
            }

            override def run(implicit ctx: Context): Unit = unsupported("run")
        }

        class TastyFromClass extends TASTYCompiler {
            override protected def frontendPhases: List[List[Phase]] = List(new ReadTasty) :: Nil
            override protected def picklerPhases: List[List[Phase]] = Nil
            override protected def transformPhases: List[List[Phase]] = Nil
            override protected def backendPhases: List[List[Phase]] =
                List(new TastyInspectorPhase) :: Nil

            override def newRun(implicit ctx: Context): Run = {
                reset()
                val ctx2 = ctx.fresh
                    .addMode(Mode.ReadPositions)
                    .setSetting(ctx.settings.YreadComments, true)
                new TASTYRun(this, ctx2)
            }
        }

        new Driver {
            override protected def newCompiler(implicit ctx: Context): Compiler =
                new TastyFromClass
        }
    }
}
