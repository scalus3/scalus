package scalus.llmapi

import scala.quoted.*
import scala.tasty.inspector.*
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Extracts public API signatures from tasty files into a plain-text cheatsheet for LLM
  * consumption. Not published; run via the `generateLlmsApi` sbt task.
  */
object LlmApiGen {

    /** Packages that form the public surface worth showing to an LLM. */
    val packages: Set[String] = Set(
      "scalus.cardano.onchain.plutus.prelude", // the real prelude; scalus.prelude re-exports it
      "scalus.compiler",
      "scalus.uplc",
      "scalus.uplc.builtin",
      "scalus.cardano.onchain.plutus.v1",
      "scalus.cardano.onchain.plutus.v2",
      "scalus.cardano.onchain.plutus.v3",
      "scalus.cardano.ledger",
      "scalus.cardano.node",
      "scalus.cardano.txbuilder",
      "scalus.testing.kit",
      "scalus.testing.dsl"
    )

    /** For packages with many internals, only these classes are emitted. */
    val classAllowlist: Map[String, Set[String]] = Map(
      "scalus.uplc" -> Set("PlutusV1", "PlutusV2", "PlutusV3", "CompiledPlutus", "Program"),
      "scalus.compiler" -> Set("Options")
    )

    def main(args: Array[String]): Unit = {
        val outPath = args(0)
        // Strip the sbt-git snapshot suffix (+N-hash-SNAPSHOT) so the header - and the
        // committed file - stay stable between releases.
        val version = args(1).takeWhile(_ != '+')
        val tastyFiles = args
            .drop(2)
            .toList
            .flatMap { dir =>
                Using.resource(Files.walk(Paths.get(dir))) { stream =>
                    stream
                        .iterator()
                        .asScala
                        .filter(p => p.toString.endsWith(".tasty"))
                        // A .tasty without its sibling .class (stale incremental output)
                        // makes TastyInspector abort with unpickling errors - skip it.
                        .filter { p =>
                            val cls = p.toString.stripSuffix(".tasty") + ".class"
                            val ok = Files.exists(Paths.get(cls))
                            if !ok then System.err.println(s"llms-api: skipping stale $p")
                            ok
                        }
                        .map(_.toString)
                        .toList
                }
            }
            .sorted
        val sb = new StringBuilder
        sb ++= s"# Scalus $version public API cheatsheet\n"
        sb ++= "# Generated - do not edit. Signatures are Scala 3.\n"
        TastyInspector.inspectTastyFiles(tastyFiles)(new ApiInspector(sb, packages))
        Files.writeString(Paths.get(outPath), sb.toString)
        println(s"llms-api: wrote $outPath (${sb.length} chars)")
    }
}

class ApiInspector(sb: StringBuilder, packages: Set[String]) extends Inspector {

    private def classAllowed(pkg: String, name: String): Boolean =
        LlmApiGen.classAllowlist.get(pkg) match
            case Some(allowed) => allowed.contains(name.stripSuffix("$"))
            case None          => true

    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
        import quotes.reflect.*

        def isPublicApi(sym: Symbol): Boolean =
            !sym.flags.is(Flags.Private) && !sym.flags.is(Flags.Protected)
                && !sym.flags.is(Flags.Synthetic) && !sym.flags.is(Flags.Artifact)
                // module classes end in "$" (object Foo -> class Foo$); keep those,
                // drop compiler-generated names with inner "$"s
                && !sym.name.stripSuffix("$").contains("$")

        def show(tree: Tree): String = tree.show(using Printer.TreeShortCode)

        def sig(dd: DefDef): String = {
            val kw = if dd.symbol.flags.is(Flags.Given) then "given" else "def"
            val parts = dd.paramss.map {
                case tpc: TypeParamClause =>
                    tpc.params.map(_.name).mkString("[", ", ", "]")
                case tc: TermParamClause =>
                    val prefix = if tc.isGiven then "using " else ""
                    tc.params
                        .map(p => s"${p.name}: ${show(p.tpt)}")
                        .mkString(s"($prefix", ", ", ")")
            }.mkString
            s"$kw ${dd.name}$parts: ${show(dd.returnTpt)}"
        }

        def valSig(vd: ValDef): String = {
            val kw =
                if vd.symbol.flags.is(Flags.Given) then "given"
                else if vd.symbol.flags.is(Flags.Mutable) then "var"
                else "val"
            s"$kw ${vd.name}: ${show(vd.tpt)}"
        }

        def classKind(sym: Symbol): String =
            if sym.flags.is(Flags.Module) then "object"
            else if sym.flags.is(Flags.Trait) then "trait"
            else if sym.flags.is(Flags.Enum) then "enum"
            else if sym.flags.is(Flags.Case) then "case class"
            else "class"

        def emitClass(cd: ClassDef, owner: String): Unit = {
            val name = s"$owner${cd.name.stripSuffix("$")}"
            sb ++= s"\n${classKind(cd.symbol)} $name\n"
            for stat <- cd.body do
                stat match {
                    case dd: DefDef
                        if isPublicApi(dd.symbol) && !dd.symbol.isClassConstructor
                            && !dd.name.endsWith("_=") =>
                        sb ++= s"  ${sig(dd)}\n"
                    case vd: ValDef if isPublicApi(vd.symbol) =>
                        sb ++= s"  ${valSig(vd)}\n"
                    case nested: ClassDef if isPublicApi(nested.symbol) =>
                        emitClass(nested, s"$name.")
                    case _ => ()
                }
        }

        def walk(tree: Tree, pkg: String): Unit = tree match {
            case PackageClause(pid, stats) =>
                val p = if pkg.isEmpty then pid.show else s"$pkg.${pid.show}"
                stats.foreach(walk(_, p))
            case cd: ClassDef
                if packages.contains(pkg) && isPublicApi(cd.symbol)
                    && classAllowed(pkg, cd.name) =>
                emitClass(cd, s"$pkg.")
            case _ => ()
        }

        for tasty <- tastys do walk(tasty.ast, "")
    }
}
