package scalus.tsexport

import java.nio.file.{Files, Paths}

/** CLI: generate a `.d.ts` file from the TASTy of Scala.js modules.
  *
  * Usage: `--tasty-root <dir>` (repeatable) `--classpath <path-list>` `--output <file>`
  * `--source-root <dir>` (default ".") `--exclude <fqnPrefix>` (repeatable)
  */
object Main {
    case class Config(
        tastyRoots: List[String] = Nil,
        classpath: List[String] = Nil,
        output: String = "",
        sourceRoot: String = ".",
        excludes: List[String] = Nil
    )

    def parse(args: List[String], cfg: Config = Config()): Either[String, Config] = args match
        case Nil =>
            if cfg.tastyRoots.isEmpty then Left("at least one --tasty-root is required")
            else if cfg.output.isEmpty then Left("--output is required")
            else
                Right(
                  cfg.copy(tastyRoots = cfg.tastyRoots.reverse, excludes = cfg.excludes.reverse)
                )
        case "--tasty-root" :: v :: rest => parse(rest, cfg.copy(tastyRoots = v :: cfg.tastyRoots))
        case "--classpath" :: v :: rest =>
            parse(rest, cfg.copy(classpath = v.split(java.io.File.pathSeparator).toList))
        case "--output" :: v :: rest      => parse(rest, cfg.copy(output = v))
        case "--source-root" :: v :: rest => parse(rest, cfg.copy(sourceRoot = v))
        case "--exclude" :: v :: rest     => parse(rest, cfg.copy(excludes = v :: cfg.excludes))
        case other :: _                   => Left(s"unknown argument: $other")

    def run(cfg: Config): Either[List[ExportError], String] = {
        val result =
            ExportCollector.collect(cfg.tastyRoots, cfg.classpath, cfg.sourceRoot, cfg.excludes)
        if result.errors.nonEmpty then Left(result.errors)
        else Right(Emitter.emit(result.module))
    }

    def main(args: Array[String]): Unit = parse(args.toList) match
        case Left(msg) =>
            System.err.println(s"scalus-ts-exporter: $msg")
            sys.exit(1)
        case Right(cfg) =>
            run(cfg) match
                case Left(errors) =>
                    errors.foreach(e => System.err.println(s"error: ${e.render}"))
                    System.err.println(s"${errors.size} export error(s); no output written")
                    sys.exit(1)
                case Right(text) =>
                    val out = Paths.get(cfg.output)
                    Option(out.getParent).foreach(Files.createDirectories(_))
                    Files.writeString(out, text)
                    println(s"wrote ${cfg.output}")
}
