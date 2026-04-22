package scalus.cardano.node.stream.engine.persistence

import java.nio.file.{Files, Path, Paths}

/** Resolve the platform-appropriate data directory for an `appId`.
  *
  * Mirrors the table in `docs/local/claude/indexer/indexer-node.md` under *App identity and
  * persistence location*.
  */
object PlatformPaths {

    /** Return the directory `<platformDataRoot>/scalus-stream/<appId>`, creating parent directories
      * lazily on first write — this method does not create the directory itself.
      */
    def dataDirFor(appId: String): Path = {
        require(appId.nonEmpty, "appId must be non-empty")
        baseDataRoot().resolve("scalus-stream").resolve(appId)
    }

    private def baseDataRoot(): Path = {
        val os = sys.props.getOrElse("os.name", "").toLowerCase
        if os.contains("mac") || os.contains("darwin") then macOsDataRoot()
        else if os.contains("windows") then windowsDataRoot()
        else linuxDataRoot()
    }

    private def linuxDataRoot(): Path = sys.env.get("XDG_DATA_HOME") match {
        case Some(xdg) if xdg.nonEmpty => Paths.get(xdg)
        case _                         => Paths.get(sys.props("user.home"), ".local", "share")
    }

    private def macOsDataRoot(): Path =
        Paths.get(sys.props("user.home"), "Library", "Application Support")

    private def windowsDataRoot(): Path = sys.env.get("LOCALAPPDATA") match {
        case Some(local) if local.nonEmpty => Paths.get(local)
        case _                             => Paths.get(sys.props("user.home"), "AppData", "Local")
    }

    /** Ensure `dir` exists. Returns the same path for chaining. */
    def ensureDir(dir: Path): Path = {
        Files.createDirectories(dir)
        dir
    }
}
