package scalus.sbt

import sbt.*

/** The work behind the `blueprint` task: load each Contract from the compiled classes, write its
  * CIP-57 JSON and the aggregate `plutus.json`, and skip the work when the cache fingerprint is
  * unchanged. Kept apart from the sbt task wiring so it runs without an sbt build.
  */
private[sbt] object BlueprintGenerator {

    /** Generate per-contract blueprints (package-nested) plus an aggregate `plutus.json`
      * under `resourceRoot`, skipping all work when the fingerprint matches the previous run.
      * The fingerprint covers the content of `classesDir` and a stamp of `dependencyClasspath`
      * (path, size and modification time of each file). The dependency part matters because the
      * script comes from the Scalus lowering and optimizer in a dependency: a Scalus bump changes
      * the script while the project's class files can stay byte-identical. The class files are
      * hashed by content, so a recompile that rewrites them unchanged keeps the cache.
      *
      * Layout:
      *   - `resourceRoot/META-INF/scalus/blueprints/<package>/<Contract>.json` per contract
      *   - `resourceRoot/plutus.json` aggregate (JAR root; Aiken-style single document)
      *
      * Stale outputs from renamed/deleted contracts are pruned so the JAR always holds
      * exactly the current set. Returns the produced (or cached) files.
      *
      * Contracts load from `dependencyClasspath` plus `classesDir`.
      */
    def generate(
        classesDir: java.io.File,
        dependencyClasspath: Seq[java.io.File],
        resourceRoot: java.io.File,
        cacheDir: java.io.File,
        projectName: String,
        projectVersion: String,
        scalaVer: String,
        log: sbt.util.Logger
    ): Seq[java.io.File] = {
        val outDir = resourceRoot / "META-INF" / "scalus" / "blueprints"
        val aggregateFile = resourceRoot / "plutus.json"
        val header =
            s"scheme=${BlueprintLayout.Scheme};scala=$scalaVer;version=$projectVersion;name=$projectName\n" +
                BlueprintLayout.classpathStamp(dependencyClasspath)
        // Prune before fingerprinting, so the cache describes the manifest we actually use.
        val classNames = BlueprintLayout.readManifestClassNames(classesDir).sorted
        val classFiles = BlueprintLayout.listFilesRecursively(classesDir)
        val fp = BlueprintLayout.fingerprint(header, classesDir, classFiles)
        val fpFile = cacheDir / "fingerprint"

        readCachedOutputs(fpFile, fp, resourceRoot) match {
            case Some(cached) =>
                log.debug("Blueprints are up to date; skipping generation")
                cached
            case None =>
                if (classNames.isEmpty) {
                    log.warn("No Contract implementations found")
                    BlueprintLayout.pruneStale(outDir, Set.empty[java.io.File])
                    IO.delete(aggregateFile)
                    writeCachedOutputs(fpFile, fp, resourceRoot, Seq.empty)
                    Seq.empty
                } else {
                    IO.createDirectory(outDir)
                    val cp = dependencyClasspath :+ classesDir
                    val urls = cp.map(_.toURI.toURL).toArray
                    val cl = new java.net.URLClassLoader(urls, ClassLoader.getPlatformClassLoader)
                    try {
                        val jsons = loadContractJsons(cl, classNames, log)
                        val tool = BlueprintToolBridge.load(cl, log)
                        val utf8 = java.nio.charset.StandardCharsets.UTF_8
                        val perContract = jsons.map { case (className, json) =>
                            val file = outDir / BlueprintLayout.contractRelativePath(className)
                            IO.write(file, tool.stamp(json, scalaVer), utf8)
                            log.info(s"Wrote ${file.relativeTo(resourceRoot).getOrElse(file)}")
                            file
                        }
                        val aggregated =
                            tool.aggregate(jsons.map(_._2), projectName, projectVersion, scalaVer)
                                .map { json =>
                                    IO.write(aggregateFile, json, utf8)
                                    log.info(
                                      s"Wrote ${aggregateFile.relativeTo(resourceRoot).getOrElse(aggregateFile)}"
                                    )
                                    aggregateFile
                                }
                        val produced = perContract ++ aggregated.toSeq
                        BlueprintLayout
                            .pruneStale(outDir, perContract.toSet)
                            .foreach(f => log.info(s"Pruned stale blueprint $f"))
                        // Cache only complete, error-free runs so contracts that failed to load
                        // are retried on the next build instead of being silently skipped.
                        if (jsons.size == classNames.size)
                            writeCachedOutputs(fpFile, fp, resourceRoot, produced)
                        produced
                    } finally {
                        cl.close()
                    }
                }
        }
    }

    /** Load each Contract's blueprint JSON via reflection. Failures are logged and skipped. */
    private def loadContractJsons(
        cl: ClassLoader,
        classNames: Seq[String],
        log: sbt.util.Logger
    ): Seq[(String, String)] =
        classNames.flatMap { className =>
            try {
                val cls = cl.loadClass(className)
                val instance = cls.getField("MODULE$").get(null)
                val method = cls.getMethod("blueprintJson")
                val json = method.invoke(instance).asInstanceOf[String]
                Some((className, json))
            } catch {
                case e: java.lang.reflect.InvocationTargetException =>
                    log.error(s"Failed to load Contract $className: ${e.getCause.getMessage}")
                    None
                case e: Exception =>
                    log.error(s"Failed to load Contract $className: ${e.getMessage}")
                    None
            }
        }

    /** Cache file format: fingerprint on the first line, then one produced path per line,
      * relative to the managed-resource root. A hit requires an identical fingerprint AND
      * every recorded file still present on disk.
      */
    private def readCachedOutputs(
        fpFile: java.io.File,
        fp: String,
        resourceRoot: java.io.File
    ): Option[Seq[java.io.File]] =
        if (!fpFile.isFile) None
        else
            IO.readLines(fpFile) match {
                case `fp` :: rest =>
                    val files = rest.map(resourceRoot / _)
                    if (files.forall(_.isFile)) Some(files) else None
                case _ => None
            }

    private def writeCachedOutputs(
        fpFile: java.io.File,
        fp: String,
        resourceRoot: java.io.File,
        produced: Seq[java.io.File]
    ): Unit = {
        val rels = produced.flatMap(f => IO.relativize(resourceRoot, f))
        IO.writeLines(fpFile, (fp +: rels).toList)
    }

    /** Reflective bridge to scalus-core's `BlueprintTool`. Degrades gracefully when the
      * project depends on a scalus version that predates it: per-contract files are written
      * unstamped and no aggregate is produced.
      */
    private trait BlueprintToolBridge {
        def stamp(json: String, scalaVer: String): String
        def aggregate(
            jsons: Seq[String],
            title: String,
            version: String,
            scalaVer: String
        ): Option[String]
    }

    private object BlueprintToolBridge {
        def load(cl: ClassLoader, log: sbt.util.Logger): BlueprintToolBridge =
            try {
                val cls = cl.loadClass("scalus.cardano.blueprint.BlueprintTool$")
                val instance = cls.getField("MODULE$").get(null)
                val stampM = cls.getMethod("stampScalaVersion", classOf[String], classOf[String])
                val aggM = cls.getMethod(
                  "aggregate",
                  classOf[java.util.List[?]],
                  classOf[String],
                  classOf[String],
                  classOf[String]
                )
                new BlueprintToolBridge {
                    def stamp(json: String, scalaVer: String): String =
                        stampM.invoke(instance, json, scalaVer).asInstanceOf[String]
                    def aggregate(
                        jsons: Seq[String],
                        title: String,
                        version: String,
                        scalaVer: String
                    ): Option[String] = {
                        val list = new java.util.ArrayList[String]
                        jsons.foreach(j => list.add(j))
                        Some(aggM.invoke(instance, list, title, version, scalaVer).asInstanceOf[String])
                    }
                }
            } catch {
                case _: ClassNotFoundException | _: NoSuchMethodException =>
                    log.warn(
                      "scalus-core on the classpath predates BlueprintTool: blueprints are " +
                          "written without scalus.scalaVersion and no aggregate plutus.json is " +
                          "produced. Upgrade the scalus dependency for full output."
                    )
                    new BlueprintToolBridge {
                        def stamp(json: String, scalaVer: String): String = json
                        def aggregate(
                            jsons: Seq[String],
                            title: String,
                            version: String,
                            scalaVer: String
                        ): Option[String] = None
                    }
            }
    }
}
