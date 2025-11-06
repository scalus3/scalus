package scalus.cardano.ledger.utils

import java.io.File
import java.net.{URI, URL}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.boundary.break
import scala.util.{boundary, Try, Using}

object TraitObjectScanner:

    /** Finds all Scala top level objects implementing the specified trait in the given package.
      *
      * @param packageName
      *   The package to search (e.g., "com.example.services")
      * @tparam T
      *   The trait type to search for
      * @return
      *   A map of simple class names (without $) to instances of the objects implementing the trait
      */
    def findImplementors[T: ClassTag](packageName: String): Set[T] =
        val classLoader = Thread.currentThread().getContextClassLoader
        val path = packageName.replace('.', '/')

        // getResources returns an Enumeration - no resources to close
        val resources = classLoader.getResources(path).asScala.toSeq

        resources.view.flatMap { url =>
            url.getProtocol match
                case "file" => findObjectsInDirectory[T](url, packageName, classLoader)
                case "jar"  => findObjectsInJar[T](url, packageName, classLoader)
                case _      => Seq.empty
        }.toSet // Convert to Set for automatic deduplication

    /** Finds objects in a directory (filesystem).
      */
    private def findObjectsInDirectory[T: ClassTag](
        url: URL,
        packageName: String,
        classLoader: ClassLoader
    ): Seq[T] =
        val directory = new File(url.toURI)

        if !directory.exists() || !directory.isDirectory then return Seq.empty

        val files = directory.listFiles()
        if files == null then return Seq.empty

        val classFiles = files.view
            .filter(f => f.isFile && f.getName.endsWith(".class"))
            .map(_.getName)
            .toSeq

        loadAndFilterObjects[T](classFiles, packageName, classLoader)

    /** Finds objects in a JAR file.
      */
    private def findObjectsInJar[T: ClassTag](
        url: URL,
        packageName: String,
        classLoader: ClassLoader
    ): Seq[T] =
        // Extract JAR file path from URL
        // Format: jar:file:/path/to/file.jar!/package/path
        val jarUrlString = url.getPath.split("!")(0)
        val uri = URI.create(jarUrlString)
        val jarPath = uri.getPath

        // Properly handle JarFile resource with exception safety
        val result = Using.resource(new JarFile(jarPath)) { jar =>
            val path = packageName.replace('.', '/')
            val pathWithSlash = if path.isEmpty then "" else path + "/"

            // Eagerly evaluate to Seq before JarFile closes
            // jar.entries() returns an Enumeration tied to the JarFile
            val classFiles = jar
                .entries()
                .asScala
                .filter { entry =>
                    val name = entry.getName
                    // Must start with our package path
                    name.startsWith(pathWithSlash) &&
                    name.endsWith(".class") &&
                    // Exclude subdirectories: no '/' after the package prefix
                    !name.drop(pathWithSlash.length).contains('/')
                }
                .map { entry =>
                    // Remove package path prefix and get just the class name
                    entry.getName.drop(pathWithSlash.length)
                }
                .toSeq // IMPORTANT: materialize to Seq before JarFile closes

            classFiles
        }

        // Load classes outside the Using block (JarFile already closed)
        loadAndFilterObjects[T](result, packageName, classLoader)

    /** Loads classes and filters for objects implementing the trait.
      */
    private def loadAndFilterObjects[T: ClassTag](
        classFiles: Seq[String],
        packageName: String,
        classLoader: ClassLoader
    ): Seq[T] =
        val traitClass = summon[ClassTag[T]].runtimeClass

        classFiles.flatMap { fileName =>
            // Remove .class extension
            val className = fileName.stripSuffix(".class")

            // Skip synthetic classes and module classes
            if isSyntheticClass(className) then None
            else loadObject[T](packageName, className, classLoader, traitClass)
        }

    /** Attempts to load an object instance if it implements the trait.
      */
    private def loadObject[T](
        packageName: String,
        className: String,
        classLoader: ClassLoader,
        traitClass: Class[?]
    ): Option[T] =
        Try {
            boundary {
                // For Scala objects, the class name ends with $
                val fullClassName =
                    if className.endsWith("$") then s"$packageName.$className".stripSuffix("$")
                    else s"$packageName.$className"

                val clazz = Class.forName(s"$fullClassName$$", true, classLoader)

                // Check if it's a Scala object (has MODULE$ or INSTANCE field)
                if !isScalaObject(clazz) then break(None)

                // Check if it implements the trait
                if !traitClass.isAssignableFrom(clazz) then break(None)

                // Get the singleton instance
                val moduleField = clazz.getField("MODULE$")
                val instance = moduleField.get(null)

                Some(instance.asInstanceOf[T])
            }
        }.toOption.flatten

    /** Checks if a class represents a Scala object (has MODULE$ or INSTANCE field).
      */
    private def isScalaObject(clazz: Class[?]): Boolean =
        Try {
            // Scala 2 and Scala 3 use MODULE$
            clazz.getField("MODULE$")
            true
        }.orElse(Try {
            // Some frameworks or alternative patterns use INSTANCE
            clazz.getField("INSTANCE")
            true
        }).getOrElse(false)

    /** Filters out synthetic and compiler-generated classes. This comprehensive filter excludes
      * various compiler artifacts.
      */
    private def isSyntheticClass(className: String): Boolean =
        val simpleName = className.stripSuffix("$")

        simpleName.contains("$anon") || // Anonymous classes
        simpleName.contains("$lambda") || // Lambda functions
        simpleName.contains("$specialized") || // Specialized methods
        simpleName.contains("$macro") || // Macro-generated code
        simpleName.contains("$deserializer") || // Serialization helpers
        simpleName.contains("$serializer") || // Serialization helpers
        simpleName.contains("$accessor") || // Lazy val accessors
        simpleName.contains("$module") || // Module initialization
        simpleName.contains("$line") || // REPL line wrappers
        simpleName.contains("$read") || // REPL read wrappers
        simpleName == "package$" || // Package objects
        simpleName.startsWith("$") || // Compiler prefixed
        simpleName.contains("$$") || // Double dollar synthetics
        simpleName.matches(".*\\$\\d+.*") || // Numbered synthetics
        simpleName.matches(".*\\$[A-Z]\\$\\d+.*") || // Type-parameterized synthetics
        (className.count(_ == '$') > 1 && !simpleName.endsWith("$")) // Multiple dollars (nested)

end TraitObjectScanner
