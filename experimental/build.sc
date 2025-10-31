import mill.*, scalalib.*

object scalusPlugin extends RootModule with SbtModule {
    def scalaVersion = "3.3.3"

    // Add (or replace) source folders for the module to use
    def sources = T.sources {
        val baseDir = millSourcePath / "shared" / "src" / "main" / "scala"
        super.sources() ++ Seq(
          PathRef(millSourcePath / "scalus-plugin" / "src" / "main" / "scala")
        ) ++ Seq(
          PathRef(baseDir / "scalus" / "utils" / "Hex.scala"),
          PathRef(baseDir / "scalus" / "utils" / "HashConsed.scala"),
          PathRef(baseDir / "scalus" / "builtin" / "ByteString.scala"),
          PathRef(baseDir / "scalus" / "builtin" / "Data.scala"),
          PathRef(baseDir / "scalus" / "builtin" / "List.scala"),
          PathRef(baseDir / "scalus" / "sir" / "SIR.scala"),
          PathRef(baseDir / "scalus" / "sir" / "SIRType.scala"),
          PathRef(baseDir / "scalus" / "sir" / "SIRToExpr.scala"),
          PathRef(baseDir / "scalus" / "sir" / "SIRBuiltins.scala"),
          PathRef(baseDir / "scalus" / "sir" / "SIRUnify.scala"),
          PathRef(baseDir / "scalus" / "sir" / "SIRHashCodeInRec.scala"),
          PathRef(baseDir / "scalus" / "sir" / "FlatInstances.scala"),
          PathRef(baseDir / "scalus" / "uplc" / "Constant.scala"),
          PathRef(baseDir / "scalus" / "uplc" / "DefaultFun.scala"),
          PathRef(baseDir / "scalus" / "uplc" / "DefaultUni.scala"),
          PathRef(baseDir / "scalus" / "uplc" / "CommonFlatInstances.scala"),
          PathRef(baseDir / "scalus" / "uplc" / "TypeScheme.scala"),
          PathRef(baseDir / "scalus" / "flat" / "package.scala")
        )
    }

    def compileIvyDeps = Agg(
      ivy"org.scala-lang::scala3-compiler:${scalaVersion()}"
    )

    def scalacOptions: T[Seq[String]] = Seq("-deprecation", "-Xfatal-warnings", "-explain")

    object test extends SbtModuleTests with TestModule.ScalaTest {
        def ivyDeps = Agg(
          ivy"org.scalatest::scalatest:3.2.18",
          ivy"org.scalatestplus::scalacheck-1-16:3.2.14.0"
        )
        def sources = T.sources {
            super.sources() ++ Seq(
              PathRef(millSourcePath / "scalus-plugin" / "src" / "test")
            )
        }
    }
}
