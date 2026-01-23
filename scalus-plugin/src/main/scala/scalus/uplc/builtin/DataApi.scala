package scalus.uplc.builtin

// We copy several files from the shared project to the plugin project during sbt build
// as it's not that easy to add dependencies for Scala Plugins.
// This is a workaround to avoid original DataApi transitive dependencies.
// Just leave the trait empty. It's needed for Data to compile.
private trait DataApi {}

// FromData is used in the Data companion object in the scalus runtime.
//  Her is an empty stub for the compiler plugin.
@FunctionalInterface
trait FromData[A] extends Function1[Data, A] {
    override def apply(x: Data): A
}

// ToData is used in the Data companion object in the scalus runtime.
//  Her is an empty stub for the compiler plugin.
@FunctionalInterface
trait ToData[A] extends Function1[A, Data] {
    override def apply(x: A): Data
}
