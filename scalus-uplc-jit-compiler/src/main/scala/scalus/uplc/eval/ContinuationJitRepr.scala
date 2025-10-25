package scalus.uplc.eval

sealed trait ContinuationJitRepr {}

object ContinuationJitRepr {

    case class ReturnJitRepr(value: Any) extends ContinuationJitRepr

    case class App(func: Any, arg: Any) extends ContinuationJitRepr

    trait Continue extends ContinuationJitRepr {
        def invoke(): Any
    }

    abstract class Continue1(arg1: Any) extends Continue {
        def invoke(): Any
    }

}
