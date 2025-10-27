package scalus.uplc.eval

import scalus.uplc.{Constant, DefaultUni}

case class ListJitRepr(elementType: DefaultUni, elements: List[Any]) {

    def toConstant: scalus.uplc.Constant.List = {
        val constElems = elements.map(RuntimeHelper.anyUplcConstant)
        scalus.uplc.Constant.List(elementType, constElems)
    }

}

object ListJitRepr {

    def fromConstantList(cl: Constant.List): ListJitRepr = {
        ListJitRepr(
          cl.elemType,
          cl.value.map(RuntimeHelper.uplcToJitAny)
        )
    }

}
