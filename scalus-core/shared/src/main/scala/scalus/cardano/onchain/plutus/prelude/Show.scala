package scalus.cardano.onchain.plutus.prelude

import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.{fromString, utf8}
import scalus.uplc.builtin.{BuiltinPair, ByteString, Data}
import scalus.{Compile, CompileDerivations}

/** * A typeclass for converting values of type `A` to a `String`.
  *
  * This is used to provide a string representation of values, which can be useful for debugging,
  * logging, or displaying information about the value.
  *
  * @tparam A
  *   the type of the value to be shown
  */
@FunctionalInterface
trait Show[A] extends (A => String) with CompileDerivations {
    override def apply(v: A): String
}

@FunctionalInterface
trait ShowByteString[A] extends (A => ByteString) with CompileDerivations {
    override def apply(v: A): ByteString
}

extension [A: Show](self: A) inline def show: String = summon[Show[A]].apply(self)
extension [A: ShowByteString](self: A)
    inline def showByteString: ByteString = summon[ShowByteString[A]].apply(self)

@Compile
object ShowByteString {
    inline def apply[A: ShowByteString]: ShowByteString[A] = summon[ShowByteString[A]]

    given ShowByteString[Unit] = (x: Unit) => utf8"()"
    given ShowByteString[BigInt] = (x: BigInt) => Prelude.showByteStringBigInt(x)
    given ShowByteString[ByteString] = (x: ByteString) =>
        appendByteString(
          appendByteString(fromString("\""), Prelude.encodeHexByteString(x)),
          fromString("\"")
        )
    given ShowByteString[String] = (x: String) => encodeUtf8(x)
    given ShowByteString[Boolean] = (x: Boolean) => if x then utf8"True" else utf8"False"
    given ShowByteString[Data] = (x: Data) => {
        import scalus.uplc.builtin
        def showBuiltinList(xs: builtin.BuiltinList[Data]): ByteString = {
            if xs.isEmpty then utf8""
            else
                val head = xs.head.showByteString
                if xs.tail.isEmpty then head
                else
                    appendByteString(
                      head,
                      appendByteString(utf8", ", showBuiltinList(xs.tail))
                    )
        }
        val showConstr = () => {
            val p = unConstrData(x)
            val lst =
                appendByteString(
                  utf8"[",
                  appendByteString(showBuiltinList(p.snd), utf8"]")
                )
            appendByteString(
              utf8"<",
              appendByteString(
                p.fst.showByteString,
                appendByteString(utf8", ", appendByteString(lst, utf8">"))
              )
            )
        }

        val showMap = () => {
            import scalus.uplc.builtin
            val lst = unMapData(x)
            def showDataPair(x: BuiltinPair[Data, Data]): ByteString = {
                val fstShow = x.fst.showByteString
                val sndShow = x.snd.showByteString
                appendByteString(appendByteString(fstShow, utf8": "), sndShow)
            }
            def go(xs: builtin.BuiltinList[BuiltinPair[Data, Data]]): ByteString = {
                if xs.isEmpty then utf8""
                else
                    val head = showDataPair(xs.head)
                    if xs.tail.isEmpty then head
                    else appendByteString(head, appendByteString(utf8", ", go(xs.tail)))
            }
            appendByteString(utf8"{", appendByteString(go(lst), utf8"}"))
        }
        val showList = () => {
            val lst = unListData(x)
            appendByteString(
              utf8"[",
              appendByteString(showBuiltinList(lst), utf8"]")
            )
        }
        val showI = () => unIData(x).showByteString
        val showB = () => unBData(x).showByteString
        val f: () => ByteString = chooseData(x, showConstr, showMap, showList, showI, showB)
        f()
    }

}

@Compile
object Show {
    inline def apply[A: Show]: Show[A] = summon[Show[A]]

    given Show[Unit] = (x: Unit) => "()"
    given Show[BigInt] = (x: BigInt) => Prelude.showBigInt(x)
    given Show[String] = (x: String) => appendString(appendString("\"", x), "\"")
    given Show[Boolean] = (x: Boolean) => if x then "True" else "False"

    given Show[Data] = (x: Data) => {
        decodeUtf8(summon[ShowByteString[Data]].apply(x))
    }
}
