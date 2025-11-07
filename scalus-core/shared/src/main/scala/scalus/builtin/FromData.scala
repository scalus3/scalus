package scalus.builtin

import scalus.builtin.Builtins.{decodeUtf8, unBData, unConstrData, unIData}
import scalus.{Compile, CompileDerivations, Ignore}

import scala.quoted.*

@FunctionalInterface
trait FromData[+A] extends Function1[Data, A] with CompileDerivations {
    override def apply(v: Data): A
}

/** FromData[A] derivation
  */
@Compile
object FromData {

    /** Derives a FromData instance for type A
      */
    inline def derived[A]: FromData[A] = ${
        FromDataMacros.fromDataImpl[A]
    }

    @uplcIntrinsic("unIData")
    given FromData[BigInt] = unIData
    @uplcIntrinsic("unBData")
    given FromData[ByteString] = unBData
    given FromData[String] = (d: Data) => decodeUtf8(unBData(d))
    given FromData[Data] = (d: Data) => d

    given FromData[Unit] = (d: Data) =>
        if unConstrData(d).fst == BigInt(0) then ()
        else scalus.prelude.fail("Not a unit")

    given FromData[Boolean] = (d: Data) =>
        val constr = unConstrData(d).fst
        if constr == BigInt(0) then false
        else if constr == BigInt(1) then true
        else scalus.prelude.fail("Not a boolean")

    given unsafeTupleFromData[A, B](using
        fromA: FromData[A],
        fromB: FromData[B]
    ): FromData[(A, B)] =
        (d: Data) =>
            val args = unConstrData(d).snd
            (fromA(args.head), fromB(args.tail.head))

}
