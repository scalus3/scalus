package scalus.uplc.builtin.internal

import scalus.Compile
import scalus.uplc.builtin.*

/** This module exists to make Linker happy about the existence of the internal data representation,
  * where FromData/ToData become NoOps.
  */
@Compile
object UniversalDataConversion {

    /** This method is not used really, but we use symbol in the compiler plugin as a marker to use
      * a data representation on a SIR level, when we need to eliminate scala ForData/ToData from
      * the SIR.
      *
      * i.e. on the step
      * ```
      *   toDataConvertor(a1)
      *   toDataConvertor(a2)(using c1: ToData[B1], .... cN: ToData[Bn])
      * ```
      * converted to
      * ```
      *   SIR.Apply("scalus.uplc.builtin.internal.UniversalDataConversion.toData", List(a1))
      *   SIR.Apply("scalus.uplc.builtin.internal.UniversalDataConversion.toData", List(a2))
      * ```
      */
    def toData[A](a: A): Data = {
        throw RuntimeException(
          "impossible to call this method at runtime, it is used only in the compiler plugin"
        )
    }

    /** This method is not used really, but we use symbol in the compiler plugin as a marker to use
      * a data representation on a SIR level, when we need to eliminate scala ForData/ToData from
      * the SIR. * i.e. on the step
      * ```
      *  fromDataConvertor(data1)
      *  fromDataConvertor(data2)(using c1: FromData[B1], .... cN: FromData[Bn])
      *  * ```
      *  converted to
      * ```
      * SIR.Apply("scalus.uplc.builtin.internal.UniversalDataConversion.fromData", List(data1))
      * SIR.Apply("scalus.uplc.builtin.internal.UniversalDataConversion.fromData", List(data2))
      * ````
      */
    def fromData[A](data: Data): A = {
        throw RuntimeException(
          "impossible to call this method at runtime, it is used only in the compiler plugin"
        )
    }

}
