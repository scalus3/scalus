
Can we create a structure EnrichedSIR[P,T] which will mirror structure of SIR trait
(see scalus-core/share/src/main/scalus/sir/SIR.scala)  and without definition of top-level methods.

For SIR case class Xxx we will have a sealed trait with two cases:
- XxxPattern[P](pattern: P)
- XxxInfo[P,T] with the same parameters as origin SIR, but used appropriate treaits from EnrichedSIR[P,T] instead and with additional info: T

please mirror structure closly and don't use instanceof.
