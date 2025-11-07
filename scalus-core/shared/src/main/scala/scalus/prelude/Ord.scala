package scalus.prelude

import scalus.{Compile, CompileDerivations}
import scalus.builtin.Builtins.*
import scalus.builtin.{BuiltinPair, Data}
import scala.annotation.nowarn

@FunctionalInterface
trait Ord[-A] extends ((A, A) => Order) with CompileDerivations {
    override def apply(lhs: A, rhs: A): Order
}

extension [A: Ord](self: A)
    inline infix def <=>(inline other: A): Order = Ord[A].compare(self, other)

@Compile
object Ord:
    inline def apply[A: Ord]: Ord[A] = summon[Ord[A]]

    given Ord[Unit] = (_: Unit, _: Unit) => Order.Equal

    given Ord[BigInt] = (x: BigInt, y: BigInt) =>
        if lessThanInteger(x, y) then Order.Less
        else if lessThanInteger(y, x) then Order.Greater
        else Order.Equal

    // TODO it's not truly overriding and doesn't work in generic function,
    //  see extension methods <, <=, >, >=, equiv, nonEquiv for [A: Ord]
    //  Don't accidentally mixed with standard scala math.Ordering.BigInt
    extension (self: BigInt)
        @nowarn inline infix def <(inline other: BigInt): Boolean = lessThanInteger(self, other)
        @nowarn inline infix def <=(inline other: BigInt): Boolean =
            lessThanEqualsInteger(self, other)
        @nowarn inline infix def >(inline other: BigInt): Boolean = lessThanInteger(other, self)
        @nowarn inline infix def >=(inline other: BigInt): Boolean =
            lessThanEqualsInteger(other, self)
        inline infix def equiv(inline other: BigInt): Boolean = equalsInteger(self, other)
        inline infix def nonEquiv(inline other: BigInt): Boolean = !equalsInteger(self, other)

    end extension

    given Ord[Boolean] = (x: Boolean, y: Boolean) =>
        if x then if y then Order.Equal else Order.Greater
        else if y then Order.Less
        else Order.Equal

    // TODO it's not truly overriding and doesn't work in generic function,
    //  see extension methods <, <=, >, >=, equiv, nonEquiv for [A: Ord]
    //  Don't accidentally mixed with standard scala math.Ordering.BigInt
    extension (self: Boolean)
        @nowarn inline infix def <(inline other: Boolean): Boolean =
            if self then false else other
        @nowarn inline infix def <=(inline other: Boolean): Boolean =
            if self then other else true
        @nowarn infix def >(other: Boolean): Boolean =
            if self then if other then false else true else false
        @nowarn infix def >=(other: Boolean): Boolean =
            if self then true else if other then false else true
        infix def equiv(other: Boolean): Boolean =
            if self then other else if other then false else true
        infix def nonEquiv(other: Boolean): Boolean =
            if self then if other then false else true else other

    end extension

    given Ord[Data] = {
        val yLess = () => Order.Less
        val yGreater = () => Order.Greater

        (x: Data, y: Data) => {
            import scalus.builtin

            def compareBuiltinList(
                xs: builtin.BuiltinList[Data],
                ys: builtin.BuiltinList[Data]
            ): Order = {
                if xs.isEmpty && ys.isEmpty then Order.Equal
                else if xs.isEmpty then Order.Less
                else if ys.isEmpty then Order.Greater
                else (xs.head <=> ys.head) ifEqualThen compareBuiltinList(xs.tail, ys.tail)
            }

            val xConstr = () => {
                val yConstr = () => {
                    val px = unConstrData(x)
                    val py = unConstrData(y)
                    (px.fst <=> py.fst) ifEqualThen compareBuiltinList(px.snd, py.snd)
                }

                chooseData(y, yConstr, yLess, yLess, yLess, yLess)()
            }

            val xMap = () => {
                val yMap = () => {
                    val lstx = unMapData(x)
                    val lsty = unMapData(y)

                    def compareDataPair(
                        px: BuiltinPair[Data, Data],
                        py: BuiltinPair[Data, Data]
                    ): Order =
                        (px.fst <=> py.fst) ifEqualThen (px.snd <=> py.snd)

                    def go(
                        xs: builtin.BuiltinList[BuiltinPair[Data, Data]],
                        ys: builtin.BuiltinList[BuiltinPair[Data, Data]]
                    ): Order = {
                        if xs.isEmpty && ys.isEmpty then Order.Equal
                        else if xs.isEmpty then Order.Less
                        else if ys.isEmpty then Order.Greater
                        else compareDataPair(xs.head, ys.head) ifEqualThen go(xs.tail, ys.tail)
                    }

                    go(lstx, lsty)
                }

                chooseData(y, yGreater, yMap, yLess, yLess, yLess)()
            }

            val xList = () => {
                val yList = () => {
                    val lstx = unListData(x)
                    val lsty = unListData(y)
                    compareBuiltinList(lstx, lsty)
                }

                chooseData(y, yGreater, yGreater, yList, yLess, yLess)()
            }

            val xI = () => {
                val yI = () => unIData(x) <=> unIData(y)

                chooseData(y, yGreater, yGreater, yGreater, yI, yLess)()
            }

            val xB = () => {
                val yB = () => unBData(x) <=> unBData(y)

                chooseData(y, yGreater, yGreater, yGreater, yGreater, yB)()
            }

            chooseData(x, xConstr, xMap, xList, xI, xB)()
        }
    }

    given [A: Ord, B: Ord]: Ord[(A, B)] = (lhs: (A, B), rhs: (A, B)) =>
        (lhs._1 <=> rhs._1) ifEqualThen (lhs._2 <=> rhs._2)

    given [A: Ord, B: Ord, C: Ord]: Ord[(A, B, C)] = (lhs: (A, B, C), rhs: (A, B, C)) =>
        (lhs._1 <=> rhs._1) ifEqualThen (lhs._2 <=> rhs._2) ifEqualThen (lhs._3 <=> rhs._3)

    extension [A: Ord](self: A)
        inline infix def <(inline other: A): Boolean = Ord[A].lt(self, other)
        inline infix def <=(inline other: A): Boolean = Ord[A].lteq(self, other)
        inline infix def >(inline other: A): Boolean = Ord[A].gt(self, other)
        inline infix def >=(inline other: A): Boolean = Ord[A].gteq(self, other)
        inline infix def equiv(inline other: A): Boolean = Ord[A].equiv(self, other)
        inline infix def nonEquiv(inline other: A): Boolean = Ord[A].nonEquiv(self, other)

    end extension

    def by[A, B: Ord](mapper: A => B): Ord[A] = (lhs: A, rhs: A) => mapper(lhs) <=> mapper(rhs)

    extension [A](self: Ord[A])
        inline def compare(inline lhs: A, inline rhs: A): Order = self.apply(lhs, rhs)
        def lt(lhs: A, rhs: A): Boolean = self.compare(lhs, rhs).isLess
        def lteq(lhs: A, rhs: A): Boolean = self.compare(lhs, rhs).isLessEqual
        def gt(lhs: A, rhs: A): Boolean = self.compare(lhs, rhs).isGreater
        def gteq(lhs: A, rhs: A): Boolean = self.compare(lhs, rhs).isGreaterEqual
        def equiv(lhs: A, rhs: A): Boolean = self.compare(lhs, rhs).isEqual
        def nonEquiv(lhs: A, rhs: A): Boolean = self.compare(lhs, rhs).nonEqual

        def orElse(other: Ord[A]): Ord[A] = (lhs: A, rhs: A) =>
            self.compare(lhs, rhs) ifEqualThen other.compare(lhs, rhs)

        def orElseBy[B: Ord](mapper: A => B): Ord[A] = (lhs: A, rhs: A) =>
            self.compare(lhs, rhs) ifEqualThen Ord[B].compare(mapper(lhs), mapper(rhs))

    end extension

    def keyPairOrd[A: Ord, B]: Ord[(A, B)] = (lhs: (A, B), rhs: (A, B)) => lhs._1 <=> rhs._1

end Ord
