package scalus.cardano.onchain.plutus.prelude

import scalus.Compile

enum Order:
    case Less, Equal, Greater

@Compile
object Order:
    given Eq[Order] = (lhs, rhs) =>
        lhs match
            case Less    => rhs.isLess
            case Greater => rhs.isGreater
            case Equal   => rhs.isEqual

    given Ord[Order] = (lhs, rhs) =>
        lhs match
            case Less    => if rhs.isLess then Equal else Less
            case Greater => if rhs.isGreater then Equal else Greater
            case Equal =>
                rhs match
                    case Less    => Greater
                    case Greater => Less
                    case Equal   => Equal

    extension (self: Order)
        def isLess: Boolean = self match { case Less => true; case _ => false }
        def isLessEqual: Boolean = self match { case Greater => false; case _ => true }
        def isGreater: Boolean = self match { case Greater => true; case _ => false }
        def isGreaterEqual: Boolean = self match { case Less => false; case _ => true }
        def isEqual: Boolean = self match { case Equal => true; case _ => false }
        def nonEqual: Boolean = self match { case Equal => false; case _ => true }

        inline infix def ifEqualThen(inline other: => Order): Order =
            if self.nonEqual then self else other

    end extension

end Order
