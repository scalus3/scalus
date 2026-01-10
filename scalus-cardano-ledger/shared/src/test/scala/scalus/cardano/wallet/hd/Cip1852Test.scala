package scalus.cardano.wallet.hd

import org.scalatest.funsuite.AnyFunSuite

/** CIP-1852 path utilities tests. */
class Cip1852Test extends AnyFunSuite {

    test("constants") {
        assert(Cip1852.Purpose == 1852)
        assert(Cip1852.CoinType == 1815)
        assert(Cip1852.RoleExternal == 0)
        assert(Cip1852.RoleInternal == 1)
        assert(Cip1852.RoleStaking == 2)
        // CIP-105 roles
        assert(Cip1852.RoleDRep == 3)
        assert(Cip1852.RoleCCCold == 4)
        assert(Cip1852.RoleCCHot == 5)
    }

    test("Role enum") {
        assert(Cip1852.Role.External.value == 0)
        assert(Cip1852.Role.Internal.value == 1)
        assert(Cip1852.Role.Staking.value == 2)
        assert(Cip1852.Role.DRep.value == 3)
        assert(Cip1852.Role.CCCold.value == 4)
        assert(Cip1852.Role.CCHot.value == 5)

        assert(Cip1852.Role.fromInt(0) == Some(Cip1852.Role.External))
        assert(Cip1852.Role.fromInt(1) == Some(Cip1852.Role.Internal))
        assert(Cip1852.Role.fromInt(2) == Some(Cip1852.Role.Staking))
        assert(Cip1852.Role.fromInt(3) == Some(Cip1852.Role.DRep))
        assert(Cip1852.Role.fromInt(4) == Some(Cip1852.Role.CCCold))
        assert(Cip1852.Role.fromInt(5) == Some(Cip1852.Role.CCHot))
        assert(Cip1852.Role.fromInt(6) == None)
    }

    test("accountPath") {
        assert(Cip1852.accountPath(0) == "m/1852'/1815'/0'")
        assert(Cip1852.accountPath(1) == "m/1852'/1815'/1'")
        assert(Cip1852.accountPath(100) == "m/1852'/1815'/100'")
    }

    test("paymentPath") {
        assert(Cip1852.paymentPath(0, 0) == "m/1852'/1815'/0'/0/0")
        assert(Cip1852.paymentPath(0, 1) == "m/1852'/1815'/0'/0/1")
        assert(Cip1852.paymentPath(1, 5) == "m/1852'/1815'/1'/0/5")
    }

    test("changePath") {
        assert(Cip1852.changePath(0, 0) == "m/1852'/1815'/0'/1/0")
        assert(Cip1852.changePath(0, 1) == "m/1852'/1815'/0'/1/1")
        assert(Cip1852.changePath(2, 3) == "m/1852'/1815'/2'/1/3")
    }

    test("stakingPath") {
        assert(Cip1852.stakingPath(0) == "m/1852'/1815'/0'/2/0")
        assert(Cip1852.stakingPath(0, 0) == "m/1852'/1815'/0'/2/0")
        assert(Cip1852.stakingPath(1, 0) == "m/1852'/1815'/1'/2/0")
    }

    test("drepPath (CIP-105)") {
        assert(Cip1852.drepPath(0) == "m/1852'/1815'/0'/3/0")
        assert(Cip1852.drepPath(0, 0) == "m/1852'/1815'/0'/3/0")
        assert(Cip1852.drepPath(1, 0) == "m/1852'/1815'/1'/3/0")
    }

    test("path with Role enum") {
        assert(Cip1852.path(0, Cip1852.Role.External, 0) == "m/1852'/1815'/0'/0/0")
        assert(Cip1852.path(0, Cip1852.Role.Internal, 0) == "m/1852'/1815'/0'/1/0")
        assert(Cip1852.path(0, Cip1852.Role.Staking, 0) == "m/1852'/1815'/0'/2/0")
    }

    test("parsePath - valid paths") {
        val result1 = Cip1852.parsePath("m/1852'/1815'/0'/0/0")
        assert(result1 == Some((0, Cip1852.Role.External, 0)))

        val result2 = Cip1852.parsePath("m/1852'/1815'/0'/1/5")
        assert(result2 == Some((0, Cip1852.Role.Internal, 5)))

        val result3 = Cip1852.parsePath("m/1852'/1815'/2'/2/0")
        assert(result3 == Some((2, Cip1852.Role.Staking, 0)))
    }

    test("parsePath - invalid paths") {
        // Wrong purpose
        assert(Cip1852.parsePath("m/44'/1815'/0'/0/0") == None)

        // Wrong coin type
        assert(Cip1852.parsePath("m/1852'/60'/0'/0/0") == None)

        // Wrong number of components
        assert(Cip1852.parsePath("m/1852'/1815'/0'") == None)

        // Invalid role (6 is not defined in CIP-1852/CIP-105)
        assert(Cip1852.parsePath("m/1852'/1815'/0'/6/0") == None)
    }

    test("parseAccountPath - valid paths") {
        assert(Cip1852.parseAccountPath("m/1852'/1815'/0'") == Some(0))
        assert(Cip1852.parseAccountPath("m/1852'/1815'/5'") == Some(5))
    }

    test("parseAccountPath - invalid paths") {
        // Wrong purpose
        assert(Cip1852.parseAccountPath("m/44'/1815'/0'") == None)

        // Too many components
        assert(Cip1852.parseAccountPath("m/1852'/1815'/0'/0/0") == None)
    }

    test("round-trip paths") {
        val account = 2
        val role = Cip1852.Role.External
        val index = 10

        val pathStr = Cip1852.path(account, role, index)
        val parsed = Cip1852.parsePath(pathStr)

        assert(parsed == Some((account, role, index)))
    }

    test("negative index validation") {
        intercept[IllegalArgumentException] {
            Cip1852.accountPath(-1)
        }

        intercept[IllegalArgumentException] {
            Cip1852.paymentPath(0, -1)
        }

        intercept[IllegalArgumentException] {
            Cip1852.changePath(-1, 0)
        }
    }
}
