package scalus.sbt

import org.scalatest.funsuite.AnyFunSuite

class LeanLayoutTest extends AnyFunSuite {

    test("packageName joins the words of a project's name, each with a capital") {
        assert(LeanLayout.packageName("my-contracts") == "MyContracts")
        assert(LeanLayout.packageName("htlc") == "Htlc")
        assert(LeanLayout.packageName("scalus_examples.jvm") == "ScalusExamplesJvm")
        assert(LeanLayout.packageName("LinearVesting") == "LinearVesting")
    }

    test("packageName starts with a letter") {
        assert(LeanLayout.packageName("3d-auction") == "DAuction")
        assert(LeanLayout.packageName("2024") == "Proofs")
        assert(LeanLayout.packageName("--") == "Proofs")
    }
}
