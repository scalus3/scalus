package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
import scalus.uplc.{Constant, NamedDeBruijn, Program, Term}
import scalus.uplc.Term.*
import scalus.utils.Hex.toHex

import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** CSE output must not change by accident. New output bytes give every contract compiled with it a
  * new script hash and address, even when size and budget stay the same.
  *
  * The batched CSE does not give the bytes of the one-at-a-time loop of Scalus 1.5.0: see the tie
  * cases below. A deliberate change of the output updates the table; the failure message prints the
  * new one.
  */
class CseOutputHashTest extends AnyFunSuite {

    private val dataDir = Paths.get("scalus-core/jvm/src/test/resources/data/plutus_use_cases")

    /** The first 8 bytes of the SHA-256 of the program's Flat encoding, in hex. */
    private def hash(program: Program): String =
        MessageDigest.getInstance("SHA-256").digest(program.flatEncoded).take(8).toHex

    // name, hash of the program after CSE
    private val expected: Map[String, String] = """
        auction_1-1 5663f4de37b9954b
        auction_1-2 42eca4f52a2e40f5
        auction_1-3 73978b74e956b751
        auction_1-4 8796bdf29cccce30
        auction_2-1 5663f4de37b9954b
        auction_2-2 42eca4f52a2e40f5
        auction_2-3 120161165f1daab1
        auction_2-4 3efb1c3c0941da58
        auction_2-5 ab4f23d7e44cf541
        coop-1 006dc021c34a2633
        coop-2 84a0e2b90e11b75c
        coop-3 cc1654c3db62a6c2
        coop-4 bb8838ba86788473
        coop-5 0c3519f6c3b17b0c
        coop-6 28d2358ecf2a2b27
        coop-7 a870965910912def
        crowdfunding-success-1 93e1b48310140955
        crowdfunding-success-2 14e3518e9a7ca310
        crowdfunding-success-3 155fcc405183afb0
        currency-1 e3a48f7434d0afe3
        escrow-redeem_1-1 85abf27221aeec3b
        escrow-redeem_1-2 d291b85ca25f189d
        escrow-redeem_2-1 2cdcefb8517a5a6f
        escrow-redeem_2-2 11f17de58333eff5
        escrow-redeem_2-3 1006c4befacf9f28
        escrow-refund-1 886ec06d1e14450a
        future-increase-margin-1 1bf9d8a4de8eb173
        future-increase-margin-2 b37870c679a16541
        future-increase-margin-3 d62aa4494715c4a4
        future-increase-margin-4 6236c91d48a2c19a
        future-increase-margin-5 216fb26095f9cc69
        future-pay-out-1 1bf9d8a4de8eb173
        future-pay-out-2 b37870c679a16541
        future-pay-out-3 d62aa4494715c4a4
        future-pay-out-4 7cfac8fa725b0803
        future-settle-early-1 1bf9d8a4de8eb173
        future-settle-early-2 b37870c679a16541
        future-settle-early-3 d62aa4494715c4a4
        future-settle-early-4 00852b8de94f7deb
        game-sm-success_1-1 0643911050b55801
        game-sm-success_1-2 9aafd680bf0048bf
        game-sm-success_1-3 a82d88c753a04fc2
        game-sm-success_1-4 d06a6de429e2be67
        game-sm-success_2-1 0643911050b55801
        game-sm-success_2-2 9aafd680bf0048bf
        game-sm-success_2-3 a82d88c753a04fc2
        game-sm-success_2-4 d06a6de429e2be67
        game-sm-success_2-5 04e1308eb85133be
        game-sm-success_2-6 675563265e8cfb33
        guardrail-sorted-large e2d845ec808c3880
        guardrail-sorted-small 516eb5af3763eaac
        guardrail-unsorted-large d89ea57323538979
        guardrail-unsorted-small b89aaca93bde9da3
        multisig-sm-01 46d33e83ace345bd
        multisig-sm-02 d5f37f87b3dbf184
        multisig-sm-03 c88576bba71a3601
        multisig-sm-04 d24ecf470f912a53
        multisig-sm-05 8a4d7d4a9e9c7b53
        multisig-sm-06 78dc6a44bf8ed9a5
        multisig-sm-07 eca61fa8d29e5dd7
        multisig-sm-08 7005c361290fc06b
        multisig-sm-09 4e8513ab061ad424
        multisig-sm-10 e9df9b72438e9cb1
        ping-pong-1 a05244d4d1b1b588
        ping-pong-2 1484cc8b936d6bb4
        ping-pong_2-1 3463e5bc8e9c558f
        prism-1 e09802e9b688dc6b
        prism-2 6a6d196dc7a97076
        prism-3 e7f74ffb1f7bb5ff
        pubkey-1 0cd76e86b20df089
        stablecoin_1-1 fe058c857b82e6b4
        stablecoin_1-2 d8bc1905a52bd200
        stablecoin_1-3 3a28ff5efe1066a6
        stablecoin_1-4 c2e858df5ac1395e
        stablecoin_1-5 8321aa31c770ce21
        stablecoin_1-6 abfff86c308d7d96
        stablecoin_2-1 fe058c857b82e6b4
        stablecoin_2-2 d8bc1905a52bd200
        stablecoin_2-3 3a28ff5efe1066a6
        stablecoin_2-4 c2e858df5ac1395e
        token-account-1 07c2004c23387686
        token-account-2 effae0474ba4c406
        uniswap-1 eb319d6f73217c44
        uniswap-2 870ebc548540e5d4
        uniswap-3 907f9ae017339c21
        uniswap-4 236147efea769e81
        uniswap-5 0626b28f0a015cda
        uniswap-6 0d1186ab1484066c
        vesting-1 d0f2b2e15d427a29
        """.linesIterator
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(_.split(' '))
        .map(a => a(0) -> a(1))
        .toMap

    test("CSE output of the Plutus use-case scripts keeps its hashes") {
        val stream = Files.list(dataDir)
        val files =
            try stream.iterator.asScala.filter(_.toString.endsWith(".flat")).toVector
            finally stream.close()
        val actual = files.map { file =>
            val program = Program.fromFlatEncoded(Files.readAllBytes(file))
            val shared = program.copy(term = CommonSubexpressionElimination(program.term))
            file.getFileName.toString.stripSuffix(".flat") -> hash(shared)
        }.sorted
        assert(actual.size == 89)
        val changed = actual.filter((name, h) => !expected.get(name).contains(h)).map(_._1)
        val table = actual.map((name, h) => s"$name $h").mkString("\n")
        assert(
          changed.isEmpty,
          s"CSE output changed for ${changed.mkString(", ")}. If on purpose, the table is:\n$table"
        )
    }

    // The tie cases: f, g, h and a are free. Each of [f a], [g a] and (force a) saves the same
    // estimated lovelace, so traversal order decides between them.
    private def v(name: String): Term = Var(NamedDeBruijn(name))
    private def tuple(ts: Term*): Term = Constr(Word64.Zero, ts.toList)
    private def fa: Term = Apply(v("f"), v("a"))
    private def ga: Term = Apply(v("g"), v("a"))
    private def forceA: Term = Force(v("a"))
    private def hDelayFa: Term = Apply(v("h"), Delay(fa))

    /** The names that CSE gives its bindings, in the order it extracts them. */
    private def extracted(term: Term): Seq[String] = {
        val cse = new CommonSubexpressionElimination()
        cse(term)
        cse.logs.map(_.split(" as ").last)
    }

    test("tied candidates keep their order from before the first extraction") {
        // (h (delay [f a])) twice, then lam z. Constr(0, [g a, g a, f a, f a]).
        // The first [f a] is inside the first [h ..], so [f a] ranks before [g a]. Scalus 1.5.0
        // ranked again after extracting [h ..], which moves that [f a] behind the let body, and
        // extracted [g a] first: its let was the outer one.
        val term = tuple(hDelayFa, hDelayFa, LamAbs("z", tuple(ga, ga, fa, fa)))
        assert(extracted(term) == Seq("__cse_h_f_a", "__cse_f_a", "__cse_g_a"))
    }

    test("a held-back candidate goes inside the let of a candidate taken before it") {
        // [f a] occurs three times, once inside an [h ..], so it waits for the second pass.
        // (force a) is taken in the first pass, and [f a] then binds inside its let. Scalus
        // 1.5.0 extracted [f a] first, as it saves more, and its let was the outer one.
        val term =
            tuple(hDelayFa, Delay(tuple(hDelayFa, tuple(fa, fa, forceA, forceA))))
        assert(extracted(term) == Seq("__cse_h_f_a", "__cse_a", "__cse_f_a"))
    }

    test("a tie can give other bindings than Scalus 1.5.0") {
        // T = [(lam y 0) (error)] three times in each of two delays; the constant 0 occurs once
        // more in the first delay and four times more in the second. T in each delay and the
        // constant in the second all save the same estimated lovelace. One pass takes T in both
        // delays. Scalus 1.5.0 took T in the first delay, then ranked the constant first; after
        // the constant was shared, T in the second delay no longer saved anything.
        def zero: Term = Const(Constant.Integer(0))
        def t: Term = Apply(LamAbs("y", zero), Error())
        val term = tuple(
          Delay(tuple(t, t, t, zero)),
          Delay(tuple(t, t, t, zero, zero, zero, zero))
        )
        assert(extracted(term) == Seq("__cse_Lam_c_Err", "__cse_Lam_c_Err_1"))
    }
}
