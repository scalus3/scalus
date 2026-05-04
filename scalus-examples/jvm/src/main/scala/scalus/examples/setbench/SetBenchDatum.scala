package scalus.examples.setbench

import scalus.compiler.Compile

import scalus.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.Eq

case class SetBenchDatum(
    remaining: BigInt,
    root: ByteString
) derives FromData,
      ToData

@Compile
object SetBenchDatum {
    given Eq[SetBenchDatum] = Eq.derived
}

enum SetBenchRedeemer derives FromData, ToData:
    case Withdraw(key: ByteString, value: ByteString, proof: Data)
    case Deposit(key: ByteString, value: ByteString, proof: Data)

@Compile
object SetBenchRedeemer

case class AccWithdrawRedeemer(
    element: BigInt,
    compressedProof: ByteString
) derives FromData,
      ToData

@Compile
object AccWithdrawRedeemer
