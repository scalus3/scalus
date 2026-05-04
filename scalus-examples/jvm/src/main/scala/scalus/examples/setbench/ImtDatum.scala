package scalus.examples.setbench

import scalus.compiler.Compile

import scalus.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.Eq

case class ImtDatum(
    remaining: BigInt,
    root: ByteString,
    size: BigInt,
    depth: BigInt
) derives FromData,
      ToData

@Compile
object ImtDatum {
    given Eq[ImtDatum] = Eq.derived
}

enum ImtRedeemer derives FromData, ToData:
    case Add(key: ByteString, proof: Data)
    case Deposit(key: ByteString, proof: Data)
    case Withdraw(key: ByteString, proof: Data)

@Compile
object ImtRedeemer
