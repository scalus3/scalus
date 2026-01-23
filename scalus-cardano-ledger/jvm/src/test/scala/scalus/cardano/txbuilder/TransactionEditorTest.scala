package scalus.cardano.txbuilder

import monocle.syntax.all.*
import monocle.{Focus, Lens}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.ledger.*
import scalus.cardano.ledger.RedeemerTag.Spend
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionEditor.{editTransaction, editTransactionSafe}
import scalus.|>

private def addInput(input: TransactionInput): Transaction => Transaction =
    txBodyL
        .refocus(_.inputs)
        .modify((is: TaggedSortedSet[TransactionInput]) =>
            TaggedSortedSet.from(
              is.toSet + input
            )
        )

class TransactionEditorTest extends AnyFunSuite, ScalaCheckPropertyChecks {

    val oneInput: Transaction = {
        val l1 = txBodyL
            .refocus(_.inputs)
            .replace(TaggedSortedSet(input1))
        val l2 = txWitnessSetL
            .refocus(_.redeemers)
            .replace(
              Some(
                KeepRaw(
                  Redeemers(
                    Redeemer(
                      tag = Spend,
                      index = 0,
                      data = ByteString.fromHex("").toData,
                      exUnits = ExUnits.zero
                    )
                  )
                )
              )
            )
        anyNetworkTx |> l1 |> l2
    }

    test("do nothing") {
        assert(editTransaction(identity)(oneInput) == oneInput)
    }

    test("attach one input to the end") {
        val tx1 = txBodyL.refocus(_.inputs).replace(TaggedSortedSet(input1, input2))(anyNetworkTx)
        val expectedTx = tx1 |> txWitnessSetL
            .refocus(_.redeemers)
            .replace(
              Some(
                KeepRaw(
                  Redeemers(
                    Redeemer(
                      tag = Spend,
                      index = 0,
                      data = ByteString.fromHex("").toData,
                      exUnits = ExUnits.zero
                    )
                  )
                )
              )
            )

        assert(
          editTransaction(
            txBodyL
                .refocus(_.inputs)
                .modify((i: TaggedSortedSet[TransactionInput]) =>
                    TaggedSortedSet.from(i.toSeq :+ input2)
                )
          )(
            oneInput
          ) == expectedTx
        )
    }

    test("remove two inputs, before and after") {
        val tx1 = {
            val l1 =
                txWitnessSetL
                    .refocus(_.redeemers)
                    .replace(Some(KeepRaw(Redeemers(unitRedeemer.focus(_.index).replace(1)))))
            val l2 = txBodyL
                .refocus(_.inputs)
                .replace(TaggedSortedSet(input0, input1, input2))
            anyNetworkTx |> l1 |> l2
        }
        val tx2 = {
            val l1 = txWitnessSetL
                .refocus(_.redeemers)
                .replace(Some(KeepRaw(Redeemers(unitRedeemer))))
            val l2 = txBodyL
                .refocus(_.inputs)
                .replace(TaggedSortedSet(input1))
            anyNetworkTx |> l1 |> l2
        }

        assert(
          editTransactionSafe(
            txBodyL.refocus(_.inputs).replace(TaggedSortedSet(input1))
          )(tx1) == Right(tx2)
        )
    }

    test("remove two inputs with redeemers, before and after") {
        val tx1 = {
            val l1 = txWitnessSetL
                .refocus(_.redeemers)
                .replace(
                  Some(
                    KeepRaw(
                      Redeemers(
                        unitRedeemer,
                        Redeemer(
                          tag = Spend,
                          index = 1,
                          data = Data.List(PList.Nil),
                          exUnits = ExUnits.zero
                        ),
                        Redeemer(
                          tag = Spend,
                          index = 2,
                          data = Data.Map(PList.Nil),
                          exUnits = ExUnits.zero
                        )
                      )
                    )
                  )
                )
            val l2 = txBodyL
                .refocus(_.inputs)
                .replace(TaggedSortedSet(input0, input1, input2))
            anyNetworkTx |> l1 |> l2
        }
        val tx2 = {
            val l1 = txWitnessSetL
                .refocus(_.redeemers)
                .replace(
                  Some(
                    KeepRaw(
                      Redeemers(
                        Redeemer(
                          tag = Spend,
                          index = 0,
                          data = Data.List(PList.Nil),
                          exUnits = ExUnits.zero
                        )
                      )
                    )
                  )
                )
            val l2 =
                txBodyL
                    .refocus(_.inputs)
                    .replace(TaggedSortedSet(input1))

            anyNetworkTx |> l1 |> l2
        }
        assert(
          editTransactionSafe(
            txBodyL.refocus(_.inputs).replace(TaggedSortedSet(input1))
          )(tx1) == Right(tx2)
        )
    }

    test("remove input & redeemer, add another input & redeemer") {
        val tx1 = {
            val l1 = txWitnessSetL
                .refocus(_.redeemers)
                .replace(
                  Some(
                    KeepRaw(
                      Redeemers(
                        unitRedeemer,
                        Redeemer(
                          tag = Spend,
                          index = 1,
                          data = Data.Map(PList.Nil),
                          exUnits = ExUnits.zero
                        )
                      )
                    )
                  )
                )
            val l2 = txBodyL
                .refocus(_.inputs)
                .replace(TaggedSortedSet(input1, input2))
            anyNetworkTx |> l1 |> l2
        }

        val tx2 = {
            val l1 = txWitnessSetL
                .refocus(_.redeemers)
                .replace(
                  Some(
                    KeepRaw(
                      Redeemers(
                        Redeemer(
                          tag = Spend,
                          index = 1,
                          data = Data.Map(PList.Nil),
                          exUnits = ExUnits.zero
                        ),
                        Redeemer(
                          tag = Spend,
                          index = 0,
                          data = Data.List(PList.Nil),
                          exUnits = ExUnits.zero
                        )
                      )
                    )
                  )
                )
            val l2 =
                txBodyL
                    .refocus(_.inputs)
                    .replace(TaggedSortedSet(input0, input2))
            anyNetworkTx |> l1 |> l2
        }
        assert(
          (tx1 |> editTransactionSafe(
            txBodyL
                .refocus(_.inputs)
                .replace(TaggedSortedSet(input0, input2))
                .compose(
                  txWitnessSetL
                      .refocus(_.redeemers)
                      .replace(
                        Some(
                          KeepRaw(
                            Redeemers(
                              Redeemer(
                                tag = Spend,
                                index = 0,
                                data = Data.List(PList.Nil),
                                exUnits = ExUnits.zero
                              )
                            )
                          )
                        )
                      )
                )
          )) == Right(tx2)
        )
    }
}
