package scalus.benchmarks

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.{ExUnits, Language}
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.prelude.*
import scalus.testing.kit.ScalusTest

import scala.annotation.nowarn
import scala.language.implicitConversions

class ClausifyTest extends AnyFunSuite, ScalusTest:
    import ClausifyTest.*
    import scalus.uplc.eval.PlutusVM

    inline val isAlwaysPrintComparison = true

    // Use PlutusV4 VM to evaluate code compiled with PlutusV4 features (case on booleans)
    override protected def plutusVM: PlutusVM = PlutusVM.makePlutusV4VM()

    @Ignore
    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      targetLanguage = Language.PlutusV4,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("F1") {
        val sir = Compiler
            .compile {
                // (a = a) = (a = a) = (a = a)
                val formula = (1 <-> 1) <-> ((1 <-> 1) <-> (1 <-> 1))
                val expected = List.empty[LRVars]
                require(formula.clauses === expected)
            }
        val uplc = sir.toUplcOptimized(generateErrorTraces = false)

        val result = uplc.evaluateDebug

        val options = summon[Options]
        val scalusBudget =
            if options.targetLanguage == Language.PlutusV4 then
                ExUnits(memory = 39553100L, steps = 10777880482L)
            else if options.targetLoweringBackend == TargetLoweringBackend.SirToUplcV3Lowering
            then ExUnits(memory = 75014277L, steps = 22595514040L)
            else if options.targetLoweringBackend == TargetLoweringBackend.SumOfProductsLowering
            then ExUnits(memory = 45835971L, steps = 7879192811L)
            else {
                // not tested
                ExUnits(memory = 100000L, steps = 1000000000L)
            }
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "ClausifyTest.F1",
          scalusBudget = scalusBudget,
          refBudget = ExUnits(memory = 39891097L, steps = 12325496028L),
          isPrintComparison = true
        )
    }

    test("F2") {
        val result = Compiler
            .compile {
                // (a = a = a) = (a = a = a)
                val formula = (1 <-> (1 <-> 1)) <-> (1 <-> (1 <-> 1))
                val expected = List.empty[LRVars]
                require(formula.clauses === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        val options = summon[Options]
        val scalusBudget =
            if options.targetLanguage == Language.PlutusV4 then
                ExUnits(memory = 49393832L, steps = 13402487336L)
            else
                options.targetLoweringBackend match {
                    case TargetLoweringBackend.SirToUplcV3Lowering =>
                        ExUnits(memory = 93117893L, steps = 27997423766L)
                    case TargetLoweringBackend.SumOfProductsLowering =>
                        ExUnits(memory = 57029883L, steps = 9813458115L)
                    case TargetLoweringBackend.ScottEncodingLowering =>
                        // not tested
                        ExUnits(memory = 100000L, steps = 1000000000L)
                }

        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "ClausifyTest.F2",
          scalusBudget = scalusBudget,
          refBudget = ExUnits(memory = 50524767L, steps = 15570882882L),
          isPrintComparison = true
        )
    }

    test("F3") {
        val result = Compiler
            .compile {
                // (a = a = a) = (a = a) = (a = a)
                val formula = (1 <-> (1 <-> 1)) <-> ((1 <-> 1) <-> (1 <-> 1))
                val expected = List.single[LRVars]((List.single[Var](1), List.empty[Var]))
                require(formula.clauses === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        val options = summon[Options]
        val scalusBudget =
            if options.targetLanguage == Language.PlutusV4 then
                ExUnits(memory = 131034190L, steps = 35505498890L)
            else if options.targetLoweringBackend == TargetLoweringBackend.SirToUplcV3Lowering
            then ExUnits(memory = 248968345L, steps = 74900219564L)
            else ExUnits(memory = 152347441L, steps = 26254484239L)
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "ClausifyTest.F3",
          scalusBudget = scalusBudget,
          refBudget = ExUnits(memory = 136054751L, steps = 41872495549L),
          isPrintComparison = true
        )
    }

    test("F4") {
        val result = Compiler
            .compile {
                // (a = b = c) = (d = e) = (f = g)
                val formula = (1 <-> (2 <-> 3)) <-> ((4 <-> 5) <-> (6 <-> 7))

                import scalus.prelude.List.*
                val expected = Cons[LRVars](
                  (Cons(1, Nil), Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Nil))))))),
                  Cons(
                    (Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Cons(7, Nil))))),
                    Cons(
                      (Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))), Cons(6, Cons(7, Nil))),
                      Cons(
                        (Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Nil))))))), Nil),
                        Cons(
                          (Cons(1, Cons(2, Cons(3, Cons(4, Cons(6, Nil))))), Cons(5, Cons(7, Nil))),
                          Cons(
                            (
                              Cons(1, Cons(2, Cons(3, Cons(4, Cons(7, Nil))))),
                              Cons(5, Cons(6, Nil))
                            ),
                            Cons(
                              (
                                Cons(1, Cons(2, Cons(3, Cons(5, Cons(6, Nil))))),
                                Cons(4, Cons(7, Nil))
                              ),
                              Cons(
                                (
                                  Cons(1, Cons(2, Cons(3, Cons(5, Cons(7, Nil))))),
                                  Cons(4, Cons(6, Nil))
                                ),
                                Cons(
                                  (
                                    Cons(1, Cons(2, Cons(3, Cons(6, Cons(7, Nil))))),
                                    Cons(4, Cons(5, Nil))
                                  ),
                                  Cons(
                                    (
                                      Cons(1, Cons(2, Cons(4, Nil))),
                                      Cons(3, Cons(5, Cons(6, Cons(7, Nil))))
                                    ),
                                    Cons(
                                      (
                                        Cons(1, Cons(2, Cons(4, Cons(5, Cons(6, Nil))))),
                                        Cons(3, Cons(7, Nil))
                                      ),
                                      Cons(
                                        (
                                          Cons(1, Cons(2, Cons(4, Cons(5, Cons(7, Nil))))),
                                          Cons(3, Cons(6, Nil))
                                        ),
                                        Cons(
                                          (
                                            Cons(1, Cons(2, Cons(4, Cons(6, Cons(7, Nil))))),
                                            Cons(3, Cons(5, Nil))
                                          ),
                                          Cons(
                                            (
                                              Cons(1, Cons(2, Cons(5, Nil))),
                                              Cons(3, Cons(4, Cons(6, Cons(7, Nil))))
                                            ),
                                            Cons(
                                              (
                                                Cons(1, Cons(2, Cons(5, Cons(6, Cons(7, Nil))))),
                                                Cons(3, Cons(4, Nil))
                                              ),
                                              Cons(
                                                (
                                                  Cons(1, Cons(2, Cons(6, Nil))),
                                                  Cons(3, Cons(4, Cons(5, Cons(7, Nil))))
                                                ),
                                                Cons(
                                                  (
                                                    Cons(1, Cons(2, Cons(7, Nil))),
                                                    Cons(3, Cons(4, Cons(5, Cons(6, Nil))))
                                                  ),
                                                  Cons(
                                                    (
                                                      Cons(1, Cons(3, Cons(4, Nil))),
                                                      Cons(2, Cons(5, Cons(6, Cons(7, Nil))))
                                                    ),
                                                    Cons(
                                                      (
                                                        Cons(
                                                          1,
                                                          Cons(3, Cons(4, Cons(5, Cons(6, Nil))))
                                                        ),
                                                        Cons(2, Cons(7, Nil))
                                                      ),
                                                      Cons(
                                                        (
                                                          Cons(
                                                            1,
                                                            Cons(3, Cons(4, Cons(5, Cons(7, Nil))))
                                                          ),
                                                          Cons(2, Cons(6, Nil))
                                                        ),
                                                        Cons(
                                                          (
                                                            Cons(
                                                              1,
                                                              Cons(
                                                                3,
                                                                Cons(4, Cons(6, Cons(7, Nil)))
                                                              )
                                                            ),
                                                            Cons(2, Cons(5, Nil))
                                                          ),
                                                          Cons(
                                                            (
                                                              Cons(1, Cons(3, Cons(5, Nil))),
                                                              Cons(
                                                                2,
                                                                Cons(4, Cons(6, Cons(7, Nil)))
                                                              )
                                                            ),
                                                            Cons(
                                                              (
                                                                Cons(
                                                                  1,
                                                                  Cons(
                                                                    3,
                                                                    Cons(5, Cons(6, Cons(7, Nil)))
                                                                  )
                                                                ),
                                                                Cons(2, Cons(4, Nil))
                                                              ),
                                                              Cons(
                                                                (
                                                                  Cons(1, Cons(3, Cons(6, Nil))),
                                                                  Cons(
                                                                    2,
                                                                    Cons(4, Cons(5, Cons(7, Nil)))
                                                                  )
                                                                ),
                                                                Cons(
                                                                  (
                                                                    Cons(1, Cons(3, Cons(7, Nil))),
                                                                    Cons(
                                                                      2,
                                                                      Cons(4, Cons(5, Cons(6, Nil)))
                                                                    )
                                                                  ),
                                                                  Cons(
                                                                    (
                                                                      Cons(
                                                                        1,
                                                                        Cons(4, Cons(5, Nil))
                                                                      ),
                                                                      Cons(
                                                                        2,
                                                                        Cons(
                                                                          3,
                                                                          Cons(6, Cons(7, Nil))
                                                                        )
                                                                      )
                                                                    ),
                                                                    Cons(
                                                                      (
                                                                        Cons(
                                                                          1,
                                                                          Cons(
                                                                            4,
                                                                            Cons(
                                                                              5,
                                                                              Cons(6, Cons(7, Nil))
                                                                            )
                                                                          )
                                                                        ),
                                                                        Cons(2, Cons(3, Nil))
                                                                      ),
                                                                      Cons(
                                                                        (
                                                                          Cons(
                                                                            1,
                                                                            Cons(4, Cons(6, Nil))
                                                                          ),
                                                                          Cons(
                                                                            2,
                                                                            Cons(
                                                                              3,
                                                                              Cons(5, Cons(7, Nil))
                                                                            )
                                                                          )
                                                                        ),
                                                                        Cons(
                                                                          (
                                                                            Cons(
                                                                              1,
                                                                              Cons(4, Cons(7, Nil))
                                                                            ),
                                                                            Cons(
                                                                              2,
                                                                              Cons(
                                                                                3,
                                                                                Cons(
                                                                                  5,
                                                                                  Cons(6, Nil)
                                                                                )
                                                                              )
                                                                            )
                                                                          ),
                                                                          Cons(
                                                                            (
                                                                              Cons(
                                                                                1,
                                                                                Cons(
                                                                                  5,
                                                                                  Cons(6, Nil)
                                                                                )
                                                                              ),
                                                                              Cons(
                                                                                2,
                                                                                Cons(
                                                                                  3,
                                                                                  Cons(
                                                                                    4,
                                                                                    Cons(7, Nil)
                                                                                  )
                                                                                )
                                                                              )
                                                                            ),
                                                                            Cons(
                                                                              (
                                                                                Cons(
                                                                                  1,
                                                                                  Cons(
                                                                                    5,
                                                                                    Cons(7, Nil)
                                                                                  )
                                                                                ),
                                                                                Cons(
                                                                                  2,
                                                                                  Cons(
                                                                                    3,
                                                                                    Cons(
                                                                                      4,
                                                                                      Cons(6, Nil)
                                                                                    )
                                                                                  )
                                                                                )
                                                                              ),
                                                                              Cons(
                                                                                (
                                                                                  Cons(
                                                                                    1,
                                                                                    Cons(
                                                                                      6,
                                                                                      Cons(7, Nil)
                                                                                    )
                                                                                  ),
                                                                                  Cons(
                                                                                    2,
                                                                                    Cons(
                                                                                      3,
                                                                                      Cons(
                                                                                        4,
                                                                                        Cons(5, Nil)
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                ),
                                                                                Cons(
                                                                                  (
                                                                                    Cons(2, Nil),
                                                                                    Cons(
                                                                                      1,
                                                                                      Cons(
                                                                                        3,
                                                                                        Cons(
                                                                                          4,
                                                                                          Cons(
                                                                                            5,
                                                                                            Cons(
                                                                                              6,
                                                                                              Cons(
                                                                                                7,
                                                                                                Nil
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  ),
                                                                                  Cons(
                                                                                    (
                                                                                      Cons(
                                                                                        2,
                                                                                        Cons(
                                                                                          3,
                                                                                          Cons(
                                                                                            4,
                                                                                            Nil
                                                                                          )
                                                                                        )
                                                                                      ),
                                                                                      Cons(
                                                                                        1,
                                                                                        Cons(
                                                                                          5,
                                                                                          Cons(
                                                                                            6,
                                                                                            Cons(
                                                                                              7,
                                                                                              Nil
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    ),
                                                                                    Cons(
                                                                                      (
                                                                                        Cons(
                                                                                          2,
                                                                                          Cons(
                                                                                            3,
                                                                                            Cons(
                                                                                              4,
                                                                                              Cons(
                                                                                                5,
                                                                                                Cons(
                                                                                                  6,
                                                                                                  Nil
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        ),
                                                                                        Cons(
                                                                                          1,
                                                                                          Cons(
                                                                                            7,
                                                                                            Nil
                                                                                          )
                                                                                        )
                                                                                      ),
                                                                                      Cons(
                                                                                        (
                                                                                          Cons(
                                                                                            2,
                                                                                            Cons(
                                                                                              3,
                                                                                              Cons(
                                                                                                4,
                                                                                                Cons(
                                                                                                  5,
                                                                                                  Cons(
                                                                                                    7,
                                                                                                    Nil
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          ),
                                                                                          Cons(
                                                                                            1,
                                                                                            Cons(
                                                                                              6,
                                                                                              Nil
                                                                                            )
                                                                                          )
                                                                                        ),
                                                                                        Cons(
                                                                                          (
                                                                                            Cons(
                                                                                              2,
                                                                                              Cons(
                                                                                                3,
                                                                                                Cons(
                                                                                                  4,
                                                                                                  Cons(
                                                                                                    6,
                                                                                                    Cons(7, Nil)
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            ),
                                                                                            Cons(
                                                                                              1,
                                                                                              Cons(
                                                                                                5,
                                                                                                Nil
                                                                                              )
                                                                                            )
                                                                                          ),
                                                                                          Cons(
                                                                                            (
                                                                                              Cons(
                                                                                                2,
                                                                                                Cons(
                                                                                                  3,
                                                                                                  Cons(
                                                                                                    5,
                                                                                                    Nil
                                                                                                  )
                                                                                                )
                                                                                              ),
                                                                                              Cons(
                                                                                                1,
                                                                                                Cons(
                                                                                                  4,
                                                                                                  Cons(
                                                                                                    6,
                                                                                                    Cons(7, Nil)
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            ),
                                                                                            Cons(
                                                                                              (
                                                                                                Cons(
                                                                                                  2,
                                                                                                  Cons(
                                                                                                    3,
                                                                                                    Cons(
                                                                                                      5,
                                                                                                      Cons(
                                                                                                        6,
                                                                                                        Cons(7, Nil)
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                ),
                                                                                                Cons(
                                                                                                  1,
                                                                                                  Cons(
                                                                                                    4,
                                                                                                    Nil
                                                                                                  )
                                                                                                )
                                                                                              ),
                                                                                              Cons(
                                                                                                (
                                                                                                  Cons(
                                                                                                    2,
                                                                                                    Cons(
                                                                                                      3,
                                                                                                      Cons(6, Nil)
                                                                                                    )
                                                                                                  ),
                                                                                                  Cons(
                                                                                                    1,
                                                                                                    Cons(
                                                                                                      4,
                                                                                                      Cons(
                                                                                                        5,
                                                                                                        Cons(7, Nil)
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                ),
                                                                                                Cons(
                                                                                                  (
                                                                                                    Cons(
                                                                                                      2,
                                                                                                      Cons(
                                                                                                        3,
                                                                                                        Cons(7, Nil)
                                                                                                      )
                                                                                                    ),
                                                                                                    Cons(
                                                                                                      1,
                                                                                                      Cons(
                                                                                                        4,
                                                                                                        Cons(
                                                                                                          5,
                                                                                                          Cons(6, Nil)
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  ),
                                                                                                  Cons(
                                                                                                    (
                                                                                                      Cons(
                                                                                                        2,
                                                                                                        Cons(
                                                                                                          4,
                                                                                                          Cons(5, Nil)
                                                                                                        )
                                                                                                      ),
                                                                                                      Cons(
                                                                                                        1,
                                                                                                        Cons(
                                                                                                          3,
                                                                                                          Cons(
                                                                                                            6,
                                                                                                            Cons(7, Nil)
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    ),
                                                                                                    Cons(
                                                                                                      (
                                                                                                        Cons(
                                                                                                          2,
                                                                                                          Cons(
                                                                                                            4,
                                                                                                            Cons(
                                                                                                              5,
                                                                                                              Cons(
                                                                                                                6,
                                                                                                                Cons(7, Nil)
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        ),
                                                                                                        Cons(
                                                                                                          1,
                                                                                                          Cons(3, Nil)
                                                                                                        )
                                                                                                      ),
                                                                                                      Cons(
                                                                                                        (
                                                                                                          Cons(
                                                                                                            2,
                                                                                                            Cons(
                                                                                                              4,
                                                                                                              Cons(6, Nil)
                                                                                                            )
                                                                                                          ),
                                                                                                          Cons(
                                                                                                            1,
                                                                                                            Cons(
                                                                                                              3,
                                                                                                              Cons(
                                                                                                                5,
                                                                                                                Cons(7, Nil)
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        ),
                                                                                                        Cons(
                                                                                                          (
                                                                                                            Cons(
                                                                                                              2,
                                                                                                              Cons(
                                                                                                                4,
                                                                                                                Cons(7, Nil)
                                                                                                              )
                                                                                                            ),
                                                                                                            Cons(
                                                                                                              1,
                                                                                                              Cons(
                                                                                                                3,
                                                                                                                Cons(
                                                                                                                  5,
                                                                                                                  Cons(6, Nil)
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          ),
                                                                                                          Cons(
                                                                                                            (
                                                                                                              Cons(
                                                                                                                2,
                                                                                                                Cons(
                                                                                                                  5,
                                                                                                                  Cons(6, Nil)
                                                                                                                )
                                                                                                              ),
                                                                                                              Cons(
                                                                                                                1,
                                                                                                                Cons(
                                                                                                                  3,
                                                                                                                  Cons(
                                                                                                                    4,
                                                                                                                    Cons(7, Nil)
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            ),
                                                                                                            Cons(
                                                                                                              (
                                                                                                                Cons(
                                                                                                                  2,
                                                                                                                  Cons(
                                                                                                                    5,
                                                                                                                    Cons(7, Nil)
                                                                                                                  )
                                                                                                                ),
                                                                                                                Cons(
                                                                                                                  1,
                                                                                                                  Cons(
                                                                                                                    3,
                                                                                                                    Cons(4, Cons(6, Nil))
                                                                                                                  )
                                                                                                                )
                                                                                                              ),
                                                                                                              Cons(
                                                                                                                (
                                                                                                                  Cons(
                                                                                                                    2,
                                                                                                                    Cons(6, Cons(7, Nil))
                                                                                                                  ),
                                                                                                                  Cons(
                                                                                                                    1,
                                                                                                                    Cons(
                                                                                                                      3,
                                                                                                                      Cons(4, Cons(5, Nil))
                                                                                                                    )
                                                                                                                  )
                                                                                                                ),
                                                                                                                Cons(
                                                                                                                  (
                                                                                                                    Cons(3, Nil),
                                                                                                                    Cons(
                                                                                                                      1,
                                                                                                                      Cons(
                                                                                                                        2,
                                                                                                                        Cons(
                                                                                                                          4,
                                                                                                                          Cons(
                                                                                                                            5,
                                                                                                                            Cons(6, Cons(7, Nil))
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ),
                                                                                                                  Cons(
                                                                                                                    (
                                                                                                                      Cons(
                                                                                                                        3,
                                                                                                                        Cons(4, Cons(5, Nil))
                                                                                                                      ),
                                                                                                                      Cons(
                                                                                                                        1,
                                                                                                                        Cons(
                                                                                                                          2,
                                                                                                                          Cons(6, Cons(7, Nil))
                                                                                                                        )
                                                                                                                      )
                                                                                                                    ),
                                                                                                                    Cons(
                                                                                                                      (
                                                                                                                        Cons(
                                                                                                                          3,
                                                                                                                          Cons(
                                                                                                                            4,
                                                                                                                            Cons(
                                                                                                                              5,
                                                                                                                              Cons(6, Cons(7, Nil))
                                                                                                                            )
                                                                                                                          )
                                                                                                                        ),
                                                                                                                        Cons(1, Cons(2, Nil))
                                                                                                                      ),
                                                                                                                      Cons(
                                                                                                                        (
                                                                                                                          Cons(
                                                                                                                            3,
                                                                                                                            Cons(4, Cons(6, Nil))
                                                                                                                          ),
                                                                                                                          Cons(
                                                                                                                            1,
                                                                                                                            Cons(
                                                                                                                              2,
                                                                                                                              Cons(5, Cons(7, Nil))
                                                                                                                            )
                                                                                                                          )
                                                                                                                        ),
                                                                                                                        Cons(
                                                                                                                          (
                                                                                                                            Cons(
                                                                                                                              3,
                                                                                                                              Cons(4, Cons(7, Nil))
                                                                                                                            ),
                                                                                                                            Cons(
                                                                                                                              1,
                                                                                                                              Cons(
                                                                                                                                2,
                                                                                                                                Cons(5, Cons(6, Nil))
                                                                                                                              )
                                                                                                                            )
                                                                                                                          ),
                                                                                                                          Cons(
                                                                                                                            (
                                                                                                                              Cons(
                                                                                                                                3,
                                                                                                                                Cons(5, Cons(6, Nil))
                                                                                                                              ),
                                                                                                                              Cons(
                                                                                                                                1,
                                                                                                                                Cons(
                                                                                                                                  2,
                                                                                                                                  Cons(4, Cons(7, Nil))
                                                                                                                                )
                                                                                                                              )
                                                                                                                            ),
                                                                                                                            Cons(
                                                                                                                              (
                                                                                                                                Cons(
                                                                                                                                  3,
                                                                                                                                  Cons(5, Cons(7, Nil))
                                                                                                                                ),
                                                                                                                                Cons(
                                                                                                                                  1,
                                                                                                                                  Cons(
                                                                                                                                    2,
                                                                                                                                    Cons(4, Cons(6, Nil))
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              ),
                                                                                                                              Cons(
                                                                                                                                (
                                                                                                                                  Cons(
                                                                                                                                    3,
                                                                                                                                    Cons(6, Cons(7, Nil))
                                                                                                                                  ),
                                                                                                                                  Cons(
                                                                                                                                    1,
                                                                                                                                    Cons(
                                                                                                                                      2,
                                                                                                                                      Cons(4, Cons(5, Nil))
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                ),
                                                                                                                                Cons(
                                                                                                                                  (
                                                                                                                                    Cons(4, Nil),
                                                                                                                                    Cons(
                                                                                                                                      1,
                                                                                                                                      Cons(
                                                                                                                                        2,
                                                                                                                                        Cons(
                                                                                                                                          3,
                                                                                                                                          Cons(
                                                                                                                                            5,
                                                                                                                                            Cons(6, Cons(7, Nil))
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  ),
                                                                                                                                  Cons(
                                                                                                                                    (
                                                                                                                                      Cons(
                                                                                                                                        4,
                                                                                                                                        Cons(5, Cons(6, Nil))
                                                                                                                                      ),
                                                                                                                                      Cons(
                                                                                                                                        1,
                                                                                                                                        Cons(
                                                                                                                                          2,
                                                                                                                                          Cons(3, Cons(7, Nil))
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    ),
                                                                                                                                    Cons(
                                                                                                                                      (
                                                                                                                                        Cons(
                                                                                                                                          4,
                                                                                                                                          Cons(5, Cons(7, Nil))
                                                                                                                                        ),
                                                                                                                                        Cons(
                                                                                                                                          1,
                                                                                                                                          Cons(
                                                                                                                                            2,
                                                                                                                                            Cons(3, Cons(6, Nil))
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      ),
                                                                                                                                      Cons(
                                                                                                                                        (
                                                                                                                                          Cons(
                                                                                                                                            4,
                                                                                                                                            Cons(6, Cons(7, Nil))
                                                                                                                                          ),
                                                                                                                                          Cons(
                                                                                                                                            1,
                                                                                                                                            Cons(
                                                                                                                                              2,
                                                                                                                                              Cons(3, Cons(5, Nil))
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        ),
                                                                                                                                        Cons(
                                                                                                                                          (
                                                                                                                                            Cons(5, Nil),
                                                                                                                                            Cons(
                                                                                                                                              1,
                                                                                                                                              Cons(
                                                                                                                                                2,
                                                                                                                                                Cons(
                                                                                                                                                  3,
                                                                                                                                                  Cons(
                                                                                                                                                    4,
                                                                                                                                                    Cons(6, Cons(7, Nil))
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          ),
                                                                                                                                          Cons(
                                                                                                                                            (
                                                                                                                                              Cons(
                                                                                                                                                5,
                                                                                                                                                Cons(6, Cons(7, Nil))
                                                                                                                                              ),
                                                                                                                                              Cons(
                                                                                                                                                1,
                                                                                                                                                Cons(
                                                                                                                                                  2,
                                                                                                                                                  Cons(3, Cons(4, Nil))
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            ),
                                                                                                                                            Cons(
                                                                                                                                              (
                                                                                                                                                Cons(6, Nil),
                                                                                                                                                Cons(
                                                                                                                                                  1,
                                                                                                                                                  Cons(
                                                                                                                                                    2,
                                                                                                                                                    Cons(
                                                                                                                                                      3,
                                                                                                                                                      Cons(
                                                                                                                                                        4,
                                                                                                                                                        Cons(5, Cons(7, Nil))
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              ),
                                                                                                                                              Cons(
                                                                                                                                                (
                                                                                                                                                  Cons(7, Nil),
                                                                                                                                                  Cons(
                                                                                                                                                    1,
                                                                                                                                                    Cons(
                                                                                                                                                      2,
                                                                                                                                                      Cons(
                                                                                                                                                        3,
                                                                                                                                                        Cons(
                                                                                                                                                          4,
                                                                                                                                                          Cons(5, Cons(6, Nil))
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                ),
                                                                                                                                                Nil
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
                require(formula.clauses === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        val options = summon[Options]
        val scalusBudget =
            if options.targetLanguage == Language.PlutusV4 then
                ExUnits(memory = 185769466L, steps = 47117431014L)
            else ExUnits(memory = 344589971L, steps = 100725854354L)
        // val scalusBudget = ExUnits(memory = 214968623L, steps = 37733187149L)
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "ClausifyTest.F4",
          scalusBudget = scalusBudget,
          refBudget = ExUnits(memory = 181055087L, steps = 56754761923L),
          isPrintComparison = true
        )
    }

    test("F5") {
        val result = Compiler
            .compile {
                // (a = a = a) = (a = a = a) = (a = a)
                val formula = (1 <-> (1 <-> 1)) <-> ((1 <-> (1 <-> 1)) <-> (1 <-> 1))
                val expected = List.empty[LRVars]
                require(formula.clauses === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        val options = summon[Options]
        val scalusBudget =
            if options.targetLanguage == Language.PlutusV4 then
                ExUnits(memory = 625050874L, steps = 169481682322L)
            else ExUnits(memory = 1205574641L, steps = 363306861308L)
        // val scalusBudget = ExUnits(memory = 736503639L, steps = 127163562591L)
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "ClausifyTest.F5",
          scalusBudget = scalusBudget,
          refBudget = ExUnits(memory = 660668247L, steps = 203182153626L),
          isPrintComparison = true
        )
    }

end ClausifyTest

@Compile
object ClausifyTest:
    type Var = BigInt
    type LRVars = (List[Var], List[Var])

    enum Formula:
        case Sym(arg: Var)
        case Not(arg: Formula)
        case And(arg1: Formula, arg2: Formula)
        case Or(arg1: Formula, arg2: Formula)
        case Implication(arg1: Formula, arg2: Formula)
        case Equivalence(arg1: Formula, arg2: Formula)

    import Formula.*

    extension (self: Int)
        inline infix def <->(other: Int): Formula = Equivalence(Sym(self), Sym(other))
        inline infix def <->(other: Formula): Formula = Equivalence(Sym(self), other)

    end extension

    extension (self: Var) inline def toFormula: Formula = Sym(self)
    extension (self: Int) @Ignore inline def toFormula: Formula = Sym(self)

    @nowarn @Ignore
    given Conversion[Var, Formula] = (arg: Var) => arg.toFormula
    @nowarn @Ignore
    given Conversion[Int, Formula] = (arg: Int) => arg.toFormula

    extension (self: Formula)
        inline def unary_! : Formula = Not(self)
        inline def not: Formula = !self
        inline infix def &&(other: Formula): Formula = And(self, other)
        inline infix def and(other: Formula): Formula = self && other
        inline infix def ||(other: Formula): Formula = Or(self, other)
        inline infix def or(other: Formula): Formula = self || other
        inline infix def ->(other: Formula): Formula = Implication(self, other)
        inline infix def impl(other: Formula): Formula = self -> other
        inline infix def <->(other: Formula): Formula = Equivalence(self, other)
        inline infix def eqv(other: Formula): Formula = self <-> other

        /// eliminate connectives other than not, disjunction and conjunction
        def eliminate: Formula =
            self match
                case Sym(arg)                => self
                case Not(arg)                => !arg.eliminate
                case And(arg1, arg2)         => arg1.eliminate && arg2.eliminate
                case Or(arg1, arg2)          => arg1.eliminate || arg2.eliminate
                case Implication(arg1, arg2) => !arg1.eliminate || arg2.eliminate
                case Equivalence(arg1, arg2) => (arg1 -> arg2).eliminate && (arg2 -> arg1).eliminate

        /// -- shift negation to innermost positions
        def negin: Formula =
            self match
                case Not(expr) =>
                    expr match
                        case Not(arg)        => arg.negin
                        case And(arg1, arg2) => (!arg1).negin || (!arg2).negin
                        case Or(arg1, arg2)  => (!arg1).negin && (!arg2).negin
                        case _               => self
                case And(arg1, arg2) => arg1.negin && arg2.negin
                case Or(arg1, arg2)  => arg1.negin || arg2.negin
                case _               => self

        /// shift disjunction within conjunction
        def disin: Formula = {
            extension (self: Formula)
                def isAnd: Boolean = self match { case And(_, _) => true; case _ => false }

                def unwrapAnd: (Formula, Formula) =
                    self match
                        case And(lhs, rhs) => (lhs, rhs)
                        case _             => throw IllegalArgumentException("Not an And formula")

            end extension

            self match
                case Or(left, right) =>
                    if right.isAnd then
                        val (rightArg1, rightArg2) = right.unwrapAnd
                        (left || rightArg1).disin && (left || rightArg2).disin
                    else if left.isAnd then
                        val (leftArg1, leftArg2) = left.unwrapAnd
                        (leftArg1 || right).disin && (leftArg2 || right).disin
                    else
                        val leftDisin = left.disin
                        val rightDisin = right.disin
                        if leftDisin.isAnd || rightDisin.isAnd then (leftDisin || rightDisin).disin
                        else leftDisin || rightDisin
                case And(arg1, arg2) => arg1.disin && arg2.disin
                case _               => self
        }

        /// split conjunctive proposition into a list of conjuncts
        def split: List[Formula] = {
            def doSplit(formula: Formula, list: List[Formula]): List[Formula] =
                formula match
                    case And(arg1, arg2) => doSplit(arg1, doSplit(arg2, list))
                    case _               => list.prepended(formula)

            doSplit(self, List.empty)
        }

        def clauses: List[LRVars] = eliminate.negin.disin.split.unicl

    end extension

    extension (self: List[Formula])
        /// form unique clausal axioms excluding tautologies
        def unicl: List[LRVars] = {
            extension [A: Ord](self: List[A])
                def insertUniqueOrdered(elem: A): List[A] =
                    self match
                        case List.Nil => List.single(elem)
                        case List.Cons(head, tail) =>
                            elem <=> head match
                                case Order.Less    => self.prepended(elem)
                                case Order.Greater => tail.insertUniqueOrdered(elem).prepended(head)
                                case Order.Equal   => self

            end extension

            extension (self: Formula)
                def clause: LRVars =
                    def doClause(formula: Formula, vars: LRVars): LRVars =
                        formula match
                            case Or(arg1, arg2) => doClause(arg1, doClause(arg2, vars))
                            case Sym(arg)       => (vars._1.insertUniqueOrdered(arg), vars._2)
                            case Not(expr) =>
                                expr match
                                    case Sym(arg) =>
                                        (vars._1, vars._2.insertUniqueOrdered(arg))
                                    case _ => fail("Invalid formula")
                            case _ => fail("Invalid formula")

                    doClause(self, (List.empty, List.empty))

            end extension

            self.foldRight(List.empty) { (formula, list) =>
                val lrVars = formula.clause
                if lrVars._1.exists(lrVars._2.contains(_)) then list
                else list.insertUniqueOrdered(lrVars)
            }
        }

    end extension

end ClausifyTest
