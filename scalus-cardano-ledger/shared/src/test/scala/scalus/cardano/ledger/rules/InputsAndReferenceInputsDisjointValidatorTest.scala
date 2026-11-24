package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class InputsAndReferenceInputsDisjointValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    private val inputs = Set(input(0), input(1))

    private def tx(inputs: Set[TransactionInput], referenceInputs: Set[TransactionInput]) =
        Transaction(
          emptyBody.copy(
            inputs = TaggedSortedSet.from(inputs),
            referenceInputs = TaggedSortedSet.from(referenceInputs)
          ),
          TransactionWitnessSet.empty
        )

    // The disjointness requirement only applies for Babbage <= PV < 11 (van Rossem lifted it),
    // so the failure cases need a context with another protocol version.
    private def contextAt(major: Int): Context = {
        val c = Context()
        c.copy(env =
            c.env.copy(params = c.env.params.copy(protocolVersion = ProtocolVersion(major, 0)))
        )
    }

    test("InputsAndReferenceInputsDisjointValidator rule success") {
        val transaction = tx(inputs, Set.empty)

        val result = validate(InputsAndReferenceInputsDisjointValidator, Map.empty, transaction)
        assert(result.isRight)
        val body = transaction.body.value
        assert(body.inputs.toSet.nonEmpty && body.referenceInputs.toSet.isEmpty)
    }

    test("InputsAndReferenceInputsDisjointValidator rule failure") {
        // Pin the context to PV10 to exercise the failure path.
        val context = contextAt(10)
        val transaction = tx(inputs, inputs)

        val result =
            InputsAndReferenceInputsDisjointValidator.validate(context, State(), transaction)
        assert(result.isLeft)
        val body = transaction.body.value
        assert(body.inputs.toSet.nonEmpty && body.referenceInputs.toSet.nonEmpty)
        assert(body.inputs.toSet == body.referenceInputs.toSet)
    }

    test("InputsAndReferenceInputsDisjointValidator allows overlap since PV11 (van Rossem)") {
        // default params are PV11
        val transaction = tx(inputs, inputs)

        val result = validate(InputsAndReferenceInputsDisjointValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("overlap is rejected only at PV 9 and 10, as Babbage.disjointRefInputs") {
        // cardano-ledger checks when ProtVerHigh BabbageEra (8) < PV major < 11
        val transaction = tx(inputs, inputs)
        val expected = Seq(7 -> true, 8 -> true, 9 -> false, 10 -> false, 11 -> true)
        for (major, accepted) <- expected do
            val result = InputsAndReferenceInputsDisjointValidator.validate(
              contextAt(major),
              State(),
              transaction
            )
            assert(result.isRight == accepted, s"PV $major")
    }
}
