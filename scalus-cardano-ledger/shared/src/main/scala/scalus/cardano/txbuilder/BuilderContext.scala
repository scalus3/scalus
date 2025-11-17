package scalus.cardano.txbuilder

import scalus.cardano.ledger.{ExUnits, PlutusScriptEvaluator}

case class BuilderContext(
    env: Environment,
    wallet: Wallet,
    evaluator: PlutusScriptEvaluator
)

object BuilderContext {

    /** Create a BuilderContext with a dummy evaluator.
      *
      * Useful for tests that don't require actual Plutus script evaluation, such as transaction
      * building tests.
      *
      * @param env
      *   The environment containing protocol parameters and configuration
      * @param wallet
      *   The wallet to use for transaction building
      * @return
      *   BuilderContext with a no-op evaluator
      */
    def withNoopEvaluator(env: Environment, wallet: Wallet): BuilderContext =
        BuilderContext(env, wallet, PlutusScriptEvaluator.noop)

    /** Create a BuilderContext with an evaluator built from environment parameters.
      *
      * This is a convenience method that automatically creates an evaluator from the environment's
      * protocol parameters and slot configuration.
      *
      * @param env
      *   The environment containing protocol parameters and configuration
      * @param wallet
      *   The wallet to use for transaction building
      * @param initialBudget
      *   The initial execution budget (defaults to ExBudget.enormous)
      * @return
      *   BuilderContext with a configured evaluator
      */
    def withEvaluator(
        env: Environment,
        wallet: Wallet,
        initialBudget: ExUnits = ExUnits(Long.MaxValue, Long.MaxValue)
    ): BuilderContext = {
        val evaluator = PlutusScriptEvaluator(
          slotConfig = env.slotConfig,
          initialBudget = initialBudget,
          protocolMajorVersion = env.majorProtocolVersion,
          costModels = env.protocolParams.costModels
        )
        BuilderContext(env, wallet, evaluator)
    }

    /** Create a BuilderContext with a constant max budget evaluator.
      *
      * This evaluator uses the maximum transaction execution units defined in the protocol
      * parameters as a constant budget for each script evaluations.
      *
      * @param env
      *   The environment containing protocol parameters and configuration
      * @param wallet
      *   The wallet to use for transaction building
      * @return
      *   BuilderContext with a constant max budget evaluator
      */
    def withConstMaxBudgetEvaluator(
        env: Environment,
        wallet: Wallet
    ): BuilderContext = {
        val evaluator = PlutusScriptEvaluator.constMaxBudget(env)
        BuilderContext(env, wallet, evaluator)
    }
}
