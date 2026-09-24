// __tests__/lucid-evaluator.ts
//
// Scalus as lucid-evolution's script evaluator. This is the whole adapter; it belongs upstream in
// lucid-evolution beside the Aiken one. It lives here so `npm run typecheck` compiles it against
// both libraries' real declarations, and so the differential test has something to run.
//
//     const lucid = await Lucid(provider, "Preprod", { evaluator: scalusEvaluator });

import type { EvalRedeemer, EvaluationInput, EvaluatorAdapter, RedeemerTag } from "@lucid-evolution/core-types";
import { utxosToCores } from "@lucid-evolution/utils";
import { evaluator } from "../scalus";
import type { RedeemerBudget } from "../scalus";

/** Scalus spells a redeemer's purpose as the ledger CDDL does; Lucid uses Ogmios' names. */
const TAG: Record<RedeemerBudget["tag"], RedeemerTag> = {
  Spend: "spend",
  Mint: "mint",
  Cert: "publish",
  Reward: "withdraw",
  Voting: "vote",
  Proposing: "propose",
};

const toEvalRedeemer = (r: RedeemerBudget): EvalRedeemer => ({
  redeemer_tag: TAG[r.tag],
  redeemer_index: r.index,
  ex_units: { mem: Number(r.budget.memory), steps: Number(r.budget.steps) },
});

export const scalusEvaluator: EvaluatorAdapter = {
  name: "scalus",
  evaluate: async ({ tx, additionalUTxOs, context }: EvaluationInput) =>
    evaluator
      .evaluateTx(
        tx,
        utxosToCores(additionalUTxOs).map((utxo) => utxo.to_cbor_bytes()),
        context.slotConfig,
        context.protocolParameters.costModels,
        context.protocolParameters.protocolMajorVersion ?? 11,
      )
      .map(toEvalRedeemer),
};
