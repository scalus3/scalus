// Typed consumer of the published scalus.d.ts.
//
// This is NOT a vitest test: vitest only collects `__tests__/**/*.test.ts`, and nothing here
// runs. It exists so `npm run typecheck` can compile the *shipped* declarations against real
// call sites. `vitest run` transpiles with esbuild and never type-checks, so without this file
// a generator bug could ship a scalus.d.ts that does not compile, with CI fully green.
//
// Keep it exercising the shapes that are easy to get wrong in the generator: overloads,
// optional parameters, static members, interfaces used as object literals, and the deprecated
// re-exported aliases in type position.
//
// The `typecheck` script passes both files on the tsc command line, which makes tsc ignore
// tsconfig.json entirely. That is deliberate: tsconfig.json sets `skipLibCheck: true`, which
// hides every error inside scalus.d.ts, and its `include` pulls in the other __tests__ files,
// which have pre-existing type errors. On the command line, `skipLibCheck` defaults to false
// and the program is exactly these two files.

import {
    CardanoInfo,
    Emulator,
    EmulatorInitialState,
    EvaluationOptions,
    EvaluationError,
    EvaluationResult,
    ExUnits,
    PlutusScriptEvaluationError,
    ProtocolParams,
    RedeemerBudget,
    Redeemer,
    Result,
    Scalus,
    SlotConfig,
    SlotConfigLike,
    CostModelsLike,
    SubmitResult,
    DelegationInfo,
    applyDataArgToScript,
    evalPlutusScripts,
    evaluateScript,
    evaluateScriptProfile,
    evaluator,
} from "../scalus";
// @ts-expect-error structural evaluation protocol parameters are no longer public
import type { EvaluationProtocolParams } from "../scalus";

const options: EvaluationOptions = EvaluationOptions.mainnet("PlutusV3");
const protocolParams: ProtocolParams = CardanoInfo.mainnet().protocolParams;
const suppliedV1: EvaluationOptions = EvaluationOptions.fromProtocolParams("PlutusV1", protocolParams);
const suppliedV2: EvaluationOptions = EvaluationOptions.fromProtocolParams("PlutusV2", protocolParams);
const suppliedV3: EvaluationOptions = EvaluationOptions.fromProtocolParams("PlutusV3", protocolParams);
// @ts-expect-error `of` is gone: a plain object literal is an options record
EvaluationOptions.of("PlutusV3", protocolParams.costModels.PlutusV3, 11);
const bounded: EvaluationOptions = { ...options, maxBudget: { memory: 1, steps: 2n } };
// @ts-expect-error a cost model needs its protocol version too
EvaluationOptions.fromProtocolParams("PlutusV3", protocolParams.costModels.PlutusV3);
// @ts-expect-error factories require ProtocolParams handles, not plain structural records
EvaluationOptions.fromProtocolParams("PlutusV3", protocolParams.toObject());
const copied: EvaluationOptions = { ...options };
const readonlyCostModel: readonly (number | bigint)[] = options.costModel;
// @ts-expect-error a cost may be a bigint, so a reader narrows before treating it as a number
const onlyNumbers: readonly number[] = options.costModel;
const bigintCosts: EvaluationOptions = { ...options, costModel: [1n, 2n, 3n] };
const explicit: EvaluationOptions = {
    plutusVersion: "PlutusV3",
    protocolMajorVersion: 11,
    costModel: readonlyCostModel,
};
// @ts-expect-error all required configuration fields must be present
const missingCostModel: EvaluationOptions = {
    plutusVersion: "PlutusV3",
    protocolMajorVersion: 11,
};
// @ts-expect-error options are records, not factory objects
const invalid: EvaluationOptions = EvaluationOptions;

const bytes: Uint8Array = new Uint8Array([1, 2, 3]);
const scriptHex = "545301010023357389210753756363657373004981";

// --- top-level functions -----------------------------------------------------------------

const applied: string = applyDataArgToScript(scriptHex, JSON.stringify({int: 42}));

const evaluated: EvaluationResult = evaluateScript(applied);
const steps: bigint = evaluated.budget.steps;
const memory: bigint = evaluated.budget.memory;
const succeeded: boolean = evaluated.isSuccess;
const logs: string[] = evaluated.logs;

const readonlyArgs: readonly (string | Uint8Array)[] = ["182a", bytes];
const configured: EvaluationResult = evaluator.evaluateScript(scriptHex, readonlyArgs, options);
const structuredError: EvaluationError | undefined = configured.error;
// @ts-expect-error raw traces are deliberately not a separate result field
configured.traces;
// @ts-expect-error options are required by the new evaluator
evaluator.evaluateScript(scriptHex, readonlyArgs);
// @ts-expect-error arguments are required by the new evaluator
evaluator.evaluateScript(scriptHex, options);
// @ts-expect-error the evaluator exposes exactly one method
evaluator.evaluateUplc(scriptHex, readonlyArgs, options);
// @ts-expect-error computed values are deliberately not part of this iteration
configured.value;

// profileJson is optional: it must be assignable to `string | undefined`, not to `string`.
const profile: string | undefined = evaluateScriptProfile(applied).profileJson;

// The trailing protocolMajorVersion parameter is optional, so both arities must compile.
const budgets: RedeemerBudget[] = evalPlutusScripts(bytes, bytes, SlotConfig.preprod, [[1, 2]]);
const budgetsAtPv: RedeemerBudget[] = evalPlutusScripts(
    bytes,
    bytes,
    SlotConfig.preprod,
    [[1, 2]],
    11,
);
const firstTag: string = budgets[0].tag;
const firstIndex: number = budgetsAtPv[0].index;

// --- deprecated aliases in type position -------------------------------------------------

const asResult: Result = evaluated;
const asRedeemers: Redeemer[] = budgets;

// --- the deprecated namespace object ------------------------------------------------------

const viaNamespace: EvaluationResult = Scalus.evaluateScript(
    Scalus.applyDataArgToScript(scriptHex, "{}"),
);

// --- classes with constructors --------------------------------------------------------------

const units: ExUnits = new ExUnits(1000n, 2000n);
const handMade: EvaluationResult = new EvaluationResult(true, units, ["log"], "{}");
const withoutProfile: EvaluationResult = new EvaluationResult(false, units, []);
const failure: PlutusScriptEvaluationError = new PlutusScriptEvaluationError("boom", ["trace"]);
const failureMessage: string = failure.message;

// --- SlotConfig ------------------------------------------------------------------------------

const slotConfig: SlotConfig = new SlotConfig(1_596_059_091_000, 4_492_800, 1_000);
const withEpochs: SlotConfig = new SlotConfig(1_596_059_091_000, 4_492_800, 1_000, 432_000, 208);
const slot: number = slotConfig.timeToSlot(Date.now());
const time: number = withEpochs.slotToTime(slot);
const epoch: number = SlotConfig.mainnet.epochOf(slot);
const firstSlot: number = SlotConfig.preview.firstSlotOfEpoch(epoch);

// --- Emulator ---------------------------------------------------------------------------------

const emulator: Emulator = new Emulator(bytes, SlotConfig.preprod);
const seeded: Emulator = new Emulator(bytes, SlotConfig.preprod, {stakeHashHex: "0"});

const initialState: EmulatorInitialState = {
    utxos: bytes,
    stakeRegistrations: [{credentialType: "script", credentialHash: "aa", rewards: 0n}],
    datums: [{hash: "bb", datum: "cc"}],
};
const fromState: Emulator = Emulator.withState(initialState, SlotConfig.preprod);
const funded: Emulator = Emulator.withAddresses(["addr_test1..."], SlotConfig.preprod);
const richlyFunded: Emulator = Emulator.withAddresses(
    ["addr_test1..."],
    SlotConfig.preprod,
    100_000_000n,
);

// both submitTx overloads
const submitted: SubmitResult = emulator.submitTx(bytes);
const submittedWithDebug: SubmitResult = seeded.submitTx(bytes, {scriptHashHex: "doubleCborHex"});
const txHash: string | undefined = submitted.txHash;
const submitLogs: string[] | undefined = submittedWithDebug.logs;

// getDelegation and getStakeReward take a bech32 reward address, not raw/CBOR bytes, and poolId
// and getStakeReward's result are `| undefined`, not `| null`.
const delegation: DelegationInfo = fromState.getDelegation("stake_test1...");
const poolId: string | undefined = delegation.poolId;
const rewards: bigint = delegation.rewards;

const reward: bigint | undefined = funded.getStakeReward("stake_test1...");
const datum: Uint8Array | undefined = richlyFunded.getDatum("bb");
const utxos: Uint8Array[] = emulator.getAllUtxos();
const addressUtxos: Uint8Array[] = emulator.getUtxosForAddress("addr_test1...");
const allUtxosCbor: Uint8Array = emulator.getUtxosCbor();
const snapshot: Emulator = emulator.snapshot();
const seen: boolean = snapshot.hasTx("aa");
emulator.setSlot(42);
emulator.tick(1);
const currentSlot: number = emulator.getSlot();

// Reference every binding once so this file is also a `noUnusedLocals`-clean sample.
export const surface = {
    options,
    protocolParams,
    suppliedV1,
    suppliedV2,
    suppliedV3,
    bounded,
    bigintCosts,
    onlyNumbers,
    copied,
    explicit,
    missingCostModel,
    invalid,
    steps,
    memory,
    succeeded,
    logs,
    configured,
    structuredError,
    profile,
    firstTag,
    firstIndex,
    asResult,
    asRedeemers,
    viaNamespace,
    handMade,
    withoutProfile,
    failureMessage,
    time,
    firstSlot,
    txHash,
    submitLogs,
    poolId,
    rewards,
    reward,
    datum,
    utxos,
    addressUtxos,
    allUtxosCbor,
    seen,
    currentSlot,
};


// Optional discriminator keeps the existing hash-only query valid.
const anyCredential: import("../scalus.js").UtxoFilter = { paymentCredential: "11".repeat(28) };
const keyCredential: import("../scalus.js").UtxoFilter = { paymentCredential: "11".repeat(28), paymentCredentialType: "key" };
const scriptCredential: import("../scalus.js").UtxoFilter = { paymentCredential: "11".repeat(28), paymentCredentialType: "script" };
// @ts-expect-error credential kinds use the declared lower-case literals
const invalidCredential: import("../scalus.js").UtxoFilter = { paymentCredentialType: "Key" };

// The error's fields are declared, typed, and usable without a cast: `!` narrows away the
// `undefined` the legacy 2-arg constructor leaves, it does not paper over the wrong type. spec [DOC-1]
{
  const probe = (e: PlutusScriptEvaluationError): readonly string[] => e.args!;
  const purpose = (e: PlutusScriptEvaluationError): RedeemerBudget["tag"] => e.redeemer!.tag;
  void probe;
  void purpose;
}

// evaluateTx takes the shape each SDK already holds: a SlotConfig or CostModels handle, a plain
// object with number fields (Lucid, Mesh, with extra fields), or bigint anchors (Evolution SDK).
{
  const meshSlots = { zeroTime: 1596059091000, zeroSlot: 4492800, slotLength: 1000, startEpoch: 208 };
  const slots: SlotConfigLike[] = [
    SlotConfig.mainnet,
    meshSlots,
    { zeroTime: 1596059091000, zeroSlot: 4492800, slotLength: 1000 },
    { zeroTime: 1596059091000n, zeroSlot: 4492800n, slotLength: 1000 },
  ];
  const models: CostModelsLike[] = [
    CardanoInfo.mainnet().protocolParams.costModels,
    { PlutusV3: [1, 2, 3] },
    { PlutusV2: [1n, 2n] },
  ];
  // @ts-expect-error slotLength is a number of milliseconds, never a bigint
  const bigSlotLength: SlotConfigLike = { zeroTime: 0, zeroSlot: 0, slotLength: 1000n };
  void slots;
  void models;
  void bigSlotLength;
}
