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
    Emulator,
    EmulatorInitialState,
    EvaluationResult,
    ExUnits,
    PlutusScriptEvaluationError,
    RedeemerBudget,
    Redeemer,
    Result,
    Scalus,
    SlotConfig,
    SubmitResult,
    DelegationInfo,
    applyDataArgToScript,
    evalPlutusScripts,
    evaluateScript,
    evaluateScriptProfile,
} from "../scalus";

const bytes: Uint8Array = new Uint8Array([1, 2, 3]);
const scriptHex = "545301010023357389210753756363657373004981";

// --- top-level functions -----------------------------------------------------------------

const applied: string = applyDataArgToScript(scriptHex, JSON.stringify({int: 42}));

const evaluated: EvaluationResult = evaluateScript(applied);
const steps: bigint = evaluated.budget.steps;
const memory: bigint = evaluated.budget.memory;
const succeeded: boolean = evaluated.isSuccess;
const logs: string[] = evaluated.logs;

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
    steps,
    memory,
    succeeded,
    logs,
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
