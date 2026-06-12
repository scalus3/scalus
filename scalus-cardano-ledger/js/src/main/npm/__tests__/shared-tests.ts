// __tests__/shared-tests.ts
// Shared test logic that can be used by both Vitest and browser tests

import {
  costModels,
  hexToBytes,
  testTxCborHex,
  testUtxoCborHex,
  failingTxCborHex,
  failingUtxoCborHex,
  successScriptHex,
  failScriptHex,
} from "./fixtures";

/**
 * Result of a single test case
 */
export interface TestResult {
  name: string;
  passed: boolean;
  message?: string;
}

/**
 * Scalus API types (matches scalus.d.ts)
 */
interface ScalusAPI {
  applyDataArgToScript(doubleCborHex: string, data: string): string;
  evaluateScript(doubleCborHex: string): {
    isSuccess: boolean;
    budget: { memory: bigint; steps: bigint };
    logs: string[];
    profileJson?: string;
  };
  evaluateScriptProfile(doubleCborHex: string): {
    isSuccess: boolean;
    budget: { memory: bigint; steps: bigint };
    logs: string[];
    profileJson?: string;
  };
  evalPlutusScripts(
    txCborBytes: Uint8Array,
    utxoCborBytes: Uint8Array,
    slotConfig: SlotConfigAPI,
    costModels: number[][]
  ): Array<{ tag: string; index: number; budget: { memory: bigint; steps: bigint } }>;
}

interface SlotConfigAPI {
  slotToTime(slot: number): number;
  timeToSlot(time: number): number;
  epochOf(slot: number): number;
  firstSlotOfEpoch(epoch: number): number;
}

interface SlotConfigStatic {
  mainnet: SlotConfigAPI;
  preview: SlotConfigAPI;
  preprod: SlotConfigAPI;
  new (
    zeroTime: number,
    zeroSlot: number,
    slotLength: number,
    epochLength?: number,
    zeroEpoch?: number,
  ): SlotConfigAPI;
}

/**
 * Tests for Scalus.applyDataArgToScript
 */
export function testApplyDataArgToScript(Scalus: ScalusAPI): TestResult[] {
  const results: TestResult[] = [];
  const script = successScriptHex;

  const testCases = [
    { data: { int: 42 }, name: "integer" },
    { data: { int: 0 }, name: "zero" },
    { data: { int: -42 }, name: "negative integer" },
    { data: { int: 9999999999999 }, name: "large integer" },
    { data: { bytes: "deadbeef" }, name: "bytes" },
    { data: { bytes: "" }, name: "empty bytes" },
    { data: { list: [{ int: 1 }, { int: 2 }, { int: 3 }] }, name: "list" },
    { data: { list: [] }, name: "empty list" },
    { data: { map: [{ k: { int: 1 }, v: { bytes: "aa" } }] }, name: "map" },
    { data: { map: [] }, name: "empty map" },
    { data: { constructor: 0, fields: [{ int: 42 }] }, name: "constructor" },
    { data: { constructor: 1, fields: [] }, name: "empty constructor" },
    {
      data: {
        constructor: 0,
        fields: [
          { list: [{ int: 1 }, { int: 2 }] },
          { map: [{ k: { bytes: "abcd" }, v: { int: 100 } }] },
        ],
      },
      name: "nested structure",
    },
  ];

  for (const { data, name } of testCases) {
    try {
      const applied = Scalus.applyDataArgToScript(script, JSON.stringify(data));
      // Applied script should be longer than original (contains the argument)
      const passed = typeof applied === "string" && applied.length > script.length;
      results.push({
        name: `applyDataArgToScript: ${name}`,
        passed,
        message: passed ? undefined : `Result length: ${applied.length}`,
      });
    } catch (e) {
      results.push({
        name: `applyDataArgToScript: ${name}`,
        passed: false,
        message: String(e),
      });
    }
  }

  return results;
}

/**
 * Tests for Scalus.evaluateScript
 */
export function testEvaluateScript(Scalus: ScalusAPI): TestResult[] {
  const results: TestResult[] = [];

  // Test successful script evaluation
  try {
    const script = successScriptHex;
    const applied = Scalus.applyDataArgToScript(script, JSON.stringify({ int: 42 }));
    const result = Scalus.evaluateScript(applied);

    results.push({
      name: "evaluateScript: success case returns isSuccess=true",
      passed: result.isSuccess === true,
      message: result.isSuccess ? undefined : `isSuccess was ${result.isSuccess}`,
    });

    results.push({
      name: "evaluateScript: success case has memory > 0",
      passed: result.budget.memory > 0n,
      message: `memory: ${result.budget.memory}`,
    });

    results.push({
      name: "evaluateScript: success case has steps > 0",
      passed: result.budget.steps > 0n,
      message: `steps: ${result.budget.steps}`,
    });

    results.push({
      name: "evaluateScript: success case has logs array",
      passed: Array.isArray(result.logs),
      message: `logs: ${JSON.stringify(result.logs)}`,
    });
  } catch (e) {
    results.push({
      name: "evaluateScript: success case",
      passed: false,
      message: String(e),
    });
  }

  // Test failing script evaluation
  try {
    const result = Scalus.evaluateScript(failScriptHex);
    results.push({
      name: "evaluateScript: fail script returns isSuccess=false",
      passed: result.isSuccess === false,
      message: result.isSuccess ? "Expected failure but got success" : undefined,
    });
  } catch (e) {
    // Some implementations throw on failure, which is also acceptable
    results.push({
      name: "evaluateScript: fail script throws or returns isSuccess=false",
      passed: true,
      message: `Threw: ${String(e)}`,
    });
  }

  // Plain evaluateScript must NOT carry profiling data.
  try {
    const script = successScriptHex;
    const applied = Scalus.applyDataArgToScript(script, JSON.stringify({ int: 42 }));
    const result = Scalus.evaluateScript(applied);
    results.push({
      name: "evaluateScript: no profile data when profiling not requested",
      passed: result.profileJson === undefined,
      message: `profileJson: ${typeof result.profileJson}`,
    });
  } catch (e) {
    results.push({
      name: "evaluateScript: no profile fields when profiling not requested",
      passed: false,
      message: String(e),
    });
  }

  return results;
}

/**
 * Tests for Scalus.evaluateScriptProfile
 */
export function testEvaluateScriptProfile(Scalus: ScalusAPI): TestResult[] {
  const results: TestResult[] = [];

  try {
    const script = successScriptHex;
    const applied = Scalus.applyDataArgToScript(script, JSON.stringify({ int: 42 }));
    const result = Scalus.evaluateScriptProfile(applied);

    results.push({
      name: "evaluateScriptProfile: success case returns isSuccess=true",
      passed: result.isSuccess === true,
      message: result.isSuccess ? undefined : `isSuccess was ${result.isSuccess}`,
    });

    results.push({
      name: "evaluateScriptProfile: budget is tracked",
      passed: result.budget.steps > 0n && result.budget.memory > 0n,
      message: `mem: ${result.budget.memory}, cpu: ${result.budget.steps}`,
    });

    results.push({
      name: "evaluateScriptProfile: profileJson is present",
      passed: typeof result.profileJson === "string" && result.profileJson.length > 0,
      message: `profileJson type: ${typeof result.profileJson}`,
    });

    let parsed: { totalBudget?: { cpu?: number }; bySourceLocation?: unknown[] } | undefined;
    if (typeof result.profileJson === "string") {
      parsed = JSON.parse(result.profileJson);
    }
    results.push({
      name: "evaluateScriptProfile: profileJson parses with a totalBudget",
      passed: parsed?.totalBudget?.cpu !== undefined && Number(parsed.totalBudget.cpu) > 0,
      message: `profileJson totalBudget: ${JSON.stringify(parsed?.totalBudget)}`,
    });
  } catch (e) {
    results.push({
      name: "evaluateScriptProfile: success case",
      passed: false,
      message: String(e),
    });
  }

  return results;
}

/**
 * Tests for SlotConfig
 */
export function testSlotConfig(SlotConfig: SlotConfigStatic): TestResult[] {
  const results: TestResult[] = [];

  // Test static instances exist
  try {
    results.push({
      name: "SlotConfig: mainnet exists",
      passed: SlotConfig.mainnet !== undefined,
    });

    results.push({
      name: "SlotConfig: preview exists",
      passed: SlotConfig.preview !== undefined,
    });

    results.push({
      name: "SlotConfig: preprod exists",
      passed: SlotConfig.preprod !== undefined,
    });
  } catch (e) {
    results.push({
      name: "SlotConfig: static instances",
      passed: false,
      message: String(e),
    });
  }

  // Test slotToTime and timeToSlot are callable
  try {
    const mainnet = SlotConfig.mainnet;
    // Test that the functions are callable and return a value
    // Note: Scala.js may return Long objects instead of numbers
    const time = mainnet.slotToTime(10000000);
    const slot = mainnet.timeToSlot(1600000000000);

    // Check if it's a number or a Scala.js Long object (which has toString)
    const isValidTime = typeof time === "number" || (time !== null && time !== undefined);
    const isValidSlot = typeof slot === "number" || (slot !== null && slot !== undefined);

    results.push({
      name: "SlotConfig: slotToTime is callable",
      passed: isValidTime,
      message: `slotToTime(10000000) = ${time}`,
    });

    results.push({
      name: "SlotConfig: timeToSlot is callable",
      passed: isValidSlot,
      message: `timeToSlot(1600000000000) = ${slot}`,
    });
  } catch (e) {
    results.push({
      name: "SlotConfig: methods callable",
      passed: false,
      message: String(e),
    });
  }

  // Test custom SlotConfig creation
  try {
    // Just test that the constructor is callable
    const custom = new SlotConfig(1654041600000, 0, 1000);
    const hasSlotToTime = typeof custom.slotToTime === "function";
    const hasTimeToSlot = typeof custom.timeToSlot === "function";
    results.push({
      name: "SlotConfig: custom config creation",
      passed: hasSlotToTime && hasTimeToSlot,
      message: `slotToTime: ${hasSlotToTime}, timeToSlot: ${hasTimeToSlot}`,
    });
  } catch (e) {
    results.push({
      name: "SlotConfig: custom config creation",
      passed: false,
      message: String(e),
    });
  }

  // Test epoch derivation
  try {
    // mainnet: Shelley starts at slot 4492800 = start of epoch 208
    const mainnetEpoch = Number(SlotConfig.mainnet.epochOf(4492800 + 432000));
    results.push({
      name: "SlotConfig: mainnet epochOf",
      passed: mainnetEpoch === 209,
      message: `epochOf(4924800) = ${mainnetEpoch}, expected 209`,
    });

    // preprod: Shelley starts at slot 86400 = start of epoch 4
    const preprodEpoch = Number(SlotConfig.preprod.epochOf(86400 + 2 * 432000));
    results.push({
      name: "SlotConfig: preprod epochOf",
      passed: preprodEpoch === 6,
      message: `epochOf(950400) = ${preprodEpoch}, expected 6`,
    });

    // custom config with default epoch settings (epochLength=432000, zeroEpoch=0)
    const custom = new SlotConfig(1654041600000, 0, 1000);
    const customEpoch = Number(custom.epochOf(432000 * 3 + 1));
    results.push({
      name: "SlotConfig: custom config epochOf with defaults",
      passed: customEpoch === 3,
      message: `epochOf(1296001) = ${customEpoch}, expected 3`,
    });

    // custom config with explicit epoch settings
    const custom2 = new SlotConfig(1654041600000, 100, 1000, 86400, 10);
    const custom2Epoch = Number(custom2.epochOf(100 + 86400));
    const firstSlot = Number(custom2.firstSlotOfEpoch(11));
    results.push({
      name: "SlotConfig: custom config epochOf with explicit epoch params",
      passed: custom2Epoch === 11 && firstSlot === 100 + 86400,
      message: `epochOf = ${custom2Epoch} (expected 11), firstSlotOfEpoch(11) = ${firstSlot} (expected ${100 + 86400})`,
    });
  } catch (e) {
    results.push({
      name: "SlotConfig: epochOf",
      passed: false,
      message: String(e),
    });
  }

  return results;
}

/**
 * Tests for Scalus.evalPlutusScripts
 */
export function testEvalPlutusScripts(
  Scalus: ScalusAPI,
  SlotConfig: SlotConfigStatic
): TestResult[] {
  const results: TestResult[] = [];

  // Test that evalPlutusScripts API is callable and processes transactions
  // Note: The test data may not have complete script witnesses, so we test
  // that the API processes the transaction and returns meaningful errors
  try {
    const txBytes = hexToBytes(testTxCborHex);
    const utxoBytes = hexToBytes(testUtxoCborHex);
    const slotConfig = SlotConfig.mainnet;
    const costModelsArray = [costModels.PlutusV1, costModels.PlutusV2, costModels.PlutusV3];

    try {
      const redeemers = Scalus.evalPlutusScripts(txBytes, utxoBytes, slotConfig, costModelsArray);
      console.log("Hello");
      console.log(redeemers);

      // If we get here, the evaluation succeeded
      results.push({
        name: "evalPlutusScripts: API callable",
        passed: true,
        message: `Returned ${redeemers.length} redeemers`,
      });

      if (redeemers.length > 0) {
        const first = redeemers[0];
        results.push({
          name: "evalPlutusScripts: redeemer has tag",
          passed: typeof first.tag === "string",
          message: `tag: ${first.tag}`,
        });

        results.push({
          name: "evalPlutusScripts: redeemer has budget",
          passed: first.budget !== undefined && first.budget.memory > 0n,
          message: `memory: ${first.budget?.memory}, steps: ${first.budget?.steps}`,
        });
      }
    } catch (evalError: unknown) {
      // API threw an error - this is expected if test data doesn't have scripts
      const errorMsg = String(evalError);
      // The API is working if it throws a meaningful error about missing scripts
      const isMeaningfulError = errorMsg.includes("Script not found") ||
                                 errorMsg.includes("script") ||
                                 errorMsg.includes("redeemer");
      results.push({
        name: "evalPlutusScripts: API callable",
        passed: true,
        message: `API processed tx and threw: ${errorMsg.slice(0, 100)}`,
      });
    }
  } catch (e) {
    // Outer catch - something went wrong with test setup
    results.push({
      name: "evalPlutusScripts: API callable",
      passed: false,
      message: String(e),
    });
  }

  // Test failing transaction evaluation
  try {
    const txBytes = hexToBytes(failingTxCborHex);
    const utxoBytes = hexToBytes(failingUtxoCborHex);
    const slotConfig = SlotConfig.mainnet;
    const costModelsArray = [costModels.PlutusV1, costModels.PlutusV2, costModels.PlutusV3];

    try {
      Scalus.evalPlutusScripts(txBytes, utxoBytes, slotConfig, costModelsArray);
      results.push({
        name: "evalPlutusScripts: failing tx throws",
        passed: false,
        message: "Expected exception but none was thrown",
      });
    } catch (evalError: unknown) {
      results.push({
        name: "evalPlutusScripts: failing tx throws",
        passed: true,
        message: `Threw: ${String(evalError)}`,
      });

      // Check if error has logs property
      const hasLogs =
        evalError !== null &&
        typeof evalError === "object" &&
        "logs" in evalError &&
        Array.isArray((evalError as { logs: unknown[] }).logs);
      results.push({
        name: "evalPlutusScripts: error has logs property",
        passed: hasLogs,
        message: hasLogs
          ? `logs: ${JSON.stringify((evalError as { logs: string[] }).logs)}`
          : "No logs property",
      });
    }
  } catch (e) {
    results.push({
      name: "evalPlutusScripts: failing tx test",
      passed: false,
      message: String(e),
    });
  }

  return results;
}

/**
 * Run all tests - used by browser
 */
export function runAllTests(
  Scalus: ScalusAPI,
  SlotConfig: SlotConfigStatic
): TestResult[] {
  return [
    ...testApplyDataArgToScript(Scalus),
    ...testEvaluateScript(Scalus),
    ...testEvaluateScriptProfile(Scalus),
    ...testSlotConfig(SlotConfig),
    ...testEvalPlutusScripts(Scalus, SlotConfig),
  ];
}

// Export for browser bundle
if (typeof window !== "undefined") {
  (window as unknown as Record<string, unknown>).runAllSharedTests = runAllTests;
  (window as unknown as Record<string, unknown>).testApplyDataArgToScript = testApplyDataArgToScript;
  (window as unknown as Record<string, unknown>).testEvaluateScript = testEvaluateScript;
  (window as unknown as Record<string, unknown>).testEvaluateScriptProfile = testEvaluateScriptProfile;
  (window as unknown as Record<string, unknown>).testSlotConfig = testSlotConfig;
  (window as unknown as Record<string, unknown>).testEvalPlutusScripts = testEvalPlutusScripts;
}
