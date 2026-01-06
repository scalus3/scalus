// scalus.d.ts

/** Main API exported by Scalus */
export namespace Scalus {
  /** Execution units representation. */
  export class ExUnits {
    constructor(memory: bigint, steps: bigint);
    memory: bigint;
    steps: bigint;
  }

  /** Script evaluation result. */
  export class Result {
    constructor(isSuccess: boolean, budget: ExUnits, logs: string[]);
    isSuccess: boolean;
    budget: ExUnits;
    logs: string[];
  }

  /** Redeemer with execution budget. */
  export class Redeemer {
    constructor(tag: string, index: number, budget: ExUnits);
    tag: string;
    index: number;
    budget: ExUnits;
  }

  /** Exception thrown when Plutus script evaluation fails. Includes logs from the failing script. */
  export class PlutusScriptEvaluationException extends Error {
    constructor(message: string, logs: string[]);
    logs: string[];
  }

  /**
   * Applies a data argument to a Plutus script given its double-CBOR-encoded hex.
   * @param doubleCborHex The double-CBOR-encoded hex of the script.
   * @param data The JSON-encoded data argument.
   * @returns The double-CBOR-encoded hex of the script with the data argument applied.
   */
  export function applyDataArgToScript(
    doubleCborHex: string,
    data: string,
  ): string;

  /**
   * Evaluates a Plutus script given its double-CBOR-encoded hex.
   * @param doubleCborHex The double-CBOR-encoded hex of the script.
   * @returns A Result object with the evaluation outcome, budget, and logs.
   */
  export function evaluateScript(doubleCborHex: string): Result;

  /**
   * Evaluates all Plutus scripts in a transaction against the provided UTxO set.
   * @param txCborBytes The CBOR bytes of the transaction containing the Plutus scripts.
   * @param utxoCborBytes The CBOR bytes of the UTxO map (Map[TransactionInput, TransactionOutput]).
   * @param slotConfig The slot configuration for time conversions.
   * @param costModels Array of cost models for each Plutus language version [V1, V2, V3]. Each cost model is an array of cost values.
   * @returns An array of Redeemers with computed execution budgets.
   */
  export function evalPlutusScripts(
    txCborBytes: Uint8Array,
    utxoCborBytes: Uint8Array,
    slotConfig: SlotConfig,
    costModels: number[][],
  ): Redeemer[];
}

/** Slot configuration for time conversions between slots and POSIX time. */
export class SlotConfig {
  constructor(zeroTime: number, zeroSlot: number, slotLength: number);

  /**
   * Convert a slot number to POSIX time in milliseconds.
   * @param slot The slot number.
   * @returns The POSIX time in milliseconds.
   */
  slotToTime(slot: number): number;

  /**
   * Convert POSIX time in milliseconds to a slot number.
   * @param time The POSIX time in milliseconds.
   * @returns The slot number.
   */
  timeToSlot(time: number): number;

  /** Mainnet slot configuration (starting at Shelley era). */
  static mainnet: SlotConfig;

  /** Preview testnet slot configuration. */
  static preview: SlotConfig;

  /** Preprod testnet slot configuration. */
  static preprod: SlotConfig;
}
