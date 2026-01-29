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

/** Transaction submission result. */
export interface SubmitResult {
  isSuccess: boolean;
  txHash?: string;
  error?: string;
  logs?: string[];
}

/** Cardano emulator for testing transactions. */
export class Emulator {
  /**
   * Create an emulator with initial UTxOs.
   * @param initialUtxosCbor CBOR-encoded UTxO map (Map[TransactionInput, TransactionOutput]).
   * @param slotConfig Slot configuration for time conversions.
   */
  constructor(initialUtxosCbor: Uint8Array, slotConfig: SlotConfig);

  /**
   * Submit a transaction to the emulator.
   * @param txCborBytes CBOR-encoded transaction bytes.
   * @returns Result with isSuccess, txHash (on success), or error and logs (on failure).
   */
  submitTx(txCborBytes: Uint8Array): SubmitResult;

  /**
   * Get all UTxOs as a single CBOR-encoded map.
   * @returns CBOR-encoded UTxO map.
   */
  getUtxosCbor(): Uint8Array;

  /**
   * Get UTxOs for a specific address.
   * @param addressBech32 Address in bech32 format.
   * @returns Array of CBOR-encoded UTxO entries.
   */
  getUtxosForAddress(addressBech32: string): Uint8Array[];

  /**
   * Get all UTxOs as CBOR-encoded entries.
   * @returns Array of CBOR-encoded UTxO entries.
   */
  getAllUtxos(): Uint8Array[];

  /**
   * Set the current slot.
   * @param slot The slot number.
   */
  setSlot(slot: number): void;

  /**
   * Create a snapshot of the current emulator state.
   * @returns A new Emulator with the same state.
   */
  snapshot(): Emulator;

  /**
   * Create an emulator with funded addresses.
   * @param addressesBech32 Array of addresses in bech32 format.
   * @param slotConfig Slot configuration.
   * @param lovelacePerAddress Amount of lovelace per address (default: 10,000 ADA).
   * @returns A new Emulator with funded addresses.
   */
  static withAddresses(
    addressesBech32: string[],
    slotConfig: SlotConfig,
    lovelacePerAddress?: bigint
  ): Emulator;
}
