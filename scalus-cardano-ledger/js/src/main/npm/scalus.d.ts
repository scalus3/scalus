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
    constructor(
      isSuccess: boolean,
      budget: ExUnits,
      logs: string[],
      profileJson?: string,
    );
    isSuccess: boolean;
    budget: ExUnits;
    logs: string[];
    /**
     * CEK machine profiling data as JSON. Present only when the result was produced by
     * {@link evaluateScriptProfile}; `undefined` otherwise.
     */
    profileJson?: string;
  }

  /** Redeemer with execution budget. */
  export class Redeemer {
    constructor(tag: string, index: number, budget: ExUnits);
    tag: string;
    index: number;
    budget: ExUnits;
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
   * Evaluates a Plutus script with profiling enabled.
   *
   * Like {@link evaluateScript}, but the returned Result also carries the CEK machine profiling
   * data as JSON in `profileJson` (per-source-location and per-builtin cost plus the transition
   * edges). The interactive HTML report renderer is intentionally not bundled in `scalus.js` (to
   * keep the transaction-builder bundle small); generate it from this data with the Scala/JVM
   * `ProfileFormatter` if needed.
   * @param doubleCborHex The double-CBOR-encoded hex of the script.
   * @returns A Result with `profileJson` populated.
   */
  export function evaluateScriptProfile(doubleCborHex: string): Result;

  /**
   * Evaluates all Plutus scripts in a transaction against the provided UTxO set.
   * @param txCborBytes The CBOR bytes of the transaction containing the Plutus scripts.
   * @param utxoCborBytes The CBOR bytes of the UTxO map (Map[TransactionInput, TransactionOutput]).
   * @param slotConfig The slot configuration for time conversions.
   * @param costModels Array of cost models for each Plutus language version [V1, V2, V3]. Each cost model is an array of cost values.
   * @param protocolMajorVersion Optional Cardano protocol major version used for Plutus semantics and costing.
   * @returns An array of Redeemers with computed execution budgets.
   */
  export function evalPlutusScripts(
    txCborBytes: Uint8Array,
    utxoCborBytes: Uint8Array,
    slotConfig: SlotConfig,
    costModels: number[][],
    protocolMajorVersion?: number,
  ): Redeemer[];
}

/**
 * Thrown by `Scalus.evalPlutusScripts` when a Plutus script fails to evaluate.
 * Carries the failure message and the script's trace logs. Note: this is a plain
 * object (not a subclass of `Error`), so check it by shape or name rather than
 * `instanceof Error`.
 */
export class PlutusScriptEvaluationError {
  constructor(message: string, logs: string[]);
  message: string;
  logs: string[];
}

/** Slot and epoch configuration for a Cardano network. */
export class SlotConfig {
  /**
   * @param zeroTime POSIX time in milliseconds of slot `zeroSlot`.
   * @param zeroSlot First slot of the linear (post-Byron) era.
   * @param slotLength Slot length in milliseconds.
   * @param epochLength Epoch length in slots (default: 432000).
   * @param zeroEpoch Epoch number at `zeroSlot` (default: 0).
   */
  constructor(
    zeroTime: number,
    zeroSlot: number,
    slotLength: number,
    epochLength?: number,
    zeroEpoch?: number,
  );

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

  /**
   * Epoch containing the given slot. Slots before `zeroSlot` are clamped to `zeroEpoch`.
   * @param slot The slot number.
   * @returns The epoch number.
   */
  epochOf(slot: number): number;

  /**
   * First slot of the given epoch (for epochs >= `zeroEpoch`).
   * @param epoch The epoch number.
   * @returns The slot number.
   */
  firstSlotOfEpoch(epoch: number): number;

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

/** Delegation info returned by getDelegation. */
export interface DelegationInfo {
  /** Pool key hash bytes, or null if not delegated. */
  poolId: Uint8Array | null;
  /** Reward balance in lovelace. */
  rewards: bigint;
}

/** Stake registration entry for EmulatorInitialState. */
export interface StakeRegistration {
  /** Credential type: "key" for pub key hash, "script" for script hash. */
  credentialType: "key" | "script";
  /** Hex-encoded 28-byte credential hash. */
  credentialHash: string;
  /** Initial reward balance in lovelace. */
  rewards: bigint;
  /** Hex-encoded 28-byte pool key hash to delegate to (optional). */
  delegatedTo?: string;
}

/** Pool registration entry for EmulatorInitialState. */
export interface PoolRegistration {
  /** CBOR-encoded PoolRegistration certificate. */
  params: Uint8Array;
}

/** DRep registration entry for EmulatorInitialState. */
export interface DRepRegistration {
  /** Credential type: "key" for pub key hash, "script" for script hash. */
  credentialType: "key" | "script";
  /** Hex-encoded 28-byte credential hash. */
  credentialHash: string;
  /** Deposit in lovelace. */
  deposit: bigint;
  /** CBOR-encoded anchor (optional). */
  anchor?: Uint8Array;
}

/** Datum entry for EmulatorInitialState. */
export interface DatumEntry {
  /** Hex-encoded 32-byte datum hash. */
  hash: string;
  /** Hex-encoded CBOR-encoded datum. */
  datum: string;
}

/** Initial state for Emulator.withState. */
export interface EmulatorInitialState {
  /** CBOR-encoded UTxO map. */
  utxos: Uint8Array;
  /** Pre-registered stake credentials with rewards and optional delegation. */
  stakeRegistrations?: ReadonlyArray<StakeRegistration>;
  /** Pre-registered stake pools. */
  poolRegistrations?: ReadonlyArray<PoolRegistration>;
  /** Pre-registered DReps. */
  drepRegistrations?: ReadonlyArray<DRepRegistration>;
  /** Pre-seeded datum store. */
  datums?: ReadonlyArray<DatumEntry>;
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
   * Get the reward balance for a script-based stake credential.
   * @param scriptHashHex Hex-encoded script hash.
   * @returns Reward amount in lovelace, or null if not registered.
   */
  getStakeReward(scriptHashHex: string): bigint | null;

  /**
   * Set the current slot.
   * @param slot The slot number.
   */
  setSlot(slot: number): void;

  /**
   * Get the current slot number.
   * @returns The current slot.
   */
  getSlot(): number;

  /**
   * Advance the current slot by n slots.
   * @param n Number of slots to advance.
   */
  tick(n: number): void;

  /**
   * Check whether a transaction has been accepted.
   * @param txHashBytes 32-byte transaction hash.
   * @returns True if the transaction has been submitted and accepted.
   */
  hasTx(txHashBytes: Uint8Array): boolean;

  /**
   * Get delegation info for a stake credential.
   * @param stakeCredentialCbor CBOR-encoded stake credential.
   * @returns Delegation info with poolId and rewards.
   */
  getDelegation(stakeCredentialCbor: Uint8Array): DelegationInfo;

  /**
   * Look up a datum by its hash.
   * @param datumHashBytes 32-byte datum hash.
   * @returns CBOR-encoded datum, or null if unknown.
   */
  getDatum(datumHashBytes: Uint8Array): Uint8Array | null;

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

  /**
   * Create an emulator with full initial ledger state.
   * @param state Initial state including UTxOs, stake/pool/DRep registrations, and datums.
   * @param slotConfig Slot configuration.
   * @returns A new Emulator seeded with the given state.
   */
  static withState(
    state: EmulatorInitialState,
    slotConfig: SlotConfig
  ): Emulator;
}
