package scalus.cardano.node;

import scalus.cardano.address.Address;
import scalus.cardano.ledger.ProtocolParams;
import scalus.cardano.ledger.Transaction;
import scalus.cardano.ledger.TransactionInput;
import scalus.cardano.ledger.TransactionOutput;
import scalus.cardano.ledger.UnitInterval;
import scalus.cardano.ledger.Utxo;
import scalus.cardano.ledger.Value;
import scalus.cardano.ledger.epoch.FuturePParams;
import scalus.cardano.ledger.epoch.IndividualPoolStake;
import scalus.cardano.ledger.epoch.NewEpochState;
import scalus.cardano.ledger.epoch.PoolPerformance;
import scalus.cardano.ledger.epoch.RatifyState;
import scalus.cardano.ledger.epoch.RewardUpdate;
import scalus.cardano.ledger.epoch.SnapShots;
import scalus.cardano.node.emulator.AwaitTxPolicy;
import scalus.cardano.node.emulator.BlockIssuer;
import scalus.cardano.node.emulator.BlockProduction;
import scalus.cardano.node.emulator.ChainConfig;
import scalus.cardano.node.emulator.ChainView;
import scalus.cardano.node.emulator.ChainViewError;
import scalus.cardano.node.emulator.DropReason;
import scalus.cardano.node.emulator.EmulatorResult;
import scalus.cardano.node.emulator.ForgeError;
import scalus.cardano.node.emulator.OrphanTxPolicy;
import scalus.cardano.node.emulator.RollbackError;
import scalus.cardano.node.emulator.TxStatus;
import scalus.cardano.node.stream.ChainPoint;
import scalus.cardano.node.stream.ChainTip;
import scalus.cardano.node.stream.internal.ChainEvent;
import scalus.uplc.builtin.ByteString;

import java.util.EnumSet;
import java.util.List;
import java.util.Map;

/**
 * Exercises the chain members of the Emulator from real Java, spec [SF-5]. If this compiles, they
 * are Java-usable.
 *
 * <p>A case of a Scala 3 enum is a static field of the enum's module class, such as
 * {@code TxStatus$.Pending}, and Java cannot write {@code new ChainView.Stable(3)}, because the
 * class carries the enum flag. So the companions forward the cases as static methods:
 * {@code ChainView.tip()}, {@code TxStatus.pending()}, {@code DropReason.expired()}.
 */
public final class JavaEmulatorChainInterop {
    private JavaEmulatorChainInterop() {}

    // ChainConfig from Java: the presets and the with-methods, spec [CF-2], [CF-3].
    public static Emulator createManual(List<Address> addresses, Value value) {
        ChainConfig config = ChainConfig.manual().withSecurityParam(10L);
        return Emulator.withAddresses(addresses, value, config);
    }

    // BlockIssuer from Java: the static forwarders and the with-method, ledger-rules spec [ES-1a].
    // A pool id is a ByteString to Java, since PoolKeyHash is an opaque type.
    public static ChainConfig issuedBy(ByteString poolId) {
        return ChainConfig.manual().withBlockIssuer(BlockIssuer.pool(poolId));
    }

    public static BlockIssuer unattributed() {
        return BlockIssuer.unattributed();
    }

    // The reward settings from Java: the static forwarders of PoolPerformance and the
    // with-methods, ledger-rules spec [ES-1], [ES-2a], [ES-1a].
    public static ChainConfig measured(long fNumerator, long fDenominator) {
        return ChainConfig.manual()
                .withPoolPerformance(PoolPerformance.measured())
                .withActiveSlotsCoeff(new UnitInterval(fNumerator, fDenominator));
    }

    public static PoolPerformance ideal() {
        return PoolPerformance.ideal();
    }

    // The chain policies are Java enums, interop rule 8: an EnumSet of each.
    public static EnumSet<BlockProduction> blockProductions() {
        return EnumSet.allOf(BlockProduction.class);
    }

    public static EnumSet<AwaitTxPolicy> awaitTxPolicies() {
        return EnumSet.allOf(AwaitTxPolicy.class);
    }

    public static EnumSet<OrphanTxPolicy> orphanTxPolicies() {
        return EnumSet.allOf(OrphanTxPolicy.class);
    }

    // PoolPerformance is a Java enum: a switch over its cases and an EnumSet of them.
    public static String performanceWord(PoolPerformance performance) {
        switch (performance) {
            case Ideal:
                return "ideal";
            case Measured:
                return "measured";
            default:
                throw new IllegalArgumentException(performance.name());
        }
    }

    public static int performanceCount() {
        return EnumSet.allOf(PoolPerformance.class).size();
    }

    public static ChainPoint produceBlock(Emulator emulator) {
        return emulator.produceBlock();
    }

    public static EmulatorResult<ForgeError, ChainPoint> produceBlockOn(
            Emulator emulator, ChainPoint on, List<Transaction> txs) {
        return emulator.tryProduceBlock(on, txs);
    }

    public static EmulatorResult<ForgeError, ChainPoint> produceBlockAt(
            Emulator emulator, ChainPoint on, long slot, List<Transaction> txs) {
        return emulator.tryProduceBlock(on, slot, txs);
    }

    public static EmulatorResult<RollbackError, ChainPoint> rollback(Emulator emulator, int n) {
        return emulator.tryRollback(n);
    }

    public static ChainPoint genesis() {
        return ChainPoint.origin();
    }

    public static ChainTip tip(Emulator emulator) {
        return emulator.tip();
    }

    public static boolean isPending(Emulator emulator, ByteString txHash) {
        return emulator.txStatus(txHash) == TxStatus.pending();
    }

    /** The status as one word, through every static TxStatus forwarder. */
    public static String statusWord(Emulator emulator, ByteString txHash) {
        TxStatus status = emulator.txStatus(txHash);
        if (status == TxStatus.pending()) return "pending";
        if (status == TxStatus.rolledBack()) return "rolledBack";
        if (status == TxStatus.notFound()) return "notFound";
        if (status instanceof TxStatus.Dropped) {
            DropReason reason = ((TxStatus.Dropped) status).reason();
            if (reason == DropReason.expired()) return "expired";
            if (reason == DropReason.inputsSpent()) return "inputsSpent";
            return "invalid";
        }
        return "confirmed";
    }

    /** Whether revalidation dropped the tx because its validity interval ended. */
    public static boolean isExpired(Emulator emulator, ByteString txHash) {
        TxStatus status = emulator.txStatus(txHash);
        return status instanceof TxStatus.Dropped
                && ((TxStatus.Dropped) status).reason() == DropReason.expired();
    }

    /** The depth of a confirmed tx, or -1. */
    public static int confirmedDepth(Emulator emulator, ByteString txHash) {
        TxStatus status = emulator.txStatus(txHash);
        if (status instanceof TxStatus.Confirmed) return ((TxStatus.Confirmed) status).depth();
        return -1;
    }

    public static List<Transaction> mempool(Emulator emulator) {
        return emulator.getMempoolTxs();
    }

    public static List<ChainEvent> diff(Emulator emulator, ChainTip from, ChainTip to) {
        return emulator.getDiff(from, to);
    }

    public static EmulatorResult<ChainViewError, List<Utxo>> utxosAtTip(
            Emulator emulator, Address address) {
        return emulator.findUtxosForAddress(address, ChainView.tip());
    }

    public static EmulatorResult<ChainViewError, List<Utxo>> utxosInMempool(
            Emulator emulator, Address address) {
        return emulator.findUtxosForAddress(address, ChainView.mempool());
    }

    public static EmulatorResult<ChainViewError, List<Utxo>> utxosAtDepth(
            Emulator emulator, Address address, int depth) {
        return emulator.findUtxosForAddress(address, ChainView.stable(depth));
    }

    // The stake reads from Java, ledger-rules spec [ES-9a]

    public static SnapShots snapshots(Emulator emulator) {
        return emulator.getSnapshots();
    }

    /** The pending reward update, or null when nothing is pending. */
    public static RewardUpdate pendingRewards(Emulator emulator) {
        return emulator.getPendingRewardsOrNull();
    }

    /** The pending ratify result, ledger-rules spec [ES-9b]. */
    public static RatifyState pendingRatify(Emulator emulator) {
        return emulator.getPendingRatify();
    }

    // The node-shaped ledger state from Java, ledger-rules spec D122

    /** The whole ledger state, read once, D122. */
    public static NewEpochState ledgerState(Emulator emulator) {
        return emulator.getLedgerState();
    }

    /** The reward update the next boundary pays, or null before the first boundary. */
    public static RewardUpdate ledgerStatePendingRewards(NewEpochState state) {
        return state.getPendingRewardsOrNull();
    }

    /** The blocks of each pool in the epoch before the last boundary. A pool id is a ByteString. */
    public static Map<ByteString, Long> blocksMadePrev(NewEpochState state) {
        return state.getBlocksMadePrev();
    }

    /** The blocks of each pool since the last boundary. */
    public static Map<ByteString, Long> blocksMadeCur(NewEpochState state) {
        return state.getBlocksMadeCur();
    }

    /** The stake of each pool in the pool distribution, D123. */
    public static Map<ByteString, IndividualPoolStake> poolDistr(NewEpochState state) {
        return state.poolDistr().getPools();
    }

    /** The pool-ranking log-weights of each pool, D124. */
    public static Map<ByteString, List<Float>> likelihoods(NewEpochState state) {
        return state.epochState().nonMyopic().getLikelihoods();
    }

    /** The UTxO set, read through the ledger state of the epoch state. */
    public static scala.collection.immutable.Map<TransactionInput, TransactionOutput> ledgerStateUtxos(
            Emulator emulator) {
        return emulator.ledgerState().epochState().ledgerState().utxoState().utxo();
    }

    // FuturePParams from Java: the static forwarders of its cases, interop rule 9, D125.
    public static FuturePParams noPParamsUpdate() {
        return FuturePParams.noPParamsUpdate();
    }

    public static FuturePParams definitePParamsUpdate(ProtocolParams params) {
        return FuturePParams.definitePParamsUpdate(params);
    }

    /** The pparams the next boundary adopts, or null when it adopts none, D125. */
    public static ProtocolParams futureParamsOrNull(Emulator emulator) {
        FuturePParams future = emulator.ledgerState().epochState().futureParams();
        if (future instanceof FuturePParams.DefinitePParamsUpdate) {
            return ((FuturePParams.DefinitePParamsUpdate) future).params();
        }
        return null;
    }
}
