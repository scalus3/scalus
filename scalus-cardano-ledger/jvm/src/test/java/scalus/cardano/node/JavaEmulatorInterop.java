package scalus.cardano.node;

import scalus.cardano.address.Address;
import scalus.cardano.ledger.Coin;
import scalus.cardano.ledger.Committee;
import scalus.cardano.ledger.Constitution;
import scalus.cardano.ledger.Credential;
import scalus.cardano.ledger.GovState;
import scalus.cardano.ledger.Transaction;
import scalus.cardano.ledger.TransactionInput;
import scalus.cardano.ledger.TransactionOutput;
import scalus.uplc.builtin.ByteString;

import java.util.List;

/** Exercises the Emulator API from real Java. If this compiles, the API is Java-usable. */
public final class JavaEmulatorInterop {
    private JavaEmulatorInterop() {}

    // Factory with Java collections — no Seq, no default args.
    public static Emulator createWithAddresses(List<Address> addresses) {
        return Emulator.withAddresses(addresses);
    }

    // Initial-state builder — the case class's Scala defaults are unusable from Java.
    public static Emulator createFromBuilder(TransactionInput input, TransactionOutput output) {
        EmulatorInitialState state = EmulatorInitialState.builder()
                .putUtxo(input, output)
                .build();
        return Emulator.withState(state);
    }

    // The gov fields of the initial state, spec [ES-6], and the gov read, spec [ES-9].
    public static GovState govStateWithCommittee(
            Committee committee, Credential cold, Credential hot, Constitution constitution) {
        EmulatorInitialState state = EmulatorInitialState.builder()
                .committee(committee)
                .addCommitteeHotKey(cold, hot)
                .constitution(constitution)
                .build();
        return Emulator.withState(state).getGovState();
    }

    // The reserves of the initial state, spec [ES-7], and the account-state reads, spec [ES-9].
    public static Coin reservesFromBuilder(Coin reserves) {
        EmulatorInitialState state = EmulatorInitialState.builder()
                .reserves(reserves)
                .build();
        return Emulator.withState(state).getReserves();
    }

    public static long currentEpoch(Emulator emulator) {
        return emulator.getCurrentEpoch();
    }

    // SubmitResult instead of Either — expected failures need no try/catch.
    public static SubmitResult submit(Emulator emulator, Transaction tx) {
        return emulator.trySubmit(tx);
    }

    // CompletableFuture variant (already-completed for the Emulator, so join() never blocks).
    public static SubmitResult submitAsync(Emulator emulator, Transaction tx) {
        return emulator.submitAsync(tx).join();
    }

    // Every ledger failure of a tx, by its cardano-ledger names, ledger-rules spec [FL-8].
    public static List<String> ledgerFailureNames(Emulator emulator, Transaction tx) {
        return emulator.getLedgerFailureNames(tx);
    }

    public static int utxoCount(Emulator emulator) {
        return emulator.getUtxos().size();
    }

    public static List<scalus.cardano.ledger.Utxo> utxosAt(Emulator emulator, Address address) {
        return emulator.findUtxosForAddress(address);
    }

    public static Transaction lookupOrNull(Emulator emulator, ByteString txHash) {
        return emulator.getTransactionOrNull(txHash);
    }

    // Slot control and snapshot are Java-friendly as-is.
    public static long advanceTo(Emulator emulator, long slot) {
        emulator.setSlot(slot);
        emulator.tick(5);
        return emulator.getCurrentSlot();
    }

    public static int snapshotUtxoCount(Emulator emulator) {
        return emulator.snapshot().getUtxos().size();
    }
}
