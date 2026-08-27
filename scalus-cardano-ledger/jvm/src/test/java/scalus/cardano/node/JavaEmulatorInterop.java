package scalus.cardano.node;

import scalus.cardano.address.Address;
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

    // SubmitResult instead of Either — expected failures need no try/catch.
    public static SubmitResult submit(Emulator emulator, Transaction tx) {
        return emulator.trySubmit(tx);
    }

    // CompletableFuture variant (already-completed for the Emulator, so join() never blocks).
    public static SubmitResult submitAsync(Emulator emulator, Transaction tx) {
        return emulator.submitAsync(tx).join();
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
