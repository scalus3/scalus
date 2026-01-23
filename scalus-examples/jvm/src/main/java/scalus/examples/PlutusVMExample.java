package scalus.examples;

import scala.math.BigInt;
import scalus.uplc.builtin.Data;
import scalus.cardano.ledger.Language;
import scalus.cardano.ledger.ProtocolParams;
import scalus.uplc.DeBruijnedProgram;
import scalus.uplc.eval.*;

import java.nio.file.Paths;
import java.util.Arrays;

import static java.nio.file.Files.readAllBytes;
import static java.nio.file.Files.readString;

class PlutusVMExample {

    public static void evaluationExample1() throws Exception {
        // Read script from a double-CBOR-encoded hex string, like in *.plutus files
        var script = DeBruijnedProgram.fromDoubleCborHex("545301010023357389210753756363657373004981");
        // Read script from a CBOR-encoded hex string, like in CIP-57 Blueprint files
        var script2 = DeBruijnedProgram.fromCborHex("5301010023357389210753756363657373004981");
        // Apply an argument to the script
        var appliedScript = script.applyArg(Data.I.apply(BigInt.apply(42)));
        // Load protocol parameters from a JSON file
        var params = readString(Paths.get("../../scalus-core/shared/src/main/resources/protocol-params.json"));
        var pparams = ProtocolParams.fromCardanoCliJson(params);
        // Setup PlutusV3 machine parameters (cost model) from the protocol parameters
        var machineParams = MachineParams.fromProtocolParams(pparams, Language.PlutusV3);
        // Setup Plutus 3 VM with machine parameters
        var plutusVM = PlutusVM.makePlutusV3VM(machineParams);
        // Evaluate the script
        var result = plutusVM.evaluateScriptDebug(appliedScript);
        System.out.println("Evaluation Result:" + result);
        System.out.println("Is success: " + result.isSuccess());
        System.out.println("Budget used: " + result.budget());
    }

    public static void evaluationExample2() throws Exception {
        // Read flat encoded Plutus script from a file
        var flatScript = readAllBytes(Paths.get("../../bench/src/main/resources/data/auction_1-1.flat"));
        var script = DeBruijnedProgram.fromFlatEncoded(flatScript);
        // Evaluate the script using default parameters for Plutus V2 VM
        var plutusVM = PlutusVM.makePlutusV2VM();
        try {
            // Pass custom budget spender and logger
            var result = plutusVM.evaluateScript(script, NoBudgetSpender$.MODULE$, NoLogger$.MODULE$);
            System.out.println(result);
        } catch (StackTraceMachineError e) {
            String stacktrace = Arrays.stream(e.getCekStack()).map(Object::toString).reduce("", (a, b) -> a + "\n" + b);
            e.env().foreach((t) -> {
                System.out.println(t._1() + " -> " + t._2());
                return null;
            });
            System.out.println(stacktrace);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) throws Exception {
        evaluationExample1();
        evaluationExample2();
    }
}
