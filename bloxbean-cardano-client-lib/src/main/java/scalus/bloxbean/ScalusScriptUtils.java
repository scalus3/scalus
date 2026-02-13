package scalus.bloxbean;

import com.bloxbean.cardano.client.plutus.spec.ListPlutusData;
import com.bloxbean.cardano.client.plutus.spec.PlutusData;
import scalus.uplc.Constant;
import scalus.uplc.Program;
import scalus.uplc.Term;
import scalus.utils.ScalusSourcePos;

public class ScalusScriptUtils {
    /**
     * Apply params to the compiled script
     *
     * @param serializedPlutusProgram double cbor encoded hex string of the compiled program
     * @param params       ListPlutusData of params
     * @return double cbor encoded hex string of the compiled program with params applied
     */
    public static String applyParamsToScript(String serializedPlutusProgram, ListPlutusData params) {
        return applyParamsToScript(serializedPlutusProgram, params.getPlutusDataList().toArray(new PlutusData[0]));
    }

    /**
     * Apply params to the compiled script
     *
     * @param serializedPlutusProgram double cbor encoded hex string of the compiled program
     * @param params       PlutusData params
     * @return double cbor encoded hex string of the compiled program with params applied
     */
    public static String applyParamsToScript(String serializedPlutusProgram, PlutusData... params) {
        var program = Program.fromDoubleCborHex(serializedPlutusProgram);
        for (var p : params) {
            var scalusData = Interop.toScalusData(p);
            var term = Term.Const.apply(Constant.Data.apply(scalusData), ScalusSourcePos.empty());
            program = program.applyArg(term);
        }
        return program.doubleCborHex();
    }
}
