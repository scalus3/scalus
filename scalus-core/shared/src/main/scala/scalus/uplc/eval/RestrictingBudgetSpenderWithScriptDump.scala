package scalus.uplc.eval

import scalus.uplc.builtin.platform
import scalus.cardano.ledger.ExUnits
import scalus.uplc.eval.*

class RestrictingBudgetSpenderWithScriptDump(
    maxBudget: ExUnits,
    debugDumpFilesForTesting: Boolean,
    logPath: String
) extends RestrictingBudgetSpender(maxBudget) {

    /** Back-compat constructor: appends to the historical `scalus.log` in the working directory. */
    def this(maxBudget: ExUnits, debugDumpFilesForTesting: Boolean) =
        this(maxBudget, debugDumpFilesForTesting, "scalus.log")

    override def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit = {
        if debugDumpFilesForTesting then
            cat match
                case ExBudgetCategory.BuiltinApp(fun) =>
                    val logMessage =
                        s"fun $$${fun}, cost: ExUnits { mem: ${budget.memory}, cpu: ${budget.steps} }\n"
                    platform.appendFile(logPath, logMessage.getBytes("UTF-8"))
                case _ =>
        super.spendBudget(cat, budget, env)
    }
}
