package scalus.uplc.eval

import scalus.builtin.platform
import scalus.cardano.ledger.ExUnits
import scalus.uplc.eval.*

class RestrictingBudgetSpenderWithScriptDump(
    maxBudget: ExUnits,
    debugDumpFilesForTesting: Boolean
) extends RestrictingBudgetSpender(maxBudget) {
    override def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit = {
        if debugDumpFilesForTesting then
            cat match
                case ExBudgetCategory.BuiltinApp(fun) =>
                    val logMessage =
                        s"fun $$${fun}, cost: ExUnits { mem: ${budget.memory}, cpu: ${budget.steps} }\n"
                    platform.appendFile("scalus.log", logMessage.getBytes("UTF-8"))
                case _ =>
        super.spendBudget(cat, budget, env)
    }
}
