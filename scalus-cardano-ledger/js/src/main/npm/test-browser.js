const resultsDiv = document.getElementById('results');

function log(message, type = 'info') {
    const div = document.createElement('div');
    div.className = `test-result ${type}`;
    div.innerHTML = message;
    resultsDiv.appendChild(div);
    console.log(message.replace(/<[^>]*>/g, ''));
}

function clearResults() {
    resultsDiv.innerHTML = '';
}

function testBasicScriptEvaluation() {
    log('<b>Test 1: Basic Script Evaluation</b>');
    try {
        const script = "545301010023357389210753756363657373004981";
        const applied = Scalus.applyDataArgToScript(script, JSON.stringify({"int": 42}));
        const result = Scalus.evaluateScript(applied);

        console.log('Evaluation result:', result);

        if (result.isSuccess) {
            log(`Script evaluated successfully<br>
                Memory: ${result.budget.memory}<br>
                Steps: ${result.budget.steps}<br>
                Logs: ${JSON.stringify(result.logs)}`, 'success');
        } else {
            log(`Script evaluation failed: ${JSON.stringify(result)}`, 'error');
        }
    } catch (e) {
        log(`Error: ${e.message}`, 'error');
        console.error(e);
    }
}

function testSlotConfig() {
    log('<b>Test 2: SlotConfig</b>');
    try {
        if (typeof SlotConfig === 'undefined') {
            log('SlotConfig not available at global scope', 'error');
            return;
        }

        const mainnet = SlotConfig.Mainnet;
        const preview = SlotConfig.Preview;
        const preprod = SlotConfig.Preprod;

        // Test slot to time conversion
        const slot = 100000;
        const time = mainnet.slotToTime(slot);
        const backToSlot = mainnet.timeToSlot(time);

        log(`Mainnet SlotConfig available<br>
            Slot ${slot} -> Time ${time} -> Slot ${backToSlot}<br>
            Preview config available: ${!!preview}<br>
            Preprod config available: ${!!preprod}`, 'success');

        console.log('SlotConfig.Mainnet:', mainnet);
        console.log('SlotConfig.Preview:', preview);
        console.log('SlotConfig.Preprod:', preprod);
    } catch (e) {
        log(`SlotConfig Error: ${e.message}`, 'error');
        console.error(e);
    }
}

function testApplyDataArgToScript() {
    log('<b>Test 3: Apply Multiple Data Args</b>');
    try {
        const script = "545301010023357389210753756363657373004981";

        // Test with different data types using correct Scalus JSON format:
        // - int: {"int": number}
        // - bytes: {"bytes": "hex"}
        // - list: {"list": [...]}
        // - map: {"map": [{"k": key, "v": value}, ...]}
        // - constructor: {"constructor": number, "fields": [...]}
        const testCases = [
            {"int": 0},
            {"int": -42},
            {"int": 9999999999999},
            {"bytes": "deadbeef"},
            {"bytes": ""},
            {"list": [{"int": 1}, {"int": 2}, {"int": 3}]},
            {"list": []},
            {"map": [{"k": {"int": 1}, "v": {"bytes": "aa"}}]},
            {"map": []},
            {"constructor": 0, "fields": [{"int": 42}]},
            {"constructor": 1, "fields": []},
            // Nested structure
            {"constructor": 0, "fields": [
                {"list": [{"int": 1}, {"int": 2}]},
                {"map": [{"k": {"bytes": "abcd"}, "v": {"int": 100}}]}
            ]}
        ];

        let passed = 0;
        let failed = 0;
        const failures = [];

        for (const data of testCases) {
            try {
                const applied = Scalus.applyDataArgToScript(script, JSON.stringify(data));
                console.log(`Applied ${JSON.stringify(data)} -> ${applied.substring(0, 40)}...`);
                passed++;
            } catch (e) {
                failed++;
                failures.push(`${JSON.stringify(data)}: ${e.message}`);
                console.error(`Failed for ${JSON.stringify(data)}:`, e);
            }
        }

        if (failed === 0) {
            log(`All ${testCases.length} data types applied successfully`, 'success');
        } else {
            log(`Passed: ${passed}, Failed: ${failed}<br>Failures:<br>${failures.join('<br>')}`, 'error');
        }
    } catch (e) {
        log(`Error: ${e.message}`, 'error');
        console.error(e);
    }
}

function testExUnits() {
    log('<b>Test 4: ExUnits from Result</b>');
    try {
        // Get ExUnits from an actual evaluation result
        const script = "545301010023357389210753756363657373004981";
        const applied = Scalus.applyDataArgToScript(script, JSON.stringify({"int": 42}));
        const result = Scalus.evaluateScript(applied);

        const budget = result.budget;
        log(`ExUnits from result: memory=${budget.memory}, steps=${budget.steps}<br>
            Budget type: ${budget.constructor?.name || typeof budget}`, 'success');
        console.log('Budget object:', budget);
        console.log('Budget prototype:', Object.getPrototypeOf(budget));

        // Check what's available on Scalus
        console.log('Scalus.ExUnits:', Scalus.ExUnits);
        console.log('All Scalus keys:', Object.keys(Scalus));
    } catch (e) {
        log(`ExUnits Error: ${e.message}`, 'error');
        console.error(e);
    }
}

function testScriptFailure() {
    log('<b>Test 5: Script Failure Handling</b>');
    try {
        // This script should fail - it's a simple "fail" script
        // UPLC: (error)
        const failScript = "450100002261";
        const result = Scalus.evaluateScript(failScript);

        if (!result.isSuccess) {
            log(`Script correctly failed as expected<br>
                Logs: ${JSON.stringify(result.logs)}`, 'success');
        } else {
            log('Script unexpectedly succeeded', 'error');
        }
        console.log('Failure result:', result);
    } catch (e) {
        // Some failure modes throw exceptions with logs property
        const hasLogs = e.logs !== undefined;
        log(`Script threw exception: ${e.message}<br>
            Has logs property: ${hasLogs}${hasLogs ? '<br>Logs: ' + JSON.stringify(e.logs) : ''}`, 'success');
        console.log('Exception:', e);
        if (hasLogs) console.log('Exception logs:', e.logs);
    }
}

function testEvalPlutusScripts() {
    log('<b>Test 6: evalPlutusScripts API check</b>');
    try {
        if (typeof Scalus.evalPlutusScripts === 'function') {
            log(`evalPlutusScripts is available<br>
                Usage: Scalus.evalPlutusScripts(txCborBytes, utxoCborBytes, slotConfig, costModels)<br>
                - txCborBytes: number[] (CBOR bytes of transaction)<br>
                - utxoCborBytes: number[] (CBOR bytes of UTxO map)<br>
                - slotConfig: SlotConfig (e.g., SlotConfig.Mainnet)<br>
                - costModels: number[][] ([PlutusV1, PlutusV2, PlutusV3] cost models)<br>
                Returns: Redeemer[] with computed execution budgets<br>
                Throws: PlutusScriptEvaluationException with .message and .logs on failure`, 'success');
            console.log('evalPlutusScripts:', Scalus.evalPlutusScripts);
        } else {
            log(`evalPlutusScripts not found. Available: ${Object.keys(Scalus).join(', ')}`, 'error');
        }
    } catch (e) {
        log(`Error: ${e.message}`, 'error');
        console.error(e);
    }
}

function testApiSummary() {
    log('<b>Test 7: API Summary</b>');
    try {
        const apis = [];

        // Check Scalus methods
        if (typeof Scalus.applyDataArgToScript === 'function') apis.push('Scalus.applyDataArgToScript');
        if (typeof Scalus.evaluateScript === 'function') apis.push('Scalus.evaluateScript');
        if (typeof Scalus.evalPlutusScripts === 'function') apis.push('Scalus.evalPlutusScripts');
        if (Scalus.ExUnits) apis.push('Scalus.ExUnits');
        if (Scalus.Result) apis.push('Scalus.Result');
        if (Scalus.Redeemer) apis.push('Scalus.Redeemer');
        if (Scalus.PlutusScriptEvaluationException) apis.push('Scalus.PlutusScriptEvaluationException');

        // Check SlotConfig
        if (typeof SlotConfig !== 'undefined') {
            apis.push('SlotConfig');
            if (SlotConfig.Mainnet) apis.push('SlotConfig.Mainnet');
            if (SlotConfig.Preview) apis.push('SlotConfig.Preview');
            if (SlotConfig.Preprod) apis.push('SlotConfig.Preprod');
        }

        log(`Available APIs (${apis.length}):<br>${apis.join('<br>')}`, 'success');
        console.log('Full Scalus object:', Scalus);
        console.log('Full SlotConfig:', SlotConfig);
    } catch (e) {
        log(`Error: ${e.message}`, 'error');
        console.error(e);
    }
}

function runAllTests() {
    clearResults();
    log('<b>Running Scalus Cardano Ledger Browser Tests</b><br>Bundle: scalus.js', 'info');

    // Check if Scalus is loaded
    if (typeof Scalus === 'undefined') {
        log('ERROR: Scalus is not loaded. Check console for module.exports details.', 'error');
        console.log('window.Scalus:', window.Scalus);
        console.log('module.exports:', typeof module !== 'undefined' ? module.exports : 'module not defined');
        return;
    }

    const methods = Object.keys(Scalus);
    log(`Scalus object loaded. Available methods: ${methods.join(', ')}`, 'info');
    console.log('Scalus:', Scalus);

    testBasicScriptEvaluation();
    testSlotConfig();
    testApplyDataArgToScript();
    testExUnits();
    testScriptFailure();
    testEvalPlutusScripts();
    testApiSummary();

    log('<b>All tests completed. Check console for details.</b>', 'info');
}

// Auto-run on load
window.onload = () => {
    log('Page loaded. Click "Run All Tests" to start.', 'info');
    // Log what we got from the module
    console.log('Scalus available:', typeof Scalus !== 'undefined');
    console.log('SlotConfig available:', typeof SlotConfig !== 'undefined');
};
