import { createServer } from "node:http";
import { readFile, mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, extname, join, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn } from "node:child_process";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const chrome = process.env.CHROME_BIN ?? (process.platform === "darwin"
    ? "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    : "google-chrome");
const mime = { ".html": "text/html", ".js": "text/javascript" };
const server = createServer(async (req, res) => {
    try {
        const path = resolve(root, "." + new URL(req.url, "http://localhost").pathname);
        if (!path.startsWith(root + sep)) throw new Error("Invalid path");
        const body = await readFile(path);
        res.writeHead(200, { "Content-Type": mime[extname(path)] ?? "application/octet-stream" });
        res.end(body);
    } catch {
        res.writeHead(404);
        res.end();
    }
});
const profile = await mkdtemp(join(tmpdir(), "scalus-browser-"));
let child;
try {
    await new Promise((resolve, reject) => {
        server.once("error", reject);
        server.listen(0, "127.0.0.1", resolve);
    });
    const url = `http://127.0.0.1:${server.address().port}/__tests__/browser-smoke.html`;
    await new Promise((resolve, reject) => {
        let stdout = "";
        let stderr = "";
        child = spawn(chrome, [
            "--headless", "--disable-gpu", "--no-first-run", "--no-default-browser-check",
            `--user-data-dir=${profile}`, "--dump-dom", "--virtual-time-budget=10000", url,
        ], { stdio: ["ignore", "pipe", "pipe"] });
        const timeout = setTimeout(() => finish(new Error("Browser test timed out")), 30000);
        let finished = false;
        function finish(error) {
            if (finished) return;
            finished = true;
            clearTimeout(timeout);
            child.kill("SIGTERM");
            if (error) reject(new Error(`${error.message}\n${stdout}\n${stderr}`));
            else resolve();
        }
        child.on("error", finish);
        child.stdout.on("data", chunk => {
            stdout += chunk;
            if (/<html\b[^>]*data-status="passed"/.test(stdout)) finish();
            else if (/<html\b[^>]*data-status="failed"/.test(stdout))
                finish(new Error("Browser assertions failed"));
        });
        child.stderr.on("data", chunk => { stderr += chunk; });
        child.on("close", code => {
            if (!finished) finish(new Error(`Chrome exited (${code}) without passing tests`));
        });
    });
    console.log("PASS: Chromium shared tests and typed Emulator query");
} finally {
    if (child && child.exitCode === null && child.signalCode === null) {
        await new Promise(resolve => {
            child.once("exit", resolve);
            child.kill("SIGTERM");
            setTimeout(() => { child.kill("SIGKILL"); resolve(); }, 2000).unref();
        });
    }
    server.closeAllConnections();
    await new Promise(resolve => server.close(resolve));
    await rm(profile, { recursive: true, force: true, maxRetries: 5, retryDelay: 100 });
}
