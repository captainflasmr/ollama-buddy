/// ollama-buddy-pi.ts — pi-coding-agent extension
///
/// Bridges ollama-buddy assets (user prompts / skills) into pi-mono and adds
/// context-aware tooling inspired by ollama-buddy's safety model.
///
/// Install:
///   ln -sf /path/to/ollama-buddy/pi-extension/ollama-buddy-pi.ts \
///          ~/.pi/agent/extensions/ollama-buddy-pi.ts
///
/// Or add to ~/.pi/agent/settings.json:
///   { "extensions": ["/path/to/ollama-buddy/pi-extension/ollama-buddy-pi.ts"] }

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { readdir, readFile } from "fs/promises";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

// ─── Resolve paths relative to this file ────────────────────────────────────

const __dirname = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = join(__dirname, "..");
const USER_PROMPTS_DIR = join(REPO_ROOT, "ollama-buddy-user-prompts");

// ─── Configuration ────────────────────────────────────────────────────────────

/**
 * Tiered tool safety (inspired by ollama-buddy's tool calling model):
 *
 * SAFE_TOOLS      — read-only, always auto-approved silently
 * STANDARD_TOOLS  — write operations, auto-approved but logged
 * Everything else — always prompts for confirmation (e.g. bash, eval)
 */
const SAFE_TOOLS = new Set(["read", "list_directory", "search_files", "glob"]);
// const STANDARD_TOOLS = new Set(["write", "edit"]);
const STANDARD_TOOLS = new Set([]);

/**
 * Maximum characters allowed in a single tool result before it gets trimmed.
 * Bash output from builds/tests can easily be tens of thousands of chars.
 * Tune this down if your Ollama model has a small context window.
 */
const MAX_TOOL_RESULT_CHARS = 4000;

/**
 * When trimming, bias toward keeping the tail for bash results (errors
 * cluster at the end) vs an even split for everything else.
 */
const TRIM_BASH_HEAD_RATIO = 0.3;
const TRIM_DEFAULT_HEAD_RATIO = 0.5;

// ─── Session State ──────────────────────────────────────────────────────────

let sessionInputTokens = 0;
let sessionOutputTokens = 0;
let sessionMessages = 0;
let activeSkill: UserPrompt | null = null;

// ─── Helpers ─────────────────────────────────────────────────────────────────

function trimToolOutput(output: string, toolName?: string): string {
    if (output.length <= MAX_TOOL_RESULT_CHARS) return output;

    const isBash = toolName === "bash" || toolName === "execute_shell";
    const headRatio = isBash ? TRIM_BASH_HEAD_RATIO : TRIM_DEFAULT_HEAD_RATIO;

    const budget = MAX_TOOL_RESULT_CHARS - 80; // room for the summary line
    const headLen = Math.floor(budget * headRatio);
    const tailLen = budget - headLen;

    const head = output.slice(0, headLen);
    const tail = output.slice(-tailLen);
    const dropped = output.length - headLen - tailLen;

    return (
        head +
        `\n\n[... ${dropped} characters trimmed — keeping ${isBash ? "more tail (errors)" : "even split"} ...]\n\n` +
        tail
    );
}

interface UserPrompt {
    title: string;
    category: string;
    body: string;
    file: string;
}

function parseOrgPrompt(content: string, file: string): UserPrompt {
    const titleMatch = content.match(/^#\+TITLE:\s*(.+)$/m);
    const categoryMatch = content.match(/^#\+CATEGORY:\s*(.+)$/m);

    // Strip org headers to get the prompt body
    const body = content
        .split("\n")
        .filter((l) => !l.startsWith("#+"))
        .join("\n")
        .trim();

    return {
        title: titleMatch?.[1]?.trim() ?? file,
        category: categoryMatch?.[1]?.trim() ?? "general",
        body,
        file,
    };
}

async function loadUserPrompts(): Promise<UserPrompt[]> {
    const files = await readdir(USER_PROMPTS_DIR);
    const orgFiles = files.filter((f) => f.endsWith("__system.org"));

    const prompts: UserPrompt[] = [];
    for (const file of orgFiles) {
        const content = await readFile(join(USER_PROMPTS_DIR, file), "utf-8");
        prompts.push(parseOrgPrompt(content, file));
    }

    return prompts.sort((a, b) => {
        const cat = a.category.localeCompare(b.category);
        return cat !== 0 ? cat : a.title.localeCompare(b.title);
    });
}

function formatTokens(n: number): string {
    if (n >= 1000) return `${(n / 1000).toFixed(1)}k`;
    return n.toLocaleString();
}

// ─── Extension ───────────────────────────────────────────────────────────────

export default function (pi: ExtensionAPI) {

    // ── 1. Session start ──────────────────────────────────────────────────────

    pi.on("session_start", async (_event, ctx) => {
        sessionInputTokens = 0;
        sessionOutputTokens = 0;
        sessionMessages = 0;
        ctx.ui.notify("ollama-buddy-pi extension loaded!", "info");
    });


    // ── 2. Tiered tool safety ─────────────────────────────────────────────────
    //
    // Safe tools (read-only): always auto-approved, no noise
    // Standard tools (write/edit): auto-approved but logged
    // Everything else (bash, etc.): always requires confirmation

    pi.on("tool_call", async (event, ctx) => {
        if (SAFE_TOOLS.has(event.toolName)) {
            return;
        }

        if (STANDARD_TOOLS.has(event.toolName)) {
            ctx.ui.setStatus("tool", `✎ ${event.toolName}`);
            return;
        }

        // Dangerous tool — always confirm
        const ok = await ctx.ui.confirm(
            `Allow tool: ${event.toolName}`,
            `The model wants to call "${event.toolName}". Approve?`
        );

        if (!ok) {
            return { block: true, reason: `Tool "${event.toolName}" was denied by the user.` };
        }
    });


    // ── 3. Smart output trimming ──────────────────────────────────────────────
    //
    // Bash/shell output biases toward the tail (where errors appear).
    // Other tools get an even head/tail split.

    pi.on("tool_result", async (event, _ctx) => {
        if (!event.result?.content) return;

        for (const block of event.result.content) {
            if (block.type === "text" && typeof block.text === "string") {
                block.text = trimToolOutput(block.text, (event as any).toolName);
            }
        }
    });


    // ── 4. Cumulative token tracking ──────────────────────────────────────────
    //
    // Tracks per-message and cumulative session tokens in the status bar.

    pi.on("message_end", async (event, ctx) => {
        const usage = (event as any).message?.usage;
        if (!usage) return;

        const msgInput = usage.input ?? 0;
        const msgOutput = usage.output ?? 0;
        const msgTotal = usage.totalTokens ?? (msgInput + msgOutput);

        sessionInputTokens += msgInput;
        sessionOutputTokens += msgOutput;
        sessionMessages++;

        const sessionTotal = sessionInputTokens + sessionOutputTokens;

        ctx.ui.setStatus(
            "ctx",
            `msg: ~${formatTokens(msgTotal)} | session: ~${formatTokens(sessionTotal)}`
        );
    });


    // ── 5. Context limit warning ──────────────────────────────────────────────
    //
    // Warns when approaching the model's context window limit.

    pi.on("message_end", async (event, ctx) => {
        const msg = (event as any).message;
        const usage = msg?.usage;
        if (!usage) return;

        const total = usage.totalTokens ?? ((usage.input ?? 0) + (usage.output ?? 0));
        const contextWindow = msg?.model?.contextWindow ?? msg?.contextWindow;
        if (!contextWindow) return;

        const pct = (total / contextWindow) * 100;

        if (pct >= 90) {
            ctx.ui.notify(
                `Context at ${pct.toFixed(0)}% — consider /compact or starting a new session`,
                "warning"
            );
        } else if (pct >= 80) {
            ctx.ui.notify(`Context at ${pct.toFixed(0)}%`, "info");
        }
    });


    // ── 6. /ctx-stats command ─────────────────────────────────────────────────
    //
    // Shows a full breakdown of cumulative token usage for the session.

    pi.registerCommand("ctx-stats", {
        description: "Show cumulative session token usage breakdown",
        handler: async (_args, ctx) => {
            const sessionTotal = sessionInputTokens + sessionOutputTokens;
            const lines = [
                `── Session Token Stats ──`,
                `Messages:      ${sessionMessages}`,
                `Input tokens:  ~${sessionInputTokens.toLocaleString()}`,
                `Output tokens: ~${sessionOutputTokens.toLocaleString()}`,
                `Total tokens:  ~${sessionTotal.toLocaleString()}`,
                `Active skill:  ${activeSkill ? `${activeSkill.title} (${activeSkill.category})` : "none"}`,
            ];
            ctx.ui.notify(lines.join("\n"), "info");
        },
    });


    // ── 7. /skill command ─────────────────────────────────────────────────────
    //
    // Loads an ollama-buddy user prompt as the system prompt for this session.
    // Type /skill to pick from the library, /skill reset to clear.

    pi.registerCommand("skill", {
        description: "Load an ollama-buddy user prompt as system prompt",
        handler: async (args, ctx) => {
            if (args?.trim().toLowerCase() === "reset") {
                activeSkill = null;
                ctx.ui.setStatus("skill", "");
                ctx.ui.notify("System prompt reset to default", "info");
                return;
            }

            let prompts: UserPrompt[];
            try {
                prompts = await loadUserPrompts();
            } catch (e) {
                ctx.ui.notify(`Failed to load prompts from ${USER_PROMPTS_DIR}: ${e}`, "warning");
                return;
            }

            if (prompts.length === 0) {
                ctx.ui.notify("No user prompts found", "warning");
                return;
            }

            // Build selection list grouped by category
            const choices = prompts.map(
                (p) => `[${p.category}] ${p.title}`
            );

            const selected = await ctx.ui.select(
                "Load skill as system prompt",
                choices
            );

            if (selected === undefined || selected === null) return;

            const selectedIndex = typeof selected === "number"
                ? selected
                : choices.indexOf(selected as string);

            if (selectedIndex < 0 || selectedIndex >= prompts.length) return;

            activeSkill = prompts[selectedIndex];
            ctx.ui.setStatus("skill", `⚙ ${activeSkill.title}`);
            ctx.ui.notify(
                `Loaded skill: ${activeSkill.title} (${activeSkill.category})\nSystem prompt will be applied to next message.`,
                "info"
            );
        },
    });


    // ── 8. Apply skill as system prompt ───────────────────────────────────────
    //
    // When a skill is active, prepend it to the system prompt before the
    // agent starts processing.

    pi.on("before_agent_start", async (event, _ctx) => {
        if (!activeSkill) return;

        return {
            systemPrompt: activeSkill.body + "\n\n" + (event as any).systemPrompt,
        };
    });
}
