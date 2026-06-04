# Agent Loop Architecture

This document describes how the Achatina coding agent loop works. It is intended
for developers who want to understand, modify, or extend the runtime. For the
staged IR/runtime model, see `ARCHITECTURE.md`; for context-management behavior,
see `COMPACTION.md`.

---

## Overview

The coding agent loop is a **synchronous, turn-based tool-calling loop** that
drives an LLM through a bounded coding task (inspect → edit → verify → complete).
It runs inside `execute-provider-turn-loop` in `lisp/core/runtime.lisp` and
terminates when:

- The model responds with **no tool calls** (text-only response) — normal exit.
- A **completion trigger** fires (verification passes, model confirms, or max
  coding iterations).
- The **stagnation guard** fails closed (no file-modifying progress over N turns).
- The **iteration budget** is exhausted (graceful stop, not an error).

---

## State Machine

```
[IDLE]
  └─(user message submitted)─────────────▶ [STREAMING]

[STREAMING]
  ├─(response has tool_calls)─────────────▶ [TOOL_EXECUTION]
  └─(response is text-only / no tools)───▶ [HALTED] (return response)

[TOOL_EXECUTION]
  └─(all tools executed → results)────────▶ [REFLECTING]

[REFLECTING]  ← all loop-control decisions happen here
  ├─(completion trigger fires)────────────▶ [COMPLETED] (return)
  ├─(stagnation hard-limit hit)───────────▶ [FAILED] (error)
  ├─(iteration budget reached)────────────▶ [HALTED] (graceful return)
  └─(otherwise)───────────────────────────▶ [STREAMING] (next turn)
```

The `[REFLECTING]` step runs `assess-loop-progress` + `maybe-auto-complete` and
decides whether to continue, steer, or stop.

---

## Key Modules and Their Responsibilities

### 1. `lisp/core/runtime.lisp` — The Loop Driver

| Function | Role |
|---|---|
| `execute-provider-turn-loop` | The outer loop: streams a turn, executes tools, reflects, repeats or stops. |
| `assess-loop-progress` | **Single progress assessment**. Classifies the turn by its *results* (not its requests). Returns `(values stall-count nudge-kind)`. |
| `make-tool-result-message` | Builds the user message sent back to the model containing tool results + optional reflection/nudge text. |
| `make-differential-reflection-text` | When any tool fails: reports which failed, names the successes, says "don't resend." |
| `read-only-tool-names` | Returns the names of tools classified `:read` by the capability system; used to build the exclusion list. |
| `provider-tool-descriptors` | Returns the tool definitions sent to the provider, optionally excluding read-only tools after stagnation. |
| `turn-verification-status` | Classifies this turn's shell-command results as `:passed` / `:failed` / `nil`. |
| `register-tool` | Registers a tool AND propagates its capability into the classification registry. |

**Constants:**

| Name | Value | Purpose |
|---|---|---|
| `+max-provider-tool-iterations+` | 12 | Budget cap. Graceful return, not error. |
| `+max-stagnant-read-only-tool-iterations+` | 2 | Hard-limit: stall count that triggers fail-closed. |
| `+read-only-tool-loop-nudge-threshold+` | 2 | Stall count that triggers the nudge + read-tool suppression. |

### 2. `lisp/core/tool-capability.lisp` — Classification Source of Truth

Every tool declares a **capability plist** once:

```lisp
'(:class :read|:write|:exec   ;; loop progress class
  :valid-phases (:inspect :edit :verify :complete)
  :mutates-fs nil|t)           ;; does it modify files?
```

| Tool | `:class` | `:valid-phases` | `:mutates-fs` |
|---|---|---|---|
| `file-read` | `:read` | `(:inspect :verify)` | `nil` |
| `grep` | `:read` | `(:inspect)` | `nil` |
| `glob` | `:read` | `(:inspect)` | `nil` |
| `file-write` | `:write` | `(:edit)` | `t` |
| `file-replace` | `:write` | `(:edit)` | `t` |
| `shell-command` | `:exec` | `(:edit :verify)` | `nil` |
| `echo` | default | all | `nil` |

**Three consumers** resolve classification through this one module:

1. `runtime.lisp` (`read-only-tool-call-p` / `write-tool-call-p`) → uses `tool-name-read-only-p` / `tool-name-mutation-p`
2. `tool-envelope.lisp` (`envelope-is-read-only-p` / `-mutation-p`) → same
3. `phase-progression.lisp` (`classify-tool-calls-for-phase`) → delegates to the envelope

An agreement test (`test-tool-classification-agreement-across-subsystems`) pins that all three paths always agree.

### 3. `lisp/core/phases.lisp` — Phase State Machine

Coding sessions progress through phases:

```
:inspect  →  :edit  →  :verify  →  :complete
              ↑____________↲  (retry after failed verify)
```

Valid transitions are enforced by `valid-transition-p`. Each phase has a counter (number of tool turns spent in it). The runtime tracks: current phase, phase history, per-phase counters, turn count, and last-verify-result.

### 4. `lisp/core/phase-progression.lisp` — Transition Policy

`apply-progression-policy` runs after tool execution and recommends a phase transition based on tool patterns:

- In `:inspect` with mutation tools → advance to `:edit`
- In `:inspect` too long with read-only tools → nudge to `:edit`
- In `:edit` too long with read-only tools → advance to `:verify`

### 5. `lisp/core/completion.lisp` — Task Completion

`maybe-auto-complete` checks three triggers after each turn:

1. **Verify passed** — phase is `:verify` AND `last-verify-result` is `t` (set by `turn-verification-status` when all shell-commands in the turn succeed).
2. **Model confirmed** — phase is `:verify` AND the response is text-only (no tools).
3. **Max coding iterations** — phase is `:verify` AND turn count ≥ 20.

When any trigger fires, the session transitions to `:complete` and the loop returns.

### 6. `lisp/core/system-prompt.lisp` — Model-Specific Prompts

`build-system-prompt` selects the system prompt based on `:model`:

- **`:moonshot` / `:qwen`** → `+base-system-prompt-directive+` (short, 3-turn budget, strict rules: don't re-read, use `file-write` on failed `file-replace`, use the specified verification command)
- **`:anthropic` / `:openai` / `:default`** → `+base-system-prompt+` (conversational, multi-section)

`model-family` dispatches on substrings in the model ID string.

### 7. `lisp/providers/http-utils.lisp` — Provider Serialization

`conversation->chat-json` serializes the conversation for OpenRouter/OpenAI-compatible APIs:

- Assistant messages with `tool_calls` → `role:"assistant"` with a `tool_calls` array
- Tool result messages → expanded to one `role:"tool"` message per result (matching `tool_call_id`)
- Text blocks after tool results (reflection, nudge) → trailing `role:"user"` message

This is the layer where multi-turn context is threaded to the model. If it drops
tool results, the model starts cold every turn and never completes — historically
the single most important loop bug.

---

## How a Turn Flows (step by step)

```
execute-provider-turn-loop iteration N:
│
├─ 1. Build tool list
│     Read stagnant-count from session state.
│     If stagnant-count >= nudge threshold: exclude read-only tools.
│     Call provider-tool-descriptors(runtime, :exclude-names ...).
│
├─ 2. Context management
│     Check context window usage (proactive compaction if needed).
│     Inject durable memory context.
│
├─ 3. Stream the turn
│     Call stream-turn(provider, conversation, :tools ..., :system ...).
│     Normalize response → extract tool-calls.
│
├─ 4. Append assistant message
│     Record the response in conversation history.
│     If no tool calls → return response (text-only exit).
│
├─ 5. Execute tools
│     For each tool-call: validate input, execute, record result.
│     Errors → formatted as "[error] Tool X failed: ..." result.
│
├─ 6. Record verification status
│     turn-verification-status(new-results) → :passed/:failed/nil.
│     Set last-verify-result on session.
│
├─ 7. Assess loop progress [REFLECTING step]
│     assess-loop-progress(session, tool-calls, new-results):
│       • Successful write in results? → reset stall to 0 (progress)
│       • Failed write in results? → advance stall to threshold + :write-failed
│       • No write at all? → increment stall + :stall if at threshold
│
├─ 8. Build and send tool results message
│     make-tool-result-message(results,
│       :reflection-text  → differential feedback (what failed, what succeeded)
│       :progression-nudge → :write-failed nudge OR :stall nudge OR nil)
│     Append to conversation.
│
├─ 9. Phase progression
│     apply-progression-policy → may transition :inspect→:edit or :edit→:verify.
│     If still in :edit after tools → fallback to :verify.
│
├─ 10. Completion check
│      maybe-auto-complete(session, response):
│        If verify-passed → transition to :complete → return.
│
└─ 11. Loop back to step 1 (next iteration)

If iteration budget reached: emit tool_loop_budget_reached, return last-response.
```

---

## Tool Set

The default tools registered by `register-default-tools`:

| Name | Class | Purpose |
|---|---|---|
| `file-read` | read | Read a text file by path |
| `grep` | read | Search for a pattern in files |
| `glob` | read | Find files matching a pattern |
| `file-write` | write | Write complete file content |
| `file-replace` | write | Replace exact substring in a file |
| `shell-command` | exec | Execute a shell command (verification, build) |
| `echo` | exec | Return provided text (testing, acknowledgement) |

---

## Stagnation Guard (`assess-loop-progress`)

The single progress rule, run **after** tool execution:

| Turn outcome | Action | Rationale |
|---|---|---|
| Successful write present | Reset stall counter to 0 | Genuine progress earned |
| Failed write (no successful write) | Advance counter to nudge threshold | Model has the file in context; must rewrite, not re-read |
| No write at all (reads, shell, etc.) | Increment counter | ANY no-write turn is a stall, even with interleaved shell |

**At nudge threshold (2):** suppress read-only tools from the next provider call + inject a progression nudge telling the model to write.

**At hard limit (3):** fail closed with a clear error message.

This design ensures interleaved `shell-command` test runs (e.g., `[file-read, file-read, shell-command]`) are correctly classified as stalls — previously they dodged detection because they weren't *purely* read-only.

---

## Differential Reflection

After tool execution, if any tool failed, `make-differential-reflection-text` injects:

```
1 tool call(s) failed:
  - file-replace: substring not found
1 call(s) succeeded (file-read). Do not re-call these — only fix the failure(s) above.
```

This tells the model exactly what worked and what didn't, so it doesn't re-send successful calls.

---

## Provider Serialization (Critical Path)

The OpenRouter/chat-completions path (`message->chat-completion-blocks`) expands
internal messages into the wire format models consume:

```
Internal message (role:user, content: [tool-result-block ...])
    ↓ expands to ↓
[
  {role: "tool", tool_call_id: "call_1", content: "file body..."},
  {role: "tool", tool_call_id: "call_2", content: "test output..."},
  {role: "user", content: "1 tool call failed: ..."}  ← nudge/reflection
]
```

Without this expansion, models receive no tool results and start cold every turn.
The Anthropic path (`content-block->anthropic-block`) handles `tool_result` natively.

---

## Files Reference

| File | Purpose |
|---|---|
| `lisp/core/runtime.lisp` | Loop driver, stagnation guard, tool execution |
| `lisp/core/tool-capability.lisp` | Tool classification source of truth |
| `lisp/core/tool-envelope.lisp` | Normalized result envelope + classification predicates |
| `lisp/core/phases.lisp` | Phase state, transitions, counters |
| `lisp/core/phase-progression.lisp` | Transition policy recommendations |
| `lisp/core/completion.lisp` | Task completion triggers |
| `lisp/core/system-prompt.lisp` | Model-specific prompt dispatch |
| `lisp/providers/http-utils.lisp` | Message serialization (chat/Anthropic) |
| `lisp/tools/*.lisp` | Individual tool implementations + capability declarations |

---

## Invariants for Contributors

1. **One classification.** Tool class is declared once per tool via `tool-capability`. Never hardcode tool-name lists elsewhere. The agreement test catches violations.
2. **Progress = successful write.** Only a successful file mutation resets the stall counter. Reads, greps, observational shell commands, and failed writes do not count.
3. **Verification = shell-command success.** A turn where all shell-commands succeed sets `last-verify-result` to `t`, which can trigger completion.
4. **Provider threading is load-bearing.** Tool results MUST be serialized as `role:"tool"` messages (chat path) or `tool_result` blocks (Anthropic path). Dropping them makes the model amnesiac.
5. **The iteration budget is a graceful stop.** Only the stagnation guard produces errors. Reaching the budget means the model was making progress but verbose.
6. **Model-specific prompts for weaker models.** Moonshot/Qwen get the directive prompt (explicit turn budget, strict rules). Anthropic/OpenAI/default get the conversational prompt.
