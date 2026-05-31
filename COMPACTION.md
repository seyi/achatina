# Compaction Architecture

## Purpose

Achatina keeps advanced local context-management behavior public because it is
part of the repository's technical story, not an accidental leftover.

The local runtime is not only a trivial executor. It also demonstrates how a
workflow engine can:

- monitor context pressure
- degrade gracefully before context failure
- preserve useful local artifacts
- emit inspectable summaries when context must be reduced

This file describes that local compaction architecture.

## What Problem It Solves

Long-running local sessions accumulate:

- user and assistant messages
- tool calls and tool results
- persisted artifact references
- memory-related context

Without any management, the session eventually becomes too large for the active
model context window.

Achatina therefore includes a local compaction path whose job is to reduce
context pressure while keeping execution inspectable and deterministic.

## Layers

### 1. Context Monitoring

The runtime continuously estimates context usage and compares it to configured
thresholds.

This layer answers:

- how full is the current effective context window?
- is the session only approaching pressure, or already beyond a compaction
  threshold?
- should the runtime do nothing, warn, microcompact, or escalate to a stronger
  local compaction path?

This logic is intentionally local and inspectable. It is not distributed
orchestration or managed-service scheduling logic.

### 2. Microcompaction

Microcompaction is the lightest-weight response to context pressure.

Its purpose is to trim or reduce lower-value local context without rebuilding
the full session history.

In practice, this is used for bounded local hygiene such as limiting retained
tool-result preview payloads while preserving the surrounding conversation and
artifact references.

Microcompaction exists so that many sessions can recover from moderate context
growth without requiring a heavier summarization boundary.

### 3. Compaction IR

When a stronger local compaction path is required, Achatina materializes an
explicit compaction representation rather than treating the operation as a black
box.

That representation is referred to here as **compaction IR**.

It captures structured information about:

- source of compaction
- session identity
- preserved context
- summarized or dropped context
- tool-result/artifact references
- provenance about how the compaction was derived

This keeps compaction auditable and reproducible in the same spirit as the rest
of the IR-first pipeline.

## Why Compaction IR Exists

Compaction IR is public because it reinforces several core Achatina principles:

- staged representations are preferable to hidden mutation
- runtime transitions should remain inspectable
- local execution should still produce artifacts that explain what happened

Instead of silently rewriting history, the runtime can produce a structured
compaction artifact and then render a deterministic boundary message from it.

That makes context reduction part of the observable local runtime contract.

## Relationship to CAS and Artifacts

Compaction can participate in the same artifact/provenance story as other
runtime stages.

In the public repo, this matters for:

- transcript inspection
- artifact identity
- compaction-boundary visibility
- deterministic rendering of the reduced context story

## Why This Is Still Public

The public repo intentionally includes:

- context monitoring
- microcompaction
- compaction IR

because these features help developers understand how Achatina handles
non-trivial local runtime pressure.

They make the project more attractive to systems developers evaluating:

- IR-first execution design
- artifact-backed runtime transitions
- practical context-management strategies in local agents

## Scope

This document describes **local runtime compaction architecture** as it exists
in this repository.

Those remain outside the public `achatina` repo boundary.
