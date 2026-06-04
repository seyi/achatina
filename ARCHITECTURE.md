# Architecture

## Overview

Achatina is a locally runnable coding-agent runtime built around explicit IR
stages, inspectable execution plans, and CAS-backed artifacts.

It is built around explicit staged representations rather than ad hoc runtime
mutation.

The key idea is:

```text
authoring surface -> semantic understanding -> validated IR
-> deterministic execution plan -> execution
```

Each transition is explicit, inspectable, and suited to artifact persistence.

## Stages

### 1. Surface Form

The surface form is the authored workflow representation accepted by the
compiler entrypoint.

This layer exists so that authoring can evolve independently from execution.

### 2. Semantic IR

The semantic stage captures the workflow in a normalized graph-like
representation.

This is where step kinds, dependencies, memory intent, and policy-relevant
structure become explicit.

### 3. Validation and Optimization

The IR pipeline validates structural correctness and applies deterministic
transformations before execution planning.

This stage exists to keep execution consumers simpler and more predictable.

### 4. Execution Plan

Validated semantic IR is lowered into a deterministic execution-plan artifact.

The current public plan vocabulary includes the core local/runtime subset that
the repository can lower and reason about explicitly.

### 5. Local Runtime

The local runtime executes the current supported subset directly.

## CAS-Backed Artifacts

Achatina can materialize staged artifacts into content-addressed storage.

That supports:

- provenance
- replay-oriented inspection
- stable artifact identity
- auditable transitions between stages

## Design Principles

### Stable identity, changing representation

Workflow identity should remain stable while the representation becomes more
explicit through staged lowering.

### Explicit boundaries

Execution backends should consume validated execution plans rather than rely on
mutable in-memory state as the primary contract.

### Frontend flexibility

Multiple authoring frontends can target the same IR pipeline as long as they
compile into the same staged representations.

## Local Context Management

The public runtime also includes a deliberate local context-management layer:

- context monitoring
- microcompaction
- explicit compaction IR

This is part of the public technical story because it shows how Achatina
handles non-trivial local runtime pressure without hiding the transition.

See:

- `COMPACTION.md`

## Coding Agent Loop

The runtime drives an LLM through a bounded coding task (inspect → edit → verify
→ complete) using a synchronous, turn-based tool-calling loop with an explicit
stagnation guard, capability-based tool classification, and completion triggers.

See:

- `AGENT_LOOP.md`

## Repository Scope

The current repository centers on:

- IR
- execution-plan lowering
- artifact/provenance handling
- local execution
