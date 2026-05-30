# Architecture

## Overview

Achatina is built around explicit staged representations rather than ad hoc
runtime mutation.

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

This public surface is intentionally centered on the local execution path rather
than distributed orchestration.

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

## Public Scope Boundary

This public release is centered on:

- IR
- execution-plan lowering
- artifact/provenance handling
- local execution

It does not attempt to include the full operational depth of private backend or
control-plane integrations.
