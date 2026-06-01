# Achatina

Achatina is a locally runnable coding-agent runtime for single-agent execution,
built around explicit IR stages, inspectable execution plans, and CAS-backed
artifacts.

It is designed to turn authored behavior into validated staged
representations, lower them into deterministic execution plans, and execute
those plans through a local runtime you can actually inspect.

## What Achatina Provides

- a locally runnable single-agent coding runtime
- surface-form compilation into semantic IR
- staged validation and optimization
- inspectable deterministic execution-plan lowering
- CAS-backed artifact persistence and provenance
- local execution of the current public runtime subset

## Naming Note

Some internal identifiers, CLI/runtime output, and local state paths still use
`claw-lisp` naming for implementation continuity. In the public repo, those
names refer to Achatina internals rather than a separate product.

## Core Pipeline

```text
surface form
  -> semantic IR
  -> validated IR
  -> optimized IR
  -> execution plan
  -> local runtime
```

Artifacts produced along the way can be persisted into content-addressed
storage so that execution history is inspectable and reproducible.

## Current Scope

The current repository includes:

- the Common Lisp runtime
- the IR pipeline
- execution-plan generation
- local execution
- artifact and provenance handling

## Why IR Matters

Achatina treats IR as the contract between authoring and execution.

That means multiple authoring surfaces can target the same internal model
without turning each frontend into its own runtime:

- Lisp-like surface forms
- constrained tabular frontends
- future visual or imported definitions

## Repository Status

This repository is source-available and intended to be runnable, inspectable,
and useful for evaluation and learning.

Current emphasis:

- coding-agent runtime behavior
- explicit IR stages and execution plans
- CAS-backed provenance and artifacts
- local context-management and compaction architecture

## Quick Start

See:

- `QUICKSTART.md`
- `ARCHITECTURE.md`
- `COMPACTION.md`
- `LICENSING.md`

## What To Read First

Use this path by intent:

- if you want to understand what the project is: `README.md`
- if you want the fastest successful local evaluation path: `QUICKSTART.md`
- if you want the staged runtime/IR model: `ARCHITECTURE.md`
- if you want the context-management story: `COMPACTION.md`
- if you want licensing details: `LICENSING.md`

## License

Achatina is source-available under **Business Source License 1.1**.

- evaluation, research, education, and non-production use are allowed
- production or commercial use requires a separate commercial license

See `LICENSE` and `LICENSING.md`.

For production or commercial licensing inquiries:

- email `seyiakadri@gmail.com`
