# Achatina Lisp Runtime

This directory contains the Common Lisp implementation for Achatina's public
runtime surface.

It is the core of the current public release:

- IR and staged representation handling
- CAS-backed artifact and provenance support
- deterministic execution-plan lowering
- local execution of the current supported plan subset
- CLI and local tool/runtime integration

## Layout

- `packages.lisp`
  - package definitions and exports
- `config.lisp`
  - runtime configuration
- `core/`
  - runtime domain model, orchestration, prompts, compaction, artifacts
- `ir/`
  - schema, validation, semantic expansion, preparation, optimization,
    execution-plan lowering, and local execution
- `storage/`
  - transcripts, CAS, refs, session memory, durable memory
- `providers/`
  - provider adapters and transport helpers
- `tools/`
  - local tool implementations
- `tests/`
  - runtime, IR, CAS, and storage coverage
- `cli/`
  - CLI entrypoint
- `cas/`
  - CAS support helpers

## Current Public Focus

The public repo is strongest today as:

- a locally runnable coding-agent runtime
- an explicit IR-stage and execution-plan implementation
- a CAS-backed artifact/provenance implementation
- a deterministic execution-plan runtime boundary

This directory is not just an early scaffold. It is the active implementation
surface used by the public `docker-load` and `docker-test` workflows.

For the public explanation of the local compaction/context-management story,
see:

- `../COMPACTION.md`

## Build and Test

From the repo root:

- `make docker-build`
- `make docker-load`
- `make docker-test`
- `make docker-achatina-cli`
- compatibility: `make docker-cli`

If Docker requires privilege escalation:

- `make DOCKER="sudo docker" docker-build`
- `make DOCKER="sudo docker" docker-load`
- `make DOCKER="sudo docker" docker-test`
- `make DOCKER="sudo docker" docker-achatina-cli`
- compatibility: `make DOCKER="sudo docker" docker-cli`

## CLI Notes

The CLI supports:

- interactive use
- non-interactive runner mode
- direct local tool execution

From the repo root, the simplest entrypoint is:

- `make docker-achatina-cli`
- compatibility: `make docker-cli`

Then use:

- `:help`

for available commands.

## Artifacts and Local State

Local runtime state is written under `.claw-lisp/`, including:

- transcripts
- session memory
- durable memory notes
- CAS objects and refs
- persisted tool-result artifacts

These are local runtime byproducts, not part of the committed source tree.

## Scope

This directory is focused on:

- local execution
- IR and execution-plan semantics
- artifact persistence and provenance
