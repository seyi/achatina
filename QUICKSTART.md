# Quickstart

## Prerequisites

The simplest path is Docker-based.

You need:

- Docker
- optional: `sudo docker` access on hosts where Docker requires privilege

## Build the Development Image

```bash
make docker-build
```

If Docker requires privilege:

```bash
make DOCKER="sudo docker" docker-build
```

## Load the System

```bash
make docker-load
```

Or:

```bash
make DOCKER="sudo docker" docker-load
```

## Run the Test Suite

```bash
make docker-test
```

Or:

```bash
make DOCKER="sudo docker" docker-test
```

## Start the CLI

```bash
make docker-cli
```

Or:

```bash
make DOCKER="sudo docker" docker-cli
```

This starts the current CLI entrypoint inside the containerized environment.

Once inside the CLI, use:

```text
:help
```

to inspect available commands.

## First Successful Run

The first guaranteed-success validation does not require API keys or provider
setup:

```bash
make docker-build
make docker-load
make docker-test
```

If `make docker-test` passes, you have already validated the public runtime
surface, IR pipeline, execution-plan lowering, and containerized development
path.

After that, you can explore the interactive CLI with:

```bash
make docker-cli
```

and inspect the available commands with:

```text
:help
```

## Current Reality

This repository is strongest today as:

- a runnable local runtime
- an IR and execution-plan implementation
- a CAS-backed artifact/provenance engine

The launch surface is intentionally focused on those capabilities.

## Recommended First Validation Steps

1. Build the image with `make docker-build`.
2. Load the system with `make docker-load`.
3. Run tests with `make docker-test`.
4. Start the CLI with `make docker-cli`.
5. Use `:help` to inspect the interactive surface.
6. Read `ARCHITECTURE.md` for the staged model.

## Notes

- Internal system and CLI identifiers may still use `claw-lisp` naming during
  the first public release.
- Public repo naming and internal package naming do not have to be fully
  synchronized on day one.
