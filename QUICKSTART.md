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
If your host shell already has a supported provider API key exported, the
Docker CLI path forwards it into the container automatically. Today that covers:

- `ANTHROPIC_API_KEY`
- `OPENAI_API_KEY`
- `OPENROUTER_API_KEY`
- `OPEN_ROUTER_API_KEY`

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

## First Interactive Runtime Check

After `make docker-test` passes, the smallest real runtime interaction is to
open the CLI and inspect the live runtime surface:

```bash
make docker-cli
```

Then run:

```text
:status
:providers
:tools
:cas
```

What this proves:

- `:status` shows the current session/runtime state
- `:providers` shows registered provider names
- `:tools` shows the public tool surface
- `:cas` shows the configured content-addressed storage roots

This is a better first runtime check than stopping at `:help`, because it shows
real runtime inspection commands without requiring API keys or provider setup.

## Real Provider Check

If you want to try a real model after the no-key validation path, export a
provider key in your host shell first:

```bash
export ANTHROPIC_API_KEY="your-key-here"
make docker-cli
```

Then, inside the CLI, enter a normal user message such as:

```text
say hello in one sentence
```

The same passthrough pattern also works for `OPENAI_API_KEY`,
`OPENROUTER_API_KEY`, and `OPEN_ROUTER_API_KEY` when those providers are
configured locally.

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
5. Run `:status`, `:providers`, `:tools`, and `:cas`.
6. Use `:help` to inspect the wider interactive surface.
7. Read `ARCHITECTURE.md` for the staged model.

## Troubleshooting

### Docker permission denied

If Docker requires privilege on your host, rerun the commands with:

```bash
make DOCKER="sudo docker" docker-build
make DOCKER="sudo docker" docker-load
make DOCKER="sudo docker" docker-test
make DOCKER="sudo docker" docker-cli
```

### `docker-load` or `docker-test` fails because the image does not exist

Build the image first:

```bash
make docker-build
```

Then rerun:

```bash
make docker-load
make docker-test
```

### `docker-test` passes but you are not sure what to do next

Use the minimal runtime check:

```bash
make docker-cli
```

Then run:

```text
:status
:providers
:tools
:cas
```

That is the smallest real post-test interaction that proves the live runtime
surface is available.
