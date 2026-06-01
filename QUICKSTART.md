# Quickstart

## Prerequisites

The simplest path is Docker-based.

You need:

- Docker
- optional: `sudo docker` access on hosts where Docker requires privilege

You may still see `claw-lisp` in internal system names, CLI/runtime output,
and local state paths such as `.claw-lisp/`. Those names are retained
implementation identifiers inside Achatina rather than a separate product.

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
- `OPEN_ROUTER_API_KEY` (accepted as an alias for `OPENROUTER_API_KEY`)

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

Expected outcome:

- `:status` should show the current provider/model and zero or more messages
- `:providers` should list the public provider set included in this build
- `:tools` should list the baseline local tools
- `:cas` should show local CAS object/ref roots under `.claw-lisp/`

If those commands work, the runtime is loaded and the CLI surface is available.

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

Expected outcome with a valid key:

- the CLI prints `[Thinking...`
- the provider returns a short assistant response

Expected outcome without a key:

- the CLI should fail clearly with a message such as:

```text
Anthropic provider registered, but credentials are not configured.
```

The same passthrough pattern also works for `OPENAI_API_KEY`,
`OPENROUTER_API_KEY`, and `OPEN_ROUTER_API_KEY` (alias) when those providers are
configured locally.

## Supported Providers In This Build

The current public build is focused on:

- `anthropic`
- `openrouter`
- `mock`

Notes:

- `mock` is the safest no-network provider for first local runtime checks
- real-provider success depends on valid API credentials in the host shell
- Bedrock is intentionally not included in the current public build

## First Productive Task

Once the runtime loads, use this path instead of stopping at inspection:

1. Start the CLI with `make docker-cli`.
2. Run `:status`, `:providers`, and `:models`.
3. If you have no API key yet, switch to `mock` with `:provider mock`.
4. Enter a normal message such as `say hello in one sentence`.
5. If you do have an API key, stay on `anthropic` and try the same prompt.

Why this path is useful:

- it proves the CLI handles both command mode and user-turn mode
- it gives one successful no-network path with `mock`
- it gives one clear success/failure path for real provider setup

## Provider And Model Naming

The CLI accepts both canonical model names and some short aliases.

Examples:

- canonical: `claude-sonnet-4-6`
- alias: `claude-sonnet`

Recommendations:

- use `:models` to see known canonical names for the current provider
- prefer canonical names in bug reports and examples
- if provider/model selection fails, check provider compatibility before assuming credentials are wrong

## Current Reality

This repository is strongest today as:

- a locally runnable coding-agent runtime
- an explicit IR and execution-plan implementation
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

### `docker-cli` starts but a real model message fails immediately

Check whether your host shell exported a supported provider key before launching
the CLI:

```bash
export ANTHROPIC_API_KEY="your-key-here"
make docker-cli
```

Then verify inside the CLI:

```text
:status
:providers
:models
```

If you want a no-network success path first, switch to:

```text
:provider mock
```
