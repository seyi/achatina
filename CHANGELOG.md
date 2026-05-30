# Changelog

All notable changes to this public repository will be documented in this file.

## v0.1.0 - 2026-05-30

First public release baseline for `achatina`.

Included in this release:

- IR-first workflow engine foundation
- semantic IR pipeline and staged execution-plan lowering
- CAS-backed artifacts and provenance handling
- locally runnable Common Lisp runtime subset
- containerized build, load, test, and CLI entrypoints
- migration helper for legacy CAS-related artifact flows

Public launch polish included:

- refreshed runtime-facing documentation
- explicit commercial licensing contact path
- security disclosure policy
- verified quickstart based on `docker-build`, `docker-load`, and `docker-test`

Notes:

- internal package and system identifiers still use `claw-lisp` naming in places
- this public release is focused on local runtime, IR, execution plans, and artifact handling
- distributed orchestration depth and broader commercial/planning material remain outside this public repo
