# badb

A lightweight, agentic microharness.

badb is a small harness core designed to grow at runtime rather than
ship a fixed feature set. The intended workflow: when you need a new
capability, you ask an agent to

1. design a TDD contract for the feature,
2. design a workflow that fulfills that contract, and
3. installs the resulting module into the harness

either system-wide or scoped to a single project.

## About the name

In Celtic mythology, **Badb** (pronounced "bave", like "cave" with a
"b") is a shapeshifting deity associated with cunning. She granted
heroes great powers, but only through rigid *geasa* (contracts);
power that was lost if the contract was broken. That is how this
harness is meant to work: the harness can morph into the shape you
require, and grants powerful capabilities through unbreakable contracts.

**Status:** alpha — the harness has evolved past its initial placeholder phase
and now ships with a functional TUI CLI.

## Quickstart

The development environment is fully reproducible via [GNU Guix],
pinned to a specific channel revision. Drop into it with:

```sh
./scripts/devenv
```

or, explicitly:

```sh
guix time-machine -C channels.scm -- shell -m manifest.scm
```

Run the CLI:

```sh
./scripts/devenv -- ./scripts/badb            # starts the TUI
./scripts/devenv -- ./scripts/badb --help
./scripts/devenv -- ./scripts/badb --version
```

Colored output is enabled on terminals and respects the `NO_COLOR`
and `FORCE_COLOR` conventions.

## Layout

```text
├── channels.scm         Guix channel pin (reproducibility)
├── manifest.scm          Guix package manifest for the dev environment
├── license.md            LGPLv3
├── scripts/
│   ├── badb              CLI launcher (Guile)
│   ├── devenv            one-command dev environment
│   ├── test              unit test runner
│   └── test-e2e          headless TUI end-to-end test runner
├── src/
│   ├── srfi/
│   │   └── srfi-253.scm  Data Type-Checking and Contract Helpers (SRFI-253)
│   └── badb/
│       ├── cli.scm           CLI presentation helpers & ANSI color support
│       ├── config.scm        S-expression configuration & provider presets
│       ├── context.scm       token estimation & conversation compaction
│       ├── digest.scm        pure-Guile SHA-256 (FIPS 180-4) implementation
│       ├── eval.scm          golden task evaluation harness
│       ├── filetree-mcp.scm  built-in filetree & symbol outline MCP server
│       ├── guardrail.scm     tool-call risk classification & safety policy
│       ├── history-tools.scm JSONL conversation history search & summarization
│       ├── journal.scm       append-only provenance journal
│       ├── log.scm           rotating debug & error logging
│       ├── main.scm          CLI entry point & mode dispatch
│       ├── mcp-server.scm    JSON-RPC 2.0 stdio MCP server framework
│       ├── mcp.scm           MCP client & server lifecycle manager
│       ├── model.scm         OpenAI-compatible LLM HTTP/SSE client
│       ├── nc.scm            libnotcurses FFI terminal backend
│       ├── prompt.scm        system prompt composer & project rule discovery
│       ├── react.scm         autonomous ReAct tool-calling loop & steering
│       ├── registry.scm      mutable runtime feature registry
│       ├── rollback.scm      workspace & registry checkpoint rollback manager
│       ├── spec-gate.scm     kernel-owned locked-spec verification gate
│       ├── spec-runner.scm   out-of-process spec execution worker
│       ├── spec.scm          declarative contract & property-based test engine
│       ├── theme.scm         semantic color theming & built-in palettes
│       ├── transaction.scm   contract-gated atomic feature transactions
│       ├── tui.scm           interactive full-screen terminal UI
│       ├── util.scm          shared string, alist, filesystem & JSON helpers
│       └── verify.scm        neuro-symbolic syntax & test verification gate
└── test/                 unit & end-to-end test suites
```

## Development

Everything runs inside the pinned Guix environment via `./scripts/devenv`.
The harness is written in [GNU Guile]; new modules live under
`src/badb/` following the `(badb ...)` module naming convention.

## Testing

Unit tests run anywhere guile is available:

```sh
./scripts/test
```

The TUI has an end-to-end suite that drives the real interface through
the [tui-test] headless terminal tool (packaged in `guix/badb/packages/`):

```sh
./scripts/devenv -- ./scripts/test-e2e
```

Each scenario boots the TUI in an isolated terminal session and asserts
on what actually renders: startup, shell and scheme modes, the command
palette, interactive menus, message selection/collapse, scrollback,
resize, exit, and a chat round-trip through the ReAct loop. The chat
scenario is fully hermetic: it starts a static fake model endpoint
(`test/e2e/fake-endpoint.scm`) and asserts on its canned response. To
run it against a real model server instead:

```sh
E2E_CHAT_ENDPOINT=http://127.0.0.1:1234/v1 \
E2E_CHAT_MODEL=<a loaded model id> \
./scripts/devenv -- ./scripts/test-e2e
```

[tui-test]: https://github.com/microsoft/tui-test


## License

LGPLv3 — see [license.md](license.md).

[GNU Guix]: https://guix.gnu.org
[GNU Guile]: https://www.gnu.org/software/guile/
