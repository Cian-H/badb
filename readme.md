# badb

A lightweight, agentic microharness.

badb is a small harness core designed to grow at runtime rather than
ship a fixed feature set. The intended workflow: when you need a new
capability, you ask an agent to

1. design a TDD contract for the feature (`spec.geis.scm`),
2. design a workflow that fulfills that contract, and
3. installs the resulting bundle into the harness

as a self-contained *geasa bundle* (contract spec + plugin + manifest),
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
├── channels.scm / manifest.scm / license.md
├── scripts/             CLI launcher, dev env, test & e2e runners, geis guards
├── src/
│   ├── srfi/srfi-253.scm          SRFI-253 contract helpers (kernel support)
│   ├── badb/                      microkernel + harness shell
│   │   ├── kernel/                digest, geis-gate, ipc, journal, registry, transaction
│   │   ├── spec.scm               declarative contract & property test engine
│   │   ├── spec-gate.scm          kernel-owned locked-spec verification gate
│   │   ├── geis/bundle.scm        geasa bundle discovery, validation, pack/install/replay
│   │   ├── main.scm               CLI entry point & mode dispatch
│   │   ├── capabilities/          filetree-mcp (stdio filetree MCP server)
│   │   ├── surfaces/cli.scm       CLI presentation helpers & ANSI color support
│   │   └── registry/journal/digest/transaction.scm   compat shims re-exporting kernel/*
│   └── geasa/<feature-id>/        self-contained contract bundles (the geasa):
│       manifest.scm + spec.geis.scm + plugin.scm (+ workflow/cassettes/prompts)
├── workflows/plugin-synthesis.sema   Sema synthesis workflow (kernel-referenced)
└── test/                unit, adversarial, flat geis specs & end-to-end suites
```

## Development

Everything runs inside the pinned Guix environment via `./scripts/devenv`.
The harness is written in [GNU Guile]. The pure microkernel lives under
`src/badb/kernel/` (`(badb kernel ...)`); all user-space capability code lives
in self-contained geasa bundles under `src/geasa/<feature-id>/` as
`(geasa <feature-id> plugin)`, verified by their `spec.geis.scm` contracts.

## Geasa contracts (bundles)

Every capability ships as a bundle under `src/geasa/<feature-id>/`:

```
<feature-id>/
├── manifest.scm      feature-id, version, plugin entry, sha256 pins
├── spec.geis.scm     the contract the kernel replays and enforces
├── plugin.scm        the implementation module (geasa <feature-id> plugin)
└── workflow.sema / cassettes/ / prompts/   (synthesized bundles only)
```

Manage them from the CLI:

```sh
./scripts/badb geis list                 # show locked + discovered bundles
./scripts/badb geis verify <fid>         # replay a locked flat spec in a
                                         # disposable kernel subprocess
./scripts/badb geis verify --all         # all flat specs + all src/geasa
                                         # bundles, replayed via the gate
./scripts/badb geis pack <fid>           # shareable tarball
./scripts/badb geis install <dir>        # lock + verify into the kernel gate
./scripts/badb geis replay <fid>         # re-run a synthesized bundle's
                                         # recorded workflow, check plugin hash
./scripts/badb synthesize <fid> ...      # run the Sema synthesis workflow
```

In the TUI, `/geis add <description>` drives the whole loop with an agent.
Bundles are searched under `src/geasa/` by default; override with
`BADB_GEIS_PATH` (colon-separated roots).

## Testing

Unit tests and contract replays run via:

```sh
./scripts/test                 # bootstrap suite, kernel invariants + all geasa contracts
./scripts/verify-geasa.sh      # geasa contracts only, in parallel
```

Kernel invariants (whitelist, epistemic purity, zero macros, zero user-space imports, public ABI surface, mutation bounds, density, and McCabe complexity) are formally verified inside Stage 1 via `test/test-kernel-design.scm`.

The end-to-end suite exercises CLI entrypoints, contract locking, transaction rollback, and interactive TUI scenarios via the [tui-test] headless terminal tool:

```sh
./scripts/devenv -- ./scripts/test-e2e                 # all integration & TUI scenarios
./scripts/devenv -- ./scripts/test-e2e --integration   # non-TUI integration suites only
./scripts/devenv -- ./scripts/test-e2e --tui           # TUI terminal scenarios only
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
