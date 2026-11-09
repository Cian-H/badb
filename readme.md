# badb

A lightweight, agentic microharness.

badb is a small, contract-governed harness core designed to grow at runtime rather than ship a fixed feature set. When you need a new capability, you or an agent:

1. designs a TDD contract for the feature (`spec.geis.scm`),
2. implements a plugin that fulfills that contract (`plugin.scm`), and
3. installs and locks the resulting bundle into the harness gate

as a self-contained *geasa bundle* (contract spec + plugin + manifest), either system-wide or scoped to a project.

## About the name

In Celtic mythology, **Badb** (pronounced "bave", like "cave" with a "b") is a shapeshifting deity associated with cunning. She granted heroes great powers, but only through rigid *geasa* (contracts); power that was lost if the contract was broken.

That is how this harness works: the harness morphs into whatever shape you require, granting powerful agentic capabilities governed by verified contracts.

**Status:** Alpha — ships with a pure microkernel, 30+ contract-verified capability and runtime geasa, and a full-featured interactive terminal user interface (TUI).

---

## Table of Contents

- [Quickstart](#quickstart)
- [Architecture & Core Concepts](#architecture--core-concepts)
  - [The Microkernel](#the-microkernel)
  - [Geasa Bundles](#geasa-bundles)
  - [Project Infrastructure (`.badb/`)](#project-infrastructure-badb)
- [CLI Reference](#cli-reference)
  - [Global Commands](#global-commands)
  - [`badb geis` Subcommands](#badb-geis-subcommands)
  - [`badb ctx` Subcommands](#badb-ctx-subcommands)
- [Interactive TUI Guide](#interactive-tui-guide)
  - [Modes & Navigation](#modes--navigation)
  - [Slash Commands](#slash-commands)
  - [Dynamic Geis Synthesis Workflow](#dynamic-geis-synthesis-workflow)
- [Model & MCP Configuration](#model--mcp-configuration)
  - [Model Providers & Endpoints](#model-providers--endpoints)
  - [MCP Server Integration](#mcp-server-integration)
- [Testing & Quality Verification](#testing--quality-verification)
- [Project Layout](#project-layout)
- [License](#license)

---

## Quickstart

The development environment is fully reproducible via [GNU Guix], pinned to a specific channel revision.

Drop into the shell:

```sh
./scripts/devenv
```

Or invoke the Guix time-machine directly:

```sh
guix time-machine -C channels.scm -- shell -m manifest.scm
```

### Launching badb

```sh
# Start the interactive TUI (default)
./scripts/devenv -- ./scripts/badb

# View CLI options
./scripts/devenv -- ./scripts/badb --help

# Initialize badb in your current repository
./scripts/devenv -- ./scripts/badb init
```

Terminal output respects standard `NO_COLOR` and `FORCE_COLOR` environment variables.

---

## Architecture & Core Concepts

badb is partitioned into a zero-macro, mutation-bounded microkernel and an extensible user space composed of self-contained *geasa*.

### The Microkernel

The core microkernel lives under `src/badb/kernel/` (`(badb kernel ...)`):

- **`registry.scm`**: In-memory registry tracking features, active tools, system components, and metadata.
- **`journal.scm`**: Append-only provenance log (`.badb/journal.jsonl`) recording iterations, checkpoints, and execution history.
- **`transaction.scm`**: Transactional isolation and rollback machinery.
- **`digest.scm`**: Cryptographic content digests and manifest pinning (`sha256`).
- **`geis-gate.scm`**: Kernel-owned verification gate that re適lays contract specifications (`spec.geis.scm`) inside isolated subprocesses before locking capabilities into runtime.
- **`ipc.scm`**: Safe inter-process communication primitives.

Formal kernel invariants (zero user-space imports, epistemic purity, McCabe complexity limits, whitelist containment) are continuously verified in the test suite (`test/test-kernel-design.scm`).

### Geasa Bundles

Every capability, surface, runtime, or persona is encapsulated as a bundle under `src/geasa/<feature-id>/` (or `.badb/geasa/<feature-id>/`):

```text
<feature-id>/
├── manifest.scm      # Feature ID, version, entrypoints, and sha256 checksums
├── spec.geis.scm     # Executable contract specifications (property tests & invariants)
├── plugin.scm        # Implementation module: (geasa <feature-id> plugin)
└── workflow.sema     # Optional: Sema workflow, prompts, and cassettes for synthesized bundles
```

When badb starts or loads a bundle, `geis-gate` executes the bundle's `spec.geis.scm`. If any contract assertion fails, the bundle is rejected.

Bundles are discovered in `BADB_GEIS_PATH` (defaults to `.badb/geasa:src/geasa`).

### Project Infrastructure (`.badb/`)

Running `./scripts/badb init` or `onboard` establishes project-scoped harness state in `.badb/`:

- `.badb/geasa/`: Local geasa bundles scoped to the repository.
- `.badb/workflows/`: Declarative workflows (e.g. `verify.sema`).
- `.badb/memories/`: Persistent project memory (e.g. `architecture.md`, `symbols.md`).
- `.badb/rules.scm`: Project-specific constraints and prompt additions.
- `.badb/journal.jsonl`: Checkpoint and audit log.

---

## CLI Reference

### Global Commands

```sh
# Launch the interactive TUI
./scripts/badb [tui]

# Initialize project geasa, workflows & memories in current workspace
./scripts/badb init

# Onboard project toolchain & synthesize baseline geasa
./scripts/badb onboard [--workspace <dir>]

# Run the test suite
./scripts/badb test

# Run the built-in stdio filetree MCP server
./scripts/badb mcp-filetree

# Run the Sema plugin synthesis workflow
./scripts/badb synthesize <feature-id> [options]

# Inspect, verify, or manage geasa bundles
./scripts/badb geis <subcommand>

# Context index token virtualization statistics
./scripts/badb ctx stats

# Help and version flags
./scripts/badb -h, --help
./scripts/badb -V, --version
```

### `badb geis` Subcommands

The `geis` CLI manages contract specifications, verification, packaging, and distillation:

| Subcommand | Description |
| :--- | :--- |
| `badb geis list` | List all discovered bundles along with lock and verification status. |
| `badb geis status [fid]` | Show status and manifest details for a specific bundle or all bundles. |
| `badb geis verify <fid>` | Replay a bundle's contract spec inside a disposable kernel subprocess. |
| `badb geis verify --all` | Run parallel verification across all registered geasa bundles. |
| `badb geis pack <fid>` | Create a distributable tarball (`.tar.gz`) of the bundle with hashes. |
| `badb geis install <dir>` | Lock and verify a bundle directory into the kernel gate. |
| `badb geis pin <dir...>` | Recompute and update `sha256` content pins in `manifest.scm`. |
| `badb geis replay <fid>` | Replay a synthesized bundle's recorded workflow and verify hash matches. |
| `badb geis distill [--analyze-only]` | Distill recent agent execution trajectories into a reusable Geis bundle. |
| `badb geis onboard [--workspace <dir>]` | Inspect workspace toolchains and synthesize baseline geasa. |

### `badb ctx` Subcommands

```sh
# Show BM25 context index stats and token virtualization savings
./scripts/badb ctx stats
```

---

## Interactive TUI Guide

The terminal interface provides an interactive coding and steering environment for agentic ReAct loops.

### Modes & Navigation

- **Input Prompt:** Type regular messages to talk to the agent.
- **Shell Commands:** Prefix input with `!` (or use `/sh <cmd>`) to execute local shell commands directly.
- **Scheme Evaluation:** Prefix with `,` or enter an s-expression `(...)` (or use `/eval <expr>`) to evaluate Guile Scheme in the running harness.
- **Message Selection:** Use `Ctrl + Up` and `Ctrl + Down` to navigate message history blocks.
- **Fold/Unfold:** Press `Ctrl + E` to collapse or expand the selected message block.
- **Interactive Steering:** While an agent loop is running, type text and press `Enter` to queue steering instructions.
- **Interrupt:** Press `Ctrl + I` (or `/interrupt`) to pause the agent at the next iteration boundary.
- **Menus:** Commands like `/model`, `/endpoint`, or `/config` open interactive navigation menus (`Up`/`Down` to navigate, `Enter` to select, `Esc` to cancel).

### Slash Commands

The TUI provides built-in commands for managing the session, models, tools, and contracts:

#### Model & Endpoint
- `/model [NAME]` — Switch active model (opens selection menu if name is omitted).
- `/models` — Query and list available models from the active endpoint.
- `/endpoint [URL]` — Switch API base URL (opens selection menu if omitted).
- `/provider [NAME]` — Switch or inspect provider presets (e.g. `openai`, `anthropic`, `local`).

#### Environment & MCP
- `/mcp` — Manage Model Context Protocol servers:
  - `/mcp list` — Show configured MCP servers and tool counts.
  - `/mcp start <name|all>` — Start MCP server processes.
  - `/mcp stop <name|all>` — Stop running MCP servers.
  - `/mcp restart <name|all>` — Restart servers.
  - `/mcp tools [name]` — Inspect available MCP tools.
  - `/mcp add <name> <cmd> [args...]` — Register a new MCP server.
  - `/mcp remove <name>` — Remove an MCP server configuration.

#### Safety & State
- `/guardrail [LEVEL]` — Set execution guardrail policy (`disabled`, `permissive`, `cautious`, `strict`, `paranoid`).
- `/checkpoint [DESC]` — Take a snapshot of the current workspace state.
- `/rollback [ID]` — Revert workspace to latest or specified checkpoint.
- `/compact` — Compact conversation history to reduce context window usage.
- `/resume` — Resume a saved chat session from disk.
- `/dump [PATH]` — Export current conversation transcript to a text file.
- `/clear` — Clear the conversation buffer.

#### Context & Knowledge
- `/rules` — View discovered project rules, conventions, and filetree structure.
- `/context` — Display token usage breakdown and context metrics.
- `/ctx [stats|query]` — Query the BM25 context index or show token virtualization savings.
- `/skills` — List active agent skills.
- `/memories [TOPIC]` — Read and query persistent knowledge notes in `.badb/memories/`.
- `/usage` — Show cumulative session token consumption.
- `/features` — List all registered kernel and geasa features.
- `/journal` — View recent checkpoint and iteration provenance from `journal.jsonl`.

#### Geasa Contracts & Synthesis
- `/geis add <feature description>` — Trigger the agentic synthesis loop to create and verify a new feature bundle.
- `/geis model [name]` — Inspect or set default LLM model for geis contract synthesis.
- `/geis list` — Display status of all discovered and locked geasa bundles.
- `/geis verify <fid>` — Run verification for a bundle.
- `/geis bootstrap` — Initialize project geasa structure.
- `/geis distill` — Analyze recent tool usage patterns and distill into a new geis bundle.

### Dynamic Geis Synthesis Workflow

To extend badb with a new capability at runtime:

1. Launch the TUI: `./scripts/badb`
2. Run `/geis add <description>` (for example: `/geis add git-commit-helper`).
3. An agent synthesizes:
   - Contract test suite: `spec.geis.scm`
   - Implementation plugin: `plugin.scm`
   - Manifest with metadata: `manifest.scm`
4. The kernel's `geis-gate` executes the spec in a subprocess.
5. Upon passing, the bundle is locked into the registry and immediately available without restarting the harness.

---

## Model & MCP Configuration

### Model Providers & Endpoints

badb supports any OpenAI-compatible or local model endpoint (such as llama.cpp, Ollama, LM Studio, or vLLM), as well as Anthropic and direct provider adapters via `providers-model`.

You can configure endpoints through:
- **Environment variables:**
  ```sh
  export BADB_API_KEY="your-api-key"
  export BADB_ENDPOINT="http://localhost:1234/v1"
  export BADB_MODEL="llama-3.3-70b-instruct"
  ```
- **Interactive TUI commands:**
  ```text
  /endpoint http://localhost:11434/v1
  /model llama3.3:latest
  /provider openai
  ```
- **Configuration files:** Saved under `.badb/config.scm` or `~/.config/badb/config.scm`.

### MCP Server Integration

Connect any standard MCP server over stdio:

```text
/mcp add filesystem guile -s src/badb/capabilities/filetree-mcp.scm
/mcp start filesystem
/mcp tools
```

Or run badb's built-in filetree MCP server as a standalone subprocess for other tools:

```sh
./scripts/badb mcp-filetree
```

---

## Testing & Quality Verification

badb features a comprehensive testing pipeline covering kernel invariants, adversarial attack vectors, contract replays, and headless TUI interactions:

```sh
# Run bootstrap test suite (kernel invariants + all geasa specs)
./scripts/test

# Run geasa contracts only in parallel
./scripts/verify-geasa

# Run end-to-end integration and TUI tests
./scripts/devenv -- ./scripts/test-e2e

# Run only non-TUI integration tests
./scripts/devenv -- ./scripts/test-e2e --integration

# Run headless TUI terminal scenarios via tui-test
./scripts/devenv -- ./scripts/test-e2e --tui
```

### Real Model Testing

By default, e2e chat tests run against a hermetic mock endpoint (`test/e2e/fake-endpoint.scm`). To execute against a live model:

```sh
E2E_CHAT_ENDPOINT=http://127.0.0.1:1234/v1 \
E2E_CHAT_MODEL="your-model-id" \
./scripts/devenv -- ./scripts/test-e2e
```

---

## Project Layout

```text
badb/
├── channels.scm / manifest.scm   # Pinned GNU Guix environment definitions
├── license.md                     # LGPLv3 license terms
├── scripts/
│   ├── badb                      # Harness CLI entrypoint launcher
│   ├── devenv                    # Guix time-machine development shell wrapper
│   ├── test                      # Bootstrap unit and kernel invariant test runner
│   ├── test-e2e                  # Headless end-to-end and TUI test runner
│   └── verify-geasa              # Parallel geis contract verification script
├── src/
│   ├── srfi/srfi-253.scm         # SRFI-253 contract specification support
│   ├── badb/                     # Microkernel core & CLI dispatch
│   │   ├── kernel/               # Pure kernel: digest, geis-gate, ipc, journal, registry, transaction
│   │   ├── spec.scm              # Contract & property test engine
│   │   ├── spec-gate.scm         # Locked-spec verification gate
│   │   ├── geis/bundle.scm       # Bundle discovery, packing, installation & verification
│   │   ├── capabilities/         # filetree-mcp (stdio MCP server)
│   │   ├── surfaces/cli.scm      # CLI rendering, formatting & ANSI colors
│   │   └── main.scm              # Command-line entrypoint & option dispatch
│   └── geasa/                    # Modular capability bundles (the geasa):
│       ├── capabilities-*        # fs-tools, context-index, code-nav, mcp, history, skills
│       ├── runtimes-*            # react, react-stream, context, prompt, rollback, synthesis
│       ├── providers-model       # Model client adapters (OpenAI, Anthropic, mock)
│       ├── surfaces-tui          # Interactive terminal UI (editor, session, transcript, menus)
│       ├── policies-guardrail    # Safety policies & security levels
│       └── persona-*             # Personas (base, badb, synthesizer)
├── workflows/                    # Declarative Sema workflows
└── test/                         # Test fixtures, adversarial suites, specs & e2e tests
```

---

## License

badb is released under the **GNU Lesser General Public License v3.0** (LGPLv3). See [license.md](license.md) for complete details.

[GNU Guix]: https://guix.gnu.org
[GNU Guile]: https://www.gnu.org/software/guile/
