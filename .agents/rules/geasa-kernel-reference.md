---
trigger: model_decision
description: badb reference for creating or modifying geasa bundles (manifest, spec, plugin, tool wiring), for any change under src/badb/kernel/, and for choosing which test suites to run. Read before bundle or kernel work.
---

# badb reference: geasa bundles, kernel invariants, tests

Companion to the root `AGENTS.md`, which takes precedence. Everything here was checked
against the source unless labelled **Recommendation**.

## 1. Bundle manifest (`src/geasa/<fid>/manifest.scm`)

Manifests are plain data: read with `read`, never evaluated. Pinning writes them back as one
line with `write`, and the formatter skips them (`.pre-commit-config.yaml`,
`scripts/format-scheme`). `geis-manifest-valid?` in `src/badb/geis/bundle.scm` enforces:

| Field | Rule |
| :--- | :--- |
| `bundle-version` | must be exactly `1` |
| `feature-id` | non-empty, chars `[A-Za-z0-9._-]`, must equal the directory name |
| `version` | non-empty string |
| `spec` | required, safe relative path (no leading `/`, no `..`), file must exist |
| `plugin` | optional; `(module sym …)` and `(file . "rel")`; the file must exist |
| `workflow` | optional; safe relative path, must exist |
| `sha256` | `(("file" . "<64 hex>") …)`; every pinned file must exist and match its content |

Also seen in the tree: `tools` (list of tool names; the project loader only attaches exports
listed here), `depends` (`guile-modules`, `geis` entries with `version`/`bundle-sha256`,
`binaries`, `badb-version`; dependency version and hash mismatches are rejected), `fixtures`,
`cassettes`, `provenance`.

- Pins cover **every** bundle file, including `fixtures/`, `cassettes/`, `prompts/`, and
  extra modules. Any byte change breaks them. Re-pin with `./scripts/format-scheme <files>`
  (re-pins the touched bundles and stages them) or `./scripts/badb geis pin <bundle-dir>`.
- A bundle's identity hash covers all files except `manifest.scm`.

## 2. Contract spec (`spec.geis.scm`)

Shape used by current bundles (e.g. `src/geasa/capabilities-macro/spec.geis.scm`):

```scheme
(define-module (test <fid>-spec)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (geasa <fid> plugin))
(spec-begin "<fid>")
(test-group "…" (test-assert "…" …))
(spec-end)
(spec-exit)   ; present in 35 of 39 bundle specs; absent in capabilities-{git,lsp,macro,treesitter}
```

- `(badb spec)` also provides `define-contract-spec` / `define-module-spec` for quickcheck
  property contracts; the header of `src/badb/spec.scm` documents the full form.
- The gate replays a **locked read-only copy** of the spec (under `.badb/`) in a disposable
  subprocess. The engine reports its observations and the gate decides the verdict. A JSON vs
  exit-code mismatch fails closed (`test/test-spec-gate.scm`).
- **Recommendation:** keep specs hermetic. Use `(geasa providers-model mock)`, in-bundle
  `fixtures/` / `cassettes/`, and `mkdtemp` workspaces (as `test/specs/geis-bundle.geis.scm`
  does) rather than live endpoints or repo state. Follow the majority and end with
  `(spec-exit)` unless you have a reason not to.
- Flat specs not tied to a bundle live in `test/specs/<name>.geis.scm` and are verified by
  `./scripts/verify-geasa` too.

## 3. How bundles reach the runtime

- **Built-in (`src/geasa`) tools are wired explicitly** in `register-builtin-tui-tools!`
  (`src/geasa/surfaces-tui/tools.scm`). At present `register-git-tools!`,
  `register-lsp-tools!`, and `register-macro-tools!` are defined but never called. Wiring the
  macro expander is an open item in `todo.md`. A bundle that passes the gate is not
  automatically exposed to the agent.
- **Project bundles** in `<workspace>/.badb/geasa` and `.badb/workflows` are mounted at TUI
  startup by `discover-and-mount-project-geasa!` (`runtimes-geis-loader`): it validates the
  manifest (including pins), loads the plugin, and attaches the exported procedures (filtered
  by `tools`).
- In this repo `.badb/geasa/` is also the gate's lock store
  (`locked-geis-bundle-path` = `<gate-root>/geasa/<fid>`, gate root `.badb` by default).
  Locking deletes and rewrites that directory read-only. **Write bundles for this repo in
  `src/geasa/`, never in `.badb/geasa/`.**
- Search-path discrepancy: `readme.md` gives the `BADB_GEIS_PATH` default as
  `.badb/geasa:src/geasa`, but `geis-bundle-search-path` falls back to
  `("src/geasa" "geis")`. Go by the code; flag the discrepancy instead of "fixing" either
  side silently.

## 4. Kernel change protocol (`src/badb/kernel/`)

Only for changes that cannot be done as a bundle (`AGENTS.md`, preference 1).
`test/test-kernel-design.scm` enforces:

| Invariant | Limit |
| :--- | :--- |
| File whitelist | exactly `digest`, `geis-gate`, `ipc`, `journal`, `registry`, `transaction` (`.scm`) |
| Epistemic purity | no symbol **containing** `prompt`, `agent`, `persona`, `model`, `token`, `chat`, `workflow`, `tui` (substring match, so `tokenize` fails) |
| Macro purity | no `define-syntax`, `defmacro`, `define-macro`, `syntax-rules`, `syntax-case` |
| Dependency boundary | no `(geasa …)` imports; no `(badb providers\|runtimes\|capabilities\|surfaces\|policies\|geis …)` |
| Public ABI | every export must be in `%approved-kernel-abi`; ≤ 86 exports total |
| Mutation surface | `!` exports must be in `%approved-mutating-exports` (11) |
| Density | ≤ 150 AST nodes/export (`ipc`, `registry`, `journal`); ≤ 350 (`transaction`, `geis-gate`); `digest` exempt |
| Cyclomatic complexity | ≤ 10 per `define`/`define*`/`define-public`; ≤ 22 for the grandfathered list |

- The test's CC counts `if` +1, `cond`/`case`/`match` clauses −1, `and`/`or` operands −1,
  `catch`/`with-exception-handler` +1. `ast_complexity_metrics` is useful for a first look but
  may count differently; the test is authoritative.
- Adding or renaming an export, adding a mutator, or raising a cap is an ABI/policy change.
  It needs explicit maintainer approval, and the matching allowlist edit should be called out
  in the summary.
- Verify with `./scripts/test` (Stage 1 includes `test-kernel-design`, `test-spec-gate`,
  `test-journal`, `test-transaction`, `test-registry`, `test-digest`).

## 5. Test inventory

- `test/test-*.scm` (run by `./scripts/test`): calibration, digest, dist, journal,
  kernel-design, registry, spec-gate, spec, token-efficiency, transaction.
- `test/specs/*.geis.scm`: `geis-bundle`, `geis-bundle-cli`, `geis-capture`,
  `geis-spec-paths` (run by `./scripts/verify-geasa`).
- `test/adversarial-*.scm`: **not run by any script.** Coverage:
  `capabilities` (kernel ipc, mcp, mcp-server, filetree-mcp, history-tools),
  `providers` (providers-model common/mock/openai/anthropic, react, guardrail),
  `runtimes` (react, context, rollback, prompt), `surfaces` (`scripts/badb`, `(badb main)`,
  cli, theme, tui). One case is env-dependent (`DEV-DEBT [env-dependent]`).
- `test/e2e/`: `test-cli-entrypoints.sh`, `test-geis-contract.scm`,
  `test-transaction-rollback.scm` (integration stage), plus `NN-*.sh` TUI scenarios that
  source `lib.sh` (one fresh `tui-test` session and scratch cwd per scenario, with
  `expect_visible` / `expect_hidden`). Live-model runs use `E2E_CHAT_ENDPOINT` /
  `E2E_CHAT_MODEL`.
- The root `*.log` files are gitignored output from the last run; their names match the
  SRFI-64 suite names. Use them for triage, never as a source of truth.

## 6. Tracked debt

Inline format: `;; DEV-DEBT [tag]: explanation`. Tags in use: `complexity`, `coverage-gap`,
`manual-repin`, `shim`, `accepted-layering`, `shell-interpolation`, `env-dependent`
(`git grep -n DEV-DEBT`). The relevant ones:

- `shell-interpolation` (`workflows/plugin-synthesis.sema`,
  `src/geasa/runtimes-synthesis/workflow.sema`): never pass untrusted paths to these
  workflows.
- `complexity`: `run-tui` (`surfaces-tui/plugin.scm`), `run-synthesis-cli`, and
  `geis-manifest-valid?` are known hotspots. Don't make them worse; decompose only when asked.
- `accepted-layering`: `src/badb/geis/bundle.scm` → `runtimes-synthesis` and
  `filetree-mcp` → `capabilities-mcp-server` are accepted exceptions, not precedents.
