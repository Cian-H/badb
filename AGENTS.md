# AGENTS.md — badb

badb is a Guile Scheme agentic microharness: a small microkernel (`src/badb/kernel/`)
plus contract-verified **geasa bundles** (`src/geasa/<feature-id>/`). User docs: `readme.md`.
Longer reference (manifest schema, kernel limits, test inventory):
`.agents/rules/geasa-kernel-reference.md`.

## Maintainer preferences (mandatory)

1. **Leave the kernel alone by default; build features as geis bundles.** New functionality
   goes in `src/geasa/<feature-id>/`. Edit `src/badb/kernel/` only when the change cannot live
   in user space, e.g. a defect in the gate/registry/journal/transaction/IPC mechanism, or a
   primitive no bundle can provide. Explain why in your summary. Kernel changes must keep
   `test/test-kernel-design.scm` green.
2. **Edit Scheme with the `structural-editing` MCP server by default** (`read_node` with
   `load_files` → `ast_*` edit tools → `commit_workspace`). Fall back to manual text edits only
   if the server is unavailable or can't reasonably express the change. In that case keep the
   edit minimal and **say you used a manual edit**. Never claim structural editing was used if
   it wasn't.
3. **Format all Scheme code with `cl-indentify`**, via `./scripts/format-scheme` (below).
   Don't hand-indent to match the house style.
4. **After code edits, run the structural-editing static-analysis tools** on the changed
   files: `ast_lint`, `ast_analyze_bindings`, `ast_complexity_metrics` (optionally
   `ast_find_duplicates` / `ast_suggest_refactorings`). Fix what you introduced; don't
   mass-fix pre-existing findings unless asked. If the server is unavailable, say the
   analysis was not run.

## Post-edit sequence for `.scm` changes

1. Edit structurally and `commit_workspace` (or make a minimal manual edit and disclose it).
2. Format: `./scripts/format-scheme <file>...`
   - Runs `cl-indentify -r` in place, **then `git add`s those files**, and re-pins **and
     stages** `manifest.scm` for every touched `src/geasa/<fid>/`.
   - If `cl-indentify` isn't on PATH, it re-runs itself through `./scripts/devenv` (Guix).
   - `--check` is read-only. `--all` reformats every tracked `.scm` and re-pins every bundle,
     so don't use it for scoped work.
   - To format without staging: `./scripts/devenv -- cl-indentify -r <file>...`, then
     `./scripts/badb geis pin src/geasa/<fid>`.
3. Reload the MCP workspace from disk (`workspace_manage` action `reload`), because formatting
   changed the files. Then run the static analysis from preference 4 (dialect `:scheme` if
   it isn't inferred).
4. Verify with the narrowest command that fits (see Testing).

## Layout

- `src/badb/kernel/`: the kernel. Only these six files are allowed: `digest`, `geis-gate`,
  `ipc`, `journal`, `registry`, `transaction`.
- `src/badb/`: harness code outside the kernel (`main.scm` CLI, `spec.scm` spec engine,
  `geis/bundle.scm` bundle tooling). `src/badb/{registry,journal,digest,transaction}.scm` are
  compatibility shims (`DEV-DEBT [shim]`); new code imports `(badb kernel …)` directly.
- `src/geasa/<fid>/`: bundles (`manifest.scm`, `spec.geis.scm`, usually `plugin.scm`;
  optional extra modules, `prompts/`, `fixtures/`, `cassettes/`, `workflow.sema`).
- `test/`: SRFI-64 suites `test-*.scm`, flat contract specs `specs/*.geis.scm`,
  `adversarial-*.scm`, `fixtures/`, `cassettes/`, `dummy-mcp-server.scm`, `e2e/`.
- `.badb/` (gitignored, generated): **don't edit** `.badb/geasa/` or `.badb/specs/`. Those
  are the gate's read-only locked copies, overwritten whenever a bundle is locked.

## Adding a geis (default path for new functionality)

1. Pick `<fid>`: `<area>-<name>` (areas in use: `capabilities`, `runtimes`, `providers`,
   `surfaces`, `policies`, `persona`, `foundation`, `agents`). Allowed characters:
   `[A-Za-z0-9._-]`. It must equal the directory name.
2. Write `src/geasa/<fid>/spec.geis.scm` first, as an executable contract (module
   `(test <fid>-spec)`, `(spec-begin "<fid>")` … `(spec-end)`). See the reference rules file
   for the exact shape.
3. Implement `src/geasa/<fid>/plugin.scm` as module `(geasa <fid> plugin)`.
4. Create `manifest.scm`: `bundle-version` 1, `feature-id`, `version`, `spec`,
   `plugin (module …) (file …)`, plus optional `tools`/`depends`. Never hand-write the
   `sha256` entries; let `format-scheme` or `./scripts/badb geis pin` generate them.
5. Format, analyze, then `./scripts/verify-geasa` (locks and verifies every bundle).
6. If the TUI agent should get its tools, wire them explicitly in
   `register-builtin-tui-tools!` (`src/geasa/surfaces-tui/tools.scm`). That edits the
   `surfaces-tui` bundle, so re-pin it too.

## Testing (verified commands)

| Command | What it does |
| :--- | :--- |
| `./scripts/test` | Stage 1: every `test/test-*.scm` (includes kernel design). If Stage 1 fails, it exits before Stage 2. Stage 2: `./scripts/verify-geasa` |
| `./scripts/verify-geasa` | Validates manifests (including sha256 pins), locks, and replays `test/specs/*.geis.scm` and every `src/geasa/*/spec.geis.scm` in parallel (`BADB_PARALLEL_JOBS`, default 4) |
| `./scripts/badb geis verify <fid>` | Verifies one bundle (documented in `readme.md`) |
| `./scripts/devenv -- ./scripts/test-e2e [--integration\|--tui]` | Integration suites plus `tui-test` scenarios against `test/e2e/fake-endpoint.scm` |

- `test`, `verify-geasa`, and `badb` re-run themselves through `./scripts/devenv` when Guix
  dependencies are missing.
- **Gap:** `test/adversarial-*.scm` are **not** run by any script (`DEV-DEBT [coverage-gap]`
  in `test/runner.scm`). There's no documented invocation. If you change code they cover,
  say they weren't run, or run them by hand and report exactly what you ran. One case is
  known to fail when notcurses is present (`DEV-DEBT [env-dependent]`).
- **There is no CI.** Checks are local only: the pre-commit hook (`.githooks/pre-commit` →
  `prek` → `format-scheme` on staged `.scm`, so a commit can re-format, re-pin, and re-stage
  files) and `.config/wt.toml` (pre-merge runs `./scripts/devenv -- ./scripts/test`).
- Any edit to a bundle file, even a comment, breaks its pins until you re-pin
  (`DEV-DEBT [manual-repin]`). `verify-geasa` fails on stale pins.

## Guardrails

- Don't change the allowlists or limits in `test/test-kernel-design.scm` (ABI list, mutator
  list, density/complexity caps, grandfathered procedures) to make a change pass unless the
  maintainer explicitly approves.
- Don't format or hand-edit `src/geasa/*/manifest.scm` beyond deliberate field changes;
  they're excluded from formatting and rewritten by pinning.
- If you knowingly leave debt, record it inline as `;; DEV-DEBT [tag]: …`. Remove the note
  when the debt is resolved. Roadmap/issues live in `todo.md` (local, gitignored; keep its
  "Task Checklist" current when landing items) and `issues.md`.
- If asked to commit: conventional style `feat(<fid>): …` / `fix(<scope>): …`.
  `scripts/commit-feature <fid> "<msg>"` runs `./scripts/test`, stages **only**
  `src/geasa/<fid>/`, commits, and force-tags `geis/<fid>`. Other touched paths (e.g.
  `surfaces-tui` wiring) need separate staging.
- For TUI scenario work, use the existing `tui-test` skill (`.agents/skills/tui-test/`) and
  `test/e2e/lib.sh`.
