# AGENTS.md --- badb

badb is a Guile Scheme agentic microharness: a small microkernel
(`src/badb/kernel/`) plus contract-verified **geis bundles**
(`src/geasa/<feature-id>/`). User docs: `readme.md`. Longer reference (manifest
schema, kernel limits, test inventory):
`.agents/rules/geasa-kernel-reference.md`.

## Workflow Policies (mandatory)

1. **Leave the kernel alone by default; build features as geis bundles.** New
   functionality goes in `src/geasa/<feature-id>/`. Edit `src/badb/kernel/` only
   when change cannot live in user space, e.g. a defect in the
   gate/registry/journal/transaction/IPC mechanism, or a primitive no bundle can
   provide. Explain why in your summary. Kernel changes must keep
   `test/test-kernel-design.scm` green.
2. **Edit Scheme with the `structural-editing` MCP server by default**
   (`read_node` with `load_files` → `ast_*` edit tools → `commit_workspace`).
   Fall back to manual text edits only if absolutely necessary. If you **must**
   manually edit, carefully count parentheses before editing. This policy is
   intended to prevent parenthesis counting errors.
3. **Format all Scheme code with `cl-indentify`**, via
   `./scripts/format-scheme`.
4. **After code edits, run the structural-editing static-analysis tools** on the
   changed files: `ast_lint`, `ast_analyze_bindings`, `ast_complexity_metrics`
   (optionally `ast_find_duplicates` / `ast_suggest_refactorings`). Fix what you
   introduced; don't mass-fix pre-existing findings unless asked. If the server
   is unavailable, say the analysis was not run.

## Post-edit sequence for `.scm` changes

1. Edit structurally and `commit_workspace` (or make a minimal manual edit and
   disclose it).
2. Format: `./scripts/format-scheme <file>...`
   - Runs `cl-indentify -r` in place, **then `git add`s those files**, and
     re-pins **and stages** `manifest.scm` for every touched `src/geasa/<fid>/`.
   - If `cl-indentify` isn't on PATH, it re-runs itself through
     `./scripts/devenv` (Guix).
   - `--check` is read-only. `--all` reformats every tracked `.scm` and re-pins
     every bundle, so don't use it for scoped work.
   - To format without staging: `./scripts/devenv -- cl-indentify -r <file>...`,
     then `./scripts/badb geis pin src/geasa/<fid>`.
3. Reload the MCP workspace from disk (`workspace_manage` action `reload`),
   because formatting changed the files. Then run the static analysis from
   preference 4 (dialect `:scheme` if it isn't inferred).
4. Verify with the narrowest command that fits (see Testing).

## Layout

- `src/badb/kernel/`: the kernel.
- `src/badb/`: harness code outside the kernel.
  `src/badb/{registry,journal,digest,transaction}.scm` are compatibility shims
  (`DEV-DEBT [shim]`); new code imports `(badb kernel …)` directly.
- `src/geasa/<fid>/`: bundles (`manifest.scm`, `spec.geis.scm`, usually
  `plugin.scm`; optional extra modules, `prompts/`, `fixtures/`, `cassettes/`,
  `workflow.sema`).
- `test/`: SRFI-64 suites `test-*.scm`, flat contract specs `specs/*.geis.scm`,
  `adversarial-*.scm`, `fixtures/`, `cassettes/`, `dummy-mcp-server.scm`,
  `e2e/`.
- `.badb/` (gitignored, generated): `badb`'s config and state files. '**don't
  edit** `.badb/geasa/` or `.badb/specs/`.

## Adding a geis (default path for new functionality)

1. Pick `<fid>`: `<area>-<name>` (areas in use: `capabilities`, `runtimes`,
   `providers`, `surfaces`, `policies`, `persona`, `foundation`, `agents`).
   Allowed characters: `[A-Za-z0-9._-]`. Must equal the directory name.
2. Write `src/geasa/<fid>/spec.geis.scm` first, as executable contract (module
   `(test <fid>-spec)`, `(spec-begin "<fid>")` ... `(spec-end)`). See reference
   rules file for exact shape.
3. Implement `src/geasa/<fid>/plugin.scm` as module `(geasa <fid> plugin)`.
4. Create `manifest.scm`: `bundle-version` 1, `feature-id`, `version` (calver),
   `spec`, `plugin (module …) (file …)`, plus optional `tools`/`depends`. Never
   hand-write `sha256` entries; let `format-scheme` or `./scripts/badb geis pin`
   generate them.
5. Format, analyze, then `./scripts/verify-geasa` (locks/verifies every bundle).
6. If the TUI agent should get its tools, wire them explicitly in
   `register-builtin-tui-tools!` (`src/geasa/surfaces-tui/tools.scm`). This
   edits the `surfaces-tui` bundle, requires re-pin.

## Testing (verified commands)

  | Command                                                         | What it does                                                                                                                         |
  | :---                                                            | :---                                                                                                                                 |
  | `./scripts/test`                                                | Stage 1: every `test/test-*.scm` (includes kernel design). If Stage 1 fails, exits before Stage 2. Stage 2: `./scripts/verify-geasa` |
  | `./scripts/verify-geasa`                                        | Validates manifests, locks, and replays `test/specs/*.geis.scm` plus every `src/geasa/*/spec.geis.scm` in parallel                   |
  | `./scripts/badb geis verify <fid>`                              | Verifies targeted bundle                                                                                                             |
  | `./scripts/devenv -- ./scripts/test-e2e [--integration\|--tui]` | Integration suites plus `tui-test` scenarios against fake endpoint                                                                   |

- `test`, `verify-geasa`, and `badb` re-run themselves through
  `./scripts/devenv` when Guix dependencies are missing.
- Adversarial suites: run `./scripts/test-adversarial` (they are not run by
  `test/runner.scm` — `test/runner.scm` only covers `test/test-*.scm`).
- **There is no CI.** Checks are local only: the pre-commit hook
  (`.githooks/pre-commit` → `prek` → `format-scheme` on staged `.scm`, so commit
  can re-format, re-pin, and re-stage) and `.config/wt.toml` (pre-merge runs
  `./scripts/devenv -- ./scripts/test`).
- Any edit to a bundle file, even a comment, breaks its pins until you re-pin
  (`DEV-DEBT [manual-repin]`). `verify-geasa` fails on stale pins.

## Guardrails

- Don't change the allowlists/limits in `test/test-kernel-design.scm` (ABI list,
  mutator list, density/complexity caps, grandfathered procedures) to make a
  change pass unless maintainer explicitly approves.
- Don't format or hand-edit `src/geasa/*/manifest.scm` beyond deliberate field
  changes; they're excluded from formatting and rewritten by pinning.
- If you knowingly leave debt, record it inline as `;; DEV-DEBT [tag]: …`.
  Remove note when debt is resolved. Local roadmap/issues live in `beads`. Local
  notes live in `serena`.
- If asked to commit: conventional style `feat(<fid>): …` / `fix(<scope>): …`.
  `scripts/commit-feature <fid> "<msg>"` runs `./scripts/test`, stages **only**
  `src/geasa/<fid>/`, commits, and force-tags `geis/<fid>`. Other touched paths
  (e.g. `surfaces-tui` wiring) need separate staging.
- For TUI scenario work, use the existing `tui-test` skill and
  `test/e2e/lib.sh`.
