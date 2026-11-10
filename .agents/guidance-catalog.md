# badb agent guidance catalog

Not auto-loaded. This is the source list behind `AGENTS.md` (always on) and
`.agents/rules/geasa-kernel-reference.md` (`trigger: model_decision`). Use it when revising
either file.

**Basis:** **M** = explicit maintainer preference · **V** = verified in the repository ·
**R** = recommendation based on verified practice.

## 1. Rules list

| # | Rule | Basis | Placement | Evidence |
| :-- | :--- | :-: | :--- | :--- |
| 1 | New functionality goes in a geis bundle under `src/geasa/<fid>/`; edit `src/badb/kernel/` only when genuinely required, and say why | M | AGENTS.md | maintainer request |
| 2 | Edit Scheme with the `structural-editing` MCP by default; manual edits only as a fallback, minimal and disclosed; never claim structural editing that didn't happen | M | AGENTS.md | maintainer request; server responds in this environment (`workspace_manage list`) |
| 3 | Format Scheme with `cl-indentify` via `./scripts/format-scheme <files>` (or `./scripts/devenv -- cl-indentify -r` + `badb geis pin` to avoid staging) | M + V | AGENTS.md | `scripts/format-scheme` |
| 4 | After edits: reload the MCP workspace, then `ast_lint` / `ast_analyze_bindings` / `ast_complexity_metrics` on changed files; report if not run | M | AGENTS.md | MCP schemas list a `:scheme` dialect |
| 5 | `format-scheme` stages files and re-pins/stages touched bundle manifests; `--all` touches everything; `--check` is read-only | V | AGENTS.md | `scripts/format-scheme` `main` |
| 6 | Never edit `.badb/geasa/` or `.badb/specs/` (read-only locked copies, overwritten on lock) | V | AGENTS.md + rules file §3 | `lock-geis-bundle`, `locked-geis-bundle-path` |
| 7 | Never hand-write `sha256` pins; any bundle-file edit needs a re-pin; `verify-geasa` fails on stale pins | V | AGENTS.md | `%validate-sha256-pins!`, `DEV-DEBT [manual-repin]` |
| 8 | Don't format manifests; they're data, rewritten by pinning | V | AGENTS.md + rules file §1 | `.pre-commit-config.yaml` exclude; `geis-bundle-pin!` |
| 9 | Kernel changes must keep `test/test-kernel-design.scm` green | V | AGENTS.md | test file |
| 10 | Don't change kernel-design allowlists or caps without maintainer approval | R | AGENTS.md | allowlists are the ABI contract in the test |
| 11 | Kernel invariant limits (whitelist, lexicon, macros, imports, ABI ≤ 86, 11 mutators, density, CC 10/22) | V | rules file §4 | `test/test-kernel-design.scm` |
| 12 | Import `(badb kernel …)`, not the `src/badb/*.scm` shims | V | AGENTS.md | `DEV-DEBT [shim]` in `src/badb/registry.scm` |
| 13 | Geis workflow: fid naming → spec first → plugin → manifest → format/pin → `verify-geasa` → explicit TUI wiring if needed | V (TDD order per readme) | AGENTS.md (short) + rules file §1–3 | readme; `bundle.scm`; `surfaces-tui/tools.scm` |
| 14 | Manifest field schema | V | rules file §1 | `geis-manifest-valid?` |
| 15 | Spec shape and `(badb spec)` usage | V | rules file §2 | bundle specs; `src/badb/spec.scm` |
| 16 | Keep specs hermetic (mock provider, fixtures/cassettes, mkdtemp) | R | rules file §2 | `providers-model/mock.scm`, `runtimes-synthesis/cassettes`, `test/specs/geis-bundle.geis.scm` |
| 17 | Passing the gate doesn't expose tools; wire them in `register-builtin-tui-tools!` | V | AGENTS.md + rules file §3 | `surfaces-tui/tools.scm`; no call sites for git/lsp/macro registrars |
| 18 | Write this repo's bundles in `src/geasa/`, not `.badb/geasa/` | V | rules file §3 | lock store is the same directory the loader mounts |
| 19 | Verified test commands and what each covers | V | AGENTS.md | `scripts/test`, `test/runner.scm`, `scripts/verify-geasa`, `scripts/test-e2e` |
| 20 | Adversarial suites are not in any runner; disclose when they weren't run | V | AGENTS.md | `DEV-DEBT [coverage-gap]` in `test/runner.scm` |
| 21 | There is no CI; only the pre-commit hook (prek → format-scheme) and the wt pre-merge `./scripts/test` | V | AGENTS.md | no `.github/` or other CI config; `.githooks/pre-commit`; `.config/wt.toml` |
| 22 | Record knowingly-left debt as `;; DEV-DEBT [tag]: …`; remove when resolved | V (convention) + R | AGENTS.md; tags in rules file §6 | 11 existing notes |
| 23 | Never pass untrusted paths to the synthesis `.sema` workflows | V | rules file §6 | `DEV-DEBT [shell-interpolation]` |
| 24 | Keep the `todo.md` "Task Checklist" current when landing items | V (existing instruction) | AGENTS.md | `todo.md` line 9 (local, gitignored) |
| 25 | Commit style `feat(<fid>)` / `fix(<scope>)`; `scripts/commit-feature` behaviour | V | AGENTS.md | `git log`; `scripts/commit-feature` |
| 26 | Use the existing `tui-test` skill for TUI scenarios | V | AGENTS.md | `.agents/skills/tui-test/` |

**Deliberately left out:** generic style advice, speculative security policy, mandates to run
adversarial suites (there's no verified invocation), and requiring `(spec-exit)` (both forms
exist in the tree).

## 2. Proposed project-specific skills

Proposals only; none created yet. Put them under `.agents/skills/<name>/SKILL.md` when
adopted.

### `geis-bundle-author` (high value)

- **When:** adding a capability or changing a bundle's behaviour or contract. This is the
  main recurring task in this repo.
- **Workflow:**
  1. Check that the feature can't extend an existing bundle; choose `<area>-<name>`.
  2. Draft `spec.geis.scm` first (template from the rules file §2). Make it hermetic.
  3. Implement `plugin.scm` structurally (extra modules as `(geasa <fid> <name>)`).
  4. Write the manifest without pins, then `./scripts/format-scheme` or `badb geis pin`.
  5. Reload the MCP workspace, run the static analysis, then `./scripts/verify-geasa` (or
     `badb geis verify <fid>`).
  6. Optionally wire tools in `surfaces-tui/tools.scm` and re-pin `surfaces-tui`.
  7. Report the gate summary line (`PASS: geis bundle <fid> (summary: …)`).
- Bundled resource idea: a spec/plugin/manifest skeleton.

### `scheme-structural-edit` (high value)

- **When:** any `.scm` edit. Covers the mechanics `AGENTS.md` can only summarise.
- **Responsibilities:** `read_node` with `load_files`, plus path addressing ([dialect, file,
  form, …]); choosing between `ast_modify` / `ast_replace_pattern` / `ast_rename` /
  `ast_extract_function`; using `workspace_manage fork` for isolated edits;
  `commit_workspace`; formatting; `workspace_manage reload`; the lint/bindings/complexity
  pass; how to tell new findings from pre-existing ones; when a manual fallback is justified
  and how to disclose it.
- Prerequisite: confirm it handles comment and docstring edits (see §3) before promising
  that.

### `kernel-change-gate` (medium value, high stakes)

- **When:** a change touches `src/badb/kernel/` or `test/test-kernel-design.scm`.
- **Workflow:** write down why a bundle can't do it → map the change against the invariant
  table (rules file §4): lexicon substrings, imports, export/mutator allowlists, density, CC →
  run `ast_complexity_metrics` on the changed procedures → if an allowlist or cap must change,
  stop and get maintainer approval → `./scripts/test` → summary listing ABI impact.

### `badb-test-triage` (medium value)

- **When:** choosing what to run for a change, or interpreting a failure.
- **Responsibilities:** map changed paths to suites (kernel → Stage 1; bundle →
  `verify-geasa`; TUI/CLI → `test-e2e`; adversarial coverage list for manual runs). Explain
  that Stage 1 failure skips Stage 2. Recognise stale-pin manifest errors and fix by
  re-pinning. Know the env-dependent adversarial case. Use root `*.log` for detail. Disclose
  exactly what was and wasn't run.

### `badb-tui-scenario` (lower value; extends the existing `tui-test` skill)

- **When:** adding or fixing `test/e2e/NN-*.sh` scenarios.
- **Responsibilities:** `lib.sh` conventions (fresh session, scratch cwd, `launch_tui`,
  `expect_visible`/`expect_hidden`, `finish`), numbering, the fake endpoint vs
  `E2E_CHAT_ENDPOINT`, and running via `./scripts/devenv -- ./scripts/test-e2e --tui`. Point
  to the `tui-test` skill for the CLI itself; don't duplicate it.

**Not proposed:** a generic formatter skill (covered by `AGENTS.md`), a DEV-DEBT sweep skill
(too infrequent), a Guix skill (`./scripts/devenv` is enough).

## 3. Unverified or inconsistent items

- **Adversarial suite invocation:** no script or document defines one. Untested candidate,
  mirroring how `scripts/test-e2e` runs Guile suites:
  `./scripts/devenv -- guile -L src -L . test/adversarial-<x>.scm`. Confirm before
  documenting it as a command.
- **`cl-indentify -r` semantics:** inferred to mean in-place from how `format-scheme` uses it
  (it formats, then compares tmp copies in `--check`). Not checked against the tool's own
  help. Neither `cl-indentify` nor `prek` is on PATH outside the Guix devenv here.
- **structural-editing MCP:** the server responds in this environment, but whether it keeps
  comments and cl-indentify layout on `commit_workspace`, and how well `ast_lint` handles
  Guile-specific forms, hasn't been tested. Other agent environments may not have it.
- **`badb geis verify <fid>`:** dispatch exists in `src/badb/main.scm`; whether it locks
  first (as `verify-geasa` does) wasn't traced.
- **`BADB_GEIS_PATH` default:** `readme.md` says `.badb/geasa:src/geasa`; the code says
  `("src/geasa" "geis")`; `scripts/badb`'s comment says `"geis"`.
- **Hook activation:** `core.hooksPath=.githooks` is set in this clone's local git config.
  It isn't versioned, so fresh clones won't run the hook unless configured.
- **Rule/skill visibility:** `.agents/` is gitignored, so this catalog, the rules file, and
  the existing `tui-test` skill are local-only. Only `AGENTS.md` can be committed. Whether to
  un-ignore `.agents/rules/` is the maintainer's call.
- **`.agents/rules` frontmatter:** `trigger: model_decision` is documented; the
  `description` key is assumed.
- The repo also has a colocated Jujutsu store (`.jj/`); the scripts use git directly.
