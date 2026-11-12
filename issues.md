# issues.md — known issues and latent defects

## Kernel

### `ipc-call` unexpected-EOF error shape breaks adversarial expectation (latent, pre-existing)

Found 2026-11-12 while running `test/adversarial-capabilities.scm` by hand against
HEAD (`4c8349f`).

- At HEAD, the unexpected-EOF path in `src/badb/kernel/ipc.scm` throws
  `(error "ipc-call: unexpected EOF (connection closed without reply)" ,req-id)`,
  so for the resulting error `(cadr args)` is the numeric request id, not a
  message string.
- `test/adversarial-capabilities.scm` (`ipc-call-and-mcp-error-paths` case)
  expects `(cadr args)` to be a descriptive string; against fresh-source
  semantics it receives the req-id and the case fails (1 of 28 fails).
- The failure is **masked on a warm machine**: Guile's auto-compile ccache
  (`~/.cache/guile/...`) serves a stale `.go` built from an older `ipc.scm`,
  under which the old error shape happens to satisfy the test. With
  `GUILE_AUTO_COMPILE=0` or a cleared cache, the failure reproduces from an
  unmodified HEAD checkout.
- `src/badb/kernel/ipc.scm` is identical between `main` and the
  `tool-usage-guardrails` branch; this is not a regression from the ACI
  guardrails work.
- Fix needs a maintainer decision since it touches the kernel: either the
  EOF path should raise `(error <message> <req-id>)` with the message as the
  first arg in a shape consistent with the other `ipc-call` error paths and
  the test updated, or the adversarial expectation should be corrected to
  match the req-id shape. Related: `test/test-kernel-design.scm` must stay
  green for any kernel change.

## Test coverage gaps

- `test/adversarial-*.scm` are not run by any script (existing
  `DEV-DEBT [coverage-gap]` in `test/runner.scm`). Beyond the missing
  invocation, there is no documented way to force fresh-source semantics;
  relying on the ambient ccache can silently mask real failures, as with the
  `ipc-call` issue above. Suggestion: a `./scripts/test-adversarial` runner
  that sets `GUILE_AUTO_COMPILE=0` (or a `guile compile` pass) so kernel
  error shapes are exercised from current source, not stale bytecode.

## Environment-dependent

- `test/adversarial-surfaces.scm`: 2 failures reproduce identically on `main`
  when notcurses is present (existing `DEV-DEBT [env-dependent]`).

## Stale test expectations

### TUI e2e scenarios red since the Nov 4 TUI rework (100% failing)

Found 2026-11-12 by running `./scripts/devenv -- ./scripts/test-e2e --tui` on the
`tool-usage-guardrails` branch and re-checking scenario `01-startup.sh` against
pristine `4c8349f` (identical failures, so fully pre-existing).

`./scripts/test-e2e --tui` reports **0/11 scenarios passing**. Every scenario
fails first on `expect_visible "Welcome to badb microharness TUI!"`, a string
that no longer exists in `src/`:

- `9ff4206` (2026-11-04, "arrows cycle history, paste batching, scheme-mode and
  startup fixes") replaced the startup notice with
  `"badb microharness\n• ! shell · ( scheme · / commands · /help for keys"` and
  changed the scheme-mode trigger from `,` to `(` — but the scenarios in
  `test/e2e/` were written 2026-10-29 (`3b1b66e`) and were never updated.
- Failure classes, all expectation drift:
  - `Welcome to badb microharness TUI!` (every scenario; plus the identity and
    `! shell · , scheme · / cmds` keymap strings in `01-startup.sh`, `08-resize.sh`).
  - Scheme-mode scenarios (`03`, `06`) press `,` and then wait for the eval hint
    and `=> 42`; the trigger is `(` now.
  - `11-mouse.sh` card-click selection depends on cards that no longer render
    in that form.
- Assertions that still match the current TUI pass, including all shell-mode,
  slash-palette, resize/typing, chat-roundtrip (fake endpoint) and theme/scroll
  checks.

Suggested fix: a sweep over `test/e2e/*.sh` updating the expected strings/keys
to the post-`9ff4206` UI, verified with
`./scripts/devenv -- ./scripts/test-e2e --tui`. Worth deciding whether
`test-e2e --tui` should be a pre-merge check (`.config/wt.toml`) once it is
green again — right now nothing runs it.
