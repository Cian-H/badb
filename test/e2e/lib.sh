# badb TUI e2e suite - shared helpers.
#
# Playwright-style helpers over the tui-test CLI (microsoft/tui-test):
# each scenario launches the real TUI in an isolated headless terminal
# session, drives it with keys, and asserts on rendered screen text.
# Source this file from a scenario script; it must run inside the
# project's guix dev environment, where tui-test and guile-ncurses are
# on PATH.
#
# Conventions:
#   - one scenario = one fresh session, one scratch cwd (.badb state is
#     created there, never in the repo), no shared state between
#     scenarios
#   - assertions are named expect_visible / expect_hidden; they wait up
#     to a timeout for the condition, like playwright's expect().toBe()
#   - the scenario exits non-zero if any assertion failed

E2E_ROOT="${E2E_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
SESSION="badb-e2e-$(basename "$0" .sh)"
E2E_SCRATCH="$(mktemp -d /tmp/badb-e2e-XXXXXX)"
E2E_TUI_ENV_ARGS=""
E2E_FAILURES=0

tui() { tui-test --session "$SESSION" "$@"; }

launch_tui() {
  # Extra --env KEY=VALUE pairs for the TUI process go in
  # E2E_TUI_ENV_ARGS (word-split, so no spaces inside values).
  # shellcheck disable=SC2086
  tui-test run --session "$SESSION" --cols 120 --rows 40 \
    --cwd "$E2E_SCRATCH" \
    --env GUILE_AUTO_COMPILE=0 $E2E_TUI_ENV_ARGS \
    -- "$E2E_ROOT/scripts/badb" tui >/dev/null
}

pass() { echo "    ok: $1"; }
fail() { echo "    FAIL: $1"; E2E_FAILURES=$((E2E_FAILURES + 1)); }

expect_visible() {
  # expect_visible TEXT [TIMEOUT_MS] [extra expect args...]
  text="$1"; tmo="${2:-5000}"; shift 2
  if tui expect text "$text" --timeout "$tmo" "$@" >/dev/null 2>&1; then
    pass "visible: $text"
  else
    fail "expected visible: $text"
  fi
}

expect_hidden() {
  # expect_hidden TEXT [TIMEOUT_MS] [extra expect args...]
  text="$1"; tmo="${2:-2000}"; shift 2
  if tui expect text "$text" --not --timeout "$tmo" "$@" >/dev/null 2>&1;
  then
    pass "hidden: $text"
  else
    fail "expected hidden: $text"
  fi
}

press() { tui key press "$@" >/dev/null 2>&1; }

type_text() { tui type "$1" >/dev/null 2>&1; }

submit_line() { tui submit "$1" >/dev/null 2>&1; }

expect_exited() {
  # Wait for the TUI child to exit and check its exit status was 0.
  if tui wait exit --timeout 15000 >/dev/null 2>&1 \
     && tui state 2>/dev/null | grep -q "^exited: 0"; then
    pass "tui exited cleanly"
  else
    fail "tui did not exit cleanly"
  fi
}

wait_port() {
  # wait_port PORT [TRIES] - block until loopback:PORT accepts an HTTP
  # request (used for the fake model endpoint readiness).
  wport="$1"; wtries="${2:-30}"; wi=0
  while [ "$wi" -lt "$wtries" ]; do
    if guile --no-auto-compile -c "
        (use-modules (web client) (web uri))
        ;; NB: (exit ...) throws a 'quit exception in guile, so it must
        ;; stay OUTSIDE catch #t or it gets swallowed and turned into a
        ;; failure.
        (if (catch #t
              (lambda ()
                (http-get (string->uri \"http://127.0.0.1:$wport/v1/models\"))
                #t)
              (lambda _ #f))
          (exit 0)
          (exit 1))" >/dev/null 2>&1; then
      return 0
    fi
    wi=$((wi + 1))
    sleep 1
  done
  return 1
}

cleanup() {
  # Scenarios may define scenario_cleanup for their own resources
  # (e.g. the fake endpoint process); it runs before session teardown.
  if type scenario_cleanup >/dev/null 2>&1; then scenario_cleanup; fi
  tui key press ctrl+d >/dev/null 2>&1
  tui wait exit --timeout 10000 >/dev/null 2>&1
  tui-test --session "$SESSION" close >/dev/null 2>&1
  rm -rf "$E2E_SCRATCH"
}
trap cleanup EXIT INT TERM

finish() {
  if [ "$E2E_FAILURES" -eq 0 ]; then
    echo "  scenario $SESSION: PASS"
    exit 0
  else
    echo "  scenario $SESSION: $E2E_FAILURES failure(s)"
    exit 1
  fi
}
