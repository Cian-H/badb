# Shell mode: ! switches modes, Enter executes, backspace returns.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "badb microharness" 20000

press '!'
expect_visible "enter:run · backspace:back to chat" 5000
if tui state 2>/dev/null | grep -q "\"shape\":\"underline\""; then
  pass "shell mode uses an underline cursor"
else
  fail "shell mode cursor is not an underline"
fi

submit_line "echo badb-e2e-shell-marker"
expect_visible "badb-e2e-shell-marker" 10000
expect_visible "shell output" 5000

# Backspace on the empty prompt leaves shell mode for chat.
press Backspace
expect_visible "^↑/^↓ select" 5000
expect_hidden "enter:run · backspace:back to chat" 3000

finish
