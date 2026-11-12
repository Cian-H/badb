# Startup: the TUI boots to a fully rendered screen.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "badb" 20000                 # wordmark in the header
expect_visible "badb microharness" 20000
# (startup notice replaced identity card; no separate identity assertion)
expect_visible "^↑/^↓ select" 5000  # keymap hint
expect_visible "❯" 5000                    # chat prompt indicator
# Terminal cursor compatibility: chat typing shows a bar cursor.
if tui state 2>/dev/null | grep -q "\"shape\":\"bar\""; then
  pass "chat mode uses a bar cursor"
else
  fail "chat mode cursor is not a bar"
fi

# Typing renders into the input row and backspace removes it again.
type_text "abc"
expect_visible "❯ abc" 5000
press Backspace Backspace Backspace
expect_hidden "❯ abc" 3000

# Enter on an empty prompt is a no-op: the session stays alive.
submit_line ""
expect_visible "❯" 5000

finish
