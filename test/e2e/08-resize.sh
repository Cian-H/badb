# Resize: the TUI keeps rendering coherently when the terminal is
# resized smaller and larger again.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "badb microharness" 20000

tui resize 100 30
expect_visible "badb microharness" 8000
expect_visible "! shell · ( scheme" 5000
if tui state 2>/dev/null | grep -q "cols: 100"; then
  pass "terminal resized to 100 columns"
else
  fail "terminal did not resize to 100 columns"
fi

tui resize 120 40
expect_visible "badb microharness" 8000

# Input still works after both resizes.
type_text "xyz"
expect_visible "❯ xyz" 5000
press Backspace Backspace Backspace

finish
