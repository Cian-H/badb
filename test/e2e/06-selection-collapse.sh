# Message selection and collapse: Ctrl+Up selects the newest card and
# Ctrl+E toggles it between collapsed summary and expanded content.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "Welcome to badb microharness TUI!" 20000

# Create a fresh collapsible card at the bottom of the transcript.
press ','
expect_visible "enter:eval · backspace:back to chat" 5000
submit_line "(+ 40 2)"
expect_visible "=> 42" 10000

# Select the newest card, collapse it, then expand it again.
press ctrl+up
press ctrl+e
expect_visible "scheme output (2 lines)" 5000
if tui state 2>/dev/null | grep -q "\"shape\":\"block\""; then
  pass "selection mode uses a block cursor"
else
  fail "selection mode cursor is not a block"
fi
expect_hidden "=> 42" 3000

press ctrl+e
expect_visible "=> 42" 5000

finish
