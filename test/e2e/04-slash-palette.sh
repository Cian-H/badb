# Slash palette: / opens the command palette, Escape dismisses it.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "Welcome to badb microharness TUI!" 20000

type_text "/"
expect_visible "up/down:choose tab:complete enter:run esc:dismiss" 5000
expect_visible "guardrail" 5000

press Escape
expect_hidden "up/down:choose tab:complete enter:run esc:dismiss" 3000

# Escape leaves the slash text in the buffer; clear it so the session
# ends in the normal chat state.
press Backspace

finish
