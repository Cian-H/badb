# Slash palette: / opens the command palette, Escape dismisses it.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "badb microharness" 20000

type_text "/"
expect_visible "↑↓:choose · tab:complete · enter:run · esc:dismiss" 5000
expect_visible "commands" 5000
expect_visible "/help" 5000

press Escape
expect_hidden "↑↓:choose · tab:complete · enter:run · esc:dismiss" 3000

# Escape leaves the slash text in the buffer; clear it so the session
# ends in the normal chat state.
press Backspace

finish
