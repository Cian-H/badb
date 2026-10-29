# Interactive menu: /config opens the menu popup, Escape closes it.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "Welcome to badb microharness TUI!" 20000

type_text "/config"
expect_visible "↑↓:choose · tab:complete · enter:run · esc:dismiss" 5000
press Enter
expect_visible "↑↓:move · enter:apply · esc:close" 5000

press Escape
expect_hidden "↑↓:move · enter:apply · esc:close" 3000

# Clear the /config text left in the buffer.
press Backspace Backspace Backspace Backspace Backspace Backspace Backspace
expect_visible "❯" 3000

finish
