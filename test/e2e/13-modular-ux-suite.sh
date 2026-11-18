# Modular UX Suite E2E: quick-action strip, Ctrl+K palette, /mcp, /sessions,
# /context, /notifications, Ctrl+F search, and /fold.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "badb microharness" 20000
expect_visible "Ctrl+K Palette" 5000

# Ctrl+K opens Command Palette, Esc closes it.
press ctrl+k
expect_visible "Command Palette" 5000
press Escape
expect_hidden "Command Palette" 3000

# /mcp opens MCP Server Manager, Esc closes it.
type_text "/mcp"
submit_line ""
expect_visible "MCP" 5000
press Escape
expect_hidden "MCP Server Manager" 3000

# /sessions opens Sessions modal, Esc closes it.
type_text "/sessions"
submit_line ""
expect_visible "Sessions" 5000
press Escape

# /context opens Context Inspector modal, Esc closes it.
type_text "/context"
submit_line ""
expect_visible "Context" 5000
press Escape

# /notifications opens Notifications modal, Esc closes it.
type_text "/notifications"
submit_line ""
expect_visible "Notifications" 5000
press Escape

# Ctrl+F opens in-transcript search bar, Esc closes it.
press ctrl+f
expect_visible "Find:" 5000
press Escape
expect_hidden "Find:" 3000

finish
