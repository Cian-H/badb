# Startup: the TUI boots to a fully rendered screen.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "badb" 20000                 # wordmark in the header
expect_visible "Welcome to badb microharness TUI!" 20000
expect_visible "You are badb, an expert autonomous coding agent" 5000
expect_visible "!:shell ,:scheme /:cmds ^up/^dn:select" 5000  # keymap hint
expect_visible "❯" 5000                    # chat prompt indicator

# Typing renders into the input row and backspace removes it again.
type_text "abc"
expect_visible "❯ abc" 5000
press Backspace Backspace Backspace
expect_hidden "❯ abc" 3000

# Enter on an empty prompt is a no-op: the session stays alive.
submit_line ""
expect_visible "❯" 5000

finish
