# Exit: Ctrl+D leaves the TUI cleanly with exit status 0.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "Welcome to badb microharness TUI!" 20000

press ctrl+d
expect_exited

finish
