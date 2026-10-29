# Scrollback: long output pushes the welcome off-screen; PageUp and
# PageDown move the transcript viewport over it.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "Welcome to badb microharness TUI!" 20000

press '!'
expect_visible "enter:run · backspace:back to chat" 5000
submit_line "seq 1 60"
expect_visible "60" 10000

# The transcript overflowed; the welcome scrolled out of view.
expect_hidden "Welcome to badb microharness TUI!" 3000

# PageUp jumps half a viewport; four jumps reach the welcome.
press PageUp PageUp PageUp PageUp
expect_visible "Welcome to badb microharness TUI!" 5000

# PageDown returns to the bottom of the transcript.
press PageDown PageDown PageDown PageDown
expect_hidden "Welcome to badb microharness TUI!" 3000

finish
