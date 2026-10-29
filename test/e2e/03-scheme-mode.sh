# Scheme mode: , evaluates Scheme in the REPL and renders the result.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "Welcome to badb microharness TUI!" 20000

press ','
expect_visible "enter:eval · backspace:back to chat" 5000

submit_line "(+ 40 2)"
expect_visible "=> 42" 10000

press Backspace
expect_visible "^↑/^↓ select" 5000
expect_hidden "enter:eval · backspace:back to chat" 3000

finish
