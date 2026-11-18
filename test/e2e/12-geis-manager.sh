# Geasa Manager modal & updater extensions: /geasa opens modal, shows updater banner/column, Tab switches scope, q closes.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "badb microharness" 20000

type_text "/geasa"
expect_visible "↑↓:choose · tab:complete · enter:run · esc:dismiss" 5000
submit_line ""

expect_visible "Geasa Manager" 5000
expect_visible "Policy:" 5000
expect_visible "Status" 5000

press Tab
expect_visible "Project" 3000

press q
expect_hidden "Geasa Manager" 3000

finish
