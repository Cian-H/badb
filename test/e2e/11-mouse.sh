# Mouse support: wheel scrolls the transcript, clicks select message
# cards, menu items apply on click, and palette rows select on click.
. "$(dirname "$0")/lib.sh"

launch_tui
expect_visible "badb microharness" 20000

# (Card-click selection depends on cards that no longer render in this
# form; covered by keyboard selection scenarios instead.)

# Clicking a non-card row keeps the bar cursor.
tui mouse click 10 0
sleep 1
if tui state 2>/dev/null | grep -q "\"shape\":\"bar\""; then
  pass "non-card click keeps bar cursor"
else
  fail "non-card click changed cursor"
fi

# Wheel: overflow the transcript, then scroll back with the wheel.
press '!'
expect_visible "enter:run · backspace:back to chat" 5000
submit_line "seq 1 60"
expect_visible "60" 10000
expect_hidden "badb microharness" 3000
tui mouse scroll up
tui mouse scroll up
tui mouse scroll up
tui mouse scroll up
tui mouse scroll up
tui mouse scroll up
tui mouse scroll up
expect_visible "badb microharness" 5000
pass "wheel-up scrolls the transcript"
tui mouse scroll down
tui mouse scroll down
tui mouse scroll down
tui mouse scroll down
tui mouse scroll down
tui mouse scroll down
tui mouse scroll down
expect_hidden "badb microharness" 3000
pass "wheel-down scrolls back"

press Backspace
expect_visible "^↑/^↓ select" 5000

# Menu click: open the theme picker and click an item to apply it.
submit_line "/theme"
expect_visible "tokyo-night" 5000
ROW=$(tui text | grep -an "tokyo-night " | head -1 | cut -d: -f1)
Y=$((ROW - 1))
tui mouse click 10 "$Y"
sleep 1
expect_visible "UI theme set to" 5000
pass "menu click applies the theme (tokyo-night blue)"

# Palette click: rows select on click (Enter still runs the command).
press Backspace
type_text "/mo"
sleep 1
ROW=$(tui text | grep -an "/models " | head -1 | cut -d: -f1)
Y=$((ROW - 1))
tui mouse click 6 "$Y"
sleep 1
if tui text --full | grep -aq "❯ /models"; then
  pass "palette click selects the row"
else
  fail "palette click did not select"
fi
press Escape

finish
