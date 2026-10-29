# Chat round-trip: a chat submit runs the autonomous ReAct loop
# against the configured model and renders the assistant response.
#
# Requires a reachable model endpoint; skipped unless
# E2E_CHAT_ENDPOINT and E2E_CHAT_MODEL are exported, e.g. against a
# local LM Studio:
#
#   E2E_CHAT_ENDPOINT=http://127.0.0.1:1234/v1 \
#   E2E_CHAT_MODEL=qwen/qwen3.6-35b-a3b \
#   ./scripts/devenv -- ./scripts/test-e2e
. "$(dirname "$0")/lib.sh"

if [ -z "$E2E_CHAT_ENDPOINT" ] || [ -z "$E2E_CHAT_MODEL" ]; then
  echo "  scenario $SESSION: SKIP (set E2E_CHAT_ENDPOINT and E2E_CHAT_MODEL)"
  exit 0
fi

E2E_TUI_ENV_ARGS="--env BADB_MODEL_ENDPOINT=$E2E_CHAT_ENDPOINT --env BADB_MODEL_NAME=$E2E_CHAT_MODEL"

launch_tui
expect_visible "Welcome to badb microharness TUI!" 20000

submit_line "Say the word ready and nothing else."
expect_visible "Say the word ready and nothing else." 10000

# The ReAct loop runs against the live model; the response renders as an
# assistant card. Local models are slow, so allow a generous timeout.
expect_visible "assistant" 120000 --after-text "Say the word ready and nothing else."

finish
