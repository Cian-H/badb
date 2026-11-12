# Chat round-trip: a chat submit runs the autonomous ReAct loop
# against the configured model endpoint and renders the assistant
# response.
#
# By default the scenario starts the static fake endpoint
# (fake-endpoint.scm) on loopback and points the TUI at it, so the full
# request/response pipeline is exercised with zero external
# dependencies. Export E2E_CHAT_ENDPOINT (and optionally
# E2E_CHAT_MODEL) to drive a real model server instead:
#
#   E2E_CHAT_ENDPOINT=http://127.0.0.1:1234/v1 \
#   E2E_CHAT_MODEL=qwen/qwen3.6-35b-a3b \
#   ./scripts/devenv -- ./scripts/test-e2e
. "$(dirname "$0")/lib.sh"

E2E_FAKE_PORT="${E2E_FAKE_PORT:-18034}"
FAKE_PID=""

scenario_cleanup() {
  if [ -n "$FAKE_PID" ]; then
    kill "$FAKE_PID" 2>/dev/null
  fi
}

if [ -n "${E2E_CHAT_ENDPOINT:-}" ]; then
  E2E_TUI_ENV_ARGS="--env BADB_MODEL_ENDPOINT=$E2E_CHAT_ENDPOINT"
  if [ -n "${E2E_CHAT_MODEL:-}" ]; then
    E2E_TUI_ENV_ARGS="$E2E_TUI_ENV_ARGS --env BADB_MODEL_NAME=$E2E_CHAT_MODEL"
  fi
  EXPECT_RESPONSE="assistant"
else
  # Reuse an already-listening fake endpoint (e.g. left over from an
  # interrupted run); otherwise start one and wait for readiness.
  if ! wait_port "$E2E_FAKE_PORT" 2; then
    E2E_FAKE_PORT="$E2E_FAKE_PORT" \
      guile --no-auto-compile "$E2E_ROOT/test/e2e/fake-endpoint.scm" \
      >/dev/null 2>&1 &
    FAKE_PID=$!
    if ! wait_port "$E2E_FAKE_PORT" 20; then
      fail "fake endpoint did not become ready on port $E2E_FAKE_PORT"
      finish
    fi
  fi
  E2E_TUI_ENV_ARGS="--env BADB_MODEL_ENDPOINT=http://127.0.0.1:$E2E_FAKE_PORT/v1 --env BADB_MODEL_NAME=fake-model"
  EXPECT_RESPONSE="e2e fake model ready."
fi

launch_tui
expect_visible "badb microharness" 20000

submit_line "Please reply."
expect_visible "Please reply." 10000

# The ReAct loop runs against the endpoint; the assistant card renders
# the response (the fake endpoint's canned content, or just the card
# label for a real model).
expect_visible "$EXPECT_RESPONSE" 30000 --after-text "Please reply."

# /usage renders the session token report card.
submit_line "/usage"
expect_visible "usage" 8000 --after-text "Please reply."

finish
