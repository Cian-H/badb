#!/usr/bin/env bash
# test/e2e/test-cli-entrypoints.sh — End-to-end tests for badb CLI & script entrypoints
set -euo pipefail

ROOT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$ROOT_DIR"

PASS_COUNT=0
FAIL_COUNT=0

assert_eq() {
  local desc="$1"
  local expected="$2"
  local actual="$3"
  if [ "$expected" = "$actual" ]; then
    echo "  PASS: $desc"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  FAIL: $desc (expected '$expected', got '$actual')" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

assert_file_contains() {
  local desc="$1"
  local needle="$2"
  local file="$3"
  if grep -qF -- "$needle" "$file"; then
    echo "  PASS: $desc"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  FAIL: $desc (expected file '$file' to contain '$needle')" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

assert_contains() {
  local desc="$1"
  local needle="$2"
  local haystack="$3"
  if printf '%s\n' "$haystack" | grep -qF -- "$needle"; then
    echo "  PASS: $desc"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  FAIL: $desc (expected to contain '$needle')" >&2
    echo "  Actual: $haystack" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

echo "=== E2E Test Suite: CLI and Script Entrypoints ==="

# -----------------------------------------------------------------------------
# 1. scripts/badb entrypoint tests
# -----------------------------------------------------------------------------
echo "[1/4] Testing scripts/badb entrypoint..."

# Test --version
out_ver=$(./scripts/badb --version 2>&1)
rc_ver=$?
assert_eq "scripts/badb --version exits 0" "0" "$rc_ver"
assert_contains "scripts/badb --version outputs program name and version" "badb 0.1.0" "$out_ver"

# Test -V
out_short_v=$(./scripts/badb -V 2>&1)
assert_contains "scripts/badb -V outputs program name and version" "badb 0.1.0" "$out_short_v"

# Test --help
out_help=$(./scripts/badb --help 2>&1)
rc_help=$?
assert_eq "scripts/badb --help exits 0" "0" "$rc_help"
assert_contains "scripts/badb --help displays Usage" "Usage: badb" "$out_help"
assert_contains "scripts/badb --help lists Commands & Options" "Commands & Options:" "$out_help"
assert_contains "scripts/badb --help mentions geis" "geis" "$out_help"
assert_contains "scripts/badb --help mentions mcp-filetree" "mcp-filetree" "$out_help"

# Test -h
out_short_h=$(./scripts/badb -h 2>&1)
assert_contains "scripts/badb -h displays Usage" "Usage: badb" "$out_short_h"

# Test geis subcommand help
out_geis_help=$(./scripts/badb geis help 2>&1)
rc_geis_help=$?
assert_eq "scripts/badb geis help exits 0" "0" "$rc_geis_help"
assert_contains "scripts/badb geis help mentions status" "status" "$out_geis_help"

# Test geis status command
out_geis_status=$(./scripts/badb geis status 2>&1)
rc_geis_status=$?
assert_eq "scripts/badb geis status exits 0" "0" "$rc_geis_status"
assert_contains "scripts/badb geis status reports Locked spec store" "Locked spec store" "$out_geis_status"

# Test unrecognized option fails closed
set +e
out_bad=$(./scripts/badb --non-existent-option 2>&1)
rc_bad=$?
set -e
assert_eq "scripts/badb with unrecognized option exits 1" "1" "$rc_bad"
assert_contains "scripts/badb reports unrecognized option in stderr" "unrecognized option" "$out_bad"

# -----------------------------------------------------------------------------
# 2. scripts/mcp-filetree entrypoint tests
# -----------------------------------------------------------------------------
echo "[2/4] Testing scripts/mcp-filetree entrypoint..."

# Test initialize handshake
init_request='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test-runner","version":"1.0"}}}'
out_mcp_init=$(printf '%s\n' "$init_request" | ./scripts/mcp-filetree 2>/dev/null)
assert_contains "scripts/mcp-filetree returns JSON-RPC 2.0 version" '"jsonrpc":"2.0"' "$out_mcp_init"
assert_contains "scripts/mcp-filetree returns filetree serverInfo name" '"name":"filetree"' "$out_mcp_init"

# Test tools/list request
tools_request='{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
out_mcp_tools=$(printf '%s\n%s\n' "$init_request" "$tools_request" | ./scripts/mcp-filetree 2>/dev/null)
assert_contains "scripts/mcp-filetree tools/list includes 'tree' tool" '"name":"tree"' "$out_mcp_tools"
assert_contains "scripts/mcp-filetree tools/list includes 'find' tool" '"name":"find"' "$out_mcp_tools"
assert_contains "scripts/mcp-filetree tools/list includes 'read' tool" '"name":"read"' "$out_mcp_tools"

# Test tools/call 'find'
find_call='{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"find","arguments":{"pattern":"manifest.scm"}}}'
out_mcp_call=$(printf '%s\n%s\n' "$init_request" "$find_call" | ./scripts/mcp-filetree 2>/dev/null)
assert_contains "scripts/mcp-filetree tools/call returns matched file" 'manifest.scm' "$out_mcp_call"

# -----------------------------------------------------------------------------
# 3. scripts/test entrypoint tests
# -----------------------------------------------------------------------------
echo "[3/4] Testing scripts/test entrypoint..."

tmp_test_out=$(mktemp /tmp/badb-test-cli-XXXXXX.log)
./scripts/test > "$tmp_test_out" 2>&1
rc_test_runner=$?

assert_eq "scripts/test exits 0" "0" "$rc_test_runner"
assert_file_contains "scripts/test runs bootstrap test suite" "Starting badb Bootstrap Test Suite" "$tmp_test_out"
assert_file_contains "scripts/test reports 0 failures" "Test Run Complete: 0 failure(s)" "$tmp_test_out"
assert_file_contains "scripts/test finishes with expected 1842 passes" "# of expected passes    : 1842" "$tmp_test_out"
rm -f "$tmp_test_out"

# -----------------------------------------------------------------------------
# 4. Summary & Exit
# -----------------------------------------------------------------------------
echo "[4/4] CLI Entrypoints E2E Summary:"
echo "  Total Passed: $PASS_COUNT"
echo "  Total Failed: $FAIL_COUNT"

if [ "$FAIL_COUNT" -gt 0 ]; then
  echo "FAIL: CLI entrypoint test suite had $FAIL_COUNT failure(s)." >&2
  exit 1
fi

echo "SUCCESS: All CLI and script entrypoint tests passed."
exit 0
